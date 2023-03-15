package main

import (
	"bufio"
	"bytes"
	"encoding/json"
	"fmt"
	"log"
	"net"
	"net/textproto"
	"os"
	"os/exec"
	"sort"
	"strings"
	"time"
)

type Clients []Client
type Client struct {
	Address   string `json:"address"`
	Mapped    bool   `json:"mapped"`
	Hidden    bool   `json:"hidden"`
	At        []int  `json:"at"`
	Size      []int  `json:"size"`
	Workspace struct {
		ID   int    `json:"id"`
		Name string `json:"name"`
	} `json:"workspace"`
	Floating       bool   `json:"floating"`
	Monitor        int    `json:"monitor"`
	Class          string `json:"class"`
	Title          string `json:"title"`
	Pid            int    `json:"pid"`
	Xwayland       bool   `json:"xwayland"`
	Pinned         bool   `json:"pinned"`
	Fullscreen     bool   `json:"fullscreen"`
	FullscreenMode int    `json:"fullscreenMode"`
	FakeFullscreen bool   `json:"fakeFullscreen"`
	Grouped        []any  `json:"grouped"`
	Swallowing     any    `json:"swallowing"`
}

type App struct {
	ID      int
	Title   string
	Class   string
	Color   string
	Address string
	Icon    string
	updated bool
	Focused bool
}

var activeColor string = "active"

type Apps struct {
	lookup map[string]*App
}

func NewApps() Apps { return Apps{lookup: make(map[string]*App)} }

type AppList []App

func (a AppList) Len() int           { return len(a) }
func (a AppList) Swap(i, j int)      { a[i], a[j] = a[j], a[i] }
func (a AppList) Less(i, j int) bool { return a[i].ID < a[j].ID }

func (a AppList) String() string {
	json, err := json.Marshal(a)
	if err != nil {
		return ""
	}
	return string(json)
}

var AddCnt int

func (a *Apps) Update(c Clients) (alist AppList) {

	// mark all list as not updated
	for _, a2 := range a.lookup {
		a2.updated = false
	}
	for _, window := range c {
		if window.Workspace.ID < 0 {
			continue
		}
		if window.Address == "" {
			continue
		}
		if _, ok := a.lookup[window.Address]; ok {
			a.lookup[window.Address].updated = true
			a.lookup[window.Address].Focused = false
		} else { // add
			a.lookup[window.Address] = &App{
				ID:      AddCnt,
				Color:   activeColor,
				Title:   window.Title,
				Class:   window.Class,
				Address: window.Address,
				Icon:    GetIcon(window.Class),
				Focused: false,
				updated: true,
			}
			AddCnt++
		}
	}
	// mark focused
	focused := GetFocusedAddress()
	if app, ok := a.lookup[focused]; ok {
		app.Focused = true
	}

	// delete all not updated and append to list for display
	for k, a2 := range a.lookup {
		if !a2.updated {
			delete(a.lookup, k)
		} else {
			alist = append(alist, *a2)
		}
	}
	sort.Sort(alist)
	return
}

func GetFocusedAddress() string {
	var outb, errb bytes.Buffer
	var client Client
	cmd := exec.Command("/usr/bin/hyprctl", "activewindow", "-j")
	cmd.Stdout = &outb
	cmd.Stderr = &errb
	err := cmd.Run()
	if err != nil {
		log.Println(err)
	}
	if err := json.Unmarshal(outb.Bytes(), &client); err != nil {
		log.Println(err)
	}
	return client.Address
}

func PrintApps(windowEvent chan string) {
	var outb, errb bytes.Buffer
	var clients Clients
	var apps = NewApps()

	for {
		line := <-windowEvent
		eventMsg := strings.Split(line, ">>")
		if len(eventMsg) != 2 {
			continue
		}
		switch event := eventMsg[0]; event {
		case "activewindow", "closewindow":
			if event == "closewindow" {
				// TODO does not remove from list without delay
				time.Sleep(time.Second)
			}
			cmd := exec.Command("/usr/bin/hyprctl", "clients", "-j")
			cmd.Stdout = &outb
			cmd.Stderr = &errb
			err := cmd.Run()
			if err != nil {
				log.Println(err)
			}
			if err := json.Unmarshal(outb.Bytes(), &clients); err != nil {
				log.Println(err)
			}
			applist := apps.Update(clients)
			fmt.Println(applist.String())
			outb.Reset()
			errb.Reset()
		}
	}
}

// HyprListen sends the
func HyprListen(hyprEvent chan string) {
	c, err := net.Dial("unix", os.ExpandEnv("/tmp/hypr/$HYPRLAND_INSTANCE_SIGNATURE/.socket2.sock"))
	if err != nil {
		panic(err)
	}
	defer c.Close()
	reader := bufio.NewReader(c)
	lreader := textproto.NewReader(reader)
	for {
		line, err := lreader.ReadLine()
		if err != nil {
			continue
		}
		out := string(line)
		hyprEvent <- out
	}
}

func main() {
	hhypr := make(chan string)
	go HyprListen(hhypr)
	PrintApps(hhypr)
}
