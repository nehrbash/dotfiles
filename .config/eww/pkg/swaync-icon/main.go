package main

import (
	"bufio"
	"encoding/json"
	"fmt"
	"log"
	"os/exec"
)

type state struct {
	Text    string
	Alt     string
	Tooltip string
	Class   interface{}
}

type outState struct {
	Icon    string
	Dnd     bool
	Visable bool
	Class   string
}

func main() {
	cmd := exec.Command("swaync-client", "-swb")

	pip, err := cmd.StdoutPipe()
	if err != nil {
		log.Fatal(err)
	}
	scanner := bufio.NewScanner(pip)

	if err := cmd.Start(); err != nil {
		log.Fatal(err)
	}
	var data state
	var res outState
	for scanner.Scan() {
		json.Unmarshal(scanner.Bytes(), &data)
		if data.Alt == "none" || data.Alt == "notification" {
			res.Dnd = false
			if data.Text != "0" {
				res.Icon = ""
			} else {
				res.Icon = ""
			}

		} else {
			res.Icon = ""
			res.Dnd = true
		}

		if _, ok := data.Class.(string); ok {
			res.Visable = false
		} else {
			res.Visable = true
		}

		if res.Visable {
			res.Class = "active"
		} else {
			res.Class = "inactive"
		}
		a, _ := json.Marshal(res)
		fmt.Println(string(a))
	}
}
