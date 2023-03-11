package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"strings"
	"time"
)

type EwwVariables struct {
	Quote    string      `json:"quote"`
	SubQuote string      `json:"subQuote"`
	Moons    [8]Moon     `json:"moons"`
	Day1     DayOverview `json:"day1"`
	Day2     DayOverview `json:"day2"`
	Day3     DayOverview `json:"day3"`
}

type DayOverview struct {
	Max   string `json:"max"`
	Min   string `json:"min"`
	Icon  string `json:"icon"`
	Desc  string `json:"desc"`
	Month string `json:"month"`
	Day   string `json:"day"`
}

type Moon struct {
	Class string `json:"class"`
	Value string `json:"value"`
}

// just panic if out of range
func NewDay(w *WeatherForcast, day int) DayOverview {
	date := strings.Split(w.Weather[day].Date, "-") // "2023-03-07"
	return DayOverview{
		Max:   w.Weather[day].MaxtempF,
		Min:   w.Weather[day].MintempF,
		Icon:  Icon(w.Weather[day].Hourly[4].WeatherCode),
		Desc:  w.Weather[day].Hourly[4].WeatherDesc[0].Value,
		Month: date[1],
		Day:   date[2],
	}
}

// Only gives 3 day forcast
func NewEwwVariables(w WeatherForcast) EwwVariables {
	return EwwVariables{
		Quote:    "",
		SubQuote: "",
		Moons:    Moons(w.Weather[0].Astronomy[0].MoonPhase),
		Day1:     NewDay(&w, 0),
		Day2:     NewDay(&w, 1),
		Day3:     NewDay(&w, 2),
	}
}

func Moons(phase string) (moons [8]Moon) {
	cur := MoonIdx(phase)
	for i, m := range MOON_PHASES {
		if i == cur {
			moons[i] = Moon{
				Value: m,
				Class: "active-moon",
			}
		} else {
			moons[i] = Moon{
				Value: m,
			}
		}
	}
	return
}

func (e *EwwVariables) String() string {
	json, err := json.Marshal(e)
	if err != nil {
		log.Printf("Marshalling Weather Data: %s", err.Error())
		return ""
	}
	return string(json)
}

func GetWeatherData() WeatherForcast {
	resp, err := http.Get("http://wttr.in/?format=j1")
	if err != nil {
		log.Fatal(err)
	}
	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body) // response body is []byte

	var result WeatherForcast
	if err := json.Unmarshal(body, &result); err != nil { // Parse []byte tostruct
		log.Fatalf("Can not unmarshal JSON: %s", err.Error())
	}
	return result
}

// PrettyPrint to print struct in a readable way
func PrettyPrint(i interface{}) string {
	s, _ := json.MarshalIndent(i, "", "\t")
	return string(s)
}

func main() {
	for {
		RawWeather := GetWeatherData()
		ewwData := NewEwwVariables(RawWeather)
		fmt.Println(ewwData.String())
		time.Sleep(time.Minute * 25)
	}
}
