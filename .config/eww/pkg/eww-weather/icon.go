package main

var WWO_CODE = map[string]string{
	"113": "Sunny",
	"116": "PartlyCloudy",
	"119": "Cloudy",
	"122": "VeryCloudy",
	"143": "Fog",
	"176": "LightShowers",
	"179": "LightSleetShowers",
	"182": "LightSleet",
	"185": "LightSleet",
	"200": "ThunderyShowers",
	"227": "LightSnow",
	"230": "HeavySnow",
	"248": "Fog",
	"260": "Fog",
	"263": "LightShowers",
	"266": "LightRain",
	"281": "LightSleet",
	"284": "LightSleet",
	"293": "LightRain",
	"296": "LightRain",
	"299": "HeavyShowers",
	"302": "HeavyRain",
	"305": "HeavyShowers",
	"308": "HeavyRain",
	"311": "LightSleet",
	"314": "LightSleet",
	"317": "LightSleet",
	"320": "LightSnow",
	"323": "LightSnowShowers",
	"326": "LightSnowShowers",
	"329": "HeavySnow",
	"332": "HeavySnow",
	"335": "HeavySnowShowers",
	"338": "HeavySnow",
	"350": "LightSleet",
	"353": "LightShowers",
	"356": "HeavyShowers",
	"359": "HeavyRain",
	"362": "LightSleetShowers",
	"365": "LightSleetShowers",
	"368": "LightSnowShowers",
	"371": "HeavySnowShowers",
	"374": "LightSleetShowers",
	"377": "LightSleet",
	"386": "ThunderyShowers",
	"389": "ThunderyHeavyRain",
	"392": "ThunderySnowShowers",
	"395": "HeavySnowShowers",
}

var WEATHER_SYMBOL = map[string]string{
	"Unknown":             "✨",
	"Cloudy":              "☁️",
	"Fog":                 "🌫",
	"HeavyRain":           "🌧",
	"HeavyShowers":        "🌧",
	"HeavySnow":           "❄️",
	"HeavySnowShowers":    "❄️",
	"LightRain":           "🌦",
	"LightShowers":        "🌦",
	"LightSleet":          "🌧",
	"LightSleetShowers":   "🌧",
	"LightSnow":           "🌨",
	"LightSnowShowers":    "🌨",
	"PartlyCloudy":        "⛅️",
	"Sunny":               "☀️",
	"ThunderyHeavyRain":   "🌩",
	"ThunderyShowers":     "⛈",
	"ThunderySnowShowers": "⛈",
	"VeryCloudy":          "☁️",
}

var WEATHER_SYMBOL_WIDTH = map[string]int{
	"✨":  2,
	"☁️": 1,
	"🌫":  2,
	"🌧":  2,
	"❄️": 1,
	"🌦":  1,
	"🌨":  2,
	"⛅️": 2,
	"☀️": 1,
	"🌩":  2,
	"⛈":  1,
}

var MOON_PHASES = [8]string{
	"🌑", "🌒", "🌓", "🌔", "🌕", "🌖", "🌗", "🌘",
}

var MOON_PHASES_LOOKUP = map[string]int{
"New Moon": 0,
"Waxing Crescent": 1,
"First Quarter": 2,
"Waxing Gibbous": 3,
"Full Moon": 4,
"Waning Gibbous": 5,
"Last Quarter": 6,
"Waning Crescent": 7,
}


func MoonIdx(key string) int {
	return MOON_PHASES_LOOKUP[key]
}

func Icon(code string) string {
	sym := WEATHER_SYMBOL[WWO_CODE[code]]
	if width, ok := WEATHER_SYMBOL_WIDTH[sym]; ok {
		if width == 2 {
			sym += " "
		}
	}
	return sym
}
