package main

type WeatherForcast struct {
	CurrentCondition []CurrentCondition `json:"current_condition"`
	NearestArea      []NearestArea      `json:"nearest_area"`
	Request          []Request          `json:"request"`
	Weather          []Weather          `json:"weather"`
}
type WeatherDesc struct {
	Value string `json:"value"`
}
type WeatherIconURL struct {
	Value string `json:"value"`
}
type CurrentCondition struct {
	FeelsLikeC       string           `json:"FeelsLikeC"`
	FeelsLikeF       string           `json:"FeelsLikeF"`
	Cloudcover       string           `json:"cloudcover"`
	Humidity         string           `json:"humidity"`
	LocalObsDateTime string           `json:"localObsDateTime"`
	ObservationTime  string           `json:"observation_time"`
	PrecipInches     string           `json:"precipInches"`
	PrecipMM         string           `json:"precipMM"`
	Pressure         string           `json:"pressure"`
	PressureInches   string           `json:"pressureInches"`
	TempC            string           `json:"temp_C"`
	TempF            string           `json:"temp_F"`
	UvIndex          string           `json:"uvIndex"`
	Visibility       string           `json:"visibility"`
	VisibilityMiles  string           `json:"visibilityMiles"`
	WeatherCode      string           `json:"weatherCode"`
	WeatherDesc      []WeatherDesc    `json:"weatherDesc"`
	WeatherIconURL   []WeatherIconURL `json:"weatherIconUrl"`
	Winddir16Point   string           `json:"winddir16Point"`
	WinddirDegree    string           `json:"winddirDegree"`
	WindspeedKmph    string           `json:"windspeedKmph"`
	WindspeedMiles   string           `json:"windspeedMiles"`
}
type AreaName struct {
	Value string `json:"value"`
}
type Country struct {
	Value string `json:"value"`
}
type Region struct {
	Value string `json:"value"`
}
type WeatherURL struct {
	Value string `json:"value"`
}
type NearestArea struct {
	AreaName   []AreaName   `json:"areaName"`
	Country    []Country    `json:"country"`
	Latitude   string       `json:"latitude"`
	Longitude  string       `json:"longitude"`
	Population string       `json:"population"`
	Region     []Region     `json:"region"`
	WeatherURL []WeatherURL `json:"weatherUrl"`
}
type Request struct {
	Query string `json:"query"`
	Type  string `json:"type"`
}
type Astronomy struct {
	MoonIllumination string `json:"moon_illumination"`
	MoonPhase        string `json:"moon_phase"`
	Moonrise         string `json:"moonrise"`
	Moonset          string `json:"moonset"`
	Sunrise          string `json:"sunrise"`
	Sunset           string `json:"sunset"`
}
type Hourly struct {
	DewPointC        string           `json:"DewPointC"`
	DewPointF        string           `json:"DewPointF"`
	FeelsLikeC       string           `json:"FeelsLikeC"`
	FeelsLikeF       string           `json:"FeelsLikeF"`
	HeatIndexC       string           `json:"HeatIndexC"`
	HeatIndexF       string           `json:"HeatIndexF"`
	WindChillC       string           `json:"WindChillC"`
	WindChillF       string           `json:"WindChillF"`
	WindGustKmph     string           `json:"WindGustKmph"`
	WindGustMiles    string           `json:"WindGustMiles"`
	Chanceoffog      string           `json:"chanceoffog"`
	Chanceoffrost    string           `json:"chanceoffrost"`
	Chanceofhightemp string           `json:"chanceofhightemp"`
	Chanceofovercast string           `json:"chanceofovercast"`
	Chanceofrain     string           `json:"chanceofrain"`
	Chanceofremdry   string           `json:"chanceofremdry"`
	Chanceofsnow     string           `json:"chanceofsnow"`
	Chanceofsunshine string           `json:"chanceofsunshine"`
	Chanceofthunder  string           `json:"chanceofthunder"`
	Chanceofwindy    string           `json:"chanceofwindy"`
	Cloudcover       string           `json:"cloudcover"`
	Humidity         string           `json:"humidity"`
	PrecipInches     string           `json:"precipInches"`
	PrecipMM         string           `json:"precipMM"`
	Pressure         string           `json:"pressure"`
	PressureInches   string           `json:"pressureInches"`
	TempC            string           `json:"tempC"`
	TempF            string           `json:"tempF"`
	Time             string           `json:"time"`
	UvIndex          string           `json:"uvIndex"`
	Visibility       string           `json:"visibility"`
	VisibilityMiles  string           `json:"visibilityMiles"`
	WeatherCode      string           `json:"weatherCode"`
	WeatherDesc      []WeatherDesc    `json:"weatherDesc"`
	WeatherIconURL   []WeatherIconURL `json:"weatherIconUrl"`
	Winddir16Point   string           `json:"winddir16Point"`
	WinddirDegree    string           `json:"winddirDegree"`
	WindspeedKmph    string           `json:"windspeedKmph"`
	WindspeedMiles   string           `json:"windspeedMiles"`
}

type Weather struct {
	Astronomy   []Astronomy `json:"astronomy"`
	AvgtempC    string      `json:"avgtempC"`
	AvgtempF    string      `json:"avgtempF"`
	Date        string      `json:"date"`
	Hourly      []Hourly    `json:"hourly"`
	MaxtempC    string      `json:"maxtempC"`
	MaxtempF    string      `json:"maxtempF"`
	MintempC    string      `json:"mintempC"`
	MintempF    string      `json:"mintempF"`
	SunHour     string      `json:"sunHour"`
	TotalSnowCm string      `json:"totalSnow_cm"`
	UvIndex     string      `json:"uvIndex"`
}

// func GetWeather
