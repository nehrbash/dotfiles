package main

import (
	"fmt"
	"os"
	"strings"
)

const (
	iconLoc    string = "/usr/share/icons/Dracula/scalable/apps"
	defaultUrl string = "/usr/share/icons/Dracula/scalable/apps/default-application.svg"
)

var iconMap map[string]string

func init() {
	iconMap = make(map[string]string)
	// add any manual mappings of class name to icons
	iconMap["Firefox Beta"] = "/usr/share/icons/Dracula/scalable/apps/firefox.svg"
	iconMap["Electron"] = "/usr/share/icons/Dracula/scalable/apps/discord.svg"

}

func GetIcon(className string) string {
	if iconUrl, ok := iconMap[className]; ok {
		return iconUrl
	}
	// try to find and add to map
	// TODO(SN): fuzzy search?
	path := fmt.Sprintf("%s/%s.svg", iconLoc, strings.ToLower(className))
	if _, err := os.Stat(path); err == nil { // exists
		iconMap[className] = path
		return path
	}
	// use default
	iconMap[className] = defaultUrl
	return defaultUrl
}
