pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Effects
import QtQuick.Layouts
import QtQuick.Shapes
import Quickshell
import qs.components
import qs.services
import qs.config

Item {
    id: root

    required property ShellScreen screen

    readonly property bool onSpecial: (Config.bar.workspaces.perMonitorWorkspaces ? Hypr.monitorFor(screen) : Hypr.focusedMonitor)?.lastIpcObject.specialWorkspace?.name !== ""
    readonly property int activeWsId: Config.bar.workspaces.perMonitorWorkspaces ? (Hypr.monitorFor(screen).activeWorkspace?.id ?? 1) : Hypr.activeWsId
    readonly property bool isFocusedMonitor: Hypr.focusedMonitor === Hypr.monitorFor(screen)

    readonly property var occupied: {
        const occ = {};
        for (const ws of Hypr.workspaces.values)
            occ[ws.id] = ws.lastIpcObject.windows > 0;
        return occ;
    }
    readonly property int groupOffset: Math.floor((activeWsId - 1) / Config.bar.workspaces.shown) * Config.bar.workspaces.shown

    property real blur: onSpecial ? 1 : 0

    readonly property var monitorColors: [
        Colours.palette.m3primary,
        Colours.palette.m3secondary,
        Colours.palette.m3tertiary,
    ]

    readonly property var otherMonitors: {
        const thisMonName = Hypr.monitorFor(screen)?.name ?? "";
        return Hypr.monitors.values.filter(m => m.name !== thisMonName);
    }

    readonly property var otherMonitorWs: {
        const map = {};
        for (const mon of root.otherMonitors) {
            const wsId = mon.activeWorkspace?.id;
            const monIdx = Hypr.monitors.values.indexOf(mon);
            if (wsId)
                map[wsId] = root.monitorColors[monIdx % root.monitorColors.length] ?? Colours.palette.m3secondary;
        }
        return map;
    }

    implicitWidth: Config.bar.sizes.innerWidth
    implicitHeight: layout.implicitHeight + Appearance.padding.normal * 2

    // Bark trunk background — full width, organic feel
    Rectangle {
        id: barkBg
        anchors.fill: parent
        radius: Appearance.rounding.normal
        gradient: Gradient {
            GradientStop { position: 0.0; color: "#3d2b1f" }
            GradientStop { position: 0.15; color: "#4a3425" }
            GradientStop { position: 0.5; color: "#503a2a" }
            GradientStop { position: 0.85; color: "#4a3425" }
            GradientStop { position: 1.0; color: "#5c4333" }
        }

        // Bark grain lines
        Repeater {
            model: 5

            Rectangle {
                required property int index
                x: 3 + index * (barkBg.width - 6) / 5 + (index % 2 ? 2 : -1)
                y: index * 30 + 10
                width: 1
                height: barkBg.height - index * 60 - 20
                color: "#5c4333"
                opacity: 0.4
                radius: 0.5
            }
        }

        // Knots on trunk
        Repeater {
            model: 3

            Rectangle {
                required property int index
                x: 8 + (index % 2) * (barkBg.width - 22)
                y: barkBg.height * (0.2 + index * 0.3)
                width: 6
                height: 6
                radius: 3
                color: "#3d2b1f"
                border.width: 1
                border.color: "#5c4333"
            }
        }
    }

    // Focused monitor glow
    Rectangle {
        visible: root.isFocusedMonitor
        anchors.fill: parent
        radius: Appearance.rounding.normal
        color: "transparent"
        border.width: 1.5
        border.color: "#8b7355"
        opacity: 0.5
    }

    Item {
        anchors.fill: parent
        scale: root.onSpecial ? 0.8 : 1
        opacity: root.onSpecial ? 0.5 : 1

        layer.enabled: root.blur > 0
        layer.effect: MultiEffect {
            blurEnabled: true
            blur: root.blur
            blurMax: 32
        }

        ColumnLayout {
            id: layout

            anchors.centerIn: parent
            spacing: Appearance.spacing.normal

            Repeater {
                id: workspaces

                model: Config.bar.workspaces.shown

                Workspace {
                    activeWsId: root.activeWsId
                    occupied: root.occupied
                    groupOffset: root.groupOffset
                    otherMonitorWs: root.otherMonitorWs
                }
            }
        }

        MouseArea {
            anchors.fill: layout
            onClicked: event => {
                const ws = (layout.childAt(event.x, event.y) as Workspace)?.ws;
                if (!ws) return;
                const monName = Hypr.monitorFor(root.screen)?.name ?? "";
                if (Hypr.activeWsId !== ws) {
                    Hypr.dispatch(`moveworkspacetomonitor ${ws} ${monName}`);
                    Hypr.dispatch(`workspace ${ws}`);
                } else {
                    Hypr.dispatch("togglespecialworkspace special");
                }
            }
        }

        Behavior on scale {
            Anim {}
        }

        Behavior on opacity {
            Anim {}
        }
    }

    Loader {
        id: specialWs

        asynchronous: true
        anchors.fill: parent
        anchors.margins: Appearance.padding.small
        active: opacity > 0
        scale: root.onSpecial ? 1 : 0.5
        opacity: root.onSpecial ? 1 : 0

        sourceComponent: SpecialWorkspaces {
            screen: root.screen
        }

        Behavior on scale { Anim {} }
        Behavior on opacity { Anim {} }
    }

    Behavior on blur {
        Anim { duration: Appearance.anim.durations.small }
    }
}
