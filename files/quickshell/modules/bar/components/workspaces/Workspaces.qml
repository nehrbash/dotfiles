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

    readonly property color containerColor: root.isFocusedMonitor ? Colours.palette.m3surfaceContainerHigh : Colours.palette.m3surfaceContainer

    // Bark colors derived from theme
    readonly property color barkDark: Qt.darker(Colours.palette.m3surfaceContainerHighest, 1.1)
    readonly property color barkLight: Colours.palette.m3surfaceContainerHighest

    readonly property real trunkWidth: 10
    readonly property real trunkX: (implicitWidth - trunkWidth) / 2

    implicitWidth: Config.bar.sizes.innerWidth
    implicitHeight: layout.implicitHeight + Appearance.padding.normal * 2

    // Tree trunk — a narrow vertical strip with slight taper
    Shape {
        id: trunk

        anchors.fill: parent
        preferredRendererType: Shape.CurveRenderer

        ShapePath {
            strokeWidth: -1
            fillColor: root.barkDark

            // Trunk tapers: wider at bottom, narrower at top
            readonly property real topW: root.trunkWidth * 0.6
            readonly property real botW: root.trunkWidth
            readonly property real topX: (root.implicitWidth - topW) / 2
            readonly property real botX: (root.implicitWidth - botW) / 2

            startX: topX
            startY: 0

            PathLine { x: topX + topW; y: 0 }
            PathLine { x: botX + botW; y: root.implicitHeight }
            PathLine { x: botX; y: root.implicitHeight }
        }
    }

    // Trunk highlight for focused monitor
    Rectangle {
        x: root.trunkX - 1
        y: 0
        width: root.trunkWidth + 2
        height: parent.height
        radius: root.trunkWidth / 2
        color: "transparent"
        border.width: root.isFocusedMonitor ? 1 : 0
        border.color: Colours.palette.m3primary
        opacity: 0.5

        Behavior on border.width {
            Anim {}
        }
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
            spacing: Appearance.spacing.small

            Repeater {
                id: workspaces

                model: Config.bar.workspaces.shown

                Workspace {
                    activeWsId: root.activeWsId
                    occupied: root.occupied
                    groupOffset: root.groupOffset
                    otherMonitorWs: root.otherMonitorWs
                    trunkX: root.trunkX
                    trunkWidth: root.trunkWidth
                    barkColor: root.barkLight
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

        Behavior on scale {
            Anim {}
        }

        Behavior on opacity {
            Anim {}
        }
    }

    Behavior on blur {
        Anim {
            duration: Appearance.anim.durations.small
        }
    }
}
