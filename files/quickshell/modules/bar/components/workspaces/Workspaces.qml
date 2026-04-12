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

    readonly property real filletR: Config.border.rounding
    readonly property real contentHeight: layout.implicitHeight + Appearance.padding.small * 2
    readonly property color containerColor: root.isFocusedMonitor ? Colours.palette.m3surfaceContainerHigh : Colours.palette.m3surfaceContainer

    implicitWidth: Config.bar.sizes.innerWidth + Appearance.padding.small
    implicitHeight: contentHeight + filletR * 2

    // Shape background with inverse curves on the right side
    Shape {
        anchors.fill: parent
        preferredRendererType: Shape.CurveRenderer

        ShapePath {
            strokeWidth: -1
            fillColor: root.containerColor

            startX: 0
            startY: root.filletR

            // Top edge: left to right
            PathLine {
                x: root.implicitWidth - root.filletR
                y: root.filletR
            }
            // Top-right inverse curve: right and up
            PathArc {
                x: root.implicitWidth
                y: 0
                radiusX: root.filletR
                radiusY: root.filletR
                direction: PathArc.Clockwise
            }
            // Right edge: top to bottom
            PathLine {
                x: root.implicitWidth
                y: root.implicitHeight
            }
            // Bottom-right inverse curve: left and up
            PathArc {
                x: root.implicitWidth - root.filletR
                y: root.filletR + root.contentHeight
                radiusX: root.filletR
                radiusY: root.filletR
                direction: PathArc.Clockwise
            }
            // Bottom edge: right to left
            PathLine {
                x: 0
                y: root.filletR + root.contentHeight
            }
            // Left edge closes back to start

            Behavior on fillColor {
                CAnim {}
            }
        }
    }

    // Content area
    Item {
        id: contentArea

        x: 0
        y: root.filletR
        width: parent.width
        height: root.contentHeight
        clip: true

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

            Loader {
                asynchronous: true
                active: Config.bar.workspaces.occupiedBg

                anchors.fill: parent
                anchors.margins: Appearance.padding.small

                sourceComponent: OccupiedBg {
                    workspaces: workspaces
                    occupied: root.occupied
                    groupOffset: root.groupOffset
                }
            }

            ColumnLayout {
                id: layout

                anchors.top: parent.top
                anchors.topMargin: Appearance.padding.small
                anchors.horizontalCenter: parent.horizontalCenter
                spacing: Math.floor(Appearance.spacing.small / 2)

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

            Loader {
                asynchronous: true
                anchors.horizontalCenter: parent.horizontalCenter
                active: Config.bar.workspaces.activeIndicator

                sourceComponent: ActiveIndicator {
                    activeWsId: root.activeWsId
                    workspaces: workspaces
                    mask: layout
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
    }

    Behavior on blur {
        Anim {
            duration: Appearance.anim.durations.small
        }
    }
}
