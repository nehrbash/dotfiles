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

    readonly property real rounding: Config.border.rounding
    readonly property real contentHeight: layout.implicitHeight + Appearance.padding.small * 2
    readonly property color containerColor: root.isFocusedMonitor ? Colours.palette.m3surfaceContainerHigh : Colours.palette.m3surfaceContainer

    implicitWidth: Config.bar.sizes.innerWidth + Appearance.padding.small
    implicitHeight: contentHeight + rounding * 2

    // Shape background — modeled on popout Background.qml
    // Inverse curves on LEFT connecting to bar edge, normal corners on RIGHT
    Shape {
        anchors.fill: parent
        preferredRendererType: Shape.CurveRenderer

        ShapePath {
            strokeWidth: -1
            fillColor: root.containerColor

            // Start above content area on the left edge
            startX: 0
            startY: 0

            // Top-left inverse curve: right and down into content
            PathArc {
                relativeX: root.rounding
                relativeY: root.rounding
                radiusX: root.rounding
                radiusY: root.rounding
                direction: PathArc.Counterclockwise
            }
            // Top edge
            PathLine {
                relativeX: root.implicitWidth - root.rounding * 2
                relativeY: 0
            }
            // Top-right normal corner
            PathArc {
                relativeX: root.rounding
                relativeY: root.rounding
                radiusX: root.rounding
                radiusY: root.rounding
            }
            // Right edge
            PathLine {
                relativeX: 0
                relativeY: root.contentHeight - root.rounding * 2
            }
            // Bottom-right normal corner
            PathArc {
                relativeX: -root.rounding
                relativeY: root.rounding
                radiusX: root.rounding
                radiusY: root.rounding
            }
            // Bottom edge
            PathLine {
                relativeX: -(root.implicitWidth - root.rounding * 2)
                relativeY: 0
            }
            // Bottom-left inverse curve: left and down away from content
            PathArc {
                relativeX: -root.rounding
                relativeY: root.rounding
                radiusX: root.rounding
                radiusY: root.rounding
                direction: PathArc.Counterclockwise
            }
            // Left edge closes implicitly back to (0, 0)

            Behavior on fillColor {
                CAnim {}
            }
        }
    }

    // Content area
    Item {
        id: contentArea

        x: 0
        y: root.rounding
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
