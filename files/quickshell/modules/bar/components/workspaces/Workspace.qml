pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import QtQuick.Shapes
import Quickshell
import Quickshell.Widgets
import qs.components
import qs.services
import qs.config
import qs.utils

Item {
    id: root

    required property int index
    required property int activeWsId
    required property var occupied
    required property int groupOffset
    required property var otherMonitorWs
    required property real trunkX
    required property real trunkWidth
    required property color barkColor

    readonly property bool isWorkspace: true
    readonly property int size: implicitHeight

    readonly property int ws: groupOffset + index + 1
    readonly property bool isOccupied: occupied[ws] ?? false
    readonly property bool hasWindows: isOccupied && Config.bar.workspaces.showWindows
    readonly property var otherMonOnThis: otherMonitorWs[ws] ?? null

    // Alternate sloths left/right of trunk
    readonly property bool onLeft: index % 2 === 0
    readonly property real slothSize: (Config.bar.sizes.innerWidth - root.trunkWidth) * 0.85
    readonly property real branchY: slothSize * 0.15

    Layout.alignment: Qt.AlignHCenter
    Layout.preferredHeight: size

    implicitWidth: Config.bar.sizes.innerWidth
    implicitHeight: slothSize + (hasWindows ? windowLoader.implicitHeight + 2 : 0)

    // Branch — a line from trunk to sloth
    Shape {
        width: parent.width
        height: parent.height
        preferredRendererType: Shape.CurveRenderer

        ShapePath {
            strokeWidth: 2.5
            strokeColor: root.barkColor
            fillColor: "transparent"

            startX: root.trunkX + root.trunkWidth / 2
            startY: root.branchY + 2

            PathQuad {
                x: root.onLeft ? slothItem.x + slothItem.width * 0.6 : slothItem.x + slothItem.width * 0.4
                y: root.branchY
                controlX: root.onLeft ? root.trunkX - 2 : root.trunkX + root.trunkWidth + 2
                controlY: root.branchY + 6
            }
        }
    }

    // Trunk knot for active workspace
    Rectangle {
        visible: root.activeWsId === root.ws
        x: root.trunkX - 2
        y: root.branchY - 3
        width: root.trunkWidth + 4
        height: root.trunkWidth + 4
        radius: width / 2
        color: Colours.palette.m3primaryContainer
        opacity: 0.8

        Behavior on opacity {
            Anim {}
        }

        scale: root.activeWsId === root.ws ? 1 : 0

        Behavior on scale {
            Anim {
                easing.bezierCurve: Appearance.anim.curves.standardDecel
            }
        }
    }

    // Sloth image
    Item {
        id: slothItem

        x: root.onLeft ? 0 : parent.width - width
        y: 0
        width: root.slothSize
        height: root.slothSize

        Image {
            id: indicator

            readonly property int slothNum: Math.min(root.ws, 8)

            anchors.fill: parent
            source: `images/sloth${slothNum}.png`
            fillMode: Image.PreserveAspectFit
            smooth: true
            mirror: !root.onLeft
            opacity: root.activeWsId === root.ws ? 1.0 : root.isOccupied ? 0.7 : 0.3

            Behavior on opacity {
                Anim {}
            }
        }

        // Other monitor dot
        Rectangle {
            visible: root.otherMonOnThis !== null
            anchors.right: parent.right
            anchors.top: parent.top
            anchors.margins: 2
            width: 6
            height: 6
            radius: 3
            color: root.otherMonOnThis ?? "transparent"
        }
    }

    // App icons as small "fruits" below the branch
    Loader {
        id: windowLoader

        asynchronous: true

        x: root.onLeft ? 2 : parent.width - width - 2
        y: slothItem.y + slothItem.height + 2

        visible: active
        active: root.hasWindows

        sourceComponent: Row {
            spacing: 1

            Repeater {
                model: ScriptModel {
                    values: {
                        const ws = root.ws;
                        const windows = Hypr.toplevels.values.filter(c => c.workspace?.id === ws).map(c => c.lastIpcObject?.class ?? "");
                        const maxIcons = Config.bar.workspaces.maxWindowIcons;
                        return maxIcons > 0 ? windows.slice(0, maxIcons) : windows;
                    }
                }

                IconImage {
                    required property var modelData

                    implicitSize: Config.bar.sizes.innerWidth / 3.5
                    source: Icons.getAppIcon(modelData ?? "", "application-x-executable")
                    asynchronous: true
                }
            }
        }
    }
}
