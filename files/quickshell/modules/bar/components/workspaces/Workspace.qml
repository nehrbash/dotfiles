pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
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

    readonly property bool isWorkspace: true
    readonly property int size: implicitHeight + (hasWindows ? windowLoader.height + 2 : 0)

    readonly property int ws: groupOffset + index + 1
    readonly property bool isOccupied: occupied[ws] ?? false
    readonly property bool hasWindows: isOccupied && Config.bar.workspaces.showWindows
    readonly property var otherMonOnThis: otherMonitorWs[ws] ?? null
    readonly property bool isActive: activeWsId === ws

    readonly property real slothSize: Config.bar.sizes.innerWidth

    Layout.alignment: Qt.AlignHCenter
    Layout.preferredHeight: size

    implicitWidth: Config.bar.sizes.innerWidth
    implicitHeight: slothSize

    // Active workspace — warm carved glow
    Rectangle {
        anchors.fill: parent
        anchors.margins: 2
        radius: Appearance.rounding.small
        color: root.isActive ? "#6b4c2a" : "transparent"
        opacity: root.isActive ? 0.6 : 0
        border.width: root.isActive ? 1 : 0
        border.color: "#9b7b4a"

        Behavior on color { CAnim {} }
        Behavior on opacity { Anim {} }
    }

    // Sloth image — centered, full size
    Image {
        id: indicator

        readonly property int slothNum: Math.min(root.ws, 8)

        anchors.centerIn: parent
        width: root.slothSize
        height: root.slothSize
        source: `images/sloth${slothNum}.png`
        fillMode: Image.PreserveAspectFit
        smooth: true
        opacity: root.isActive ? 1.0 : root.isOccupied ? 0.75 : 0.3

        // Slight scale bump for active
        scale: root.isActive ? 1.05 : 1.0

        Behavior on opacity { Anim {} }
        Behavior on scale { Anim { easing.bezierCurve: Appearance.anim.curves.standardDecel } }
    }

    // Other monitor indicator — a colored leaf
    Image {
        visible: root.otherMonOnThis !== null
        anchors.right: parent.right
        anchors.top: parent.top
        anchors.margins: 2
        width: 12
        height: 12
        source: "images/leaf1.svg"
        rotation: 30
        opacity: 0.9
    }

    // App icons — small icons below sloth
    Loader {
        id: windowLoader

        asynchronous: true

        anchors.horizontalCenter: parent.horizontalCenter
        y: root.slothSize

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

    Behavior on Layout.preferredHeight {
        Anim {}
    }
}
