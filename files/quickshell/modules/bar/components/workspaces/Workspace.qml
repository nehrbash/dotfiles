pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Widgets
import qs.components
import qs.services
import qs.config
import qs.utils

ColumnLayout {
    id: root

    required property int index
    required property int activeWsId
    required property var occupied
    required property int groupOffset

    readonly property bool isWorkspace: true // Flag for finding workspace children
    // Unanimated prop for others to use as reference
    readonly property int size: implicitHeight + (hasWindows ? Appearance.padding.small : 0)

    readonly property int ws: groupOffset + index + 1
    readonly property bool isOccupied: occupied[ws] ?? false
    readonly property bool hasWindows: isOccupied && Config.bar.workspaces.showWindows

    Layout.alignment: Qt.AlignHCenter
    Layout.preferredHeight: size

    spacing: 0

    Image {
        id: indicator

        readonly property int slothNum: Math.min(root.ws, 8)

        Layout.alignment: Qt.AlignHCenter | Qt.AlignTop
        Layout.preferredWidth: (Config.bar.sizes.innerWidth - Appearance.padding.small * 2) * 0.7
        Layout.preferredHeight: Layout.preferredWidth

        source: `images/sloth${slothNum}.png`
        fillMode: Image.PreserveAspectFit
        smooth: true
        opacity: root.activeWsId === root.ws ? 1.0 : root.isOccupied ? 0.7 : 0.3

        Behavior on opacity {
            Anim {}
        }
    }

    Loader {
        id: windows

        asynchronous: true

        Layout.alignment: Qt.AlignHCenter
        Layout.fillHeight: true
        Layout.topMargin: -Config.bar.sizes.innerWidth / 10

        visible: active
        active: root.hasWindows

        sourceComponent: Column {
            spacing: 0

            add: Transition {
                Anim {
                    properties: "scale"
                    from: 0
                    to: 1
                    easing.bezierCurve: Appearance.anim.curves.standardDecel
                }
            }

            move: Transition {
                Anim {
                    properties: "scale"
                    to: 1
                    easing.bezierCurve: Appearance.anim.curves.standardDecel
                }
                Anim {
                    properties: "x,y"
                }
            }

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

                    implicitSize: Config.bar.sizes.innerWidth / 2.5
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
