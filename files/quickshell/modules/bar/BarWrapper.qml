pragma ComponentBehavior: Bound

import QtQuick
import Quickshell
import qs.components
import qs.config
import qs.modules.bar.popouts as BarPopouts

Item {
    id: root

    required property ShellScreen screen
    required property DrawerVisibilities visibilities
    required property BarPopouts.Wrapper popouts
    required property bool disabled

    readonly property int clampedWidth: Math.max(Config.border.minThickness, implicitWidth)
    readonly property int padding: Math.max(Appearance.padding.smaller, Config.border.thickness)
    readonly property int contentWidth: Config.bar.sizes.innerWidth + padding * 2
    readonly property int exclusiveZone: !disabled && (Config.bar.persistent || visibilities.bar) ? contentWidth : Config.border.thickness
    readonly property bool shouldBeVisible: !disabled && (Config.bar.persistent || visibilities.bar || isHovered)
    property bool isHovered

    function closeTray(): void {
        (content.item as Bar)?.closeTray();
    }

    function checkPopout(y: real): void {
        (content.item as Bar)?.checkPopout(y);
    }

    function handleWheel(y: real, angleDelta: point): void {
        (content.item as Bar)?.handleWheel(y, angleDelta);
    }

    visible: width > Config.border.thickness
    implicitWidth: Config.border.thickness

    states: State {
        name: "visible"
        when: root.shouldBeVisible

        PropertyChanges {
            root.implicitWidth: root.contentWidth
        }
    }

    transitions: [
        Transition {
            from: ""
            to: "visible"

            Anim {
                target: root
                property: "implicitWidth"
                duration: Appearance.anim.durations.expressiveDefaultSpatial
                easing.bezierCurve: Appearance.anim.curves.expressiveDefaultSpatial
            }
        },
        Transition {
            from: "visible"
            to: ""

            Anim {
                target: root
                property: "implicitWidth"
                easing.bezierCurve: Appearance.anim.curves.emphasized
            }
        }
    ]

    Loader {
        id: content

        anchors.top: parent.top
        anchors.bottom: parent.bottom
        anchors.right: parent.right

        active: root.shouldBeVisible || root.visible

        sourceComponent: Bar {
            width: root.contentWidth
            screen: root.screen
            visibilities: root.visibilities
            popouts: root.popouts // qmllint disable incompatible-type
        }
    }

    // Tree decorations — all in BarWrapper to avoid breaking
    // Bar.qml's childAt() popout detection

    // Full-height bark background
    Image {
        anchors.fill: content
        source: "images/bark.svg"
        fillMode: Image.Stretch
        visible: root.visible
        opacity: 0.9
        z: -1
    }

    // Bark edge — left
    Image {
        source: "components/workspaces/images/bark-edge.svg"
        x: content.x - 3; y: 0
        width: 6; height: root.height
        fillMode: Image.Stretch
        visible: root.visible
        z: -1; opacity: 0.7
    }

    // Bark edge — right
    Image {
        source: "components/workspaces/images/bark-edge.svg"
        x: content.x + content.width - 3; y: 0
        width: 6; height: root.height
        fillMode: Image.Stretch
        mirror: true
        visible: root.visible
        z: -1; opacity: 0.7
    }

    // Canopy at top
    Image {
        source: "components/workspaces/images/canopy.svg"
        anchors.top: parent.top
        anchors.topMargin: -20
        anchors.horizontalCenter: content.horizontalCenter
        width: root.contentWidth * 3
        height: 70
        visible: root.visible
        z: 2
        opacity: 0.95
    }

    // Roots at bottom — z: -1 so they don't cover the power button
    Image {
        source: "components/workspaces/images/roots-large.svg"
        anchors.bottom: parent.bottom
        anchors.bottomMargin: -15
        anchors.horizontalCenter: content.horizontalCenter
        width: root.contentWidth * 3
        height: 55
        visible: root.visible
        z: -1
        opacity: 0.9
    }
}
