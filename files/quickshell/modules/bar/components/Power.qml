import QtQuick
import qs.components
import qs.services
import qs.config

Item {
    id: root

    required property DrawerVisibilities visibilities

    implicitWidth: Config.bar.sizes.innerWidth * 0.55
    implicitHeight: implicitWidth

    StateLayer {
        function onClicked(): void {
            root.visibilities.session = !root.visibilities.session;
        }

        anchors.fill: undefined
        anchors.centerIn: parent
        implicitWidth: implicitHeight
        implicitHeight: root.implicitHeight + Appearance.padding.small * 2
        radius: Appearance.rounding.full
    }

    Image {
        anchors.centerIn: parent
        width: parent.width
        height: parent.height
        source: "workspaces/images/power-bark.svg"
        fillMode: Image.PreserveAspectFit
        smooth: true
    }
}
