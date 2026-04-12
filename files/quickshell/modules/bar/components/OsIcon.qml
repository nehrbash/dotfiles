import QtQuick
import qs.components
import qs.services
import qs.config

Item {
    id: root

    implicitWidth: Config.bar.sizes.innerWidth * 0.8
    implicitHeight: implicitWidth * 1.2

    MouseArea {
        anchors.fill: parent
        cursorShape: Qt.PointingHandCursor
        onClicked: {
            const visibilities = Visibilities.getForActive();
            visibilities.launcher = !visibilities.launcher;
        }
    }

    // Tree hollow behind the owl
    Image {
        anchors.fill: parent
        anchors.margins: -4
        source: "workspaces/images/hollow.svg"
        fillMode: Image.Stretch
        opacity: 0.85
    }

    // Owl
    Image {
        anchors.centerIn: parent
        width: parent.width * 0.85
        height: parent.height * 0.85
        source: "workspaces/images/owl.svg"
        fillMode: Image.PreserveAspectFit
        smooth: true
    }
}
