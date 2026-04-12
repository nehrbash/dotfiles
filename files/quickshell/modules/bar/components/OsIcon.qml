import QtQuick
import qs.components
import qs.services
import qs.config

Item {
    id: root

    implicitWidth: Config.bar.sizes.innerWidth * 0.7
    implicitHeight: implicitWidth

    MouseArea {
        anchors.fill: parent
        cursorShape: Qt.PointingHandCursor
        onClicked: {
            const visibilities = Visibilities.getForActive();
            visibilities.launcher = !visibilities.launcher;
        }
    }

    // Owl as launcher button
    Image {
        anchors.fill: parent
        source: "workspaces/images/owl.svg"
        fillMode: Image.PreserveAspectFit
        smooth: true
    }
}
