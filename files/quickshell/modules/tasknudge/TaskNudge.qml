pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Wayland
import qs.components
import qs.components.containers
import qs.components.controls
import qs.services
import qs.config

Scope {
    id: root

    property bool deferred
    readonly property bool shouldShow: Tasks.pomodoro.task !== undefined && !Tasks.clockedIn && !Tasks.onBreak && !deferred

    // Re-arm deferral after 5 minutes
    Timer {
        interval: 5 * 60 * 1000
        running: root.deferred
        onTriggered: root.deferred = false
    }

    Connections {
        function onSnapshotUpdated(): void {
            if (Tasks.clockedIn)
                root.deferred = false;
        }

        target: Tasks
    }

    LazyLoader {
        id: loader

        active: root.shouldShow

        Variants {
            model: Quickshell.screens

            StyledWindow {
                id: win

                required property ShellScreen modelData

                screen: modelData
                name: "task-nudge"
                WlrLayershell.layer: WlrLayer.Overlay
                WlrLayershell.keyboardFocus: WlrKeyboardFocus.Exclusive
                WlrLayershell.exclusionMode: ExclusionMode.Ignore

                anchors.top: true
                anchors.bottom: true
                anchors.left: true
                anchors.right: true

                Rectangle {
                    anchors.fill: parent
                    color: Qt.rgba(0, 0, 0, 0.55)

                    MouseArea {
                        anchors.fill: parent
                    }

                    StyledRect {
                        anchors.centerIn: parent
                        implicitWidth: Math.min(720, parent.width - 80)
                        implicitHeight: card.implicitHeight + Appearance.padding.large * 2
                        radius: Appearance.rounding.large
                        color: Colours.palette.m3surfaceContainer
                        focus: true

                        Keys.onEscapePressed: root.deferred = true

                        ColumnLayout {
                            id: card

                            anchors.fill: parent
                            anchors.margins: Appearance.padding.large
                            spacing: Appearance.spacing.normal

                            RowLayout {
                                spacing: Appearance.spacing.normal

                                MaterialIcon {
                                    text: "schedule"
                                    font.pointSize: Appearance.font.size.extraLarge
                                    color: Colours.palette.m3primary
                                }

                                ColumnLayout {
                                    Layout.fillWidth: true
                                    spacing: 0

                                    StyledText {
                                        text: qsTr("Not clocked in")
                                        font.pointSize: Appearance.font.size.large
                                        font.weight: 500
                                    }

                                    StyledText {
                                        text: qsTr("Pick a task to start a timer, or press Esc to defer 5 min.")
                                        color: Colours.palette.m3outline
                                        wrapMode: Text.Wrap
                                        Layout.fillWidth: true
                                    }
                                }
                            }

                            Repeater {
                                model: {
                                    const out = [];
                                    for (let i = 0; i < Tasks.tasks.length && out.length < 6; i++) {
                                        const t = Tasks.tasks[i];
                                        if (t.state !== "PROJECT")
                                            out.push(t);
                                    }
                                    return out;
                                }

                                delegate: StyledRect {
                                    required property var modelData

                                    Layout.fillWidth: true
                                    radius: Appearance.rounding.small
                                    color: ma.containsMouse ? Colours.layer(Colours.palette.m3surfaceContainer, 2) : "transparent"
                                    implicitHeight: row.implicitHeight + Appearance.padding.normal * 2

                                    MouseArea {
                                        id: ma

                                        anchors.fill: parent
                                        hoverEnabled: true
                                        cursorShape: Qt.PointingHandCursor
                                        onClicked: {
                                            Tasks.clockIn(modelData.title);
                                            root.deferred = false;
                                        }
                                    }

                                    RowLayout {
                                        id: row

                                        anchors.left: parent.left
                                        anchors.right: parent.right
                                        anchors.verticalCenter: parent.verticalCenter
                                        anchors.leftMargin: Appearance.padding.normal
                                        anchors.rightMargin: Appearance.padding.normal
                                        spacing: Appearance.spacing.small

                                        StyledText {
                                            text: modelData.state
                                            font.pointSize: Appearance.font.size.small
                                            font.weight: 500
                                            Layout.preferredWidth: 96
                                            color: Colours.palette.m3primary
                                        }

                                        StyledText {
                                            Layout.fillWidth: true
                                            text: modelData.title
                                            elide: Text.ElideRight
                                        }
                                    }
                                }
                            }

                            RowLayout {
                                Layout.fillWidth: true
                                Layout.topMargin: Appearance.spacing.normal

                                Item {
                                    Layout.fillWidth: true
                                }

                                StyledRect {
                                    radius: Appearance.rounding.full
                                    color: deferMa.containsMouse ? Colours.layer(Colours.palette.m3surfaceContainer, 2) : "transparent"
                                    border.color: Colours.palette.m3outline
                                    border.width: 1
                                    implicitHeight: deferLabel.implicitHeight + Appearance.padding.normal * 2
                                    implicitWidth: deferLabel.implicitWidth + Appearance.padding.large * 2

                                    MouseArea {
                                        id: deferMa

                                        anchors.fill: parent
                                        hoverEnabled: true
                                        cursorShape: Qt.PointingHandCursor
                                        onClicked: root.deferred = true
                                    }

                                    StyledText {
                                        id: deferLabel

                                        anchors.centerIn: parent
                                        text: qsTr("Defer 5 min (Esc)")
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
