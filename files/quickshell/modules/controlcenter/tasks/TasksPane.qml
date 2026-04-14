pragma ComponentBehavior: Bound

import ".."
import "../components"
import QtQuick
import QtQuick.Layouts
import Quickshell.Widgets
import qs.components
import qs.components.containers
import qs.components.controls
import qs.services
import qs.config

Item {
    id: root

    required property Session session

    anchors.fill: parent

    Component.onCompleted: {
        Tasks.refresh();
        Tasks.fetchReport("today");
        Tasks.fetchReport("thisweek");
    }

    SplitPaneLayout {
        anchors.fill: parent
        leftContent: leftColumn
        rightContent: rightColumn
    }

    Component {
        id: leftColumn

        StyledFlickable {
            id: leftFlick

            flickableDirection: Flickable.VerticalFlick
            contentHeight: leftLayout.implicitHeight

            StyledScrollBar.vertical: StyledScrollBar {
                flickable: leftFlick
            }

            ColumnLayout {
                id: leftLayout

                anchors.left: parent.left
                anchors.right: parent.right
                spacing: Appearance.spacing.normal

                StyledText {
                    text: qsTr("Tasks")
                    font.pointSize: Appearance.font.size.large
                    font.weight: 500
                }

                // Pomodoro / active-task card
                StyledRect {
                    Layout.fillWidth: true
                    color: Colours.layer(Colours.palette.m3surfaceContainer, 1)
                    radius: Appearance.rounding.normal
                    implicitHeight: pomoRow.implicitHeight + Appearance.padding.large * 2

                    RowLayout {
                        id: pomoRow

                        anchors.fill: parent
                        anchors.margins: Appearance.padding.large
                        spacing: Appearance.spacing.normal

                        Item {
                            Layout.preferredWidth: 80
                            Layout.preferredHeight: 80

                            CircularProgress {
                                anchors.fill: parent
                                strokeWidth: 6
                                value: (Tasks.pomodoro.percent ?? 0) / 100
                                fgColour: Tasks.onBreak ? Colours.palette.m3tertiary : Colours.palette.m3primary
                                bgColour: Colours.palette.m3secondaryContainer
                            }

                            StyledText {
                                anchors.centerIn: parent
                                text: Tasks.pomodoro.time ?? "00:00"
                                font.pointSize: Appearance.font.size.normal
                                font.weight: 500
                            }
                        }

                        ColumnLayout {
                            Layout.fillWidth: true
                            spacing: Appearance.spacing.smaller

                            StyledText {
                                Layout.fillWidth: true
                                text: Tasks.pomodoro.task ?? "No Active Task"
                                wrapMode: Text.Wrap
                                font.weight: 500
                            }

                            StyledText {
                                visible: Tasks.pomodoro.enabled ?? false
                                text: qsTr("%1 / %2 keystrokes")
                                    .arg(Tasks.pomodoro.keystrokes ?? 0)
                                    .arg(Tasks.pomodoro["keystrokes-target"] ?? 0)
                                color: Colours.palette.m3outline
                                font.pointSize: Appearance.font.size.small
                            }

                            RowLayout {
                                spacing: Appearance.spacing.small

                                PomoButton {
                                    icon: "pause"
                                    label: qsTr("Clock out")
                                    enabled: Tasks.clockedIn
                                    onActivated: Tasks.clockOut()
                                }

                                PomoButton {
                                    icon: "check_circle"
                                    label: qsTr("Done")
                                    enabled: Tasks.clockedIn
                                    onActivated: Tasks.markDone()
                                }

                                PomoButton {
                                    icon: (Tasks.pomodoro.enabled ?? false) ? "timer_off" : "timer"
                                    label: (Tasks.pomodoro.enabled ?? false) ? qsTr("Stop breaks") : qsTr("Start breaks")
                                    onActivated: Tasks.toggleTypeBreak()
                                }
                            }
                        }
                    }
                }

                // Filter toggle
                RowLayout {
                    Layout.fillWidth: true
                    spacing: Appearance.spacing.small

                    StyledText {
                        text: qsTr("Agenda [%1/%2]").arg(Tasks.visibleCount).arg(Tasks.totalCount)
                        font.weight: 500
                    }

                    Item {
                        Layout.fillWidth: true
                    }

                    FilterPill {
                        label: "work"
                        active: Tasks.filter === "work"
                        onActivated: Tasks.setFilter("work")
                    }

                    FilterPill {
                        label: "personal"
                        active: Tasks.filter === "personal"
                        onActivated: Tasks.setFilter("personal")
                    }
                }

                // Task list
                Repeater {
                    model: Tasks.tasks

                    delegate: StyledRect {
                        required property var modelData

                        Layout.fillWidth: true
                        radius: Appearance.rounding.small
                        color: taskMouse.containsMouse ? Colours.layer(Colours.palette.m3surfaceContainer, 2) : "transparent"
                        implicitHeight: taskRow.implicitHeight + Appearance.padding.small * 2

                        MouseArea {
                            id: taskMouse

                            anchors.fill: parent
                            hoverEnabled: true
                            cursorShape: modelData.state === "PROJECT" ? Qt.ArrowCursor : Qt.PointingHandCursor
                            onClicked: {
                                if (modelData.state !== "PROJECT")
                                    Tasks.clockIn(modelData.title);
                            }
                        }

                        RowLayout {
                            id: taskRow

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
                                color: {
                                    switch (modelData.state) {
                                    case "INPROGRESS":
                                        return Colours.palette.m3primary;
                                    case "NEXT":
                                        return Colours.palette.m3tertiary;
                                    case "PROJECT":
                                        return Colours.palette.m3secondary;
                                    case "WAITING":
                                    case "HOLD":
                                        return Colours.palette.m3outline;
                                    default:
                                        return Colours.palette.m3onSurface;
                                    }
                                }
                                Layout.preferredWidth: 96
                            }

                            StyledText {
                                Layout.fillWidth: true
                                text: modelData.title
                                elide: Text.ElideRight
                                font.weight: modelData.state === "PROJECT" ? 500 : 400
                            }
                        }
                    }
                }
            }
        }
    }

    Component {
        id: rightColumn

        StyledFlickable {
            id: rightFlick

            flickableDirection: Flickable.VerticalFlick
            contentHeight: rightLayout.implicitHeight

            StyledScrollBar.vertical: StyledScrollBar {
                flickable: rightFlick
            }

            ColumnLayout {
                id: rightLayout

                anchors.left: parent.left
                anchors.right: parent.right
                spacing: Appearance.spacing.normal

                StyledText {
                    text: qsTr("Reports")
                    font.pointSize: Appearance.font.size.large
                    font.weight: 500
                }

                ReportBlock {
                    Layout.fillWidth: true
                    title: qsTr("Today")
                    body: Tasks.dailyReport
                    onRefresh: Tasks.fetchReport("today")
                }

                ReportBlock {
                    Layout.fillWidth: true
                    title: qsTr("This week")
                    body: Tasks.weeklyReport
                    onRefresh: Tasks.fetchReport("thisweek")
                }
            }
        }
    }

    component PomoButton: StyledRect {
        id: btn

        property string icon
        property string label
        property bool enabled: true

        signal activated

        radius: Appearance.rounding.full
        color: btn.enabled ? (ma.containsMouse ? Colours.layer(Colours.palette.m3primaryContainer, 2) : Colours.palette.m3primaryContainer) : "transparent"
        opacity: btn.enabled ? 1 : 0.4
        implicitHeight: inner.implicitHeight + Appearance.padding.small * 2
        implicitWidth: inner.implicitWidth + Appearance.padding.normal * 2

        MouseArea {
            id: ma

            anchors.fill: parent
            hoverEnabled: true
            cursorShape: btn.enabled ? Qt.PointingHandCursor : Qt.ArrowCursor
            onClicked: if (btn.enabled)
                btn.activated()
        }

        RowLayout {
            id: inner

            anchors.centerIn: parent
            spacing: Appearance.spacing.smaller

            MaterialIcon {
                text: btn.icon
                font.pointSize: Appearance.font.size.normal
                color: Colours.palette.m3onPrimaryContainer
            }

            StyledText {
                text: btn.label
                font.pointSize: Appearance.font.size.small
                color: Colours.palette.m3onPrimaryContainer
            }
        }
    }

    component FilterPill: StyledRect {
        id: pill

        property string label
        property bool active

        signal activated

        radius: Appearance.rounding.full
        color: pill.active ? Colours.palette.m3secondaryContainer : "transparent"
        border.color: Colours.palette.m3outline
        border.width: pill.active ? 0 : 1
        implicitHeight: text.implicitHeight + Appearance.padding.small * 2
        implicitWidth: text.implicitWidth + Appearance.padding.normal * 2

        MouseArea {
            anchors.fill: parent
            cursorShape: Qt.PointingHandCursor
            onClicked: pill.activated()
        }

        StyledText {
            id: text

            anchors.centerIn: parent
            text: pill.label
            font.pointSize: Appearance.font.size.small
            color: pill.active ? Colours.palette.m3onSecondaryContainer : Colours.palette.m3onSurface
        }
    }

    component ReportBlock: ColumnLayout {
        id: block

        property string title
        property string body

        signal refresh

        spacing: Appearance.spacing.smaller

        RowLayout {
            Layout.fillWidth: true

            StyledText {
                text: block.title
                font.weight: 500
            }

            Item {
                Layout.fillWidth: true
            }

            StyledRect {
                radius: Appearance.rounding.full
                color: refreshMa.containsMouse ? Colours.layer(Colours.palette.m3surfaceContainer, 2) : "transparent"
                implicitHeight: refreshIcon.implicitHeight + Appearance.padding.small * 2
                implicitWidth: implicitHeight

                MouseArea {
                    id: refreshMa

                    anchors.fill: parent
                    hoverEnabled: true
                    cursorShape: Qt.PointingHandCursor
                    onClicked: block.refresh()
                }

                MaterialIcon {
                    id: refreshIcon

                    anchors.centerIn: parent
                    text: "refresh"
                    font.pointSize: Appearance.font.size.normal
                }
            }
        }

        StyledRect {
            Layout.fillWidth: true
            color: Colours.layer(Colours.palette.m3surfaceContainer, 1)
            radius: Appearance.rounding.normal
            implicitHeight: body.implicitHeight + Appearance.padding.large * 2

            StyledText {
                id: body

                anchors.fill: parent
                anchors.margins: Appearance.padding.large
                text: block.body || qsTr("No data")
                wrapMode: Text.NoWrap
                font.family: Config.appearance.font.family.mono
                font.pointSize: Appearance.font.size.small
                textFormat: Text.PlainText
            }
        }
    }
}
