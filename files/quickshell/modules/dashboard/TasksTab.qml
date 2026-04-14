pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Widgets
import qs.components
import qs.components.containers
import qs.components.controls
import qs.services
import qs.config

Item {
    id: root

    implicitWidth: 960
    implicitHeight: layout.implicitHeight + Appearance.padding.large * 2

    Component.onCompleted: {
        Tasks.refresh();
        Tasks.fetchReport("today");
        Tasks.fetchReport("thisweek");
    }

    ColumnLayout {
        id: layout

        anchors.left: parent.left
        anchors.right: parent.right
        anchors.top: parent.top
        anchors.leftMargin: Appearance.padding.large
        anchors.rightMargin: Appearance.padding.large
        anchors.topMargin: Appearance.padding.large

        spacing: Appearance.spacing.normal

        // Pomodoro card
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
                    Layout.preferredWidth: 96
                    Layout.preferredHeight: 96

                    CircularProgress {
                        anchors.fill: parent
                        strokeWidth: 6
                        value: (root.displayPercent) / 100
                        fgColour: Tasks.onBreak ? Colours.palette.m3tertiary : Colours.palette.m3primary
                        bgColour: Colours.palette.m3secondaryContainer
                    }

                    Image {
                        anchors.centerIn: parent
                        width: 56
                        height: 56
                        source: Quickshell.shellPath(root._remaining > 0 ? "assets/images/tomato.png" : "assets/images/tomato-sad.png")
                        fillMode: Image.PreserveAspectFit
                        smooth: true
                    }

                    StyledText {
                        anchors.horizontalCenter: parent.horizontalCenter
                        anchors.bottom: parent.bottom
                        anchors.bottomMargin: -4
                        text: root.displayTime
                        font.pointSize: Appearance.font.size.small
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
                            btnEnabled: Tasks.clockedIn
                            onActivated: Tasks.clockOut()
                        }

                        PomoButton {
                            icon: "check_circle"
                            label: qsTr("Done")
                            btnEnabled: Tasks.clockedIn
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

        // Filter + title row
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
                        Layout.preferredWidth: 96
                        color: {
                            switch (modelData.state) {
                            case "ACTIVE":
                                return Colours.palette.m3primary;
                            case "TODO":
                                return Colours.palette.m3tertiary;
                            case "PROJECT":
                                return Colours.palette.m3secondary;
                            case "HOLD":
                            case "DELEGATED":
                                return Colours.palette.m3outline;
                            default:
                                return Colours.palette.m3onSurface;
                            }
                        }
                    }

                    StyledText {
                        Layout.fillWidth: true
                        text: modelData.title
                        elide: Text.ElideRight
                        font.weight: modelData.state === "PROJECT" ? 500 : 400
                    }

                    Repeater {
                        model: modelData.tags ?? []

                        delegate: StyledRect {
                            required property string modelData

                            radius: Appearance.rounding.full
                            color: Colours.layer(Colours.palette.m3surfaceContainer, 2)
                            implicitHeight: tagText.implicitHeight + Appearance.padding.small
                            implicitWidth: tagText.implicitWidth + Appearance.padding.normal

                            StyledText {
                                id: tagText

                                anchors.centerIn: parent
                                text: modelData
                                font.pointSize: Appearance.font.size.small
                                color: Colours.palette.m3onSurfaceVariant
                            }
                        }
                    }
                }
            }
        }

        // Reports
        ReportBlock {
            Layout.fillWidth: true
            Layout.topMargin: Appearance.spacing.normal
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

    // Locally ticked pomodoro countdown — keeps the ring live between 5-min syncs.
    property real _baseSeconds: Tasks.pomodoro["remaining-seconds"] ?? 0
    property real _totalSeconds: (Tasks.pomodoro["total-seconds"] ?? 1500)
    property real _snapshotAt: Date.now()
    property real _tick: 0

    readonly property real _elapsed: {
        _tick;
        return Math.max(0, (Date.now() - _snapshotAt) / 1000);
    }
    readonly property real _remaining: Math.max(0, _baseSeconds - _elapsed)
    readonly property string displayTime: {
        const m = Math.floor(_remaining / 60);
        const s = Math.floor(_remaining % 60);
        return (m < 10 ? "0" : "") + m + ":" + (s < 10 ? "0" : "") + s;
    }
    readonly property real displayPercent: _totalSeconds > 0 ? (_remaining / _totalSeconds) * 100 : 0

    Connections {
        function onSnapshotUpdated(): void {
            root._baseSeconds = Tasks.pomodoro["remaining-seconds"] ?? 0;
            root._totalSeconds = Tasks.pomodoro["total-seconds"] ?? 1500;
            root._snapshotAt = Date.now();
        }

        target: Tasks
    }

    Timer {
        interval: 1000
        running: Tasks.pomodoro.enabled ?? false
        repeat: true
        onTriggered: root._tick++
    }

    component PomoButton: StyledRect {
        id: btn

        property string icon
        property string label
        property bool btnEnabled: true

        signal activated

        radius: Appearance.rounding.full
        color: btn.btnEnabled ? (ma.containsMouse ? Colours.layer(Colours.palette.m3primaryContainer, 2) : Colours.palette.m3primaryContainer) : "transparent"
        opacity: btn.btnEnabled ? 1 : 0.4
        implicitHeight: inner.implicitHeight + Appearance.padding.small * 2
        implicitWidth: inner.implicitWidth + Appearance.padding.normal * 2

        MouseArea {
            id: ma

            anchors.fill: parent
            hoverEnabled: true
            cursorShape: btn.btnEnabled ? Qt.PointingHandCursor : Qt.ArrowCursor
            onClicked: if (btn.btnEnabled)
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

        ClippingRectangle {
            Layout.fillWidth: true
            color: Colours.layer(Colours.palette.m3surfaceContainer, 1)
            radius: Appearance.rounding.normal
            implicitHeight: bodyText.implicitHeight + Appearance.padding.large * 2

            Flickable {
                id: bodyFlick

                anchors.fill: parent
                anchors.margins: Appearance.padding.large
                contentWidth: bodyText.implicitWidth
                contentHeight: bodyText.implicitHeight
                flickableDirection: Flickable.HorizontalFlick
                clip: true

                StyledText {
                    id: bodyText

                    text: block.body || qsTr("No data")
                    wrapMode: Text.NoWrap
                    font.family: Config.appearance.font.family.mono
                    font.pointSize: Appearance.font.size.small
                    textFormat: Text.PlainText
                }
            }
        }
    }
}
