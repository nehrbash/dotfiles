pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Io
import Quickshell.Wayland
import qs.components
import qs.components.containers
import qs.components.controls
import qs.services
import qs.config

Scope {
    id: root

    property bool deferred
    property bool manuallyOpen

    readonly property bool needsClockIn: !Tasks.clockedIn && !Tasks.onBreak
    readonly property bool shouldShow: Tasks.onBreak || manuallyOpen || (needsClockIn && !deferred)

    // Re-arm deferral after 5 minutes
    Timer {
        interval: 5 * 60 * 1000
        running: root.deferred
        onTriggered: root.deferred = false
    }

    Connections {
        function onSnapshotUpdated(): void {
            if (Tasks.clockedIn || Tasks.onBreak)
                root.deferred = false;
        }

        target: Tasks
    }

    IpcHandler {
        function toggle(): void {
            root.manuallyOpen = !root.manuallyOpen;
            if (root.manuallyOpen) {
                root.deferred = false;
                Tasks.refresh();
            }
        }

        function open(): void {
            root.manuallyOpen = true;
            root.deferred = false;
            Tasks.refresh();
        }

        function close(): void {
            root.manuallyOpen = false;
        }

        target: "taskNudge"
    }

    LazyLoader {
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
                    focus: true

                    Keys.onEscapePressed: root.manuallyOpen = false

                    MouseArea {
                        anchors.fill: parent
                        onClicked: root.manuallyOpen = false
                    }

                    StyledRect {
                        anchors.centerIn: parent
                        implicitWidth: Math.min(720, parent.width - 80)
                        implicitHeight: card.implicitHeight + Appearance.padding.large * 2
                        radius: Appearance.rounding.large
                        color: Colours.palette.m3surfaceContainer

                        // Eat clicks so background MouseArea doesn't dismiss
                        MouseArea {
                            anchors.fill: parent
                        }

                        ColumnLayout {
                            id: card

                            anchors.left: parent.left
                            anchors.right: parent.right
                            anchors.top: parent.top
                            anchors.margins: Appearance.padding.large
                            spacing: Appearance.spacing.normal

                            // Header row with pomodoro ring + status
                            RowLayout {
                                Layout.fillWidth: true
                                spacing: Appearance.spacing.normal

                                Item {
                                    Layout.preferredWidth: 96
                                    Layout.preferredHeight: 96

                                    CircularProgress {
                                        anchors.fill: parent
                                        strokeWidth: 6
                                        value: (Tasks.pomodoro.percent ?? 0) / 100
                                        fgColour: Tasks.onBreak ? Colours.palette.m3tertiary : Colours.palette.m3primary
                                        bgColour: Colours.palette.m3secondaryContainer
                                    }

                                    Image {
                                        anchors.centerIn: parent
                                        width: 56
                                        height: 56
                                        source: Quickshell.shellPath((Tasks.pomodoro.time ?? "00:00") !== "00:00" ? "assets/images/tomato.png" : "assets/images/tomato-sad.png")
                                        fillMode: Image.PreserveAspectFit
                                        smooth: true
                                    }

                                    StyledText {
                                        anchors.horizontalCenter: parent.horizontalCenter
                                        anchors.bottom: parent.bottom
                                        anchors.bottomMargin: -4
                                        text: Tasks.pomodoro.time ?? "00:00"
                                        font.pointSize: Appearance.font.size.small
                                        font.weight: 500
                                    }
                                }

                                ColumnLayout {
                                    Layout.fillWidth: true
                                    spacing: Appearance.spacing.smaller

                                    StyledText {
                                        text: Tasks.onBreak ? qsTr("Take a break") : qsTr("Not clocked in")
                                        font.pointSize: Appearance.font.size.large
                                        font.weight: 500
                                    }

                                    StyledText {
                                        Layout.fillWidth: true
                                        text: Tasks.onBreak ? qsTr("Relax your hands. Timer ends in %1.").arg(Tasks.pomodoro.time ?? "00:00") : qsTr("Pick a task to clock in, or press Esc to defer 5 min.")
                                        color: Colours.palette.m3outline
                                        wrapMode: Text.Wrap
                                    }

                                    StyledText {
                                        visible: Tasks.pomodoro.task && Tasks.pomodoro.task !== "No Active Task" && Tasks.pomodoro.task !== "Take a break - relax your hands"
                                        text: qsTr("Last task: %1").arg(Tasks.pomodoro.task ?? "")
                                        color: Colours.palette.m3outline
                                        font.pointSize: Appearance.font.size.small
                                    }
                                }
                            }

                            // Work/personal toggle (only when picking a task)
                            RowLayout {
                                Layout.fillWidth: true
                                Layout.topMargin: Appearance.spacing.smaller
                                visible: !Tasks.onBreak
                                spacing: Appearance.spacing.small

                                StyledText {
                                    text: qsTr("Agenda")
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

                            // Task list (only when picking a task)
                            Repeater {
                                model: Tasks.onBreak ? [] : root._taskSlice

                                delegate: TaskRow {
                                    required property var modelData
                                    Layout.fillWidth: true
                                    task: modelData
                                    onClockIn: {
                                        Tasks.clockIn(modelData.title);
                                        root.manuallyOpen = false;
                                        root.deferred = false;
                                    }
                                }
                            }

                            // Action buttons row
                            RowLayout {
                                Layout.fillWidth: true
                                Layout.topMargin: Appearance.spacing.normal
                                spacing: Appearance.spacing.small

                                Item {
                                    Layout.fillWidth: true
                                }

                                ActionButton {
                                    visible: Tasks.onBreak
                                    primary: true
                                    icon: "stop_circle"
                                    label: qsTr("End break")
                                    onActivated: Tasks.endBreak()
                                }

                                ActionButton {
                                    visible: !Tasks.onBreak && Tasks.clockedIn
                                    icon: "pause"
                                    label: qsTr("Clock out")
                                    onActivated: Tasks.clockOut()
                                }

                                ActionButton {
                                    visible: !Tasks.onBreak
                                    icon: "schedule"
                                    label: qsTr("Defer 5 min (Esc)")
                                    onActivated: {
                                        root.deferred = true;
                                        root.manuallyOpen = false;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    readonly property var _taskSlice: {
        const out = [];
        const src = Tasks.tasks ?? [];
        for (let i = 0; i < src.length && out.length < 8; i++) {
            const t = src[i];
            if (t.state !== "PROJECT")
                out.push(t);
        }
        return out;
    }

    component TaskRow: StyledRect {
        id: trow

        required property var task

        signal clockIn

        radius: Appearance.rounding.small
        color: tma.containsMouse ? Colours.layer(Colours.palette.m3surfaceContainer, 2) : "transparent"
        implicitHeight: trowLayout.implicitHeight + Appearance.padding.normal * 2

        MouseArea {
            id: tma

            anchors.fill: parent
            hoverEnabled: true
            cursorShape: Qt.PointingHandCursor
            onClicked: trow.clockIn()
        }

        RowLayout {
            id: trowLayout

            anchors.fill: parent
            anchors.leftMargin: Appearance.padding.normal
            anchors.rightMargin: Appearance.padding.normal
            spacing: Appearance.spacing.small

            StyledText {
                text: trow.task.state
                font.pointSize: Appearance.font.size.small
                font.weight: 500
                Layout.preferredWidth: 96
                color: Colours.palette.m3primary
            }

            StyledText {
                Layout.fillWidth: true
                text: trow.task.title
                elide: Text.ElideRight
            }

            Repeater {
                model: trow.task.tags ?? []

                delegate: StyledRect {
                    required property string modelData

                    radius: Appearance.rounding.full
                    color: Colours.layer(Colours.palette.m3surfaceContainer, 3)
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

    component FilterPill: StyledRect {
        id: pill

        property string label
        property bool active

        signal activated

        radius: Appearance.rounding.full
        color: pill.active ? Colours.palette.m3secondaryContainer : "transparent"
        border.color: Colours.palette.m3outline
        border.width: pill.active ? 0 : 1
        implicitHeight: pillText.implicitHeight + Appearance.padding.small * 2
        implicitWidth: pillText.implicitWidth + Appearance.padding.normal * 2

        MouseArea {
            anchors.fill: parent
            cursorShape: Qt.PointingHandCursor
            onClicked: pill.activated()
        }

        StyledText {
            id: pillText

            anchors.centerIn: parent
            text: pill.label
            font.pointSize: Appearance.font.size.small
            color: pill.active ? Colours.palette.m3onSecondaryContainer : Colours.palette.m3onSurface
        }
    }

    component ActionButton: StyledRect {
        id: abtn

        property string icon
        property string label
        property bool primary

        signal activated

        radius: Appearance.rounding.full
        color: {
            const base = abtn.primary ? Colours.palette.m3primary : Colours.palette.m3primaryContainer;
            return abtnMa.containsMouse ? Colours.layer(base, 2) : base;
        }
        implicitHeight: abtnRow.implicitHeight + Appearance.padding.normal * 2
        implicitWidth: abtnRow.implicitWidth + Appearance.padding.large * 2

        MouseArea {
            id: abtnMa

            anchors.fill: parent
            hoverEnabled: true
            cursorShape: Qt.PointingHandCursor
            onClicked: abtn.activated()
        }

        RowLayout {
            id: abtnRow

            anchors.centerIn: parent
            spacing: Appearance.spacing.small

            MaterialIcon {
                text: abtn.icon
                font.pointSize: Appearance.font.size.normal
                color: abtn.primary ? Colours.palette.m3onPrimary : Colours.palette.m3onPrimaryContainer
            }

            StyledText {
                text: abtn.label
                color: abtn.primary ? Colours.palette.m3onPrimary : Colours.palette.m3onPrimaryContainer
            }
        }
    }
}
