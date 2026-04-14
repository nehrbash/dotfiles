pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io

Singleton {
    id: root

    readonly property var pomodoro: _snapshot.pomodoro ?? _defaultPomo
    readonly property var tasks: _snapshot.tasks ?? []
    readonly property string filter: _snapshot.filter ?? "work"
    readonly property int totalCount: _snapshot.counts?.total ?? 0
    readonly property int visibleCount: _snapshot.counts?.visible ?? 0

    readonly property bool clockedIn: pomodoro.clocked_in ?? pomodoro["clocked-in"] ?? false
    readonly property bool onBreak: pomodoro.on_break ?? pomodoro["on-break"] ?? false

    property string dailyReport
    property string weeklyReport

    signal snapshotUpdated
    signal reportUpdated(string period, string body)

    property var _snapshot: ({})
    readonly property var _defaultPomo: ({
            enabled: false,
            "clocked-in": false,
            "on-break": false,
            percent: 0,
            time: "00:00",
            task: "No Active Task",
            summary: "No Active Task 00:00",
            keystrokes: 0,
            "keystrokes-target": 0
        })

    function refresh(): void {
        if (snapshotProc.running)
            return;
        snapshotProc.running = true;
    }

    function setFilter(tag: string): void {
        snapshotProc.command = ["timeout", "5", "emacsclient", "-e", `(sn-tasks/snapshot "${tag}")`];
        snapshotProc.running = false;
        snapshotProc.running = true;
    }

    function clockIn(title: string): void {
        const escaped = title.replace(/\\/g, "\\\\").replace(/"/g, "\\\"");
        runSideEffect(`(sn-tasks/clock-in-by-title "${escaped}")`);
    }

    function clockOut(): void {
        runSideEffect("(sn-tasks/clock-out)");
    }

    function markDone(): void {
        runSideEffect("(sn-tasks/mark-done)");
    }

    function toggleTypeBreak(): void {
        runSideEffect("(sn-tasks/type-break-toggle)");
    }

    function endBreak(): void {
        runSideEffect("(sn-tasks/end-break)");
    }

    function runSideEffect(sexp: string): void {
        sideProc.command = ["timeout", "5", "emacsclient", "-e", sexp];
        sideProc.running = false;
        sideProc.running = true;
    }

    function fetchReport(period: string): void {
        if (period === "today") {
            if (dailyProc.running)
                return;
            dailyProc.running = true;
        } else {
            if (weeklyProc.running)
                return;
            weeklyProc.running = true;
        }
    }

    function _parseSnapshot(raw: string): void {
        const unquoted = _unquoteElisp(raw);
        if (!unquoted)
            return;
        try {
            _snapshot = JSON.parse(unquoted);
            snapshotUpdated();
        } catch (e) {
            console.warn("Tasks: failed to parse snapshot:", e, unquoted.slice(0, 200));
        }
    }

    function _unquoteElisp(raw: string): string {
        // emacsclient returns lisp-quoted string: "\"{...}\"" — strip wrapping and unescape.
        let s = (raw ?? "").trim();
        if (!s)
            return "";
        if (s.startsWith("\"") && s.endsWith("\""))
            s = s.slice(1, -1);
        return s.replace(/\\"/g, "\"").replace(/\\\\/g, "\\").replace(/\\n/g, "\n");
    }

    Process {
        id: snapshotProc

        command: ["timeout", "5", "emacsclient", "-e", "(sn-tasks/snapshot)"]
        running: true

        stdout: StdioCollector {
            onStreamFinished: root._parseSnapshot(text)
        }
    }

    Process {
        id: sideProc

        running: false
        onExited: root.refresh()
    }

    Process {
        id: dailyProc

        command: ["timeout", "10", "emacsclient", "-e", "(sn-tasks/report \"today\")"]
        stdout: StdioCollector {
            onStreamFinished: {
                root.dailyReport = root._unquoteElisp(text);
                root.reportUpdated("today", root.dailyReport);
            }
        }
    }

    Process {
        id: weeklyProc

        command: ["timeout", "10", "emacsclient", "-e", "(sn-tasks/report \"thisweek\")"]
        stdout: StdioCollector {
            onStreamFinished: {
                root.weeklyReport = root._unquoteElisp(text);
                root.reportUpdated("thisweek", root.weeklyReport);
            }
        }
    }

    Timer {
        interval: 5 * 60 * 1000
        running: true
        repeat: true
        onTriggered: root.refresh()
    }
}
