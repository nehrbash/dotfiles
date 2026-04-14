---
name: timecard-summary
description: Generate weekly InfiniteTactics timecard entries from GitLab activity via glab. Use when the user asks to "fill out timecard", "generate timecard summary", "weekly timecard", "/timecard-summary", or needs proposed `ittime add` commands based on MR/commit activity. Fudges numbers to total >=40h/week and ~8h/day with start time <=9:00 (standup).
argument-hint: "[week-offset] e.g. 0 (this week), -1 (last week)"
---

# timecard-summary

Generate a proposed weekly timecard from GitLab activity.

## Constraints (enforced by scripts)

- Start time must be **<=09:00** every workday (standup at 9am is the latest start).
- Week total must be **>=40 hours** (Mon-Fri).
- Per-day total **~8 hours** (7.5-8.5 acceptable; bias slightly over).
- Lunch is unpaid: insert a 30-60 min gap around noon (no entry).
- Map git activity to `ittime` project IDs (see `~/doc/projects/timecard/` or `ittime projects`).

## Workflow

### Step 1 - Fetch activity

```bash
~/.claude/skills/timecard-summary/scripts/fetch-activity.sh ${1:-0}
```

Writes `/tmp/timecard-activity.json` with MRs, commits, and notes authored by the current user in the target week. Uses `glab api` (works across all GitLab projects the user has access to via `events` endpoint).

### Step 2 - Review activity

Read the JSON, group by day (Mon-Fri), and infer the dominant project per day. Keep the summary brief - you're mapping real work to billable projects, not writing prose.

If the user has configured project mappings in `~/.config/timecard-summary/mapping.yml`, use them. Otherwise ask which `ittime` project ID applies to each repo/group.

### Step 2b - Attribute MR comments/reviews to customers

Commenting on or reviewing an MR is billable, but the correct project depends on **whose work it is**, not yours. For every MR-comment / MR-approval / MR-review event in the activity JSON:

```bash
~/.claude/skills/timecard-summary/scripts/resolve-customer.sh <project_id> <mr_iid>
```

This returns the MR plus its linked issues with their labels. Look for customer/site labels (default prefixes: `customer::`, `site::`, `org::`, `client::` - edit the regex in the script if your team uses different conventions).

Resolution rules:

1. If the linked issue has a customer label, bill that customer's `ittime` project.
2. If multiple linked issues disagree, pick the one the MR `Closes` (first in description), else ask the user.
3. If there is no linked issue or no customer label, fall back to the repo's default project and note "MR review (no customer tag)" so the user can fix it later.
4. Cache resolutions per `(project_id, mr_iid)` in `/tmp/timecard-customer-cache.json` to avoid repeated API hits.

Batch this step: collect unique `(project_id, mr_iid)` pairs from the activity JSON before calling, and run resolutions in parallel where sensible.

### Step 3 - Propose schedule

Call:

```bash
~/.claude/skills/timecard-summary/scripts/generate-schedule.sh <week-start-date>
```

It outputs a proposed block layout per weekday that satisfies all constraints (start <=9:00, total >=40h, daily ~8h, lunch gap). Fill in the project IDs and notes from Step 2.

### Step 4 - Emit ittime commands

Print the resulting `ittime add` commands as a copyable block. **Do not execute them** - the user reviews and runs them.

Example output:

```bash
# Monday 2026-04-13 (8.0h)
ittime add -d 2026-04-13 -p 246 -s 8:30 -f 12:00 -n "MR !1234 review, KVM volume work"
ittime add -d 2026-04-13 -p 246 -s 12:30 -f 17:00 -n "Volume lifecycle impl"
```

### Step 5 - Offer `ittime week` check

After the user runs the commands, suggest `ittime week` to verify the total.

## Notes for Claude

- If current day is partial (e.g. Tuesday afternoon for the current week), only generate entries through today.
- Prefer round-ish times (15-minute granularity) - don't output `9:07`.
- If real git activity for a day is thin, still emit 8h with a generic note tied to whatever project dominated that week. The user wants the total to land; don't under-bill.
- Friday can be 7.5-8h; Mon-Thu should be 8-8.5h so week lands >=40.
- If a day has zero activity (holiday/PTO), ask before inventing hours.
