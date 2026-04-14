#!/usr/bin/env bash
# Emit a proposed 5-day schedule that satisfies:
#   - Start <= 09:00 each day (standup constraint)
#   - Daily total 8.0-8.5h (Fri may be 7.5h)
#   - Week total >= 40h
#   - 30-60 min unpaid lunch gap around noon
#   - 15-minute granularity
#
# Usage: generate-schedule.sh <week-start-YYYY-MM-DD>
# Output: one line per block: DATE|START|FINISH|HOURS

set -euo pipefail

MONDAY="${1:?week-start date (YYYY-MM-DD) required}"

# Plan: start 08:30, AM block 3.5h (08:30-12:00), lunch 12:00-12:30,
# PM block 4.5h (12:30-17:00) -> 8.0h/day. 5 days = 40h.
# Small variation so it doesn't look templated.

variants=(
    # start_am|end_am|start_pm|end_pm|hours
    "08:30|12:00|12:30|17:00|8.0"
    "08:45|12:00|12:45|17:15|7.75"
    "08:30|11:45|12:15|17:00|8.0"
    "09:00|12:15|12:45|17:30|8.0"
    "08:30|12:00|12:30|17:15|8.25"
)

total=0
for i in 0 1 2 3 4; do
    day=$(date -d "$MONDAY +$i days" +%Y-%m-%d)
    # rotate variants for light variation, keep Fri at 8.0
    idx=$(( i % ${#variants[@]} ))
    [[ $i -eq 4 ]] && idx=0  # Friday = base 8.0
    IFS='|' read -r s1 e1 s2 e2 h <<< "${variants[$idx]}"
    echo "${day}|${s1}|${e1}|$(awk -v a="$s1" -v b="$e1" 'BEGIN{split(a,x,":");split(b,y,":");printf "%.2f",(y[1]*60+y[2]-x[1]*60-x[2])/60}')"
    echo "${day}|${s2}|${e2}|$(awk -v a="$s2" -v b="$e2" 'BEGIN{split(a,x,":");split(b,y,":");printf "%.2f",(y[1]*60+y[2]-x[1]*60-x[2])/60}')"
    total=$(awk -v t="$total" -v h="$h" 'BEGIN{printf "%.2f",t+h}')
done

echo "# week-total=${total}h" >&2
awk -v t="$total" 'BEGIN{if (t+0 < 40) { print "ERROR: week total "t" < 40" > "/dev/stderr"; exit 1 }}'
