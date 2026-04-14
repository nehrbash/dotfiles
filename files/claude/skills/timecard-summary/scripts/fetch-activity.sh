#!/usr/bin/env bash
# Fetch the current user's GitLab activity for a given week.
# Usage: fetch-activity.sh [week-offset]
#   week-offset: 0 = this week, -1 = last week, etc. Default: 0.
# Output: /tmp/timecard-activity.json (and prints path)

set -euo pipefail

OFFSET="${1:-0}"

# Monday of target week (Mon-Fri scope)
MONDAY=$(date -d "monday $((OFFSET)) week" +%Y-%m-%d 2>/dev/null || date -d "monday this week" +%Y-%m-%d)
# If offset != 0, adjust
if [[ "$OFFSET" != "0" ]]; then
    WEEKS="$OFFSET week"
    MONDAY=$(date -d "last monday $WEEKS" +%Y-%m-%d 2>/dev/null || \
             date -d "monday $WEEKS" +%Y-%m-%d)
fi
FRIDAY=$(date -d "$MONDAY +4 days" +%Y-%m-%d)
SATURDAY=$(date -d "$MONDAY +5 days" +%Y-%m-%d)

OUT=/tmp/timecard-activity.json

echo "Fetching activity $MONDAY to $FRIDAY..." >&2

USERNAME=$(glab api user --jq .username)
USER_ID=$(glab api user --jq .id)

# Events: pushed, merged, commented, created (for MRs/issues)
# GitLab /users/:id/events supports after/before (exclusive on before end)
EVENTS=$(glab api --paginate "users/$USER_ID/events?after=$(date -d "$MONDAY -1 day" +%Y-%m-%d)&before=$SATURDAY&per_page=100")

# MRs authored/updated this week (across all accessible projects)
MRS=$(glab api --paginate "merge_requests?scope=created_by_me&updated_after=${MONDAY}T00:00:00Z&updated_before=${SATURDAY}T00:00:00Z&per_page=100" || echo '[]')

jq -n \
    --arg monday "$MONDAY" \
    --arg friday "$FRIDAY" \
    --arg user "$USERNAME" \
    --argjson events "$EVENTS" \
    --argjson mrs "$MRS" \
    '{
        week: { monday: $monday, friday: $friday },
        user: $user,
        events: ($events | map({
            date: (.created_at | split("T")[0]),
            action: .action_name,
            target: .target_type,
            title: (.target_title // .push_data.commit_title // ""),
            ref: .push_data.ref,
            project_id: .project_id,
            commit_count: .push_data.commit_count
        })),
        mrs: ($mrs | map({
            date: (.updated_at | split("T")[0]),
            iid: .iid,
            project_id: .project_id,
            title: .title,
            web_url: .web_url,
            state: .state
        }))
    }' > "$OUT"

echo "$OUT"
echo "Summary by day:" >&2
jq -r '.events | group_by(.date) | .[] | "\(.[0].date): \(length) events"' "$OUT" >&2
