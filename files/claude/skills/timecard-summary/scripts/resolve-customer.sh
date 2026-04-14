#!/usr/bin/env bash
# Resolve an MR -> linked issue(s) -> customer label(s).
# Used to correctly attribute time on commented/reviewed MRs that aren't your own work.
#
# Usage: resolve-customer.sh <project_id> <mr_iid>
# Output: JSON { mr, linked_issues: [{iid, title, customer_labels, all_labels}] }

set -euo pipefail

PROJECT_ID="${1:?project_id required}"
MR_IID="${2:?mr_iid required}"

# Customer labels follow convention: "customer::<name>" or "site::<name>".
# Adjust the regex here if your org uses a different prefix.
CUSTOMER_PREFIX_REGEX='^(customer|site|org|client)::'

MR_JSON=$(glab api "projects/$PROJECT_ID/merge_requests/$MR_IID")

# GitLab "related issues" endpoint - includes closes, mentions, and manual links.
LINKED=$(glab api "projects/$PROJECT_ID/merge_requests/$MR_IID/related_issues" 2>/dev/null || echo '[]')

# Fallback: parse description for #123 / group/proj#123 refs if API returned empty.
if [[ "$(echo "$LINKED" | jq 'length')" == "0" ]]; then
    REFS=$(echo "$MR_JSON" | jq -r '.description // ""' | grep -oE '([a-zA-Z0-9/_.-]+)?#[0-9]+' | sort -u || true)
    LINKED='[]'
    while IFS= read -r ref; do
        [[ -z "$ref" ]] && continue
        if [[ "$ref" == *"#"* && "$ref" != "#"* ]]; then
            proj="${ref%#*}"
            iid="${ref#*#}"
            encoded=$(printf '%s' "$proj" | jq -sRr @uri)
            issue=$(glab api "projects/$encoded/issues/$iid" 2>/dev/null || echo 'null')
        else
            iid="${ref#\#}"
            issue=$(glab api "projects/$PROJECT_ID/issues/$iid" 2>/dev/null || echo 'null')
        fi
        [[ "$issue" == "null" ]] && continue
        LINKED=$(echo "$LINKED" | jq --argjson i "$issue" '. + [$i]')
    done <<< "$REFS"
fi

jq -n \
    --argjson mr "$MR_JSON" \
    --argjson linked "$LINKED" \
    --arg prefix "$CUSTOMER_PREFIX_REGEX" \
    '{
        mr: { iid: $mr.iid, title: $mr.title, web_url: $mr.web_url, labels: $mr.labels },
        linked_issues: ($linked | map({
            iid: .iid,
            title: .title,
            web_url: .web_url,
            all_labels: (.labels // []),
            customer_labels: ((.labels // []) | map(select(test($prefix))))
        }))
    }'
