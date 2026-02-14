---
name: research
description: Research a topic via web search and create an org-roam note with synthesized findings
disable-model-invocation: true
argument-hint: "<topic>"
---

Research the topic specified in $ARGUMENTS thoroughly and create an org-roam note.

## Step 1 — Research

Perform 5-10 web searches on `$ARGUMENTS`, varying query angles (overview, best practices, comparisons, recent developments, common pitfalls). Use WebFetch to read the most relevant results.

## Step 1b — Deep Dive

After the initial broad research, identify 2-4 subtopics that need deeper exploration. Launch parallel subagents (using the Task tool with `subagent_type: "general-purpose"`) to research each subtopic concurrently. Each agent should:
- Perform focused web searches on its assigned subtopic
- Read the most relevant pages
- Return a structured summary with key findings and source URLs

Wait for all agents to complete, then incorporate their findings into the note.

## Step 2 — Synthesize

Create an org file at `~/doc/Roam/` using this naming convention:

**Filename:** `YYYYMMDDHHMMSS-slug.org` where the timestamp is now and the slug is a lowercase hyphenated version of the topic.

**Template:**

```org
#+TITLE: <Topic Title>
#+FILETAGS: :research:

* Overview

<2-3 paragraph summary>

* Key Concepts

** <Concept 1>
<explanation>

** <Concept 2>
<explanation>

* Details

<Deeper coverage organized by subtopic>

* Practical Notes

<Gotchas, tips, common patterns>

* References

- [[<url>][<title>]] — <one-line description>
- [[<url>][<title>]] — <one-line description>
```

**Rules:**
- Use Org-mode syntax only (NOT Markdown)
- Do NOT add `:ID:` or `:PROPERTIES:` drawers — Emacs/Org-roam manages those
- Use `[[url][label]]` for links (org-mode style)
- Include all source URLs in the References section
- Aim for comprehensive but scannable content

## Step 3 — Index

After creating the file, append a link to `~/doc/notes.org` under a `* Research` heading (create it if it doesn't exist):

```org
- [[file:Roam/<filename>][<Topic Title>]] — <date>
```

## Step 4 — Report

Tell the user:
- The file path created
- A 2-3 sentence summary of what was found
- Number of sources consulted
