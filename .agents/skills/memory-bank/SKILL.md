---
name: memory-bank
description: |
  Discover, search, and maintain the project's memory-bank documentation.
  Use before architectural exploration or when finding, reading, updating,
  or creating project documentation. Navigate large documents through
  outlines and relevant sections.
allowed-tools:
  - Read
  - Bash
  - Grep
  - Glob
---

# Memory Bank

The `memory-bank/` directory contains architectural documentation for the shinytest2 R package. Each file covers a specific subsystem or design pattern. All files have YAML frontmatter with `title` and `description` fields.

## Finding and reading documents

Loading this skill is preparation, not completion of documentation discovery. Before proceeding with code exploration or evaluation, identify the relevant memory-bank guidance and read its applicable sections. Reuse sections already in context when they are still current.

If relevant documents are already identified, go directly to their applicable sections. Otherwise, start with the catalog to select documents by title and description. It also reports line counts and UTF-8 byte sizes:

```bash
node "${SKILL_DIR}/scripts/list_memory_bank.mjs"
```

Replace `${SKILL_DIR}` with this skill's directory. The default memory bank belongs to the checkout containing the script; pass another memory-bank directory as a positional argument to inspect it instead.

Read small, relevant documents directly. For large documents, inspect an outline before choosing sections:

```bash
node "${SKILL_DIR}/scripts/list_memory_bank.mjs" --outline appDriver.md
```

Outlines report `#` headings (H1-H6), inclusive line ranges, and section byte sizes, excluding headings inside frontmatter or fenced code. A section includes its subsections and ends before the next heading of equal or higher level, so parent and child ranges overlap.

- Select the smallest sections that answer the task, then read their ranges with `Read` (`offset=start`, `limit=end-start+1`) or a bounded shell read.
- Include relevant overviews, invariants, and referenced dependencies; keyword matches alone can miss architectural constraints. Follow source-code references when verifying current behavior.
- If the catalog or outlines do not identify relevant guidance for an affected area, search document contents with task terminology, related symbols, and synonyms before concluding that none exists. For screenshots, for example, search `screenshot`, `snapshot`, `threshold`, `variant`, and `png`.
- If a read is truncated, use the included heading outline to pick a section, or search within the document, before requesting more.

Discovery is complete when each area under investigation has applicable sections that were read, or a catalog lookup and content search found no relevant guidance. Keep a compact working note of the sections consulted and constraints they contribute, or the search scope when nothing applies. If a lookup fails or is unavailable, proceed with the available evidence and report the limitation rather than claiming that no guidance exists.

## Delegating navigation

Handle obvious lookups directly. When relevance is ambiguous across many documents, a navigation subagent can find useful sections without bringing all their contents into the parent context. Consult this skill and the catalog first; the subagent can perform the detailed section selection.

Give the subagent a self-contained question, absolute workspace and documentation paths, and access to outlines and content search. Supply selected outlines if its toolset cannot run the script. Ask for a compact map containing each relevant file, heading, line range, reason to read it, and priority (essential now or conditional on a particular investigation). Include relevant architectural constraints, not just keyword matches. Short supporting excerpts are useful when they directly answer the question; avoid reproducing section bodies by default.

The parent reads selected original sections and expands as needed. If delegating a substantive review instead of navigation, ask for findings with evidence and source locations, then validate specific claims rather than repeating the whole investigation.

## Updating documents

Update memory-bank files when:
- Discovering new architectural patterns that should be documented
- After implementing significant changes to core architecture
- When the user requests an update
- When important context needs to be added or clarified

When updating, keep the YAML frontmatter `title` and `description` in sync with the document content.

## Creating new documents

When creating a new memory-bank file:
1. Use **camelCase** naming (e.g., `newFeatureSystem.md`)
2. Add YAML frontmatter with `title` and `description`. Keep each value on one line; the catalog script only reads flat `key: value` pairs.
3. For key architecture documents, add a corresponding trigger entry in `AGENTS.md` under "Key Architecture Documents"; other documents are discoverable through their frontmatter.
