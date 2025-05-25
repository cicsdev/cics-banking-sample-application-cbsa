---
trigger: model_decision
description: When creating or editing markdown files.
---

# Markdown Formatting Rules

## List Style Preference

### Rule: Use numbered lists consistently at all levels

**Prefer:**
1. Numbered lists (1., 2., 3.) at all levels of nesting
2. Consistent use of numbers rather than mixing with letters or roman numerals
3. Proper indentation to show hierarchy

**Avoid:**
- Bullet points using dashes
* Bullet points using asterisks

**Reason:** Numbered lists provide clear reference points during discussions and make it easier to refer to specific items. They create a more structured document that's easier to navigate and reference in conversations.

**Example:**

```markdown
# Good Example
1. First item
2. Second item
   1. Sub-item one
   2. Sub-item two
      1. Nested item one
      2. Nested item two
3. Third item

# Avoid
- First item
- Second item
  - Sub-item one
  - Sub-item two
    - Nested item one
    - Nested item two
- Third item

# Also Avoid
1. First item
2. Second item
   a. Sub-item one
   b. Sub-item two
      i. Nested item one
      ii. Nested item two
3. Third item
```

**Scope:** All markdown documentation in this project
