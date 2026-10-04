# Square Fireballs RPG

Fan-made tactical fantasy RPG rulebook. Chapters are Markdown in `book/`, game data is CSV in `data/`, and the Python package in `src/sfrpg/` renders both (macros in `src/sfrpg/macros.py`, HTML in `src/sfrpg/render/templates/blocks.j2`, styles in `book/css/sfrpg.css`). See the README for commands; the usual checks are `uv run pytest` and `uv run mkdocs build --strict`.

## What Claude does and doesn't do

**Code, tooling, CI, graphic design (CSS, layout, colours):** contribute freely. For visual changes, show mockups (a local HTML page under `build/`) before changing the real styles.

**Text (rules, flavour, examples, names):** the author writes it. Claude acts as an editor only:
- fix typos, spacing, broken formatting and stale references (e.g. text describing a style that changed);
- point out unclear or inconsistent rules, but propose wording only when asked;
- never write new rules text, flavour text, or names for game content unprompted.

**Illustrations:** the author provides them. Claude may scale, crop, convert, or make minor technical edits (e.g. background transparency). Claude does not create or generate images, and does not change an image's content.

**Diagrams:** allowed when built from simple primitives (boxes, arrows, grids, text) or by composing existing images such as sprites or tiles.

When a task would cross these lines, stop and ask instead of doing it.

## Intellectual property

The game is inspired by an existing published game's mechanics and spirit. Mechanics are fine to share; expression is not. Flag, don't silently fix:
- text that looks copied or closely paraphrased from published books;
- trademarked or product-specific names (game titles, settings, distinctive monsters and races, e.g. beholder, mind flayer, eladrin);
- visual elements copied directly from published material (icons, ornaments, page layouts, sampled colour schemes), as opposed to generic conventions (coloured header bars, stat tables).

The book refers to the original game only as "legacy rules/sourcebooks". Keep it that way.

## Licensing

Code (`src/`, `tests/`, `.github/`) is MIT (`LICENSE`); content (`book/`, `data/`, `examples/`, images) is CC BY-NC-SA 4.0 (`LICENSE-CONTENT.txt`). Don't add third-party content whose licence isn't compatible with that.

## Git

- Work on short-lived branches off `main`; `main` is protected (linear history, no force-push) and PRs are rebase-merged.
- Commit only when asked. Never force-push without explicit approval.
- CI (`.github/workflows/site.yml`) builds on every PR and deploys to GitHub Pages on pushes to `main`.
