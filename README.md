# Square Fireballs - The Role Playing Game

Source data and build tooling for the rulebook of Square Fireballs, a role playing game of tactical fantasy.

[Read the book](http://capelastegui.github.io/sfrpg/ "Square Fireballs")

## Layout

| Path | Contents |
|------|----------|
| `book/` | Chapters as Markdown, plus `figures/` and `css/sfrpg.css` |
| `data/` | Game data as CSV: classes, powers, feats, items, monsters |
| `src/sfrpg/` | Python package that renders the data (Jinja templates) and builds the outputs |
| `examples/` | Example character sheet input |
| `mkdocs.yml` | Site configuration and chapter order |

Chapters are plain Markdown. Data sections are inserted with macro calls that are expanded at build time:

```markdown
### Barbarian Rager
{{ build_stats("Barbarian", "Rager") }}

{{ build_features("Barbarian", "Rager") }}

#### Class Powers { .newPage }
{{ build_powers("Barbarian", "Rager") }}
```

The available macros are listed in [`src/sfrpg/macros.py`](src/sfrpg/macros.py), and the HTML they produce is defined in [`src/sfrpg/render/templates/blocks.j2`](src/sfrpg/render/templates/blocks.j2).

## Building

Requires [uv](https://docs.astral.sh/uv/).

```sh
uv sync
uv run mkdocs serve                       # live preview at http://127.0.0.1:8000
uv run mkdocs build --strict              # static site in site/
uv run sfrpg check                        # report broken ids / duplicates in data/
uv run sfrpg pages                        # standalone HTML per class, origin and monster group
uv run sfrpg sheet examples/grok.yaml     # character sheet from YAML
uv run sfrpg pdf --html-only              # printable book as a single HTML file
uv run pytest
```

### PDF

`uv run sfrpg pdf` renders the printable book with [WeasyPrint](https://weasyprint.org/). Install it with `uv sync --extra pdf`. WeasyPrint needs the Pango libraries: on Linux and macOS they come from the system package manager, but on Windows they're awkward to install. On Windows it's easier to rely on the CI build, or open `build/sfrpg.html` from `sfrpg pdf --html-only` in a browser.

## Publishing

`.github/workflows/site.yml` does the following on every push to `main`:

1. Runs lint and tests.
2. Builds the site, the PDF (`site/sfrpg.pdf`), the standalone pages (`site/pages/`) and the example sheet.
3. Deploys the result to GitHub Pages.

For the deploy to work, the repository's Pages source must be set to **GitHub Actions**.

## History

Until 2023 the book was built with R and bookdown. That version is preserved under the git tag `r-final`.

## License

[CC BY-NC 4.0](LICENSE.md)
