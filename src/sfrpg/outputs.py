"""Non-website outputs: printable book, standalone pages and character sheets.

All of them reuse the chapter macros and blocks.j2, so they render data exactly
like the website does.
"""

from __future__ import annotations

import re
from pathlib import Path

import markdown
import yaml
from pydantic import BaseModel, Field

from . import macros
from .book import BOOK_DIR, expand
from .data import repository as repo
from .render.env import TEMPLATE_DIR, blocks, env

ROOT = BOOK_DIR.parent
MD_EXTENSIONS = ["attr_list", "md_in_html", "tables", "admonition", "toc"]


def _css() -> str:
    return (BOOK_DIR / "css" / "sfrpg.css").read_text(encoding="utf-8")


def page(title: str, body: str, extra_css: str = "", base_href: str = "") -> str:
    """A self-contained HTML document with the book stylesheet inlined."""
    return (
        env()
        .get_template("page.j2")
        .render(title=title, body=body, css=_css(), extra_css=extra_css, base_href=base_href)
    )


def _slug(text: str) -> str:
    return re.sub(r"[^a-z0-9]+", "-", text.lower()).strip("-")


def _write(path: Path, html: str) -> Path:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(html, encoding="utf-8")
    return path


# -- Printable book ----------------------------------------------------------------


def chapter_files() -> list[str]:
    """Chapter file names in navigation order, read from mkdocs.yml."""
    config = yaml.safe_load((ROOT / "mkdocs.yml").read_text(encoding="utf-8"))
    files: list[str] = []

    def walk(node):
        if isinstance(node, str):
            files.append(node)
        elif isinstance(node, list):
            for n in node:
                walk(n)
        elif isinstance(node, dict):
            for v in node.values():
                walk(v)

    walk(config["nav"])
    return files


def _toc(html: str) -> str:
    items = re.findall(r'<h([12]) id="([^"]+)"[^>]*>(.*?)</h\1>', html)
    out, depth = ['<nav class="toc"><h2>Contents</h2><ul>'], 1
    for level, anchor, text in items:
        level = int(level)
        if level > depth:
            out.append("<ul>")
        elif level < depth:
            out.append("</ul>")
        depth = level
        text = re.sub(r"<[^>]+>|¶", "", text).strip()
        out.append(f'<li><a href="#{anchor}">{text}</a></li>')
    out.append("</ul>" * depth + "</nav>")
    return "\n".join(out)


def print_html() -> str:
    """The whole book as one HTML document, ready for WeasyPrint."""
    files = chapter_files()
    index, chapters = files[0], files[1:]
    parts = []
    for name in chapters:
        md = expand((BOOK_DIR / name).read_text(encoding="utf-8"))
        # Cross-chapter links become in-document anchors.
        md = re.sub(r"\]\([a-z0-9-]+\.md#", "](#", md)
        parts.append(md)
    body = markdown.markdown("\n\n".join(parts), extensions=MD_EXTENSIONS)
    index_md = (BOOK_DIR / index).read_text(encoding="utf-8")
    cover_md = re.sub(r"^---.*?---\n", "", index_md, flags=re.S)
    cover = '<section class="cover">' + markdown.markdown(cover_md) + "</section>"
    print_css = (TEMPLATE_DIR / "print.css").read_text(encoding="utf-8")
    return page(
        "Square Fireballs RPG",
        cover + _toc(body) + body,
        extra_css=print_css,
        base_href=BOOK_DIR.as_uri() + "/",
    )


def _weasyprint_html():
    try:
        from weasyprint import HTML
    except (ImportError, OSError) as e:  # OSError: Pango/GTK libraries missing
        raise RuntimeError(
            "WeasyPrint is not available. Install with `uv sync --extra pdf`; on Windows it "
            "also needs the Pango/GTK runtime (see the WeasyPrint install docs), so building "
            "PDFs in CI or WSL is easier. Use the HTML output to preview."
        ) from e
    return HTML


def build_pdf(out: Path) -> Path:
    html = _weasyprint_html()
    out.parent.mkdir(parents=True, exist_ok=True)
    html(string=print_html(), base_url=str(BOOK_DIR)).write_pdf(out)
    return out


# -- Standalone pages ------------------------------------------------------------------


def build_section(class_name: str, build: str) -> str:
    b = repo.get_build(class_name, build)
    kind = "Origin" if b.type == "origin" else "Class"
    return "\n".join([
        f"<h1>{b.title}</h1>",
        f"<h2>{kind} Stats</h2>", macros.build_stats(class_name, build),
        f"<h2>{kind} Features</h2>", macros.build_features(class_name, build),
        f"<h2>{kind} Powers</h2>", macros.build_powers(class_name, build),
    ])  # fmt: skip


def write_pages(out_dir: Path) -> list[Path]:
    """One HTML file per class/origin build, one per monster category, plus an index."""
    written: list[Path] = []
    links: list[tuple[str, str]] = []
    for b in repo.builds():
        if b.type not in ("class", "origin") or not repo.powers_for(b) and not repo.features_for(b):
            continue
        name = f"{b.type}-{_slug(b.class_name)}-{_slug(b.build)}.html"
        written.append(_write(out_dir / name, page(b.title, build_section(b.class_name, b.build))))
        links.append((name, f"{b.type.title()}: {b.title}"))

    for category in ("Standard", "Minion", "Elite", "Solo"):
        body = "\n".join([
            f"<h1>{category} monsters</h1>",
            "<h2>Races</h2>", macros.monster_races(category),
            "<h2>Classes</h2>", macros.monster_classes(category),
        ])  # fmt: skip
        name = f"monsters-{_slug(category)}.html"
        written.append(_write(out_dir / name, page(f"{category} monsters", body)))
        links.append((name, f"Monsters: {category}"))

    index = (
        "<h1>Square Fireballs RPG - pages</h1><ul>"
        + "".join(f'<li><a href="{href}">{label}</a></li>' for href, label in links)
        + "</ul>"
    )
    written.append(_write(out_dir / "index.html", page("Square Fireballs RPG - pages", index)))
    return written


# -- Character sheets -------------------------------------------------------------------


class CharacterSheet(BaseModel):
    """Input for `sfrpg sheet`. Ids refer to data/character_creation/*.csv."""

    name: str
    features: list[str] = Field(default_factory=list, description="feature_id values")
    powers: list[str] = Field(default_factory=list, description="power_id values")
    feats: list[str] = Field(default_factory=list, description="feat names")
    items: list[str] = Field(default_factory=list, description="magic item names")

    @classmethod
    def load(cls, path: Path) -> CharacterSheet:
        return cls.model_validate(yaml.safe_load(path.read_text(encoding="utf-8")))


def sheet_html(sheet: CharacterSheet) -> str:
    b = blocks()
    powers = list({p.id: p for p in repo.powers_by_id(sheet.powers)}.values())
    parts = [f"<h1>{sheet.name}</h1>"]
    if sheet.features:
        parts += ["<h2>Features</h2>",
                  b.feature_list(repo.features_by_id(sheet.features), wrap=True)]  # fmt: skip
    if sheet.feats:
        found = repo.feats(names=sheet.feats)
        _check_names("feat", sheet.feats, [f.name for f in found])
        parts += ["<h2>Feats</h2>", b.feat_list(found, wrap=True, short=True)]
    if powers:
        parts += ["<h2>Powers</h2>", b.power_list(powers, _power_class, False)]
    if sheet.items:
        found = repo.magic_items(names=sheet.items)
        _check_names("magic item", sheet.items, [i.name for i in found])
        parts += ["<h2>Magic Items</h2>", b.item_list(found)]
    return page(sheet.name, "\n".join(parts))


def _power_class(power) -> str:
    return repo.class_name_for_power(power.id)


def _check_names(kind: str, wanted: list[str], found: list[str]) -> None:
    missing = sorted(set(wanted) - set(found))
    if missing:
        raise repo.DataError(f"Unknown {kind}(s): {', '.join(missing)}")


def write_sheet(path: Path, out_dir: Path, pdf: bool = False) -> Path:
    sheet = CharacterSheet.load(path)
    html = sheet_html(sheet)
    out = _write(out_dir / f"sheet-{_slug(sheet.name)}.html", html)
    if pdf:
        out = out.with_suffix(".pdf")
        _weasyprint_html()(string=html).write_pdf(out)
    return out
