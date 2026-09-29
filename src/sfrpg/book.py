"""Chapter pre-processing: expand data macros in Markdown source.

Used by the MkDocs hook and by the PDF / standalone builders, so every output
goes through the same code path.
"""

from __future__ import annotations

from functools import cache
from pathlib import Path

from jinja2 import Environment, StrictUndefined

from .macros import MACROS

BOOK_DIR = Path(__file__).resolve().parents[2] / "book"


@cache
def _chapter_env() -> Environment:
    # Chapters use `{#anchor}` heading attributes, which clash with Jinja's
    # default comment syntax, so comments use `{##  ##}` instead.
    e = Environment(
        undefined=StrictUndefined,
        comment_start_string="{##",
        comment_end_string="##}",
        autoescape=False,
    )
    e.globals.update(MACROS)
    return e


def expand(markdown: str) -> str:
    """Render the Jinja macros in a chapter's Markdown source."""
    return _chapter_env().from_string(markdown).render()
