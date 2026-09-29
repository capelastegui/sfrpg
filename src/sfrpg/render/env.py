"""Jinja environment shared by the book, standalone pages and character sheets."""

from __future__ import annotations

import re
from functools import cache
from pathlib import Path

from jinja2 import Environment, FileSystemLoader, StrictUndefined

TEMPLATE_DIR = Path(__file__).parent / "templates"

_ITEM_LABEL = re.compile(r"(Power.+?|Property|Special|Requirement):")


def nl2br(value: str) -> str:
    return str(value).replace("\n", "<br>")


def item_labels(value: str) -> str:
    """Bold the 'Property:' / 'Power (Encounter):' style labels in magic item text."""
    return _ITEM_LABEL.sub(lambda m: f"<strong>{m.group(0)}</strong>", value)


@cache
def env() -> Environment:
    # Data cells contain authored HTML (<br>, <i>), so autoescape stays off.
    e = Environment(
        loader=FileSystemLoader(TEMPLATE_DIR),
        autoescape=False,
        undefined=StrictUndefined,
        trim_blocks=True,
        lstrip_blocks=True,
        keep_trailing_newline=False,
    )
    e.filters["nl2br"] = nl2br
    e.filters["item_labels"] = item_labels
    return e


def blocks():
    """The macros defined in blocks.j2, callable from Python."""
    return env().get_template("blocks.j2").module
