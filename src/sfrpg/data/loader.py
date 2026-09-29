"""Read the raw CSV files in ``data/`` into lists of string dicts.

The CSVs were edited in spreadsheets over many years, so the loader smooths over
a few quirks: literal ``NA`` for missing values, ``#`` comment rows used as
section markers, header rows repeated mid-file, and mixed line endings
(including literal ``\\n`` sequences).
"""

from __future__ import annotations

import csv
import os
from functools import cache
from pathlib import Path

Row = dict[str, str]

DATA_DIR = Path(os.environ.get("SFRPG_DATA", Path(__file__).resolve().parents[3] / "data"))

MISSING = {"NA", "N/A"}


def _clean(value: str | None) -> str:
    if value is None:
        return ""
    # A few cells spell line breaks as a literal backslash-n.
    value = value.replace("\r\n", "\n").replace("\r", "\n").replace("\\n", "\n").strip()
    return "" if value in MISSING else value


@cache
def read_csv(name: str, folder: str = "character_creation") -> tuple[Row, ...]:
    """Read ``data/<folder>/<name>.csv`` as a tuple of rows (all values are strings)."""
    path = DATA_DIR / folder / f"{name}.csv"
    with path.open(encoding="utf-8", newline="") as f:
        reader = csv.reader(f)
        header = [h.strip() for h in next(reader)]
        rows = []
        for raw in reader:
            if not raw or raw[0].lstrip().startswith("#"):
                continue
            if [c.strip() for c in raw[: len(header)]] == header:
                continue  # header row repeated mid-file
            values = [_clean(v) for v in raw] + [""] * (len(header) - len(raw))
            if not any(values):
                continue
            rows.append(dict(zip(header, values, strict=False)))
    return tuple(rows)


def read_monster_csv(name: str) -> tuple[Row, ...]:
    return read_csv(name, folder="monsters")
