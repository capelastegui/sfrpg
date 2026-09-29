"""Derived rule tables (ported from R/character_stats.R)."""

from __future__ import annotations

import math


def _hp_level(level: int) -> int:
    """Base HP per level: +1/level in heroic, x2 in paragon, x4 in epic."""
    tier, step = divmod(level - 1, 10)
    return (step + 11) * (1, 2, 4)[tier]


def pc_hp_table() -> list[tuple[int, ...]]:
    """(Level, HP, Surge) for Low / Medium / High HP classes, levels 1-30."""
    rows = []
    for level in range(1, 31):
        base = _hp_level(level)
        row: list[int] = [level]
        for mult in (2, 2.5, 3):
            hp = math.floor(mult * base)
            row += [hp, hp // 4]
        rows.append(tuple(row))
    return rows


def beast_hp_table() -> list[tuple[int, int, int]]:
    """(Level, Medium HP, High HP) for beast companions."""
    return [
        (level, math.floor(1.5 * _hp_level(level)), 2 * _hp_level(level)) for level in range(1, 31)
    ]
