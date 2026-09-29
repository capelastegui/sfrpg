"""Functions available inside book chapters as Jinja globals, e.g.

    {{ build_stats("Barbarian", "Rager") }}
    {{ feats("Toughness", wrap=True) }}

Each returns an HTML string that is inserted into the Markdown as a raw HTML block.
"""

from __future__ import annotations

from collections.abc import Iterable

from .data import repository as repo
from .render.env import blocks
from .rules import beast_hp_table, pc_hp_table


def build_stats(class_name: str, build: str) -> str:
    return blocks().stat_table(repo.stats_for(repo.get_build(class_name, build)))


def build_features(class_name: str, build: str) -> str:
    return blocks().feature_list(repo.features_for(repo.get_build(class_name, build)))


def build_powers(class_name: str, build: str) -> str:
    b = repo.get_build(class_name, build)
    return blocks().power_list(repo.powers_for(b), b.class_name)


def powers(*ids: str, upgrades: bool = True) -> str:
    """Specific powers by id, titled with the class of the build they belong to."""
    return blocks().power_list(repo.powers_by_id(ids), _power_class, upgrades)


def _power_class(power) -> str:
    return repo.class_name_for_power(power.id)


def feats(category: str, wrap: bool = False) -> str:
    return blocks().feat_list(repo.feats(category=category), wrap)


def items(type: str) -> str:
    return blocks().item_list(repo.magic_items(type=type))


def equipment(kind: str, training: str | None = None) -> str:
    return blocks().table(repo.equipment(kind, training))


def csv_table(name: str, folder: str = "character_creation", css_class: str = "") -> str:
    return blocks().table(repo.table(name, folder), css_class)


def companions(category: str, subcategory: str) -> str:
    return blocks().table(repo.companions(category, subcategory), "")


def hp_table() -> str:
    return blocks().grouped_table(
        pc_hp_table(),
        ["Level"] + ["HP", "Base Surge Value"] * 3,
        [("", 1), ("Low HP", 2), ("Medium HP", 2), ("High HP", 2)],
        "",
    )


def beast_hp_table_html() -> str:
    return blocks().table(
        [{"Level": lv, "Medium HP": m, "High HP": h} for lv, m, h in beast_hp_table()], ""
    )


def monster_races(
    category: str,
    subcategory: str | Iterable[str] | None = None,
    min_level: int | None = None,
    max_level: int | None = None,
) -> str:
    rows = repo.monster_races(category, subcategory, min_level, max_level)
    return blocks().monster_list(rows, "race")


def monster_classes(category: str, subcategory: str | Iterable[str] | None = None) -> str:
    return blocks().monster_list(repo.monster_classes(category, subcategory), "class")


MACROS = {
    "build_stats": build_stats,
    "build_features": build_features,
    "build_powers": build_powers,
    "powers": powers,
    "feats": feats,
    "items": items,
    "equipment": equipment,
    "csv_table": csv_table,
    "companions": companions,
    "hp_table": hp_table,
    "beast_hp_table": beast_hp_table_html,
    "monster_races": monster_races,
    "monster_classes": monster_classes,
}
