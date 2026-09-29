"""Joins and lookups over the game data (replaces the R read_df_* / get_df_* functions)."""

from __future__ import annotations

from collections import Counter, defaultdict
from collections.abc import Iterable
from functools import cache

from .loader import Row, read_csv, read_monster_csv
from .models import Build, Feat, Feature, MagicItem, Power

SKILLS = [
    "Athletics", "Authority", "Endurance", "Concentration", "Stealth", "Finesse",
    "Perception", "Nature", "Trickery", "Diplomacy", "Arcana", "Lore",
]  # fmt: skip


class DataError(LookupError):
    pass


# -- Builds, powers, features -------------------------------------------------


@cache
def builds() -> tuple[Build, ...]:
    return tuple(Build.model_validate(r) for r in read_csv("build"))


def get_build(class_name: str, build: str) -> Build:
    for b in builds():
        if b.class_name == class_name and b.build == build:
            return b
    raise DataError(f"No build {class_name!r} / {build!r} in build.csv")


@cache
def _powers_by_id() -> dict[str, list[Power]]:
    index: dict[str, list[Power]] = defaultdict(list)
    for r in read_csv("powers"):
        p = Power.model_validate(r)
        index[p.id].append(p)
    return index


@cache
def _features_by_id() -> dict[str, list[Feature]]:
    index: dict[str, list[Feature]] = defaultdict(list)
    for r in read_csv("features"):
        f = Feature.model_validate(r)
        index[f.id].append(f)
    return index


def _mapped(map_name: str, key: str, build: Build) -> list[str]:
    return [r[key] for r in read_csv(map_name) if r["build_id"] == build.id]


def powers_for(build: Build) -> list[Power]:
    """Powers mapped to a build, in book order (features first, then type, usage, level)."""
    index = _powers_by_id()
    powers = [p for pid in _mapped("power_map", "power_id", build) for p in index.get(pid, [])]
    return sorted(powers, key=Power.sort_key)


def features_for(build: Build) -> list[Feature]:
    index = _features_by_id()
    return [f for fid in _mapped("feature_map", "feature_id", build) for f in index.get(fid, [])]


def powers_by_id(ids: Iterable[str]) -> list[Power]:
    index = _powers_by_id()
    out = []
    for pid in ids:
        if pid not in index:
            raise DataError(f"Unknown power id {pid!r}")
        out.extend(index[pid])
    return out


def features_by_id(ids: Iterable[str]) -> list[Feature]:
    index = _features_by_id()
    out = []
    for fid in ids:
        if fid not in index:
            raise DataError(f"Unknown feature id {fid!r}")
        out.extend(index[fid])
    return out


def class_name_for_power(power_id: str) -> str:
    """Class of the first build a power is mapped to (used in power titles)."""
    by_id = {b.id: b for b in builds()}
    for r in read_csv("power_map"):
        if r["power_id"] == power_id and r["build_id"] in by_id:
            return by_id[r["build_id"]].class_name
    return ""


# -- Stat blocks -----------------------------------------------------------------


def class_stats(build: Build) -> list[tuple[str, str]]:
    """Key/value rows for a class build's stat table, with skill flags collapsed."""
    for r in read_csv("class_stats"):
        if (r["Class"], r["Build"]) == (build.class_name, build.build):
            skills = ", ".join(s for s in SKILLS if r.get(s) == "1")
            out: list[tuple[str, str]] = []
            for key, value in r.items():
                if key in ("Class", "Build") or key in SKILLS:
                    continue
                out.append((key, value))
                if key == "Trained Skills":
                    out.append(("Skills", skills))
            return out
    return []


def origin_stats(build: Build) -> list[tuple[str, str]]:
    for r in read_csv("origin_stats"):
        if r["build_id"] == build.id:
            return [(k, v) for k, v in r.items() if k != "build_id"]
    return []


def stats_for(build: Build) -> list[tuple[str, str]]:
    return origin_stats(build) if build.type == "origin" else class_stats(build)


# -- Feats, items, equipment -------------------------------------------------------


def feats(category: str | None = None, names: Iterable[str] | None = None) -> list[Feat]:
    rows = [Feat.model_validate(r) for r in read_csv("feats")]
    if category is not None:
        rows = [f for f in rows if f.category == category]
    if names is not None:
        wanted = set(names)
        rows = [f for f in rows if f.name in wanted]
    return rows


def magic_items(type: str | None = None, names: Iterable[str] | None = None) -> list[MagicItem]:
    items = [MagicItem.model_validate(r) for r in read_csv("magic_items")]
    if type is not None:
        items = [i for i in items if i.type == type]
    if names is not None:
        wanted = set(names)
        items = [i for i in items if i.name in wanted]
    return sorted(items, key=MagicItem.sort_key)


def table(name: str, folder: str = "character_creation") -> list[Row]:
    return list(read_csv(name, folder))


def equipment(kind: str, training: str | None = None) -> list[Row]:
    """Rows of the weapons / armor / implements / armor_by_class_reference tables."""
    rows = table(kind)
    if kind == "weapons":
        if training is None:
            raise DataError("equipment('weapons') needs a training category")
        rows = [{k: v for k, v in r.items() if k != "Training"} for r in rows
                if r["Training"] == training]  # fmt: skip
    if kind == "armor_by_class_reference":
        rows = sorted(rows, key=lambda r: r["Class"])
    return rows


def companions(category: str, subcategory: str) -> list[Row]:
    return [
        {k: v for k, v in r.items() if k not in ("category", "subcategory")}
        for r in read_csv("companions")
        if r["category"] == category and r["subcategory"] == subcategory
    ]


# -- Monsters -------------------------------------------------------------------------


def _level_ok(row: Row, min_level: int | None, max_level: int | None) -> bool:
    try:
        level = float(row["Level"])
    except ValueError:
        return min_level is None and max_level is None
    return (min_level is None or level >= min_level) and (max_level is None or level <= max_level)


def _match(value: str, wanted: str | Iterable[str] | None) -> bool:
    if wanted is None:
        return True
    if isinstance(wanted, str):
        return value == wanted
    return value in wanted


def monster_races(
    category: str,
    subcategory: str | Iterable[str] | None = None,
    min_level: int | None = None,
    max_level: int | None = None,
) -> list[Row]:
    return [
        r for r in read_monster_csv("monster_races")
        if r["category"] == category and _match(r["subcategory"], subcategory)
        and _level_ok(r, min_level, max_level)
    ]  # fmt: skip


def monster_classes(category: str, subcategory: str | Iterable[str] | None = None) -> list[Row]:
    return [
        r for r in read_monster_csv("monster_classes")
        if r["category"] == category and _match(r["subcategory"], subcategory)
    ]  # fmt: skip


# -- Validation -----------------------------------------------------------------------


def validate() -> list[str]:
    """Referential-integrity problems in the data. Empty list means clean."""
    issues: list[str] = []
    build_ids = Counter(b.id for b in builds())
    issues += [f"build.csv: duplicate build_id {k!r}" for k, n in build_ids.items() if n > 1]
    names = Counter((b.class_name, b.build) for b in builds())
    issues += [f"build.csv: duplicate Class/Build {k!r}" for k, n in names.items() if n > 1]

    for label, csv_name, mapping, key, index in [
        ("power", "powers", "power_map", "power_id", _powers_by_id()),
        ("feature", "features", "feature_map", "feature_id", _features_by_id()),
    ]:
        issues += [f"{csv_name}.csv: duplicate {key} {k!r}" for k, v in index.items() if len(v) > 1]
        for r in read_csv(mapping):
            if r["build_id"] not in build_ids:
                issues.append(f"{mapping}.csv: unknown build_id {r['build_id']!r}")
            if r[key] not in index:
                issues.append(f"{mapping}.csv: unknown {label} {r[key]!r} ({r['build_id']})")

    for r in read_csv("class_stats"):
        if (r["Class"], r["Build"]) not in names:
            issues.append(f"class_stats.csv: unknown build {r['Class']!r} / {r['Build']!r}")
    for r in read_csv("origin_stats"):
        if r["build_id"] not in build_ids:
            issues.append(f"origin_stats.csv: unknown build_id {r['build_id']!r}")
    return sorted(set(issues))


def unmapped_powers() -> list[str]:
    mapped = {r["power_id"] for r in read_csv("power_map")}
    return sorted(set(_powers_by_id()) - mapped)
