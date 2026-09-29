"""Typed views over the CSV rows.

Field aliases are the original CSV column names, so ``Model.model_validate(row)``
works directly on loader output. Only entities that templates or joins need to
reason about are modelled; plain reference tables stay as rows.
"""

from __future__ import annotations

from pydantic import BaseModel, ConfigDict, Field

USAGE_COLOURS = {"": "green", "At-Will": "green", "Encounter": "red"}
USAGE_ORDER = ["green", "red", "gray"]

RARITY_COLOURS = {"Common": "black", "Uncommon": "darkgray", "Rare": "gold"}
RARITY_ORDER = ["black", "darkgray", "gold"]


def _num(value: str, default: float = 0) -> float:
    try:
        return float(value)
    except ValueError:
        return default


class _Row(BaseModel):
    model_config = ConfigDict(populate_by_name=True, extra="ignore", frozen=True)


class Build(_Row):
    id: str = Field(alias="build_id")
    class_name: str = Field(alias="Class")
    build: str = Field(alias="Build")
    type: str

    @property
    def title(self) -> str:
        return f"{self.class_name} {self.build}"


class Feature(_Row):
    id: str = Field(alias="feature_id")
    name: str = Field(alias="Name")
    description: str = Field("", alias="Description")


class Power(_Row):
    id: str = Field(alias="power_id")
    name: str = Field(alias="Name")
    is_feature_raw: str = Field("", alias="isFeature")
    level: str = Field("", alias="Level")
    type: str = Field("", alias="Type")
    usage_limit: str = Field("", alias="UsageLimit")
    usage_number: str = Field("", alias="UsageNumber")
    keywords: str = Field("", alias="Keywords")
    action: str = Field("", alias="Action")
    trigger: str = Field("", alias="Trigger")
    range: str = Field("", alias="Range")
    target: str = Field("", alias="Target")
    attack_roll: str = Field("", alias="AttackRoll")
    effect_pre: str = Field("", alias="EffectPre")
    hit: str = Field("", alias="Hit")
    miss: str = Field("", alias="Miss")
    effect_post: str = Field("", alias="EffectPost")
    misc: str = Field("", alias="Misc")
    secondary_attack: str = Field("", alias="SecondaryAttack")
    upgrades: str = Field("", alias="Upgrades")
    summary: str = Field("", alias="Summary")

    @property
    def is_feature(self) -> bool:
        return self.is_feature_raw == "Feature"

    @property
    def colour(self) -> str:
        if self.usage_limit.startswith("Daily"):
            return "gray"
        return USAGE_COLOURS.get(self.usage_limit, "green")

    def sort_key(self) -> tuple:
        return (
            not self.is_feature,
            self.type,
            USAGE_ORDER.index(self.colour),
            _num(self.level),
            self.name,
        )


class Feat(_Row):
    name: str = Field(alias="Name")
    category: str = Field("", alias="Category")
    level: str = Field("", alias="Level")
    keywords: str = Field("", alias="Keywords")
    requirements: str = Field("", alias="Requirements")
    text: str = Field("", alias="Text")
    summary: str = Field("", alias="Summary")


class MagicItem(_Row):
    name: str
    type: str
    rarity: str
    level_h: str = ""
    level_p: str = ""
    level_e: str = ""
    p1: str = ""
    p2: str = ""
    p3: str = ""
    p4: str = ""
    p5: str = ""

    @property
    def colour(self) -> str:
        return RARITY_COLOURS.get(self.rarity, "black")

    @property
    def properties(self) -> list[str]:
        return [p for p in (self.p1, self.p2, self.p3, self.p4, self.p5) if p]

    def sort_key(self) -> tuple:
        rarity = RARITY_ORDER.index(self.colour)
        return (self.type, rarity, _num(self.level_h), self.name)
