# Classes

## Universal Powers
The following powers are available to Player Characters of all classes.

{{ build_powers("Universal", "Universal") }}

## Barbarian { .newPage }
### Barbarian Rager

{{ build_stats("Barbarian", "Rager") }}

{{ build_features("Barbarian", "Rager") }}

#### Rage Strike
Some Barbarian daily attack powers have the Rage keyword. These powers have an effect that grants
the barbarian an ability that lasts for the rest of the encounter, and a secondary attack that
can be used immediately or on a later turn that encounter. This secondary attack is named a
Rage Strike, is not subject to any cooldown, and can be used by spending a Standard Action.
While under the effect of a Rage power, the barbarian is Raging. Some powers gain additional
abilities while raging.

Note: A character that somehow manages to use two Rage powers in the same encounter (despite the
daily attack cooldown) benefits from the effects of both powers.

Example: Grok the Barbarian spends a Free Action on their turn to use Blood Rage. They can then
spend a Standard Action immediately in order to use that power’s Rage Strike or do so on a later
turn (for example, after an ally has flanked their target).

#### Class Powers { .newPage }

{{ build_powers("Barbarian", "Rager") }}

### Barbarian Berserker

{{ build_stats("Barbarian", "Berserker") }}

{{ build_features("Barbarian", "Berserker") }}

#### Fury Powers

Some Berserker powers have the Fury keyword.These powers have 2 modes: the Peace mode is used while not in Berserker Fury, and the Fury mode is used while in Berserker Fury. A character without the Berserker Fury feature using a Fury power can only use the Peace mode.

#### Class Powers { .newPage }

{{ build_powers("Barbarian", "Berserker") }}

## Fighter { .newPage }
### Fighter Guardian

{{ build_stats("Fighter", "Guardian") }}

{{ build_features("Fighter", "Guardian") }}

#### Class Powers { .newPage }

{{ build_powers("Fighter", "Guardian") }}

### Fighter Sentinel { .newPage }

{{ build_stats("Fighter", "Sentinel") }}

{{ build_features("Fighter", "Sentinel") }}

#### Class Powers { .newPage }

{{ build_powers("Fighter", "Sentinel") }}

## Monk { .newPage }
### Fire Monk

{{ build_stats("Monk", "Fire") }}

{{ build_features("Monk", "Fire") }}

#### Class Powers { .newPage }

{{ build_powers("Monk", "Fire") }}

### Void Monk

{{ build_stats("Monk", "Void") }}

{{ build_features("Monk", "Void") }}

#### Class Powers { .newPage }

{{ build_powers("Monk", "Void") }}

### Earth Monk

{{ build_stats("Monk", "Earth") }}

{{ build_features("Monk", "Earth") }}

#### Class Powers { .newPage }

{{ build_powers("Monk", "Earth") }}

### Water Monk

{{ build_stats("Monk", "Water") }}

{{ build_features("Monk", "Water") }}

#### Class Powers { .newPage }

{{ build_powers("Monk", "Water") }}

### Air Monk

{{ build_stats("Monk", "Air") }}

{{ build_features("Monk", "Air") }}

#### Class Powers { .newPage }

{{ build_powers("Monk", "Air") }}

## Priest { .newPage }
### Priest of Light

{{ build_stats("Priest", "Light") }}

{{ build_features("Priest", "Light") }}

#### Class Powers { .newPage }

{{ build_powers("Priest", "Light") }}

### Priest of Luck { .newPage }

{{ build_stats("Priest", "Luck") }}

{{ build_features("Priest", "Luck") }}

#### Class Powers { .newPage }

{{ build_powers("Priest", "Luck") }}

## Ranger { .newPage }
### Ranger Beastmaster

{{ build_stats("Ranger", "Beastmaster") }}

{{ build_features("Ranger", "Beastmaster") }}

#### Beast Companions 

Beastmaster Rangers have a beast companion that fights alongside them. When creating your character, choose one of the following options as your companion.

{{ companions("Ranger", "Beastmaster") }}

Most stats work the same way as for PCs or monsters - use defense stats and HP when the companion is attacked, and movement when the companion takes a move action. The following stats are unique to beast companions:

- H: Beast hit bonus. Base bonus applied to hit rolls made by the companion.
- B: Beast damage die. Damage die used in attack rolls made by the companion. An attack may roll multiple of these dice, and can apply additional bonus, e.g. 2B + Wis. 

Whenever a companion trait of Beast attack asks you to use an ability modifier, use the ranger's abilities.

Beast HP depend on the ranger's level, and are determined using the following table:

{{ beast_hp_table() }}

The ranger controls their companion's actions. Companions act in the same initiative turn as the ranger, and don't have their own pool of actions. Instead, they can act when the ranger spends an action, in the following ways:

- When the ranger takes a Move Action, the companion can move its speed for free.
- When the ranger uses Total Defense, the companion can do the same for free.
- When the ranger uses an attack power with the Beast keyword, the companion can make attacks as part of that action.
- The ranger gains the class feature power Beast Strike, which allows them to spend a Free Action to make the companion attack an enemy.
- The companion has a melee basic attack, shown below. When an enemy provokes an opportunity attack from the companion, the ranger may spend a Free Reaction to have the companion make an opportunity attack. Both the ranger and the companion can make opportunity attacks the same turn.

{{ powers("rng_aw_bstmba", "rng_fe_bststk", upgrades=False) }}

The companion has no Surge Value and no Stamina of its own. It can only Heal a Surge with the Beast Recovery class feature, using the ranger's surge value. 

#### Class Powers { .newPage }

{{ build_powers("Ranger", "Beastmaster") }}

### Ranger Archer

{{ build_stats("Ranger", "Archer") }}

{{ build_features("Ranger", "Archer") }}

#### Class Powers { .newPage }

{{ build_powers("Ranger", "Archer") }}

### Ranger - Dual Weapon

{{ build_stats("Ranger", "Dual Weapon") }}

{{ build_features("Ranger", "Dual Weapon") }}

#### Class Powers { .newPage }

{{ build_powers("Ranger", "Dual Weapon") }}

### Ranger - Skirmisher

{{ build_stats("Ranger", "Skirmisher") }}

{{ build_features("Ranger", "Skirmisher") }}

#### Class Powers { .newPage }

{{ build_powers("Ranger", "Skirmisher") }}

## Rogue { .newPage }
### Rogue Scoundrel

{{ build_stats("Rogue", "Scoundrel") }}

{{ build_features("Rogue", "Scoundrel") }}

#### Class Powers { .newPage }

{{ build_powers("Rogue", "Scoundrel") }}

## Shaman { .newPage }

### Shaman Special rules

#### Spirit Powers

Some Shaman powers have the Spirit keyword. The following rules apply to a power with this keyword.

* A character may only choose to learn the power if they have the **Spirit Companion** power.
* A character may only use the power while they have an active Spirit Companion.
* Range for the power is determined from the Spirit Companion's space.

#### Spirit Companion

Shamans can conjure a Spirit Companion that assist them in combat. As Conjurations, companions are not characters and do not have HP, but they may be attacked. The following rules apply when a spirit companion is attacked:

* The Companion uses the Shaman's defenses.
* The Companion has Resist (Close and Area attacks).
* When the Companion is damaged,the Shaman takes half that much damage.
* The Companion has a Damage Threshold based on the Shaman's level (see table below). When the Companion takes damage equal or greater than this threshold, it is destroyed (shaman save negates).

Companions are unaffected by damaging effects other than direct attacks, such as damage from zone effects or auras.

<div class="very-narrow" markdown>
Level | Threshold | Level | Threshold | Level | Threshold
-- | -- | -- | -- | -- | --
1 | 8 | 11 | 20 | 21 | 44
2 | 9 | 12 | 22 | 22 | 48
3 | 10 | 13 | 24 | 23 | 52
4 | 11 | 14 | 26 | 24 | 56
5 | 12 | 15 | 28 | 25 | 60
6 | 13 | 16 | 30 | 26 | 64
7 | 14 | 17 | 32 | 27 | 68
8 | 15 | 18 | 34 | 28 | 72
9 | 16 | 19 | 36 | 29 | 76
10 | 17 | 20 | 38 | 30 | 80
</div>

### Predator Shaman { .newPage }

{{ build_stats("Shaman", "Predator") }}

{{ build_features("Shaman", "Predator") }}

#### Class Powers { .newPage }

{{ build_powers("Shaman", "Predator") }}

### Vitality Shaman { .newPage }

{{ build_stats("Shaman", "Vitality") }}

{{ build_features("Shaman", "Vitality") }}

#### Class Powers { .newPage }

{{ build_powers("Shaman", "Vitality") }}

## Warlock { .newPage }
### Shadow Warlock

{{ build_stats("Warlock", "Shadow") }}

{{ build_features("Warlock", "Shadow") }}

#### Class Powers { .newPage }

{{ build_powers("Warlock", "Shadow") }}

## Warlord { .newPage }
### Warlord Tactician

{{ build_stats("Warlord", "Tactician") }}

{{ build_features("Warlord", "Tactician") }}

#### Class Powers { .newPage }

{{ build_powers("Warlord", "Tactician") }}

## Wizard { .newPage }
### Wizard Elementalist

{{ build_stats("Wizard", "Elementalist") }}

{{ build_features("Wizard", "Elementalist") }}

#### Class Powers { .newPage }

{{ build_powers("Wizard", "Elementalist") }}
