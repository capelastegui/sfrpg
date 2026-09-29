# Monsters

To generate a standard monster, pick a monster race and a monster class.

Note - Recharging attacks:
- Some monsters have attacks with (recharge n+). These attacks are spent on use, and cannot be used again
  until recharged.
- At the end of a monster's turn, roll 1d6 for each spent recharge attack that wasn't used this turn. If
  the roll is equal or higher to the recharge value, the attack is recharged.
- PCs are aware when a monster they can see succeeds at recharging. They don't know details about
  the recharged power, but they can find out with a Skill Check (see Skills chapter).
- Some recharge attacks are labeled as Exhausted: monsters start combat with these powers spent.

## Standard Monsters

### Monster Races - Heroic, Humanoid

{{ monster_races("Standard", "Humanoid", max_level=10) }}

### Monster Races - Heroic, Undead

{{ monster_races("Standard", "Undead", max_level=10) }}

### Monster Races - Heroic, Beast

{{ monster_races("Standard", "Beast", max_level=10) }}

### Monster Races - Heroic, Other

{{ monster_races("Standard", ['Aberration', 'Plant', 'Ooze'], max_level=10) }}

### Monster Classes - Mundane

{{ monster_classes("Standard", "Mundane") }}

### Monster Classes - Humanoid

{{ monster_classes("Standard", "Humanoid") }}

### Monster Classes - Elemental

{{ monster_classes("Standard", "Elemental") }}

## Minion Monsters

Unlike most characters, minions do not have a HP value - do not keep track of damage done to minions. Instead, use
the following:

- Minions have a damage threshold value. Whenever a minion receives damage equal or greater than their damage threshold,
  they are automatically incapacitated.
- A minion that takes damag equal or greater than half their damage threshold is knocked prone.
- The damage threshold of Prone minions is halved. A minion that starts
  their turn Prone must make a saving throw - on a failed save, they can take no actions other than standing up that turn.

In addition, most minions have Partial Vulnerability (Melee or Ranged), included in their stats.

### Minion Races, Heroic Humanoid

{{ monster_races("Minion", "Humanoid", max_level=10) }}

### Minion Races, Heroic Undead

{{ monster_races("Minion", "Undead", max_level=10) }}

### Minion Races, Heroic Beast

{{ monster_races("Minion", "Beast", max_level=10) }}

### Minion Races, Heroic Other

{{ monster_races("Minion", ['Plant', 'Ooze'], max_level=10) }}

### Minion Classes

{{ monster_classes("Minion", "Mundane") }}

## Elite Monsters

All Elite Monsters gain the following rules:

- Action Point: Each Elite Monster has 1 Action Point, which can be spent with an
  Action Point Check, using the same rules as PCs.
- Endure Conditions: At the start of their turn, an Elite Monster can choose a Combat
  Condition afflicting them, and make a Saving Throw. On a successful save, the condition
  is downgraded until their next turn, and they take 2(E) damage.

### Elite Monster Templates:

Apply to Standard monsters to convert them to Elite

Basic Elite Monster:
- Double XP value
- Double HP
- Trait: Elite Attack: Melee and Ranged attacks can target 1 additional enemy in range.
  Area and Close attacks have Burst size increased by 1. Damaging attacks that target a
  single enemy deal +D/2 damage.
  
### Elite Monster Races

Monsters with the following classes gain the Elite monster special
rules, double XP and double HP (already included in stats).

{{ monster_races("Elite", max_level=10) }}

### Elite Monster Classes

Monsters with the following classes gain the Elite monster special
rules, double XP and double HP.

{{ monster_classes("Elite") }}

## Solo Monsters

All Solo Monsters gain the following rules:

- Action Point: Each Solo Monster has 2 Action Points, which can be spent with an
  Action Point Check, using the same rules as PCs.
- Endure Conditions: Twice at the start of their turn, a Solo Monster can choose a Combat
  Condition afflicting them, and make a Saving Throw with a +5 bonus. On a successful save, the condition
  is downgraded until their next turn, and they take 2(E) damage.

### Solo - Dragon - Monster Races

{{ monster_races("Solo", "Dragon") }}

### Solo - Dragon Monster Classes

{{ monster_classes("Solo", "Dragon") }}

### Solo - Eye - Monster Races

{{ monster_races("Solo", "Eye") }}

### Solo - Eye Monster Classes

{{ monster_classes("Solo", "Eye") }}

## Other Templates

Fat:
- +50% XP value
- Double HP

Veteran:
- +25% XP value
- +6/12/24 HP, depending on tier
- +2/4/8 D, depending on tier
- +1H, +1Def

## Base Stats

In this section, we show monster templates with basic stats that can be used
as reference when designing custom monsters.

### Race - Heroic

{{ monster_races("Standard", "Example", max_level=10) }}

### Race - Paragon

{{ monster_races("Standard", "Example", min_level=11, max_level=20) }}

### Race - Epic

{{ monster_races("Standard", "Example", min_level=21) }}

### Class

{{ monster_classes("Standard", "Example") }}

### Minion Race - Heroic

{{ monster_races("Minion", "Example", max_level=10) }}

### Minion Class

{{ monster_classes("Minion", "Example") }}

### Solo Race - Heroic

{{ monster_races("Solo", "Example", max_level=10) }}

### Solo Race - Paragon

{{ monster_races("Solo", "Example", min_level=11, max_level=20) }}

### Solo Race - Epic

{{ monster_races("Solo", "Example", min_level=21) }}
