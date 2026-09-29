from pathlib import Path

import pytest

from sfrpg import macros, outputs
from sfrpg.book import BOOK_DIR, expand
from sfrpg.data import repository as repo
from sfrpg.rules import beast_hp_table, pc_hp_table

EXAMPLES = Path(__file__).resolve().parents[1] / "examples"


def test_power_block():
    html = macros.powers("univ_en_powatk")
    assert '<span class="red large"><strong>Power Attack - Universal 1</strong></span>' in html
    assert "<p>Attack - Encounter - <i>Weapon</i></p>" in html
    assert "<b><i>Free Action (Interrupt)</i></b> - <i>(Trigger: you hit with" in html
    assert '<span class="Power-Upgrade">Upgrade 13: Increase extra damage to 2W<br>' in html


def test_power_block_without_upgrades():
    assert "Power-Upgrade" not in macros.powers("univ_en_powatk", upgrades=False)


def test_feature_power_title():
    html = macros.powers("rng_fe_bststk")
    assert "Beast Strike - Ranger Feature 1" in html


def test_item_labels_bolded():
    html = macros.items("Weapon")
    assert "<strong>Property:</strong> If weapon is Ranged" in html
    assert "Common Weapon - 3/13/23" in html


@pytest.mark.parametrize("build", repo.builds(), ids=lambda b: b.id)
def test_build_power_count_matches_data(build):
    html = macros.build_powers(build.class_name, build.build)
    assert html.count('<div class="Power">') == len(repo.powers_for(build))


# Block counts in the last R-built site (docs/, 2022). The migrated book must
# render the same number of each block.
R_SITE_COUNTS = {
    "classes.md": {'<div class="Power">': 214, '<div class="Feature">': 84,
                   '<table class="Class-table">': 21},
    "origins.md": {'<div class="Power">': 6, '<div class="Feature">': 19,
                   '<table class="Class-table">': 8},
    "combat.md": {'<div class="Power">': 40},
    "feats.md": {'<div class="Feat">': 146},
    "items.md": {'<div class="Power">': 126},
    "monsters.md": {'<div class="monsterRace">': 174, '<div class="monsterClass">': 83},
}  # fmt: skip


@pytest.mark.parametrize("chapter", sorted(p.name for p in BOOK_DIR.glob("*.md")))
def test_chapter_expands(chapter):
    html = expand((BOOK_DIR / chapter).read_text(encoding="utf-8"))
    assert "{{" not in html
    for block, count in R_SITE_COUNTS.get(chapter, {}).items():
        assert html.count(block) == count, block


def test_hp_tables_match_r_formula():
    table = pc_hp_table()
    assert table[0] == (1, 22, 5, 27, 6, 33, 8)
    assert table[10] == (11, 44, 11, 55, 13, 66, 16)
    assert table[29] == (30, 160, 40, 200, 50, 240, 60)
    assert beast_hp_table()[0] == (1, 16, 22)


def test_example_sheet(tmp_path):
    out = outputs.write_sheet(EXAMPLES / "grok.yaml", tmp_path)
    html = out.read_text(encoding="utf-8")
    assert "Beast Basic Attack - Ranger 1" in html
    assert 'class="Power-Upgrade"' not in html
    assert "<strong>Toughness: </strong>" in html


def test_sheet_rejects_unknown_ids(tmp_path):
    bad = tmp_path / "bad.yaml"
    bad.write_text("name: X\npowers: [not_a_power]\n", encoding="utf-8")
    with pytest.raises(repo.DataError):
        outputs.write_sheet(bad, tmp_path)


def test_print_html_has_toc_and_all_chapters():
    html = outputs.print_html()
    assert '<nav class="toc">' in html
    for name in ("Introduction", "Classes", "Monsters", "Optional Material"):
        assert f">{name}</h1>" in html
    assert "](combat.md" not in html


def test_pages(tmp_path):
    written = outputs.write_pages(tmp_path)
    names = {p.name for p in written}
    assert {"index.html", "class-barbarian-rager.html", "monsters-solo.html"} <= names
