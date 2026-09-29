import pytest

from sfrpg.data import loader
from sfrpg.data import repository as repo

# Problems present in the data when it was migrated from R (the R inner joins
# silently dropped these). Fixing one means removing it from this list; a new
# problem makes the test fail.
KNOWN_ISSUES = {
    "feature_map.csv: unknown feature 'ori_dwarf_gut' (ori_dwarf)",
    "feature_map.csv: unknown feature 'ori_gnm_cau' (ori_gnome)",
    "feature_map.csv: unknown feature 'ori_gnm_trk' (ori_gnome)",
    "feature_map.csv: unknown feature 'ori_gnome_abs' (ori_gnome)",
    "feature_map.csv: unknown feature 'ori_gnome_ing' (ori_gnome)",
    "feature_map.csv: unknown feature 'ori_gob_pll' (ori_gobln)",
    "feature_map.csv: unknown feature 'rog_scon_eva' (rog_scoun)",
    "feature_map.csv: unknown feature 'univ' (univ)",
    "feature_map.csv: unknown feature 'wlk_train' (wlk_shadow)",
    "features.csv: duplicate feature_id 'ori_dwarf_res'",
    "power_map.csv: unknown build_id 'ori_goblin'",
    "power_map.csv: unknown power 'bar_fe_ragstk' (bar_rager)",
    "power_map.csv: unknown power 'wld_aw_bdycvr' (wld_ins)",
    "powers.csv: duplicate power_id 'ori_dvl_fshld'",
    "powers.csv: duplicate power_id 'wld_aw_inscrd'",
}


def test_no_new_integrity_problems():
    issues = set(repo.validate())
    assert issues - KNOWN_ISSUES == set(), "new data problems"
    assert KNOWN_ISSUES - issues == set(), "fixed problems: remove them from KNOWN_ISSUES"


def test_loader_normalises_quirks(tmp_path, monkeypatch):
    folder = tmp_path / "character_creation"
    folder.mkdir()
    (folder / "quirks.csv").write_bytes(
        b'a,b\r\n# section marker,\r\n1,NA\r\na,b\r\n"x\\ny","p\r\nq"\r\n,\r\n'
    )
    monkeypatch.setattr(loader, "DATA_DIR", tmp_path)
    loader.read_csv.cache_clear()
    try:
        rows = loader.read_csv("quirks")
    finally:
        loader.read_csv.cache_clear()
    assert rows == ({"a": "1", "b": ""}, {"a": "x\ny", "b": "p\nq"})


def test_every_book_build_exists():
    for class_name, build in [("Barbarian", "Rager"), ("Human", "Human"),
                              ("Maneuver", "PC Maneuvers"), ("Shifter", "Brute")]:  # fmt: skip
        assert repo.get_build(class_name, build).class_name == class_name


def test_unknown_build_raises():
    with pytest.raises(repo.DataError):
        repo.get_build("Barbarian", "Nope")


def test_powers_sorted_features_first_then_usage():
    powers = repo.powers_for(repo.get_build("Barbarian", "Berserker"))
    features = [p.is_feature for p in powers]
    assert features == sorted(features, reverse=True)
    attacks = [p for p in powers if p.type == "Attack" and not p.is_feature]
    ranks = [["green", "red", "gray"].index(p.colour) for p in attacks]
    assert ranks == sorted(ranks)


def test_class_stats_collapse_skill_flags():
    stats = dict(repo.class_stats(repo.get_build("Barbarian", "Rager")))
    assert stats["Skills"] == "Athletics, Authority, Endurance, Perception, Nature"
    assert "Athletics" not in stats and "Class" not in stats
    keys = list(stats)
    assert keys.index("Skills") == keys.index("Trained Skills") + 1


def test_monster_filters():
    heroic = repo.monster_races("Standard", "Humanoid", max_level=10)
    assert heroic and all(float(r["Level"]) <= 10 for r in heroic)
    mixed = repo.monster_races("Standard", ["Aberration", "Plant", "Ooze"])
    assert {r["subcategory"] for r in mixed} <= {"Aberration", "Plant", "Ooze"}
