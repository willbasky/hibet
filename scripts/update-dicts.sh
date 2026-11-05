#!/usr/bin/env bash
# Update the dictionaries in hibet/dicts from Christian Steinert's
# tibetan-dictionary project.
#
# Usage:
#   scripts/update-dicts.sh            update files in place
#   scripts/update-dicts.sh --check    report differences, change nothing
#   REF=<branch-or-commit> scripts/update-dicts.sh
#   BASE_URL=file:///path scripts/update-dicts.sh   offline testing
#
# The upstream files are one entry per line: "tibetan|definition". The
# importer of the upstream project skips comment lines, blank lines, lines
# without a separator and entries with an empty headword or definition; we do
# the same, trim trailing whitespace and normalize the text to NFC.
set -euo pipefail
export LC_ALL=C.UTF-8

repo=$(cd "$(dirname "$0")/.." && pwd)
dicts="$repo/hibet/dicts"
titles="$repo/hibet/stuff/titles.toml"
ref=${REF:-master}
base=${BASE_URL:-https://raw.githubusercontent.com/christiansteinert/tibetan-dictionary/$ref/_input/dictionaries/public}
check=0
[[ ${1:-} == --check ]] && check=1

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

echo "source: $base"
echo "target: $dicts"

i=0
: > "$tmp/list"
while IFS=$'\t' read -r up local; do
    [[ -z $up || $up == \#* ]] && continue
    i=$((i + 1))
    if ! curl -fsSL --retry 3 --connect-timeout 20 --max-time 300 \
        "$base/$up" -o "$tmp/raw.$i"; then
        echo "FAIL: $up" >&2
        exit 1
    fi
    [[ -s $tmp/raw.$i ]] || { echo "FAIL: empty $up" >&2; exit 1; }
    # Keep two upstream regressions fixed locally (see the report).
    case $local in
        'Sera-Textbook-Def-T|T') sed -i 's/{yrongs grub/{yongs grub/' "$tmp/raw.$i" ;;
        'JimValby-T|E') sed -i 's/^sne btags|[[:space:]]*$/sne btags|tag/' "$tmp/raw.$i" ;;
    esac
    python3 - "$tmp/raw.$i" "$tmp/$i" <<'PY'
import sys, unicodedata
src, dst = sys.argv[1], sys.argv[2]
with open(src, encoding="utf-8") as inp, open(dst, "w", encoding="utf-8", newline="\n") as out:
    for line in inp:
        if line.startswith("#") or "|" not in line:
            continue
        line = line.rstrip()
        headword, _, definition = line.partition("|")
        if not headword.strip() or not definition:
            continue
        out.write(unicodedata.normalize("NFC", line) + "\n")
PY
    printf '%s\t%s\n' "$tmp/$i" "$local" >> "$tmp/list"
done <<'MAP'
01-Hopkins2015	Hopkins-2015-T|E
02-RangjungYeshe	RangjungYeshe-T|E
03-Berzin	Berzin-T|E
04-Berzin-Def	Berzin-Def-T|E
05-Hackett-Def2015	Hackett-Def-2015-T|E
05-Hopkins-Def2015	Hopkins-Def-2015-T|E
06-Hopkins-Comment	Hopkins-Comment-T|E
07-JimValby	JimValby-T|E
08-IvesWaldo	IvesWaldo-T|E
09-DanMartin	DanMartin-T|TE
10-RichardBarron	RichardBarron-T|E
11-Hopkins-Divisions2015	Hopkins-Divisions-2015-T|ES
12-Hopkins-DivisionsTib2015	Hopkins-Divisions-2015-T|T
13-Hopkins-Examples	Hopkins-Examples-T|E
14-Hopkins-ExamplesTib	Hopkins-Examples-T|T
15-Hopkins-Skt2015	Hopkins-2015-T|S
16-Hopkins-Synonyms1992	Hopkins-Synonyms-T|E
17-Hopkins-TibetanSynonyms1992	Hopkins-Synonyms-T|T
17-Hopkins-TibetanSynonyms2015	Hopkins-Synonyms-2015-T|T
18-Hopkins-TibetanDefinitions2015	Hopkins-Def-2015-T|T
19-Hopkins-TibetanTenses2015	Hopkins-Tibetan-Tenses-2015-T|T
20-Hopkins-othersEnglish2015	Hopkins-others-T|E
21-Mahavyutpatti-Skt	Mahavyutpatti-T|S
22-Yogacharabhumi-glossary	Yoghacharabhumi-glossary-T|S
23-GatewayToKnowledge	Gateway-To-Knowledge-T|E
25-tshig-mdzod-chen-mo-Tib	tshig-mdzod-chen-mo-T|T
26-Verbinator	Verbinator-T|TE
33-TsepakRigdzin	TsepakRigdzin-T|E
34-dung-dkar-tshig-mdzod-chen-mo-Tib	dung-dkar-tshig-mdzod-chen-mo-T|T
35-ThomasDoctor	ThomasDoctor-T|E
36-ComputerTerms	Computer-Terms-T|E
37-dag_tshig_gsar_bsgrigs-Tib	dag_tshig_gsar_bsgrigs-T|T
38-GaengWetzel	Gaeng-Wetzel-T|ES
40-CommonTerms-Lin	CommonTerms-Lin-T|E
42-Sera-Textbook-Definitions	Sera-Textbook-Def-T|T
43-84000Dict	84000Dict-T|E
44-84000Definitions	84000Definitions-T|E
45-84000Synonyms	84000Synonyms-T|T
46-84000Skt	84000Skt-T|S
47-Misc	Misc-T|E
48-TibTermProject	TibTermProject-T|E
49-LokeshChandraSkt	LokeshChandraSkt-T|S
50-NegiSkt	NegiSkt-T|S
51-LaineAbbreviations	LaineAbbreviations-T|T
52-ITLR	ITLR-T|ES
53-Bialek	Bialek-T|E
54-bod_rgya_nang_don_rig_pai_tshig_mdzod	bod_rgya_nang_don_rig_pai_tshig_mdzod-T|T
55-brda_dkrol_gser_gyi_me_long	brda_dkrol_gser_gyi_me_long-T|T
56-chos_rnam_kun_btus	chos_rnam_kun_btus-T|T
57-li_shii_gur_khang	li_shii_gur_khang-T|T
58-sgom_sde_tshig_mdzod_chen_mo	sgom_sde_tshig_mdzod_chen_mo-T|T
59-sgra_bye_brag_tu_rtogs_byed_chen_mo	sgra_bye_brag_tu_rtogs_byed_chen_mo-T|ES
60-sngas_rgyas_chos_gzhung_tshig_mdzod	sngas_rgyas_chos_gzhung_tshig_mdzod-T|T
61-gangs_can_mkhas_grub_rim_byon_ming_mdzod	gangs_can_mkhas_grub_rim_byon_ming_mdzod-T|T
62-bod_yig_tshig_gter_rgya_mtsho	bod_yig_tshig_gter_rgya_mtsho-T|T
67-hotl1	hotl1-T|E
67-hotl2	hotl2-T|E
67-hotl3	hotl3-T|E
68-tibetanlanguage-school	tibetanlanguage-school-T|E
MAP

same=0
changed=0
added=0
while IFS=$'\t' read -r file local; do
    dest="$dicts/$local.txt"
    if [[ -f $dest ]] && cmp -s "$file" "$dest"; then
        same=$((same + 1))
        continue
    fi
    if [[ -f $dest ]]; then
        changed=$((changed + 1))
        [[ $check == 1 ]] && echo "changed $local"
    else
        added=$((added + 1))
        [[ $check == 1 ]] && echo "added   $local"
    fi
    [[ $check == 1 ]] || mv "$file" "$dest"
done < "$tmp/list"

missing=0
while IFS=$'\t' read -r _ local; do
    if ! grep -qF "path = \"$local\"" "$titles"; then
        echo "no title in titles.toml: $local" >&2
        missing=1
    fi
done < "$tmp/list"

printf 'up to date: %d, changed: %d, added: %d\n' "$same" "$changed" "$added"
[[ $missing == 0 && ($check == 0 || ($changed == 0 && $added == 0)) ]]
