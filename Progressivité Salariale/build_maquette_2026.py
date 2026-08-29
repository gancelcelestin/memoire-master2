# -*- coding: utf-8 -*-
"""Rédige la Maquette_cas_types_2026.xlsx (barème au 1er JUIN 2026) sur le
modèle exact de la maquette DREES 2025.

Même structure de feuille « Barème » (mêmes lignes, mêmes libellés, même ordre
d'appariement avec var_names de bareme.R), valeurs remplacées par celles du
barème au 1er juin 2026 construit dans edifis_model.build_bareme_2026
("juin_gel") : SMIC 1 867,02 €, PASS 4 005 €, IR 2026 gelé, PA réformée
d'avril, allègements gelés (réforme 2026), prestations revalorisées au
1er avril 2026.

Deux paramètres ne rentrent pas dans le format historique :
- la formule des allègements 2026 (exposant 1,75, référence gelée) — les
  cellules tx_exo/plafond_exo donnent le taux au SMIC de référence (0,4021)
  et le point de sortie (3 SMIC de référence) ; la formule complète est
  documentée en colonne Explication et implémentée dans edifis_model.py ;
- la majoration d'âge des AF à 18 ans (réforme du 1er mars 2026) — changement
  de formule, traité par le drapeau af_majo_age_des_18ans du modèle.

Sortie : Maquette_cas_types_2026.xlsx (dans ce dossier).
"""

from pathlib import Path

import pandas as pd

from edifis_model import EDIFIS_DIR, load_var_names, build_bareme_2026

ICI = Path(__file__).parent
SORTIE = ICI / "Maquette_cas_types_2026.xlsx"

# ---------------------------------------------------------------------------
# 1. Barème au 1er juin 2026 et ajustements de présentation
# ---------------------------------------------------------------------------

b = build_bareme_2026("juin_gel")

# la rampe de bonification PA, définie en euros dans les codes MFrance, est
# reconvertie en parts de SMIC net (paramétrisation historique de la maquette)
b["PA_tranche1smic"] = b["PA_seuil_bonif_eur"] / b["smic_n"]
b["PA_tranche2smic"] = b["PA_plafond_bonif_eur"] / b["smic_n"]

# allègements 2026 : cellules réinterprétées (voir notes)
b["tx_exo_fillon_smic"] = b["exo_taux_2026"] + 0.02   # taux au SMIC de référence
b["plafond_exo_fillon"] = 3.0                          # sortie à 3 SMIC de référence

# bandeaux maladie/famille fusionnés dans le barème unique 2026
b["mod_cotis_fam"] = 0.0
b["emp_maladie_t1"] = 0.0

# SMIC horaire brut au 1er juin 2026
b["smic_b_horaire"] = round(b["smic_b"] / (35 * 52 / 12), 2)

# ---------------------------------------------------------------------------
# 2. Notes de mise à jour (colonne Explication)
# ---------------------------------------------------------------------------

NOTE_AVRIL = "Revalorisation du 1er avril 2026 (+0,8 %, décret n°2026-220)"
NOTE_BMAF = "Indexé BMAF : revalorisation du 1er avril 2026 (+0,8 %)"

NOTES = {
    "smic_b": "SMIC brut au 1er juin 2026 (+2,4 % au 1er juin)",
    "smic_b_horaire": "SMIC horaire brut au 1er juin 2026",
    "smic_n": "SMIC net calculé (taux salariaux 2026, PASS 4 005 €)",
    "plafond_ss": "Plafond de la Sécurité sociale 2026",
    "mont_forfaitaire_rsa": NOTE_AVRIL, "mont_forfaitaire_rsa_maj": NOTE_AVRIL,
    "forf_logement_rsa_1": NOTE_AVRIL, "forf_logement_rsa_2": NOTE_AVRIL,
    "forf_logement_rsa_3": NOTE_AVRIL,
    "forf_logement_PA_1": NOTE_AVRIL, "forf_logement_PA_2": NOTE_AVRIL,
    "forf_logement_PA_3": NOTE_AVRIL,
    "bmaf": "BMAF au 1er avril 2026",
    "AF_2enft": NOTE_BMAF, "AF_enft_sup": NOTE_BMAF, "AF_forf_20ans": NOTE_BMAF,
    "majo_age_Af": NOTE_BMAF + " ; réforme du 1er mars 2026 : majoration versée à "
                   "partir de 18 ans (au lieu de 14) pour les nouveaux bénéficiaires "
                   "— changement de formule traité dans le modèle (af_majo_age_des_18ans)",
    "p1_modulation_af": "Seuils de modulation revalorisés au 1er janvier 2026 (~+1,8 %, à confirmer)",
    "p2_modulation_af": "Seuils de modulation revalorisés au 1er janvier 2026 (~+1,8 %, à confirmer)",
    "sup_enf_modulation_af": "Revalorisé au 1er janvier 2026 (~+1,8 %, à confirmer)",
    "LC_isole": "Revalorisation IRL du 1er octobre 2025 (+1,04 %) ; prochaine hausse en octobre 2026",
    "LC_couple": "Revalorisation IRL du 1er octobre 2025 (+1,04 %)",
    "LC_1_pac": "Revalorisation IRL du 1er octobre 2025 (+1,04 %)",
    "Lc_supp_pac": "Revalorisation IRL du 1er octobre 2025 (+1,04 %)",
    "montant_PO": "Revalorisation du 1er octobre 2025 (+1,04 %)",
    "R0_isole": "GELÉ pour 2026 (décret du 28/12/2025)", "R0_couple": "Gelé pour 2026",
    "R0_1pac": "Gelé pour 2026", "R0_2pac": "Gelé pour 2026", "R0_3pac": "Gelé pour 2026",
    "R0_4pac": "Gelé pour 2026", "R0_5pac": "Gelé pour 2026", "R0_6pac": "Gelé pour 2026",
    "R0_pers_sup": "Gelé pour 2026",
    "ars6_10": "ARS 2026", "ars_11_14": "ARS 2026", "ars_15_18": "ARS 2026",
    "montant_CF": NOTE_BMAF, "montant_CF_majo": NOTE_BMAF, "pers_sup_CF": NOTE_BMAF,
    "mont_ASF_total": "Revalorisation du 1er avril 2026 (+0,9 %)",
    "mont_ASF_RSA": "Revalorisation du 1er avril 2026 (+0,9 %)",
    "montant_paje_plein": NOTE_BMAF, "montant_paje_partiel": NOTE_BMAF,
    "mont_forfaitaire_PA": "Réforme de la PA du 1er avril 2026 (638,28 €)",
    "montant_forfaitaire_PA_majo": "Réforme de la PA du 1er avril 2026 (+0,8 %)",
    "PA_bonus": "Réforme du 1er avril 2026 : bonification maximale 240,63 €",
    "PA_tranche1smic": "= 709,18 € (59 x SMIC horaire brut) exprimé en parts de SMIC net",
    "PA_tranche2smic": "Réforme du 1er avril 2026 : rampe étendue jusqu'à 1 658,76 €, "
                       "exprimée en parts de SMIC net",
    "plaf_cmuc_base": "CSS : revalorisation du 1er avril 2026 (868 €/mois personne seule)",
    "plaf_cmuc_1pac": "CSS +0,8 %", "plaf_cmuc_34pac": "CSS +0,8 %", "plaf_cmuc_5pluspac": "CSS +0,8 %",
    "AAH_montant": "Revalorisation du 1er avril 2026 (1 041,59 €) ; réforme Ésat au 1er octobre 2026 hors champ",
    "ASS_mtt_forf": "Revalorisation du 1er avril 2026 (19,48 €/jour)",
    "taux_ir_t1": "Barème IR 2026 (LF 2026), GELÉ dans le scénario du 1er juin",
    "plafond_ir_t1": "11 600 € / 12 — barème IR 2026 gelé",
    "plafond_ir_t2": "29 579 € / 12 — barème IR 2026 gelé",
    "plafond_ir_t3": "84 578 € / 12 — barème IR 2026 gelé",
    "plafond_ir_t4": "181 917 € / 12 — barème IR 2026 gelé",
    "Decote_taux": "Décote 2026 : taux de reprise 45,25 %",
    "mont_decote_celib": "= 897 € / 0,4525 / 12 (présentation DREES : décote = taux x (montant - IR))",
    "mont_decote_couple": "Montant couple, même présentation",
    "tx_exo_fillon_smic": "RÉFORME 2026 : exo = max(0,3821 x (0,5 x (3 x 1823,03/brut - 1))^1,75 + 0,02 ; 0) x brut ; "
                          "cette cellule = taux d'exonération au SMIC DE RÉFÉRENCE (barème GELÉ au SMIC du "
                          "1er janvier 2026 = 1 823,03 €) ; formule non exprimable dans le format historique, "
                          "implémentée dans edifis_model.py",
    "plafond_exo_fillon": "Point de sortie : 3 SMIC de référence (3 x 1 823,03 € = 5 469,09 €), GELÉ",
    "mod_cotis_fam": "Bandeau famille FUSIONNÉ dans le barème unique 2026 : mis à 0",
    "emp_maladie_t1": "Bandeau maladie FUSIONNÉ dans le barème unique 2026 : mis à 0",
    "part_smic_exo": "Sans objet en 2026 (bandeaux fusionnés)",
    "seuil_smic_pat_assmal": "Sans objet en 2026 (bandeaux fusionnés)",
    "deflat": "Hypothèse : déflateurs de la maquette 2025 conservés",
    "deflat_2": "Hypothèse : déflateurs de la maquette 2025 conservés",
    "seuil_D1": "Distribution des niveaux de vie : valeurs 2025 conservées (hypothèse)",
}

# ---------------------------------------------------------------------------
# 3. Construction de la feuille « Barème » 2026
# ---------------------------------------------------------------------------

src = pd.read_excel(EDIFIS_DIR / "data" / "Maquette_cas_types_2025.xls",
                    sheet_name=1, header=None)
names = load_var_names(25)

feuille = src.copy()
idx_valeurs = feuille.index[feuille[1].notna()].tolist()
# la 1re ligne non vide est l'en-tête "Valeur" -> les paramètres commencent après
idx_params = [i for i in idx_valeurs if i != idx_valeurs[0]] if str(
    feuille.loc[idx_valeurs[0], 1]).strip() == "Valeur" else idx_valeurs
assert len(idx_params) == len(names), (len(idx_params), len(names))

for i, nom in zip(idx_params, names):
    feuille.loc[i, 1] = b[nom]
    if nom in NOTES:
        feuille.loc[i, 2] = NOTES[nom]

# en-tête daté
masque = feuille[0].astype(str).str.contains("Barème au 1er janvier 2025", na=False)
feuille.loc[masque, 0] = ("Barème au 1er juin 2026 — SMIC 1 867,02 € ; barème IR et "
                          "allègements gelés ; PA réformée (1er avril) ; prestations "
                          "revalorisées (1er avril)")

# ---------------------------------------------------------------------------
# 4. Feuille Lisez-moi et écriture
# ---------------------------------------------------------------------------

lisezmoi = pd.DataFrame({"Maquette de cas types — barème au 1er juin 2026": [
    "Construite sur le modèle de data/Maquette_cas_types_2025.xls (DREES, Edifis).",
    "Même structure de feuille « Barème » : l'ordre des lignes correspond à la liste "
    "var_names de R/bareme.R (millésime 25), soit 228 paramètres.",
    "",
    "Sources : maquette DREES 2025 + codes MFrance1erjanvier2026.R / MFrance1eravril26.R "
    "(dossier PAproject) + revalorisations du 1er avril 2026 (décret n°2026-220).",
    "",
    "Contenu du barème au 1er juin 2026 :",
    " - SMIC brut 1 867,02 € (+2,4 % au 1er juin) ; PASS 4 005 € ;",
    " - barème IR 2026 gelé (11 600 / 29 579 / 84 578 / 181 917 €, décote 897 €/45,25 %) ;",
    " - prime d'activité réformée au 1er avril : 638,28 €, bonification max 240,63 €, "
    "rampe 709,18 € -> 1 658,76 € (exprimée en parts de SMIC net dans la feuille) ;",
    " - allègements généraux : barème unique 2026 GELÉ au SMIC du 1er janvier "
    "(formule en ^1,75 non exprimable dans le format historique : voir colonne "
    "Explication et edifis_model.py) ; bandeaux maladie/famille fusionnés (cellules à 0) ;",
    " - prestations revalorisées au 1er avril 2026 (+0,8 % ; ASF +0,9 %) ; R0 des APL "
    "gelé ; L+C et PO à +1,04 % (octobre 2025) ; seuils de modulation AF +1,8 % ;",
    " - majoration d'âge des AF à partir de 18 ans (réforme du 1er mars 2026) : "
    "changement de formule, traité dans le code (af_majo_age_des_18ans).",
    "",
    "Hypothèses : déflateurs et distribution des niveaux de vie 2025 conservés.",
    "Chargement Python : edifis_model.load_bareme(2026).",
]})

with pd.ExcelWriter(SORTIE, engine="openpyxl") as xl:
    lisezmoi.to_excel(xl, sheet_name="Lisez-moi", index=False)
    feuille.to_excel(xl, sheet_name="Barème", index=False, header=False)
    # largeurs de colonnes lisibles
    ws = xl.book["Barème"]
    for col, largeur in zip("ABCD", (72, 14, 90, 14)):
        ws.column_dimensions[col].width = largeur
    xl.book["Lisez-moi"].column_dimensions["A"].width = 110

print(f"Maquette écrite : {SORTIE}")
print(f"{len(idx_params)} paramètres renseignés au 1er juin 2026")
