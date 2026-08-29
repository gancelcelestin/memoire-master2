# -*- coding: utf-8 -*-
"""
Réplication Python du modèle de cas types EDIFIS (DREES).

Source : edifis-main (R/bareme.R, R/base.R, R/cas-type.R), législation 2025
(Maquette_cas_types_2025.xls, barème au 1er janvier 2025, feuille "Barème").

Objectif : pour un cas type donné, passer d'un vecteur de salaires bruts
mensuels à toutes les composantes du revenu disponible (cotisations, CSG,
allègements généraux, IR, prime d'activité, RSA, allocations logement,
prestations familiales), puis décomposer le surcoût employeur nécessaire
pour augmenter le revenu disponible du salarié de +100 euros.

Périmètre : cas types "actifs salariés" sans handicap (AAH), sans ASS,
sans chômage (ARE) — c'est-à-dire les principaux cas types de l'application
Edifis en mode "salaire".
"""

from __future__ import annotations

import re
from dataclasses import dataclass, field, replace
from pathlib import Path

import numpy as np
import pandas as pd

# ---------------------------------------------------------------------------
# Chemins par défaut
# ---------------------------------------------------------------------------

EDIFIS_DIR = Path(r"C:\projet_claude\PAproject\edifis-main")


# ---------------------------------------------------------------------------
# 1. Import du barème législatif (réplique de R/bareme.R + modif_df)
# ---------------------------------------------------------------------------

def load_var_names(year: int = 25, bareme_r_path: Path | None = None) -> list[str]:
    """Extrait la liste var_names de l'année demandée depuis R/bareme.R.

    On parse le fichier R plutôt que de recopier la liste à la main pour
    garantir l'ordre exact d'appariement avec la feuille Excel.
    """
    path = bareme_r_path or (EDIFIS_DIR / "R" / "bareme.R")
    text = path.read_text(encoding="utf-8", errors="replace")
    # bloc  "25" = c( ... )  ou  "25"=c( ... ) : on capture jusqu'à la
    # prochaine entrée  "NN"=c(  ou la fin de la liste
    m = re.search(rf'"{year}"\s*=\s*c\((.*?)\)\s*,?\s*(?:"\d+"\s*=\s*c\(|\)\s*#)',
                  text, flags=re.S)
    if m is None:
        raise ValueError(f"Année {year} introuvable dans {path}")
    names = re.findall(r'"([^"]+)"', m.group(1))
    return names


def load_bareme(year: int = 2025, data_dir: Path | None = None) -> dict[str, float]:
    """Réplique server.R (read_excel sheet=2) + modif_df() de bareme.R.

    Lit la feuille "Barème" de la maquette, garde les 2 premières colonnes,
    supprime les lignes sans valeur, et associe les noms var_names.

    year=2026 : lit Maquette_cas_types_2026.xlsx (barème au 1er juin 2026,
    produite par build_maquette_2026.py, même structure que la maquette DREES)
    et positionne les drapeaux de la réforme 2026 non exprimables dans le
    format historique (formule des allègements, majoration d'âge des AF).
    """
    if year >= 2026:
        xls_path = Path(__file__).parent / f"Maquette_cas_types_{year}.xlsx"
        names = load_var_names(25)          # même structure que la maquette 2025
    else:
        data_dir = data_dir or (EDIFIS_DIR / "data")
        xls_path = data_dir / f"Maquette_cas_types_{year}.xls"
        names = load_var_names(year % 100)
    df = pd.read_excel(xls_path, sheet_name="Barème" if year >= 2026 else 1,
                       header=0)            # comme read_excel R
    df = df.iloc[:, :2]
    df = df[df.iloc[:, 1].notna()]
    if len(df) != len(names):
        raise ValueError(
            f"Nombre de valeurs ({len(df)}) != nombre de noms ({len(names)}) "
            f"pour l'année {year}"
        )
    values = pd.to_numeric(df.iloc[:, 1], errors="raise").to_numpy(dtype=float)
    b = dict(zip(names, values))
    if year >= 2026:
        # réforme des allègements 2026 (formule ^1,75, non exprimable dans la
        # feuille) : taux lu dans tx_exo_fillon_smic (+2 % de socle inclus),
        # référence GELÉE au SMIC du 1er janvier 2026
        b["reforme_allegements_2026"] = True
        b["exo_taux_2026"] = b["tx_exo_fillon_smic"] - 0.02
        b["exo_smic_ref_2026"] = 1823.03
        # majoration d'âge des AF à partir de 18 ans (réforme du 1er mars 2026)
        b["af_majo_age_des_18ans"] = True
    return b


# ---------------------------------------------------------------------------
# 2. Définition d'un cas type
# ---------------------------------------------------------------------------

@dataclass
class CasType:
    """Composition du ménage, à la manière de choix_input() (R/base.R)."""
    label: str
    n0: int = 1                    # situation conjugale : 1 isolé, 2 couple (imposition commune), 3 concubins
    nb_enft_3: int = 0             # enfants < 3 ans
    nb_enft_35: int = 0            # 3-5 ans
    nb_enft_610: int = 0           # 6-10 ans
    nb_enft_1113: int = 0          # 11-13 ans
    nb_enft_14: int = 0            # 14 ans
    nb_enft_1519: int = 0          # 15-19 ans
    nb_enft_20: int = 0            # 20 ans
    sal_brut_conjoint: float = 0.0 # salaire brut mensuel du conjoint (fixe)
    autres_rev: float = 0.0        # autres revenus mensuels
    maj_isole_rsa: int = 0         # majoration isolement RSA/PA (parent isolé récent)
    eligible_asf: int = 0          # allocation de soutien familial (parent isolé)
    proprietaire: int = 0          # 1 = propriétaire non accédant / hébergé (pas d'AL)

    @property
    def nb_adultes(self) -> int:
        return min(self.n0, 2)

    @property
    def nb_enfants(self) -> int:
        return (self.nb_enft_3 + self.nb_enft_35 + self.nb_enft_610 +
                self.nb_enft_1113 + self.nb_enft_14 + self.nb_enft_1519 +
                self.nb_enft_20)


# Principaux cas types (mêmes familles que l'application Edifis)
CAS_TYPES: dict[str, CasType] = {
    "celibataire": CasType(
        label="Célibataire sans enfant, locataire"),
    "celibataire_proprietaire": CasType(
        label="Célibataire sans enfant, propriétaire", proprietaire=1),
    "monoparent_1enf": CasType(
        label="Famille monoparentale, 1 enfant (6-10 ans), locataire",
        n0=1, nb_enft_610=1, eligible_asf=1),
    "monoparent_2enf": CasType(
        label="Famille monoparentale, 2 enfants (6-10 et 11-13 ans), locataire",
        n0=1, nb_enft_610=1, nb_enft_1113=1, eligible_asf=1),
    "couple_monoactif_0enf": CasType(
        label="Couple monoactif sans enfant, locataire", n0=2),
    "couple_monoactif_2enf": CasType(
        label="Couple monoactif, 2 enfants (6-10 et 11-13 ans), locataire",
        n0=2, nb_enft_610=1, nb_enft_1113=1),
    "couple_monoactif_3enf": CasType(
        label="Couple monoactif, 3 enfants (3-5, 6-10 et 11-13 ans), locataire",
        n0=2, nb_enft_35=1, nb_enft_610=1, nb_enft_1113=1),
    "couple_biactif_smic_2enf": CasType(
        label="Couple biactif (conjoint au SMIC), 2 enfants (6-10 et 11-13 ans), locataire",
        n0=2, nb_enft_610=1, nb_enft_1113=1, sal_brut_conjoint=1801.80),
}


# ---------------------------------------------------------------------------
# 3. Fonctions auxiliaires (répliques de global.R et R/base.R, année >= 2019)
# ---------------------------------------------------------------------------

def SI(condition, si_vrai, si_faux):
    """Réplique de SI() de global.R : cond*vrai + (1-cond)*faux."""
    cond = np.asarray(condition, dtype=float)
    return cond * si_vrai + (1.0 - cond) * si_faux


def ceiling_dec(x, level=1):
    """Réplique de ceiling_dec() : round(x + 5*10^(-level-1), level)."""
    return np.round(np.asarray(x, dtype=float) + 5 * 10.0 ** (-level - 1), level)


def cs_emp(x, b):
    """Cotisations sociales employeur hors allègements (base.R, year>18)."""
    x = np.asarray(x, dtype=float)
    P = b["plafond_ss"]
    return ((x > 0) * b["taux_cs_emp_t1"] * np.minimum(x, P)
            + (x > P) * b["taux_cs_emp_t2"] * np.minimum(np.maximum(x - P, 0), 2 * P)
            + (x > 3 * P) * b["taux_cs_emp_t3"] * np.minimum(np.maximum(x - 3 * P, 0), P)
            + (x > 4 * P) * b["taux_cs_emp_t4"] * np.minimum(np.maximum(x - 4 * P, 0), 4 * P)
            + (x > 8 * P) * b["taux_cs_emp_t5"] * np.maximum(x - 8 * P, 0)
            + (x > P) * b["emp_retraites_comp_cet"] * np.minimum(x, 8 * P))


def cs_sal(x, b):
    """Cotisations sociales salarié (base.R, year>18)."""
    x = np.asarray(x, dtype=float)
    P = b["plafond_ss"]
    return ((x > 0) * b["taux_cs_sal_t1"] * np.minimum(x, P)
            + (x > P) * b["taux_cs_sal_t2"] * np.minimum(np.maximum(x - P, 0), 2 * P)
            + (x > 3 * P) * b["taux_cs_sal_t3"] * np.minimum(np.maximum(x - 3 * P, 0), P)
            + (x > 4 * P) * b["taux_cs_sal_t4"] * np.minimum(np.maximum(x - 4 * P, 0), 4 * P)
            + (x > 8 * P) * b["taux_cs_sal_t5"] * np.maximum(x - 8 * P, 0)
            + (x > P) * b["sal_retraites_comp_cet"] * np.minimum(x, 8 * P))


def csg_deduc(x, b):
    x = np.asarray(x, dtype=float)
    P4 = 4 * b["plafond_ss"]
    return ((x > 0) * b["taux_csg_deduc_t1"] * np.minimum(x, P4)
            + (x > P4) * b["taux_csg_deduc_t2"] * np.maximum(x - P4, 0))


def csg_non_deduc(x, b):
    x = np.asarray(x, dtype=float)
    P4 = 4 * b["plafond_ss"]
    return ((x > 0) * b["taux_csgcrds_nondeduc_t1"] * np.minimum(x, P4)
            + (x > P4) * b["taux_csgcrds_nondeduc_t2"] * np.maximum(x - P4, 0))


def exo_fillon(x, tps_travail, percen_smic, b):
    """Allègements généraux (cas-type.R : exo_fillon(x, y, z, bareme))."""
    x = np.asarray(x, dtype=float)
    y = np.asarray(tps_travail, dtype=float)
    z = np.asarray(percen_smic, dtype=float)
    T = b["tx_exo_fillon_smic"]
    C = b["plafond_exo_fillon"]
    with np.errstate(divide="ignore", invalid="ignore"):
        ratio = np.where(z > 0, y / z, 0.0)
    v = np.where(z > 0, np.maximum(T * x * (C * ratio - 1) / (C - 1), 0.0), 0.0)
    return v


def exo_fillon1(x, b):
    """Allègements du conjoint (base.R : exo_fillon1)."""
    T = b["tx_exo_fillon_smic"]
    C = b["plafond_exo_fillon"]
    if x <= 0:
        return 0.0
    return max(x * (T / (C - 1)) * (C * (b["smic_b"] / x) - 1), 0.0)


def bandeaux_maladie_famille(brut, b):
    """Réductions de taux maladie (bandeau) et famille (modulation).

    NB : jusqu'à la maquette 2024, part_smic_exo et seuil_smic_pat_assmal
    étaient exprimés en parts de SMIC (3,5 / 2,5) et cas-type.R comparait
    percen_smic <= param*100. Dans la maquette 2025, ces paramètres sont des
    montants en euros (seuils gelés par la LFSS 2024 au SMIC du 31/12/2023 :
    3,3 SMIC = 5 765,76 € et 2,25 SMIC = 3 931,20 €), ce que le code R n'a
    pas suivi (la condition devient toujours vraie). On applique ici
    l'interprétation économiquement correcte : comparaison du brut en euros.
    """
    brut = np.asarray(brut, dtype=float)
    seuil_fam = b["part_smic_exo"]
    seuil_mal = b["seuil_smic_pat_assmal"]
    if seuil_fam < 100:    # anciennes maquettes : parts de SMIC
        seuil_fam = seuil_fam * b["smic_b"]
    if seuil_mal < 100:
        seuil_mal = seuil_mal * b["smic_b"]
    reduc = (SI(brut <= seuil_fam, b["mod_cotis_fam"] * brut, 0.0)
             + SI(brut <= seuil_mal, b["emp_maladie_t1"] * brut, 0.0))
    return reduc


def exo_reforme_2026(brut, b):
    """Allègements généraux issus de la réforme 2026 (fusion des bandeaux).

    Formule reprise de MFrance1erjanvier2026.R / MFrance1eravril26.R :
        exo = max( taux_exo * (0,5 * (3*SMIC_ref/brut - 1))^1,75 + 0,02 ; 0 ) * brut
        exo = 0 si brut >= 3*SMIC_ref
    SMIC_ref = b["exo_smic_ref_2026"] : SMIC de référence du barème d'allègements
    (dans le scénario "hausse du SMIC au 1er juin + gel du barème", il reste
    gelé à 1 823,03 € alors que smic_b passe à 1 867,02 €).
    L'exonération est plafonnée aux cotisations patronales dues (sans objet
    au-delà de ~0,85 SMIC, utile seulement en bas d'échelle).
    """
    brut = np.asarray(brut, dtype=float)
    T = b["exo_taux_2026"]
    S = b["exo_smic_ref_2026"]
    with np.errstate(divide="ignore", invalid="ignore"):
        taux = np.where(brut > 0, T * (0.5 * (3 * S / brut - 1)) ** 1.75 + 0.02, 0.0)
    exo = np.where((brut > 0) & (brut < 3 * S), np.maximum(taux, 0.0) * brut, 0.0)
    return np.minimum(exo, cs_emp(brut, b))


def exo_fillon_generique(taux_smic, sortie_smic, smic_ref=None):
    """Fabrique un barème d'allègements alternatif de forme Fillon :
    taux d'exonération = taux_smic au niveau du SMIC, extinction hyperbolique
    classique taux_smic x (sortie x SMIC/brut - 1)/(sortie - 1), nulle au-delà
    de sortie_smic x SMIC. smic_ref : SMIC de référence en euros (par défaut,
    le smic_b courant du barème). À utiliser via bareme["exo_custom"].

    Exemples (nombre d'employés >= 50) :
      vision Bozio-Wasmer   : exo_fillon_generique(0.3621, 2.5)
        soit 0,3621 x (1/1,5) x (2,5 SMIC/brut - 1) x brut
      vision rapport du SMIC : exo_fillon_generique(0.4021, 2.0)
        soit 0,4021 x (2 SMIC/brut - 1) x brut
    """
    def exo(brut, b):
        brut = np.asarray(brut, dtype=float)
        S = smic_ref if smic_ref is not None else b["smic_b"]
        with np.errstate(divide="ignore", invalid="ignore"):
            taux = np.where(brut > 0,
                            taux_smic * (sortie_smic * S / brut - 1) / (sortie_smic - 1),
                            0.0)
        e = np.where((brut > 0) & (brut < sortie_smic * S),
                     np.maximum(taux, 0.0) * brut, 0.0)
        return np.minimum(e, cs_emp(brut, b))
    return exo


def cotisations_employeur(brut, b):
    """Cotisations patronales à taux fixe (hors allègements) et allègements.

    Retourne (cotis_emp, exo). Avant 2026 : barème DREES (cs_emp - bandeaux,
    allègements Fillon). À partir de 2026 (clé "reforme_allegements_2026") :
    taux pleins (les bandeaux maladie/famille sont fusionnés dans le nouveau
    barème d'allègements unique jusqu'à 3 SMIC). Si le barème contient une
    clé "exo_custom" (callable(brut, b)), elle remplace le barème
    d'allègements (taux de cotisations pleins, sans bandeaux).
    """
    brut = np.asarray(brut, dtype=float)
    if b.get("exo_custom") is not None:
        cotis = cs_emp(brut, b)
        exo = b["exo_custom"](brut, b)
    elif b.get("reforme_allegements_2026"):
        cotis = cs_emp(brut, b)
        exo = exo_reforme_2026(brut, b)
    else:
        cotis = cs_emp(brut, b) - bandeaux_maladie_famille(brut, b)
        percen = brut / b["smic_b"] * 100
        exo = exo_fillon(brut, np.minimum(percen, 100.0), percen, b)
    return cotis, exo


def bareme_ir(rfr_par_part, b):
    """Barème progressif de l'IR par part (cas-type.R, year>14)."""
    r = np.asarray(rfr_par_part, dtype=float)
    return ((r > 0) * b["taux_ir_t1"] * np.minimum(r, b["plafond_ir_t1"])
            + (r > b["plafond_ir_t1"]) * b["taux_ir_t2"]
              * np.minimum(np.maximum(r - b["plafond_ir_t1"], 0), b["plafond_ir_t2"] - b["plafond_ir_t1"])
            + (r > b["plafond_ir_t2"]) * b["taux_ir_t3"]
              * np.minimum(np.maximum(r - b["plafond_ir_t2"], 0), b["plafond_ir_t3"] - b["plafond_ir_t2"])
            + (r > b["plafond_ir_t3"]) * b["taux_ir_t4"]
              * np.minimum(np.maximum(r - b["plafond_ir_t3"], 0), b["plafond_ir_t4"] - b["plafond_ir_t3"])
            + (r > b["plafond_ir_t4"]) * b["taux_ir_t5"] * np.maximum(r - b["plafond_ir_t4"], 0))


def impot_revenu(rfr, nb_part, nb_adultes, max_qf, b):
    """Réplique de la fonction IR() de cas-type.R (year 2025 : décote year>15,
    pas de RI 2017, seuil de recouvrement). Retourne l'impôt après seuil de
    recouvrement (imp_recouvr)."""
    rfr = np.asarray(rfr, dtype=float)
    imp_tot = bareme_ir(rfr / nb_part, b) * nb_part
    imp_tot_sansdemi = bareme_ir(rfr / nb_adultes, b) * nb_adultes
    avantage_qf = imp_tot_sansdemi - imp_tot
    imp_plaf_qf = SI(avantage_qf < max_qf, imp_tot, imp_tot_sansdemi - max_qf)
    mont_decote = b["mont_decote_celib"] if nb_adultes == 1 else b["mont_decote_couple"]
    decote = np.maximum(0.0, b["Decote_taux"] * mont_decote - b["Decote_taux"] * imp_plaf_qf)
    imp_decote = np.maximum(0.0, imp_plaf_qf - decote)
    imp_recouvr = SI(imp_decote > b["seuil_recouvrement_IR"], imp_decote, 0.0)
    return imp_recouvr


# ---------------------------------------------------------------------------
# 4. Grandeurs du ménage (réplique de choix_input(), R/base.R, year 2025)
# ---------------------------------------------------------------------------

def menage_params(cas: CasType, b: dict) -> dict:
    """Montants forfaitaires et paramètres qui ne dépendent que de la
    composition familiale (RSA, PA, AF, AL, QF, ASF, conjoint)."""
    na, ne = cas.nb_adultes, cas.nb_enfants
    p = {}

    # --- RSA : montant forfaitaire ---
    if na == 1 and cas.maj_isole_rsa == 0:
        mf_rsa = b["mont_forfaitaire_rsa"] * (
            1 + (ne >= 1) * b["tx_majo_rsa_pac1"] + (ne >= 2) * b["tx_majo_rsa_enf12"]
            + max(ne - 2, 0) * b["tx_majo_rsa_enf3"])
    elif na == 2:
        mf_rsa = b["mont_forfaitaire_rsa"] * (
            1 + b["tx_majo_rsa_pac1"] + min(ne, 2) * b["tx_majo_rsa_enf12"]
            + max(ne - 2, 0) * b["tx_majo_rsa_enf3"])
    else:  # isolé avec majoration
        mf_rsa = b["mont_forfaitaire_rsa_maj"] * (
            1 + min(ne, 2) * b["tx_majo_rsa_majo_enf12"]
            + max(ne - 2, 0) * b["tx_majo_rsa_majo_enf3"])
    p["mf_RSA"] = mf_rsa

    # Forfait logement RSA / PA
    taille = na + ne
    p["fl_RSA"] = (b["forf_logement_rsa_1"] if taille == 1
                   else b["forf_logement_rsa_2"] if taille == 2
                   else b["forf_logement_rsa_3"])

    # --- Prime d'activité : montant forfaitaire (year>15) ---
    if na == 1 and cas.maj_isole_rsa == 0:
        mf_pa = b["mont_forfaitaire_PA"] * (
            1 + (ne >= 1) * b["tx_majo_rsa_pac1"] + (ne >= 2) * b["tx_majo_rsa_enf12"]
            + max(ne - 2, 0) * b["tx_majo_rsa_enf3"])
    elif na == 2:
        mf_pa = b["mont_forfaitaire_PA"] * (
            1 + b["tx_majo_rsa_pac1"] + min(ne, 2) * b["tx_majo_rsa_enf12"]
            + max(ne - 2, 0) * b["tx_majo_rsa_enf3"])
    else:
        mf_pa = b["montant_forfaitaire_PA_majo"] * (
            1 + min(ne, 2) * b["tx_majo_rsa_majo_enf12"]
            + max(ne - 2, 0) * b["tx_majo_rsa_majo_enf3"])
    p["mf_PA"] = mf_pa
    p["fl_PA"] = (b["forf_logement_PA_1"] if taille == 1
                  else b["forf_logement_PA_2"] if taille == 2
                  else b["forf_logement_PA_3"])

    # --- Allocations familiales ---
    # Réforme du 1er mars 2026 : la majoration pour âge se déclenche à 18 ans
    # (au lieu de 14) pour les nouveaux bénéficiaires. Approximation avec les
    # tranches d'âge d'Edifis : les tranches "14 ans" et "15-19 ans" ne
    # déclenchent plus la majoration (enfants supposés < 18 ans).
    ne_moins20 = ne - cas.nb_enft_20
    nb_enft_majo = 0 if b.get("af_majo_age_des_18ans") else (cas.nb_enft_1519 + cas.nb_enft_14)
    if ne_moins20 >= 3:
        af_majo_age = nb_enft_majo * b["majo_age_Af"]
    elif ne_moins20 == 2 and nb_enft_majo == 2:
        af_majo_age = b["majo_age_Af"]
    else:
        af_majo_age = 0.0
    p["af_majo_age"] = af_majo_age

    if ne_moins20 >= 2:
        montant_af = (b["AF_2enft"] + max(0.0, b["AF_enft_sup"] * (ne_moins20 - 2))
                      + (b["AF_forf_20ans"] if (cas.nb_enft_20 >= 1 and ne >= 3) else 0.0)
                      + af_majo_age)
    else:
        montant_af = 0.0
    p["montant_af"] = montant_af

    # Plafonds de modulation des AF (réplique du comportement effectif de
    # base.R : le terme lié aux enfants de 20 ans n'est pas additionné)
    supp = (ne_moins20 - 2) * b["sup_enf_modulation_af"] if ne_moins20 > 2 else 0.0
    p["plaf_1tranch_AF"] = b["p1_modulation_af"] + supp
    p["plaf_2tranch_AF"] = b["p2_modulation_af"] + supp

    # --- Allocations logement : paramètres L+C, PO, TF, R0 ---
    if ne == 0:
        al_LC = b["LC_isole"] if na == 1 else b["LC_couple"]
    elif ne == 1:
        al_LC = b["LC_1_pac"]
    else:
        al_LC = b["LC_1_pac"] + b["Lc_supp_pac"] * (ne - 1)
    p["al_LC"] = al_LC
    p["al_PO"] = max(b["montant_PO"], b["taux_PO"] * al_LC)

    if ne == 0:
        al_TF = b["TF_isole"] if na == 1 else b["TF_couple"]
    elif ne <= 4:
        al_TF = b[f"TF_{ne}pac"]
    else:
        al_TF = b["TF_4pac"] + b["Tf_pers_sup"] * (ne - 4)
    p["al_TF"] = al_TF

    if ne == 0:
        al_RO = b["R0_isole"] if na == 1 else b["R0_couple"]
    elif ne <= 6:
        al_RO = b[f"R0_{ne}pac"]
    else:
        al_RO = b["R0_6pac"] + b["R0_pers_sup"] * (ne - 6)
    p["al_RO"] = al_RO / 12.0

    # --- Conjoint : prélèvements et salaires (fixes) ---
    sbc = cas.sal_brut_conjoint
    if b.get("exo_custom") is not None:
        cotis_emp_conj = float(cs_emp(sbc, b))
        exo_conj = float(b["exo_custom"](np.array([sbc], dtype=float), b)[0])
    elif b.get("reforme_allegements_2026"):
        cotis_emp_conj = float(cs_emp(sbc, b))
        exo_conj = float(exo_reforme_2026(sbc, b))
    else:
        cotis_emp_conj = float(cs_emp(sbc, b)) - float(bandeaux_maladie_famille(sbc, b))
        exo_conj = exo_fillon1(sbc, b)
    cotis_sal_conj = float(cs_sal(sbc, b))
    csg_ded_conj = float(csg_deduc(sbc, b))
    csg_nonded_conj = float(csg_non_deduc(sbc, b))
    p["sal_declar_conj"] = (na == 2) * (sbc - cotis_sal_conj - csg_ded_conj)
    p["cout_trav_conj"] = (na == 2) * (sbc + cotis_emp_conj - exo_conj)
    p["sal_net_conj"] = (sbc - cotis_sal_conj - csg_ded_conj - csg_nonded_conj) if na == 2 else 0.0

    # --- Quotient familial ---
    nb_part = na + ne * 0.5 + (ne > 2) * (ne - 2) * 0.5 + (na == 1) * (ne > 0) * 0.5
    p["nb_part"] = nb_part
    p["max_qf"] = (b["plafond_qf"] * (nb_part - na) * 2
                   + ((b["plaf_qf_monoparent"] - 2 * b["plafond_qf"])
                      if (na == 1 and ne > 0) else 0.0))

    # --- ASF ---
    p["montant_asf"] = (cas.eligible_asf == 1) * (na == 1) * b["mont_ASF_total"] * ne
    p["asf_br_rsa"] = (cas.eligible_asf == 1) * (na == 1) * b["mont_ASF_RSA"] * ne

    return p


# ---------------------------------------------------------------------------
# 5. Simulation du cas type (réplique de castype(), législation 2025)
# ---------------------------------------------------------------------------

#: mécanismes qui peuvent être retirés dans simulate(sans=...)
MECANISMES = {
    "allegements": "Allègements généraux de cotisations patronales",
    "ir": "Impôt sur le revenu",
    "apl": "Allocations logement (APL)",
    "pa": "Prime d'activité",
    "rsa": "RSA (et prime de Noël)",
}


def simulate(cas: CasType, b: dict, brut_max: float = 4.0, pas: float = 5.0,
             sans: set | tuple = ()) -> pd.DataFrame:
    """Passe d'une échelle de salaires bruts mensuels au revenu disponible.

    brut_max : salaire maximum en multiples de SMIC brut (si <= 20) ou en
    euros (si > 20). pas : pas de l'échelle en euros.
    Réplique les formules de castype() pour year=25 (branches year>18/20/23),
    en supposant : pas d'ARE, pas d'AAH, pas d'ASS, recours PA = 1.

    sans : mécanismes à retirer (clés de MECANISMES). Le retrait est
    "cohérent" : supprimer les APL les retire aussi des bases ressources
    du RSA et de la PA ; supprimer les allègements renchérit le coût du
    travail sans toucher au revenu disponible ; etc.
    """
    sans = set(sans)
    inconnus = sans - set(MECANISMES)
    if inconnus:
        raise ValueError(f"mécanismes inconnus : {inconnus}")
    na, ne = cas.nb_adultes, cas.nb_enfants
    p = menage_params(cas, b)

    if brut_max <= 20:
        brut_max = brut_max * b["smic_b"]
    brut = np.arange(0.0, brut_max + pas, pas)

    percen_smic = brut / b["smic_b"] * 100          # % du SMIC brut temps plein
    tps_travail = np.minimum(percen_smic, 100.0)

    # --- Prélèvements sociaux (year>18 ; réforme des allègements en 2026) ---
    cotis_emp, fillon = cotisations_employeur(brut, b)
    fillon[0] = 0.0
    if "allegements" in sans:
        fillon = np.zeros_like(brut)
    cotis_sal = cs_sal(brut, b)
    csg_ded = csg_deduc(brut, b)
    csg_nonded = csg_non_deduc(brut, b)

    # --- Revenus nets et coût du travail ---
    rev_act_net = brut - cotis_sal - csg_ded - csg_nonded
    rev_act_dec = brut - cotis_sal - csg_ded          # net imposable
    cout_travail = brut + cotis_emp - fillon

    sal_declar_conj = p["sal_declar_conj"]
    sal_net_conj = p["sal_net_conj"]
    autres_rev = cas.autres_rev

    # --- Revenu imposable N-2 (avec abattement ARE sans objet ici) ---
    revimp_n_2_abatt = ((rev_act_dec + autres_rev + sal_declar_conj)
                        / (1 + b["deflat_2"]) * b["abatt_rfr"])

    # --- Allocations logement (year>20) ---
    BR_AL = ceiling_dec(
        12 * ((1 + b["deflat_2"]) * revimp_n_2_abatt
              - (na == 2) * (rev_act_dec > b["bmaf"]) * (sal_declar_conj > b["bmaf"])
              * b["APL_abatt_biactifs"]),
        -2) / 12
    al_brut = np.maximum(
        0.0,
        p["al_LC"] - (p["al_PO"] + 12 * (p["al_TF"] + b["al_taux_compl"])
                      * np.maximum(0.0, BR_AL - p["al_RO"])) - b["Mfo_deduire"]
    ) * (1 - b["tx_crds"])
    AL = np.floor((cas.proprietaire == 0) * (al_brut >= b["seuil_versement_AL"]) * al_brut)
    if "apl" in sans:
        AL = np.zeros_like(brut)

    # --- Complément familial (year>13) ---
    cond_cf = SI(np.logical_or(na == 1,
                               (rev_act_dec / (1 + b["deflat_2"]) > b["bmaf_n_2"])
                               * (sal_declar_conj / (1 + b["deflat_2"]) > b["bmaf_n_2"])), 1.0, 0.0)
    plaf_CF = (cond_cf * (b["plafond_maj_CF"] + max(ne - 3, 0) * b["pers_sup_CF"])
               + (1 - cond_cf) * (b["plafond_simple_CF"] + max(ne - 3, 0) * b["pers_sup_CF"]))
    plaf_CF_majo = (cond_cf * (b["plafond_maj_CF_majo"] + max(ne - 3, 0) * b["pers_sup_CF_majo"])
                    + (1 - cond_cf) * (b["plafond_simple_CF_majo"] + max(ne - 3, 0) * b["pers_sup_CF_majo"]))
    nb_enf_3plus = (cas.nb_enft_35 + cas.nb_enft_610 + cas.nb_enft_1113
                    + cas.nb_enft_14 + cas.nb_enft_1519 + cas.nb_enft_20)
    if nb_enf_3plus < 3 or cas.nb_enft_3 >= 1:
        mont_CF = np.zeros_like(brut)
    else:
        mont_CF = SI(revimp_n_2_abatt < plaf_CF_majo, b["montant_CF_majo"],
                     SI(revimp_n_2_abatt < plaf_CF, b["montant_CF"],
                        SI(revimp_n_2_abatt < plaf_CF + b["montant_CF"],
                           b["montant_CF"] + plaf_CF - revimp_n_2_abatt, 0.0)))

    # --- Allocation de base de la PAJE (year>14) ---
    plaf_AB_partiel = (cond_cf * (b["plafond_majore_paje"] + min(ne, 2) * b["plafond_sup_enf_12_paje"]
                                  + max(ne - 3, 0) * b["plafond_sup_enf_3_paje"])
                       + (1 - cond_cf) * (b["plaf_simple_paje"] + min(ne, 2) * b["plafond_sup_enf_12_paje"]
                                          + max(ne - 3, 0) * b["plafond_sup_enf_3_paje"]))
    plaf_AB_plein = (cond_cf * (b["plafond_majore_paje_plein"] + min(ne, 2) * b["plafond_sup_enf_12_paje_plein"]
                                + max(ne - 3, 0) * b["plafond_sup_enf_3_paje_plein"])
                     + (1 - cond_cf) * (b["plaf_simple_paje_plein"] + min(ne, 2) * b["plafond_sup_enf_12_paje_plein"]
                                        + max(ne - 3, 0) * b["plafond_sup_enf_3_paje_plein"]))
    mont_AB_paje = SI(revimp_n_2_abatt < plaf_AB_plein,
                      b["montant_paje_plein"] * (cas.nb_enft_3 > 0),
                      SI(revimp_n_2_abatt < plaf_AB_partiel,
                         b["montant_paje_partiel"] * (cas.nb_enft_3 > 0), 0.0))

    # --- RSA socle (year>15) — pas d'AAH/ASS/ARE ---
    br_rsa = (rev_act_net + sal_net_conj + (AL > 0) * np.minimum(AL, p["fl_RSA"])
              + (cas.proprietaire == 1) * p["fl_RSA"]
              + p["montant_af"] - p["af_majo_age"]
              + np.minimum(mont_CF, b["montant_CF"]) + mont_AB_paje
              + p["asf_br_rsa"] + autres_rev)
    mont_RSA = (br_rsa + b["seuil_versement_rsa"] <= p["mf_RSA"]) * (p["mf_RSA"] - br_rsa)
    if "rsa" in sans:
        mont_RSA = np.zeros_like(brut)

    # Prime de Noël
    taille = na + ne
    if taille == 1:
        noel = b["noel_1pers"]
    elif taille == 2:
        noel = b["noel_2pers"]
    elif taille == 3:
        noel = b["noel_3pers"]
    elif taille == 4:
        noel = b["noel_isole_4pers"] if na == 1 else b["noel_couple_4pers"]
    else:
        base_noel = b["noel_isole_4pers"] if na == 1 else b["noel_couple_4pers"]
        noel = base_noel + b["noel_pers_sup"] * (taille - 4)
    prime_noel = (mont_RSA > 0) * noel

    # --- Prime d'activité (year>15) — pas d'AAH/ASS ---
    br_pa = (rev_act_net + sal_net_conj + (AL > 0) * np.minimum(AL, p["fl_PA"])
             + (cas.proprietaire == 1) * p["fl_PA"]
             + p["montant_af"] - p["af_majo_age"]
             + np.minimum(mont_CF, b["montant_CF"]) + mont_AB_paje
             + p["asf_br_rsa"] + autres_rev)
    # rampe de bonification : en euros si fournie (barèmes 2026 issus des
    # codes MFrance), sinon en parts de SMIC net (paramétrisation DREES)
    smic_n = b["smic_n"]
    t1 = b.get("PA_seuil_bonif_eur", b["PA_tranche1smic"] * smic_n)
    t2 = b.get("PA_plafond_bonif_eur", b["PA_tranche2smic"] * smic_n)
    bonus_pa = b["PA_bonus"] * (
        SI(rev_act_net >= t1, (np.minimum(rev_act_net, t2) - t1) / (t2 - t1), 0.0)
        + SI(sal_net_conj >= t1, (min(sal_net_conj, t2) - t1) / (t2 - t1), 0.0))
    pa_theorique = ((p["mf_PA"] + (1 - b["PA_pente"]) * (rev_act_net + sal_net_conj)
                     + bonus_pa - np.maximum(br_pa, p["mf_PA"])) * (1 - b["tx_crds"]))
    mont_PA = ((brut + sal_net_conj > 0) * (pa_theorique >= b["seuil_versement_PA"])
               * pa_theorique)
    if "pa" in sans:
        mont_PA = np.zeros_like(brut)

    # --- ARS (year>14) ---
    ars_mens = (b["ars6_10"] * cas.nb_enft_610
                + b["ars_11_14"] * (cas.nb_enft_1113 + cas.nb_enft_14)
                + b["ars_15_18"] * cas.nb_enft_1519) / 12.0
    plaf_ars = b["plaf_ars"] + b["plaf_ars_enf"] * ne
    ars = SI(revimp_n_2_abatt < plaf_ars, ars_mens,
             SI(revimp_n_2_abatt < plaf_ars + ars_mens,
                plaf_ars + ars_mens - revimp_n_2_abatt, 0.0))

    # --- Impôt sur le revenu (year>18 : revenus contemporains) ---
    rfr1 = (rev_act_dec + autres_rev) * b["abatt_rfr"]
    rfr2 = np.full_like(brut, sal_declar_conj * b["abatt_rfr"])
    rfr = rfr1 + rfr2
    if cas.n0 == 3:
        # concubins : deux déclarations séparées ; les enfants sont rattachés
        # au déclarant qui a le plus gros revenu (formules nb_part1/nb_part2)
        part_enfants = 1 + ne * 0.5 + (ne > 2) * (ne - 2) * 0.5 + (ne > 0) * 0.5
        nb_part1 = np.where(rfr1 >= rfr2, part_enfants, 1.0)
        nb_part2 = np.where(rfr1 < rfr2, part_enfants, 1.0)
        imp1 = np.array([impot_revenu(np.array([r]), n, 1, p["max_qf"], b)[0]
                         for r, n in zip(rfr1, nb_part1)])
        imp2 = np.array([impot_revenu(np.array([r]), n, 1, p["max_qf"], b)[0]
                         for r, n in zip(rfr2, nb_part2)])
        imp_recouvr = imp1 + imp2
    else:
        imp_recouvr = impot_revenu(rfr, p["nb_part"], na, p["max_qf"], b)
    if "ir" in sans:
        imp_recouvr = np.zeros_like(brut)

    # --- Taxe d'habitation : supprimée (year>=23) ---
    mont_TH = np.zeros_like(brut)

    # --- Allocations familiales modulées (year>14) ---
    maf = p["montant_af"]
    mont_AF = SI(revimp_n_2_abatt <= p["plaf_1tranch_AF"], maf,
                 SI(revimp_n_2_abatt <= p["plaf_1tranch_AF"] + maf / 2,
                    maf + p["plaf_1tranch_AF"] - revimp_n_2_abatt,
                    SI(revimp_n_2_abatt <= p["plaf_2tranch_AF"], maf / 2,
                       SI(revimp_n_2_abatt <= p["plaf_2tranch_AF"] + maf / 4,
                          maf / 2 + p["plaf_2tranch_AF"] - revimp_n_2_abatt,
                          maf / 4))))

    # --- Revenu disponible (year>16) ---
    rev_trav_net = rev_act_net + sal_net_conj
    presta = (mont_AF + mont_RSA + mont_PA + ars + prime_noel
              + mont_AB_paje + mont_CF + AL + p["montant_asf"])
    rev_disp = rev_trav_net + autres_rev + presta - imp_recouvr - mont_TH

    # prestations "familiales et diverses" (hors PA, RSA, AL) pour la décompo
    presta_famille = mont_AF + ars + prime_noel + mont_AB_paje + mont_CF + p["montant_asf"]

    df = pd.DataFrame({
        "salaire_brut": brut,
        "pts_smic": brut / b["smic_b"],
        "tps_travail": tps_travail,
        "cotis_emp": cotis_emp,
        "fillon_exo": fillon,
        "cotis_sal": cotis_sal,
        "csg_ded": csg_ded,
        "csg_nonded": csg_nonded,
        "cotis_sal_totales": cotis_sal + csg_ded + csg_nonded,
        "rev_act_net": rev_act_net,
        "rev_act_dec": rev_act_dec,
        "cout_travail": cout_travail,
        "sal_net_conj": sal_net_conj,
        "revimp_n_2_abatt": revimp_n_2_abatt,
        "AL": AL,
        "mont_CF": mont_CF,
        "mont_AB_paje": mont_AB_paje,
        "mont_RSA": mont_RSA,
        "prime_noel": prime_noel,
        "mont_PA": mont_PA,
        "ars": ars,
        "rfr": rfr,
        "imp_recouvr": imp_recouvr,
        "mont_AF": mont_AF,
        "montant_asf": p["montant_asf"],
        "presta_famille": presta_famille,
        "prestations": presta,
        "rev_trav_net": rev_trav_net,
        "rev_disp": rev_disp,
    })

    # Taux marginal implicite (comme castype) : sur le net et sur le coût du travail
    df["TMI_net"] = 1 - df["rev_disp"].diff().shift(-1) / df["rev_act_net"].diff().shift(-1)
    df["TMI_superbrut"] = 1 - df["rev_disp"].diff().shift(-1) / df["cout_travail"].diff().shift(-1)

    return df


# ---------------------------------------------------------------------------
# 6. Décomposition marginale : +100 € de revenu disponible
# ---------------------------------------------------------------------------

#: composantes interpolées au salaire cible, avec leur signe dans le surcoût
DECOMP_COLS = {
    "cotis_emp": ("delta_cot_pat", +1),          # cot. patronales hors allègements
    "fillon_exo": ("delta_perte_exo", -1),       # allègements perdus -> surcoût
    "cotis_sal_totales": ("delta_cot_sal", +1),  # cot. salariales + CSG/CRDS
    "imp_recouvr": ("delta_ir", +1),             # impôt sur le revenu
    "mont_PA": ("delta_perte_pa", -1),           # prime d'activité perdue
    "AL": ("delta_perte_al", -1),                # allocations logement perdues
    "mont_RSA": ("delta_perte_rsa", -1),         # RSA perdu
    "presta_famille": ("delta_perte_pf", -1),    # AF, CF, PAJE, ARS, ASF, Noël
}


def decompose_plus100(df: pd.DataFrame, delta_rd: float = 100.0) -> pd.DataFrame:
    """Décomposition du surcoût employeur pour +delta_rd € de revenu disponible.

    Pour chaque salaire brut initial, on cherche (par interpolation linéaire,
    comme approx() dans contribution100deRD.R) la situation cible où le revenu
    disponible du ménage est supérieur de delta_rd €, puis on décompose :

    delta_cout_travail = delta_rd (transfert)
                       + hausse des cotisations patronales à taux fixe
                       + perte d'allègements généraux
                       + hausse des cotisations salariales et CSG/CRDS
                       + hausse de l'IR
                       + perte de prime d'activité
                       + perte d'allocations logement
                       + perte de RSA
                       + perte d'autres prestations (AF, CF, PAJE, ARS, ASF)
    """
    d = df.sort_values("salaire_brut").reset_index(drop=True).copy()

    # support d'interpolation trié par revenu disponible (comme approx(), rule=2)
    ordre = d["rev_disp"].to_numpy().argsort(kind="stable")
    rd_sorted = d["rev_disp"].to_numpy()[ordre]

    def interp(col):
        y_sorted = d[col].to_numpy()[ordre]
        return np.interp(d["rev_disp"].to_numpy() + delta_rd, rd_sorted, y_sorted)

    d["brut_cible"] = interp("salaire_brut")
    d["cout_travail_cible"] = interp("cout_travail")
    d["delta_cout_travail"] = d["cout_travail_cible"] - d["cout_travail"]
    d["delta_brut"] = d["brut_cible"] - d["salaire_brut"]

    total_check = np.full(len(d), float(delta_rd))
    for col, (name, signe) in DECOMP_COLS.items():
        cible = interp(col)
        d[name] = signe * (cible - d[col].to_numpy())
        total_check = total_check + d[name].to_numpy()

    d["transfert"] = float(delta_rd)
    d["check"] = np.round(total_check - d["delta_cout_travail"].to_numpy(), 4)

    # Les lignes dont la cible (RD + delta_rd) sort de l'échelle simulée ne
    # sont pas interprétables (l'interpolation sature en borne haute) : NaN.
    hors_support = d["rev_disp"] + delta_rd > d["rev_disp"].max()
    cols_delta = (["brut_cible", "cout_travail_cible", "delta_cout_travail",
                   "delta_brut", "check"]
                  + [name for name, _ in DECOMP_COLS.values()])
    d.loc[hors_support, cols_delta] = np.nan
    return d


LABELS_FR = {
    "transfert": "Transfert logique (100€)",
    "delta_cot_pat": "Δ Cotisations patronales (taux fixe)",
    "delta_perte_exo": "Perte d'allègements généraux",
    "delta_cot_sal": "Δ Cotisations salariales et CSG-CRDS",
    "delta_ir": "Δ Impôt sur le revenu",
    "delta_perte_pa": "Perte de prime d'activité",
    "delta_perte_al": "Perte d'allocations logement",
    "delta_perte_rsa": "Perte de RSA",
    "delta_perte_pf": "Perte d'autres prestations (AF, CF, ARS...)",
}

def decompose_cascade(cas: CasType, b: dict, mecanisme: str = "pa",
                      brut_max: float = 4.5, pas: float = 5.0,
                      delta_rd: float = 100.0) -> pd.DataFrame:
    """Décomposition +100 € de RD en isolant les effets induits d'un mécanisme.

    Méthode de la note « Les aides aux ménages constituent un frein à la
    progression salariale » : on refait la décomposition en l'absence du
    mécanisme (contrefactuel sans=...) ; pour chaque salaire brut initial :
      - effet direct   = perte du mécanisme dans le monde réel (avec) ;
      - effets induits = surcroît de coût des AUTRES composantes dû au
        mécanisme = (coût avec - coût sans) - effet direct.
    Les colonnes *_sans donnent la décomposition « de droit commun »
    (contrefactuel) ; total : delta_cout_travail = somme des bandes sans
    + effet_direct + effet_induit.
    """
    cle_directe = {"pa": "delta_perte_pa", "apl": "delta_perte_al",
                   "ir": "delta_ir", "rsa": "delta_perte_rsa",
                   "allegements": "delta_perte_exo"}[mecanisme]
    d_avec = decompose_plus100(simulate(cas, b, brut_max=brut_max, pas=pas),
                               delta_rd=delta_rd)
    d_sans = decompose_plus100(simulate(cas, b, brut_max=brut_max, pas=pas,
                                        sans={mecanisme}), delta_rd=delta_rd)
    out = d_avec.copy()
    for _, (name, _) in DECOMP_COLS.items():
        out[name + "_sans"] = d_sans[name].to_numpy()
    out["cout_sans"] = d_sans["delta_cout_travail"].to_numpy()
    out["effet_direct"] = out[cle_directe]
    out["effet_induit"] = (out["delta_cout_travail"] - out["cout_sans"]
                           - out["effet_direct"])
    return out


# ---------------------------------------------------------------------------
# 7. Barèmes 2026 (à partir des données du dossier PAproject uniquement :
#    MFrance1erjanvier2026.R et MFrance1eravril26.R)
# ---------------------------------------------------------------------------

def build_bareme_2026(scenario: str = "janvier",
                      nombre_employes: int = 99,
                      data_dir: Path | None = None) -> dict[str, float]:
    """Barème 2026 : maquette DREES 2025 + paramètres 2026 des codes MFrance.

    scenario = "janvier" : législation au 1er janvier 2026
      - SMIC brut 1 823,03 € ; PASS 4 005 €
      - barème IR 2026 : 11 600 / 29 579 / 84 578 / 181 917 € (annuels),
        décote 897 € (célibataire), taux de décote 45,25 %
      - prime d'activité inchangée (633,21 € ; bonif max 184,27 € ;
        rampe 709,18 € -> 1 442,40 €)
      - réforme des allègements généraux : barème unique jusqu'à 3 SMIC,
        taux max 38,21 % (>= 50 salariés) : (0,5*(3*SMIC/brut-1))^1,75 + 2 %

    scenario = "juin_gel" : hausse du SMIC au 1er juin 2026 + gel du barème
      (MFrance1eravril26.R). Trois différences fiscalo-sociales avec le
      1er janvier :
      - SMIC brut porté à 1 867,02 €
      - prime d'activité revalorisée : 638,28 € ; bonif max 240,63 € ;
        rampe étendue 709,18 € -> 1 658,76 €
      - barème des allègements GELÉ : point de référence maintenu à
        3 x 1 823,03 € (le SMIC de janvier), d'où une perte d'allègements
        à tous les niveaux de salaire exprimés dans le nouveau SMIC.
      Le barème de l'IR reste gelé. Les prestations, elles, intègrent la
      revalorisation du 1er avril 2026 (+0,8 %, décret n°2026-220) :
      RSA 651,69 €, BMAF 478,16 € (AF, CF, PAJE, ARS indexées), ASF +0,9 %,
      CSS, AAH, ASS ; le paramètre R0 des APL est gelé pour 2026, et la
      majoration d'âge des AF passe de 14 à 18 ans (réforme du 1er mars
      2026, approximée ici : plus de majoration pour les tranches 14 ans
      et 15-19 ans, enfants supposés < 18 ans / nouveaux bénéficiaires).

    Dans les deux scénarios : les paramètres L+C et PO des allocations
    logement sont relevés de +1,04 % (revalorisation IRL du 1er octobre
    2025, absente de la maquette au 1er janvier 2025) et les seuils de
    modulation des AF de +1,8 % (revalorisation du 1er janvier 2026).
    Au 1er janvier 2026, les autres prestations gardent leurs valeurs 2025,
    encore en vigueur à cette date.
    """
    if scenario not in ("janvier", "juin_gel"):
        raise ValueError("scenario doit être 'janvier' ou 'juin_gel'")

    b = load_bareme(2025, data_dir=data_dir)

    SMIC_JANVIER = 1823.03
    b["smic_b"] = SMIC_JANVIER if scenario == "janvier" else 1867.02
    b["plafond_ss"] = 4005.0

    # SMIC net endogène (mêmes taux salariaux que la maquette DREES)
    b["smic_n"] = float(b["smic_b"] - cs_sal(b["smic_b"], b)
                        - csg_deduc(b["smic_b"], b) - csg_non_deduc(b["smic_b"], b))

    # --- Impôt sur le revenu 2026 (gelé dans les deux scénarios) ---
    b["plafond_ir_t1"] = 11600.0 / 12
    b["plafond_ir_t2"] = 29579.0 / 12
    b["plafond_ir_t3"] = 84578.0 / 12
    b["plafond_ir_t4"] = 181917.0 / 12
    # décote : (897 - 0,4525 * IR_annuel)/12  <=>  taux*(montant - IR_mensuel)
    ratio_couple = b["mont_decote_couple"] / b["mont_decote_celib"]
    b["Decote_taux"] = 0.4525
    b["mont_decote_celib"] = 897.0 / 0.4525 / 12
    b["mont_decote_couple"] = b["mont_decote_celib"] * ratio_couple

    # --- Prime d'activité ---
    if scenario == "janvier":
        b["mont_forfaitaire_PA"] = 633.21
        b["PA_bonus"] = 184.27
        b["PA_seuil_bonif_eur"] = 709.18
        b["PA_plafond_bonif_eur"] = 1442.40
    else:
        b["mont_forfaitaire_PA"] = 638.28
        b["PA_bonus"] = 240.63
        b["PA_seuil_bonif_eur"] = 709.18
        b["PA_plafond_bonif_eur"] = 1658.76
        b["montant_forfaitaire_PA_majo"] *= 638.28 / 633.21

    # --- Réforme des allègements généraux 2026 ---
    b["reforme_allegements_2026"] = True
    b["exo_taux_2026"] = 0.3781 if nombre_employes < 50 else 0.3821
    # gel du barème : la référence reste le SMIC du 1er janvier 2026
    b["exo_smic_ref_2026"] = SMIC_JANVIER

    # --- Revalorisations communes aux deux scénarios 2026 ---
    # AL : +1,04 % sur L+C et PO au 1er octobre 2025 (IRL), absent de la
    # maquette "au 1er janvier 2025" ; R0 gelé pour 2026 (décret 28/12/2025)
    for k in ("LC_isole", "LC_couple", "LC_1_pac", "Lc_supp_pac", "montant_PO"):
        b[k] *= 1.0104
    # seuils de modulation des AF revalorisés au 1er janvier 2026 (~+1,8 %)
    for k in ("p1_modulation_af", "p2_modulation_af", "sup_enf_modulation_af"):
        b[k] *= 1.018

    # --- Revalorisation des prestations du 1er avril 2026 (scénario juin) ---
    if scenario == "juin_gel":
        r_rsa = 651.69 / 646.52          # +0,8 % (décret n°2026-220)
        b["mont_forfaitaire_rsa"] = 651.69
        b["mont_forfaitaire_rsa_maj"] *= r_rsa
        for k in ("forf_logement_rsa_1", "forf_logement_rsa_2", "forf_logement_rsa_3",
                  "forf_logement_PA_1", "forf_logement_PA_2", "forf_logement_PA_3"):
            b[k] *= r_rsa
        r_bmaf = 478.16 / 474.37         # BMAF +0,8 %
        b["bmaf"] = 478.16
        for k in ("AF_2enft", "AF_enft_sup", "majo_age_Af", "AF_forf_20ans",
                  "montant_CF", "montant_CF_majo", "pers_sup_CF",
                  "montant_paje_plein", "montant_paje_partiel"):
            b[k] *= r_bmaf               # AF_2enft -> 152,25 ; CF -> 198,16/297,26 ; PAJE -> 198,17
        b["ars6_10"], b["ars_11_14"], b["ars_15_18"] = 429.01, 452.67, 468.36  # ARS 2026 (annuel)
        r_asf = 200.97 / 199.1832        # ASF +0,9 %
        b["mont_ASF_total"] = 200.97
        b["mont_ASF_RSA"] *= r_asf
        r_css = 868.0 / b["plaf_cmuc_base"]  # CSS +0,8 % au 1er avril 2026
        for k in ("plaf_cmuc_base", "plaf_cmuc_1pac", "plaf_cmuc_34pac", "plaf_cmuc_5pluspac"):
            b[k] *= r_css
        b["AAH_montant"] = 1041.59
        b["ASS_mtt_forf"] *= 19.48 / 19.33
        # réforme du 1er mars 2026 : majoration d'âge des AF à partir de 18 ans
        # (nouveaux bénéficiaires) — approximation : tranches 14 et 15-19 exclues
        b["af_majo_age_des_18ans"] = True

    return b


# Palette Rexecode (reprise de contribution100deRD.R, complétée)
COULEURS = {
    "Transfert logique (100€)": "#FFFFFF",
    "Δ Cotisations patronales (taux fixe)": "#0071B9",
    "Δ Cotisations salariales et CSG-CRDS": "#969696",
    "Δ Impôt sur le revenu": "#D5B076",
    "Perte d'allègements généraux": "#00A486",
    "Perte de prime d'activité": "#E87511",
    "Perte d'allocations logement": "#8B5CA5",
    "Perte de RSA": "#C13B3B",
    "Perte d'autres prestations (AF, CF, ARS...)": "#5B9BD5",
}
