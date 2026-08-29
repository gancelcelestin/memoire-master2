CALCULS_FR_SANS_EXO <- function(base, nombre_employes = 99) {
  
  # === IDENTIQUE À CALCULS_FR === #
  taux_maladie_plein <- 0.13
  taux_csa <- 0.003
  taux_vieillesse_tot <- 0.0202
  taux_vieillesse_plafond <- 0.0855
  taux_allocations_familiales_plein <- 0.0525
  taux_dialogue_social <- 0.00016
  taux_chomage <- 0.0405
  taux_ags <- 0.002
  taux_fnal_moins_50 <- 0.001
  taux_fnal_50_plus <- 0.005
  taux_formation_pro_moins_11 <- 0.0055
  taux_formation_pro_11_plus <- 0.01
  taux_cpf_cdd <- 0.01
  taux_taxe_apprentissage <- 0.0068
  taux_accidents_travail <- 0.023
  taux_complementaire_tranche1 <- 0.0472
  taux_ceg_tranche1 <- 0.0129
  taux_complementaire_tranche2 <- 0.1295
  taux_ceg_tranche2 <- 0.0162
  
  PASS <- 4005
  plafond_tranche1 <- PASS
  plafond_tranche2 <- 8 * PASS
  
  cot_pat <- base %>%
    mutate(
      maladie                = salaire_brut * taux_maladie_plein,
      csa                    = salaire_brut * taux_csa,
      vieillesse             = salaire_brut * taux_vieillesse_tot + pmin(salaire_brut, PASS) * taux_vieillesse_plafond,
      allocations_familiales = salaire_brut * taux_allocations_familiales_plein,
      dialogue_social        = salaire_brut * taux_dialogue_social,
      chomage                = pmin(salaire_brut, 4 * PASS) * taux_chomage,
      ags                    = pmin(salaire_brut, 4 * PASS) * taux_ags,
      fnal                   = ifelse(nombre_employes < 50,
                                      pmin(salaire_brut, PASS) * taux_fnal_moins_50,
                                      salaire_brut * taux_fnal_50_plus),
      formation_pro          = ifelse(nombre_employes < 11,
                                      salaire_brut * taux_formation_pro_moins_11,
                                      salaire_brut * taux_formation_pro_11_plus),
      cpf_cdd                = salaire_brut * taux_cpf_cdd,
      taxe_apprentissage     = salaire_brut * taux_taxe_apprentissage,
      accidents_travail      = salaire_brut * taux_accidents_travail,
      complementaire_pat     = pmin(salaire_brut, plafond_tranche1) * taux_complementaire_tranche1 +
        ifelse(salaire_brut > plafond_tranche1,
               pmin(salaire_brut - plafond_tranche1, plafond_tranche2 - plafond_tranche1) * taux_complementaire_tranche2, 0),
      ceg                    = pmin(salaire_brut, plafond_tranche1) * taux_ceg_tranche1 +
        ifelse(salaire_brut > plafond_tranche1,
               pmin(salaire_brut - plafond_tranche1, plafond_tranche2 - plafond_tranche1) * taux_ceg_tranche2, 0),
      
      # ── SEUL CHANGEMENT : exonérations forcées à 0 ──────────────────────
      exonerations_pat = 0
    ) %>%
    mutate(
      assurance_maladie_pat = maladie + csa,
      retraite_pat          = vieillesse + complementaire_pat + ceg,
      famille_pat           = allocations_familiales + fnal,
      assurance_chomage_pat = chomage + ags,
      formation_pat         = formation_pro + taxe_apprentissage + cpf_cdd,
      autres_pat            = accidents_travail + dialogue_social,
      total_cotisations_pat = assurance_maladie_pat + retraite_pat + famille_pat +
        assurance_chomage_pat + formation_pat + autres_pat - exonerations_pat
    ) %>%
    select(PTS_SMIC, assurance_maladie_pat, retraite_pat, famille_pat,
           assurance_chomage_pat, formation_pat, autres_pat, total_cotisations_pat)
  
  base <- base %>% left_join(cot_pat, by = "PTS_SMIC")
  
  # === COTISATIONS SALARIALES — IDENTIQUE ===
  taux_vieillesse_tot          <- 0.004
  taux_vieillesse_plafond      <- 0.069
  taux_csg_imposable           <- 0.024
  taux_csg_non_imposable       <- 0.068
  taux_crds                    <- 0.005
  taux_complementaire_tranche1 <- 0.0315
  taux_ceg_tranche1            <- 0.0086
  taux_complementaire_tranche2 <- 0.0864
  taux_ceg_tranche2            <- 0.0108
  
  PASS             <- 4005
  plafond_tranche1 <- PASS
  plafond_tranche2 <- 8 * PASS
  
  cot_sal <- base %>%
    mutate(
      vieillesse         = salaire_brut * taux_vieillesse_tot + pmin(salaire_brut, PASS) * taux_vieillesse_plafond,
      csg_imposable_sal  = pmin(salaire_brut, PASS*4) * 0.9825 * taux_csg_imposable,
      csg_non_imposable  = pmin(salaire_brut, PASS*4) * 0.9825 * taux_csg_non_imposable,
      crds               = pmin(salaire_brut, PASS*4) * 0.9825 * taux_crds,
      complementaire_sal = pmin(salaire_brut, plafond_tranche1) * taux_complementaire_tranche1 +
        ifelse(salaire_brut > plafond_tranche1,
               pmin(salaire_brut - plafond_tranche1, plafond_tranche2 - plafond_tranche1) * taux_complementaire_tranche2, 0),
      ceg                = pmin(salaire_brut, plafond_tranche1) * taux_ceg_tranche1 +
        ifelse(salaire_brut > plafond_tranche1,
               pmin(salaire_brut - plafond_tranche1, plafond_tranche2 - plafond_tranche1) * taux_ceg_tranche2, 0)
    ) %>%
    mutate(
      retraite_sal                      = vieillesse + complementaire_sal + ceg,
      csg_crds_sal                      = csg_imposable_sal + csg_non_imposable + crds,
      total_cotisations_sal             = retraite_sal + csg_crds_sal,
      total_cotisations_sal_horsCSGCRDS = total_cotisations_sal - csg_crds_sal
    ) %>%
    select(PTS_SMIC, retraite_sal, csg_crds_sal, csg_imposable_sal,
           total_cotisations_sal, total_cotisations_sal_horsCSGCRDS)
  
  base <- base %>% left_join(cot_sal, by = "PTS_SMIC")
  
  # === IR, PRIME D'ACTIVITÉ, RATIOS — IDENTIQUES ===
  base <- base %>% mutate(
    salaire_net                  = salaire_brut - total_cotisations_sal,
    salaire_super_brut           = salaire_brut + total_cotisations_pat,
    salaire_net_imposable        = salaire_net + csg_imposable_sal,
    salaire_net_imposable_abattu = salaire_net_imposable -
      pmin(pmax(0.10 * salaire_net_imposable, 504 / 12), 14426 / 12)
  )
  
  tranche1_limite <- 11600/12; tranche2_limite <- 29579/12
  tranche3_limite <- 84578/12; tranche4_limite <- 181917/12
  taux1 <- 0; taux2 <- 0.11; taux3 <- 0.30; taux4 <- 0.41; taux5 <- 0.45
  
  base <- base %>%
    mutate(impot_brut = ifelse(salaire_net_imposable_abattu <= tranche1_limite,
                               salaire_net_imposable_abattu * taux1,
                               ifelse(salaire_net_imposable_abattu <= tranche2_limite,
                                      (tranche1_limite * taux1) + (salaire_net_imposable_abattu - tranche1_limite) * taux2,
                                      ifelse(salaire_net_imposable_abattu <= tranche3_limite,
                                             (tranche1_limite * taux1) + (tranche2_limite - tranche1_limite) * taux2 + (salaire_net_imposable_abattu - tranche2_limite) * taux3,
                                             ifelse(salaire_net_imposable_abattu <= tranche4_limite,
                                                    (tranche1_limite * taux1) + (tranche2_limite - tranche1_limite) * taux2 + (tranche3_limite - tranche2_limite) * taux3 + (salaire_net_imposable_abattu - tranche3_limite) * taux4,
                                                    (tranche1_limite * taux1) + (tranche2_limite - tranche1_limite) * taux2 + (tranche3_limite - tranche2_limite) * taux3 + (tranche4_limite - tranche3_limite) * taux4 + (salaire_net_imposable_abattu - tranche4_limite) * taux5))))) %>%
    mutate(decote             = ifelse(impot_brut <= 1982/12, (897 - 0.4525 * impot_brut * 12) / 12, 0),
           impot_revenu_total = pmax(impot_brut - decote, 0))
  
  base <- base %>% mutate(salaire_net_impot = salaire_net - impot_revenu_total)
  
  montant_forfaitaire <- 638.28
  seuil_bonification  <- 709.18
  bonification_max    <- 240.63
  
  base <- base %>%
    mutate(
      bonification   = ifelse(salaire_net >= seuil_bonification,
                              pmin((salaire_net - seuil_bonification) *
                                     (bonification_max / (1658.76 - seuil_bonification)), bonification_max), 0),
      prime_activite_brute = pmax(montant_forfaitaire + salaire_net * 0.5985 + bonification - salaire_net, 0),
      prime_activite = ifelse(prime_activite_brute < 15, 0, prime_activite_brute)
    ) %>%
    mutate(salaire_net_impot_prime_activite = salaire_net_impot + prime_activite)
  
  base <- base %>% mutate(
    `% cotisations patronales (%brut)`               = total_cotisations_pat / salaire_brut,
    `% cotisations salariales (%brut)`               = total_cotisations_sal / salaire_brut,
    `% cotisations salariales hors CSG-CRDS (%brut)` = total_cotisations_sal_horsCSGCRDS / salaire_brut,
    `taux d'IR (%net)`                               = (impot_revenu_total - prime_activite) / salaire_net,
    `salaire net d'impot (%brut)`                    = salaire_net_impot_prime_activite / salaire_brut,
    `salaire super brut (%brut)`                     = salaire_super_brut / salaire_brut
  )
}

# Sorties scénario sans exonérations ####
test_fr_sans_exo         <- CALCULS_FR_SANS_EXO(base)
cotisations_fr_sans_exo  <- COIN_SOCIAL_FR(test_fr_sans_exo)
retraite_chomage_fr_sans_exo <- CHOMAGE_RETRAITE_FR(cotisations_fr_sans_exo)

final_FR_sans_exo <- retraite_chomage_fr_sans_exo %>%
  mutate(
    tx_cotis_chomage_FR  = cotisations_fr_sans_exo$assurance_chomage_pat / cotisations_fr_sans_exo$salaire_brut,
    transfo_chom_FR      = retraite_chomage_fr_sans_exo$taux_remplacement_chomage / tx_cotis_chomage_FR,
    tx_cotis_retraite_FR = (cotisations_fr_sans_exo$retraite_sal + cotisations_fr_sans_exo$retraite_pat) / cotisations_fr_sans_exo$salaire_brut,
    transfo_retraite_FR  = retraite_chomage_fr_sans_exo$taux_remplacement_retraite / tx_cotis_retraite_FR
  )