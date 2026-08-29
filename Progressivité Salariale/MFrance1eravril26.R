
# Packages, working directory et base ####

library(readxl)
library(tidyr)
library(dplyr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(stringr)
library(openxlsx)


# Création base ####

base <- matrix(seq(1, 20, by = 0.01), ncol = 1) %>% data.frame()
colnames(base) <- "PTS_SMIC"

SMIC_BRUT_2026 <- 1867.02
base <- base %>% mutate("salaire_brut" = PTS_SMIC * SMIC_BRUT_2026) 
#Il faut obligatoirement que la variable s'appelle "salaire_brut" pour faire marcher la fonction



# Fonction "CALCULS_FR" ####

CALCULS_FR <- function(base, nombre_employes = 99) {
  # COTISATIONS PATRONALES #
  
  # TAUX COT PATRONALES
  taux_maladie_plein <- 0.13
  taux_csa <- 0.003
  taux_vieillesse_tot <- 0.0202                   #+9 NORMALEMENT 0.211
  taux_vieillesse_plafond <- 0.0855
  taux_allocations_familiales_plein <- 0.0525
  taux_dialogue_social <- 0.00016
  taux_chomage <- 0.0405                               #-0.5 0.04
  taux_ags <- 0.002                                 #+0.5 0.0025
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
  taux_exo <- ifelse(nombre_employes < 50, 0.3781, 0.3821)
  
  # PLAFONDS
  PASS <- 4005
  plafond_tranche1 <- PASS
  plafond_tranche2 <- 8 * PASS
  # CALCULS
  cot_pat <- base %>% 
    mutate("maladie" = salaire_brut * taux_maladie_plein,
           "csa" = salaire_brut * taux_csa,
           "vieillesse" = salaire_brut * taux_vieillesse_tot + pmin(salaire_brut, PASS) * taux_vieillesse_plafond,
           "allocations_familiales" =  salaire_brut * taux_allocations_familiales_plein,
           "dialogue_social" = salaire_brut * taux_dialogue_social,
           "chomage" = pmin(salaire_brut, 4 * PASS) * taux_chomage,
           "ags" = pmin(salaire_brut, 4 * PASS) * taux_ags,
           "fnal" = ifelse(nombre_employes < 50, pmin(salaire_brut, PASS) * taux_fnal_moins_50, salaire_brut * taux_fnal_50_plus),
           "formation_pro" = ifelse(nombre_employes < 11, salaire_brut * taux_formation_pro_moins_11, salaire_brut * taux_formation_pro_11_plus),
           "cpf_cdd" = salaire_brut * taux_cpf_cdd,
           "taxe_apprentissage" = salaire_brut * taux_taxe_apprentissage,
           "accidents_travail" = salaire_brut * taux_accidents_travail,
           "complementaire_pat" = pmin(salaire_brut, plafond_tranche1) * taux_complementaire_tranche1 + ifelse(salaire_brut > plafond_tranche1, pmin(salaire_brut - plafond_tranche1, plafond_tranche2 - plafond_tranche1) * taux_complementaire_tranche2, 0),
           "ceg" = pmin(salaire_brut, plafond_tranche1) * taux_ceg_tranche1 + ifelse(salaire_brut > plafond_tranche1, pmin(salaire_brut - plafond_tranche1, plafond_tranche2 - plafond_tranche1) * taux_ceg_tranche2, 0),
           "exonerations_pat" = ifelse(salaire_brut >= 3 * 1823.03,
                                       0,
                                       pmax(
                                         (ifelse(nombre_employes < 50, 0.3781, 0.3821)) * 
                                           (0.5 * (3 * 1823.03 / salaire_brut - 1))^1.75 + 0.02,
                                         0
                                       ) * salaire_brut)
    ) %>%
    mutate("assurance_maladie_pat" = maladie + csa,
           "retraite_pat" = vieillesse + complementaire_pat + ceg,
           "famille_pat" = allocations_familiales + fnal,
           "assurance_chomage_pat" = chomage + ags,
           "formation_pat" = formation_pro + taxe_apprentissage + cpf_cdd,
           "autres_pat" = accidents_travail + dialogue_social,
           "total_cotisations_pat" = assurance_maladie_pat + retraite_pat + famille_pat + assurance_chomage_pat + formation_pat + autres_pat - exonerations_pat
    ) %>%
    select(PTS_SMIC, assurance_maladie_pat, retraite_pat, famille_pat, assurance_chomage_pat, formation_pat, autres_pat, total_cotisations_pat)
  # BASE FINALE
  base <- base %>% left_join(cot_pat, by = "PTS_SMIC")
  
  # COTISATIONS SALARIALES #
  
  #TAUX
  taux_vieillesse_tot <- 0.004
  taux_vieillesse_plafond <- 0.069
  taux_csg_imposable <- 0.024
  taux_csg_non_imposable <- 0.068
  taux_crds <- 0.005
  taux_complementaire_tranche1 <- 0.0315
  taux_ceg_tranche1 <- 0.0086
  taux_complementaire_tranche2 <- 0.0864
  taux_ceg_tranche2 <- 0.0108
  
  # PLAFONDS
  PASS <- 4005
  plafond_tranche1 <- PASS
  plafond_tranche2 <- 8 * PASS
  
  # CALCULS
  cot_sal <- base %>% 
    mutate(
      "vieillesse" = salaire_brut * taux_vieillesse_tot + pmin(salaire_brut, PASS) * taux_vieillesse_plafond,
      "csg_imposable_sal" = pmin(salaire_brut, PASS*4) * 0.9825 * taux_csg_imposable,
      "csg_non_imposable" = pmin(salaire_brut, PASS*4) * 0.9825 * taux_csg_non_imposable,
      "crds" = pmin(salaire_brut, PASS*4) * 0.9825 * taux_crds,
      "complementaire_sal" = pmin(salaire_brut, plafond_tranche1) * taux_complementaire_tranche1 + ifelse(salaire_brut > plafond_tranche1, pmin(salaire_brut - plafond_tranche1, plafond_tranche2 - plafond_tranche1) * taux_complementaire_tranche2, 0),
      "ceg" = pmin(salaire_brut, plafond_tranche1) * taux_ceg_tranche1 + ifelse(salaire_brut > plafond_tranche1, pmin(salaire_brut - plafond_tranche1, plafond_tranche2 - plafond_tranche1) * taux_ceg_tranche2, 0)
    ) %>% 
    mutate(
      "retraite_sal" = vieillesse + complementaire_sal + ceg,
      "csg_crds_sal" = csg_imposable_sal +csg_non_imposable + crds,
      "total_cotisations_sal" = retraite_sal + csg_crds_sal,
      "total_cotisations_sal_horsCSGCRDS" = total_cotisations_sal - csg_crds_sal) %>%
    select(PTS_SMIC, retraite_sal, csg_crds_sal, csg_imposable_sal, total_cotisations_sal, total_cotisations_sal_horsCSGCRDS)
  
  # BASE FINALE
  
  base <- base %>% left_join(cot_sal, by = "PTS_SMIC")
  
  
  # IMPOT SUR LE REVENU #
  
  base <- base %>% mutate(
    salaire_net = salaire_brut - total_cotisations_sal,
    salaire_super_brut = salaire_brut + total_cotisations_pat, 
    salaire_net_imposable = salaire_net + csg_imposable_sal,
    salaire_net_imposable_abattu = salaire_net_imposable - pmin(pmax(0.10 * salaire_net_imposable, 504 / 12), 14426 / 12))
  
  #TRANCHES ET TAUX D'IR
  tranche1_limite <- 11600/12
  tranche2_limite <- 29579/12
  tranche3_limite <- 84578/12
  tranche4_limite <- 181917/12
  
  taux1 <- 0
  taux2 <- 0.11
  taux3 <- 0.30
  taux4 <- 0.41
  taux5 <- 0.45
  
  base <- base %>% mutate(impot_brut = ifelse(salaire_net_imposable_abattu <= tranche1_limite, salaire_net_imposable_abattu * taux1,
                                              ifelse(salaire_net_imposable_abattu <= tranche2_limite, 
                                                     (tranche1_limite * taux1) + (salaire_net_imposable_abattu - tranche1_limite) * taux2,
                                                     ifelse(salaire_net_imposable_abattu <= tranche3_limite, 
                                                            (tranche1_limite * taux1) + (tranche2_limite - tranche1_limite) * taux2 + (salaire_net_imposable_abattu - tranche2_limite) * taux3,
                                                            ifelse(salaire_net_imposable_abattu <= tranche4_limite, 
                                                                   (tranche1_limite * taux1) + (tranche2_limite - tranche1_limite) * taux2 + (tranche3_limite - tranche2_limite) * taux3 + (salaire_net_imposable_abattu - tranche3_limite) * taux4,
                                                                   (tranche1_limite * taux1) + (tranche2_limite - tranche1_limite) * taux2 + (tranche3_limite - tranche2_limite) * taux3 + (tranche4_limite - tranche3_limite) * taux4 + (salaire_net_imposable_abattu - tranche4_limite) * taux5
                                                            ))))) %>%
    mutate(decote = ifelse(impot_brut <= 1982/12, (897 - 0.4525 * impot_brut*12)/12, 0),
           impot_revenu_total  = pmax(impot_brut - decote, 0))
  
  base <- base %>% mutate("salaire_net_impot" = salaire_net - impot_revenu_total)
  
  
  #PRIME D'ACTIVITE
  # Constantes 2026
  montant_forfaitaire <- 638.28
  seuil_bonification <- 709.18
    bonification_max <- 240.63 
  
  base <- base %>% 
    mutate(
      # Calcul de la bonification (entre 700.92€ et ~1425€)
      bonification = ifelse(salaire_net >= seuil_bonification,
                            pmin((salaire_net - seuil_bonification) * (bonification_max / ( 1658.76 - seuil_bonification)), bonification_max),
                            0),
      # Calcul de la prime d'activité
      prime_activite = pmax(montant_forfaitaire + salaire_net * 0.5985 + bonification - salaire_net, 0)) %>%
    mutate(salaire_net_impot_prime_activite = salaire_net_impot + prime_activite)
  
 
  
  #RATIOS
  
  base <- base %>% mutate("% cotisations patronales (%brut)" = total_cotisations_pat / salaire_brut,
                          "% cotisations salariales (%brut)" = total_cotisations_sal / salaire_brut,
                          "% cotisations salariales hors CSG-CRDS (%brut)" = total_cotisations_sal_horsCSGCRDS / salaire_brut,
                          "taux d'IR (%net)" = (impot_revenu_total - prime_activite ) / salaire_net,
                          "salaire net d'impot (%brut)" = salaire_net_impot_prime_activite / salaire_brut,
                          "salaire super brut (%brut)" = salaire_super_brut / salaire_brut)
  
}



# Fonction "COIN_SOCIO_FISCAL" ####

COIN_SOCIAL_FR <- function(base) {
  if (!"prime_activite" %in% colnames(base)) {
    base$prime_activite <- 0
  }
  
  base <- base %>% mutate(coin_social = (total_cotisations_pat + total_cotisations_sal) / salaire_super_brut,
                          coin_fiscal = (impot_revenu_total - prime_activite) / salaire_super_brut,
                          coin_sociofiscal = (total_cotisations_pat + total_cotisations_sal + impot_revenu_total - prime_activite) / salaire_super_brut
  )
  
  return(base)
}




# Fonction CHOMAGE_RETRAITE_FR ####

CHOMAGE_RETRAITE_FR <- function(base){
  
  tranche1_limite <- 11600/12
  tranche2_limite <- 29579/12
  tranche3_limite <- 84578/12
  tranche4_limite <- 181917/12
  
  taux1 <- 0
  taux2 <- 0.11
  taux3 <- 0.30
  taux4 <- 0.41
  taux5 <- 0.45
  
  # CHOMAGE #a jour sur données a partir du 1 juillet 2°25
  
  base <- base %>% mutate(
    "SJR" = salaire_brut * 12 / 365,
    "ARE" = pmax(pmin(pmax(0.404 * SJR + 13.18, 0.57 * SJR), 300.21 ), 32.13),
    "ARE_mois" = ARE * 30,
    "ARE_nette" = ARE - ifelse(ARE > 32.13, SJR * 0.03, 0) - ifelse(ARE > 61, ARE * 0.9825 * 0.062, 0) - ifelse(ARE > 61, ARE * 0.9825 * 0.005, 0),
    "ARE_nette_mois" = ARE_nette * 30,
    "ARE_nette_imposable" = ARE - ifelse(ARE > 32.13, SJR * 0.03, 0) - ifelse(ARE > 61, ARE * 0.9825 * 0.038, 0),
    "ARE_nette_mois_imposable" = ARE_nette_imposable * 30
  )
  
  base <- base %>% mutate(
    "ARE_nette_mois_imposable_abattu" = ARE_nette_mois_imposable - pmax(509/12, pmin(ARE_nette_mois_imposable*0.10, 14555/12)),
    
    "impot_ARE" = ifelse(ARE_nette_mois_imposable_abattu <= tranche1_limite,
                         ARE_nette_mois_imposable_abattu * taux1,
                         ifelse(ARE_nette_mois_imposable_abattu <= tranche2_limite,
                                (tranche1_limite * taux1) + (ARE_nette_mois_imposable_abattu - tranche1_limite) * taux2,
                                ifelse(ARE_nette_mois_imposable_abattu <= tranche3_limite,
                                       (tranche1_limite * taux1) + (tranche2_limite - tranche1_limite) * taux2 + (ARE_nette_mois_imposable_abattu - tranche2_limite) * taux3,
                                       ifelse(ARE_nette_mois_imposable_abattu <= tranche4_limite,
                                              (tranche1_limite * taux1) + (tranche2_limite - tranche1_limite) * taux2 + (tranche3_limite - tranche2_limite) * taux3 + (ARE_nette_mois_imposable_abattu - tranche3_limite) * taux4,
                                              (tranche1_limite * taux1) + (tranche2_limite - tranche1_limite) * taux2 + (tranche3_limite - tranche2_limite) * taux3 + (tranche4_limite - tranche3_limite) * taux4 + (ARE_nette_mois_imposable_abattu - tranche4_limite) * taux5)))),
    
    "ARE_nette_impot" = ARE_nette_mois - impot_ARE)
  
  
  
  # RETRAITE #
  taux_complementaire_tranche1 <- 0.062
  taux_complementaire_tranche2 <- 0.17
  PASS <- 4005
  plafond_tranche1 <- PASS
  plafond_tranche2 <- 8 * PASS
  
  tranche1_limite <- 11600/12
  tranche2_limite <- 29579/12
  tranche3_limite <- 84578/12
  tranche4_limite <- 181917/12
  
  taux1 <- 0
  taux2 <- 0.11
  taux3 <- 0.30
  taux4 <- 0.41
  taux5 <- 0.45
  
  
  base <- base %>% mutate("retraite_tauxplein_base_mensuelle" = pmin(pmax(round(salaire_brut * 0.5, 2), 903.93), 2002.5),
                          "cotisations_agirc_arrco" = pmin(salaire_brut, plafond_tranche1) * taux_complementaire_tranche1 + ifelse(salaire_brut > plafond_tranche1, pmin(salaire_brut - plafond_tranche1, plafond_tranche2 - plafond_tranche1) * taux_complementaire_tranche2, 0),
                          "total_points_agirc_arrco" = cotisations_agirc_arrco * 12 * 43 / 20.1877,
                          "complementaire_retraite_menuselle" = round(total_points_agirc_arrco * 1.4386 / 12, 2),
                          "pension_retraite_totale" = retraite_tauxplein_base_mensuelle + complementaire_retraite_menuselle)
  
  base <- base %>% mutate("retraite_csg" = case_when(
    pension_retraite_totale < 13048/12 ~ 0,
    pension_retraite_totale >= 13048/12 & pension_retraite_totale <= 17057/12 ~ pension_retraite_totale * 0.038,
    pension_retraite_totale > 17057/12 & pension_retraite_totale <= 26472/12 ~ pension_retraite_totale * 0.066,
    pension_retraite_totale > 26472/12 ~ pension_retraite_totale * 0.083),
    
    "retraite_crds" = case_when(
      pension_retraite_totale < 13048/12 ~ 0,
      pension_retraite_totale >= 13048/12 ~ pension_retraite_totale * 0.005),
    
    "retraite_casa" = case_when(
      pension_retraite_totale < 17057/12 ~ 0,
      pension_retraite_totale >= 17057/12 ~ pension_retraite_totale * 0.003),
    
    "charges_complementaire" = case_when(
      pension_retraite_totale <= 17057/12 ~ 0,
      pension_retraite_totale > 17057/12 ~ complementaire_retraite_menuselle * 0.01),
    
    "pension_retraite_nette" = pension_retraite_totale - retraite_csg - retraite_crds - retraite_casa - charges_complementaire,
    
    "pension_retraite_nette_imposable" = case_when(
      pension_retraite_totale < 13048/12 ~ pension_retraite_totale - charges_complementaire,
      pension_retraite_totale >= 13048/12 & pension_retraite_totale <= 17057/12 ~ pension_retraite_totale - (pension_retraite_totale * 0.038) - charges_complementaire,
      pension_retraite_totale > 17057/12 & pension_retraite_totale <= 26472/12 ~ pension_retraite_totale - (pension_retraite_totale * 0.042) - charges_complementaire,
      pension_retraite_totale > 26472/12 ~ pension_retraite_totale - (pension_retraite_totale * 0.059) - charges_complementaire)
    
    ,
    
    "pension_retraite_nette_imposable_abattu" = pension_retraite_nette_imposable - pmax(454/12, pmin(pension_retraite_nette_imposable*0.10, 4439/12)),
    
    "impot_retraite" = ifelse(pension_retraite_nette_imposable_abattu <= tranche1_limite,
                              pension_retraite_nette_imposable_abattu * taux1,
                              ifelse(pension_retraite_nette_imposable_abattu <= tranche2_limite,
                                     (tranche1_limite * taux1) + (pension_retraite_nette_imposable_abattu - tranche1_limite) * taux2,
                                     ifelse(pension_retraite_nette_imposable_abattu <= tranche3_limite,
                                            (tranche1_limite * taux1) + (tranche2_limite - tranche1_limite) * taux2 + (pension_retraite_nette_imposable_abattu - tranche2_limite) * taux3,
                                            ifelse(pension_retraite_nette_imposable_abattu <= tranche4_limite,
                                                   (tranche1_limite * taux1) + (tranche2_limite - tranche1_limite) * taux2 + (tranche3_limite - tranche2_limite) * taux3 + (pension_retraite_nette_imposable_abattu - tranche3_limite) * taux4,
                                                   (tranche1_limite * taux1) + (tranche2_limite - tranche1_limite) * taux2 + (tranche3_limite - tranche2_limite) * taux3 + (tranche4_limite - tranche3_limite) * taux4 + (pension_retraite_nette_imposable_abattu - tranche4_limite) * taux5)))),
    
    "pension_retraite_nette_impot" = pension_retraite_nette - impot_retraite)
  
  
  
  # TAUX REMPLACEMENT #
  
  base <- base %>% mutate("taux_remplacement_chomage" = ARE_nette_impot / salaire_brut,
                          "taux_remplacement_retraite" = pension_retraite_nette_impot / salaire_brut,
                          "taux_remplacement_retraite_net" = pension_retraite_nette_impot/salaire_net, 
                          "taux_remplacement_brut" = pension_retraite_totale / salaire_brut )
  
}



#



## sorties FRANCE ####
test_fr <- CALCULS_FR(base)
cotisations_fr <- COIN_SOCIAL_FR(test_fr)
retraite_chomage_fr <- CHOMAGE_RETRAITE_FR(cotisations_fr)



final_FR <- retraite_chomage_fr  %>%  
  mutate("tx_cotis_chomage_FR" = cotisations_fr$assurance_chomage_pat / cotisations_fr$salaire_brut) %>%  
  mutate("transfo_chom_FR" = retraite_chomage_fr$taux_remplacement_chomage / tx_cotis_chomage_FR)%>%  
  mutate("tx_cotis_retraite_FR" = c(cotisations_fr$retraite_sal + cotisations_fr$retraite_pat) / cotisations_fr$salaire_brut) %>%
  mutate("transfo_retraite_FR" = retraite_chomage_fr$taux_remplacement_retraite / tx_cotis_retraite_FR)  

colnames(final_FR)

temp<-final_FR

sortie_FR<-data.frame(temp$PTS_SMIC, temp$salaire_brut, temp$salaire_super_brut, temp$salaire_net_impot_prime_activite, temp$taux_remplacement_retraite, temp$tx_cotis_retraite_FR, temp$transfo_retraite_FR, temp$taux_remplacement_chomage, temp$tx_cotis_chomage_FR, temp$transfo_chom_FR, temp$taux_remplacement_brut )


#write.xlsx(final_FR, "CSF2026BIS.xlsx",sheetName = "Feuille1", colNames = TRUE, rowNames = TRUE, append = FALSE)

#write.xlsx(cotisations_fr, "cotisations_FR.xlsx",sheetName = "Feuille1", colNames = TRUE, rowNames = TRUE, append = FALSE)
#write.xlsx(retraite_chomage_fr, "retraite_chomage_fr.xlsx",sheetName = "Feuille1", colNames = TRUE, rowNames = TRUE, append = FALSE)


