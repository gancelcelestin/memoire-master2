
#Méthode 1 réplication ------
# ==============================================================================
# IMPORTS HORS UE/OCDE (AVEC TURQUIE) - CONSOMMATION STRICTE & SUBSTITUABLE
# TOP 20 PAYS FOURNISSEURS (2019)
# ==============================================================================
library(arrow)
library(dplyr)
library(readxl)
library(stringr)
library(gt) # Pour générer un beau tableau (install.packages("gt") si besoin)

# 1. Définition des listes de pays (ISO2) --------------------------------------
UE_ISO2 <- c(
  "AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", "FR", "GR", "HR",
  "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", 
  "SK", "GB"
)

# Liste OCDE hors UE complète (Turquie exclue de la liste à retirer)
OCDE_NON_UE_SANS_TR <- c(
  "AU", "CA", "CH", "CL", "CO", "CR", "IL", "IS", "JP", "KR", "MX", "NO", "NZ", "US"
)

PAYS_A_EXCLURE <- c(UE_ISO2, OCDE_NON_UE_SANS_TR)

# 2. Chargement de la table BEC ------------------------------------------------
cat("Chargement de la table BEC...\n")
bec <- read_excel("Desktop/MiF_py/conversion_BEC.xlsx") %>%
  rename(code_prod = HS6) %>%
  mutate(
    code_prod  = str_pad(as.character(code_prod), 6, pad = "0"),
    BEC5EndUse = str_to_upper(as.character(BEC5EndUse))
  ) %>%
  select(code_prod, BEC5EndUse) %>%
  distinct(code_prod, .keep_all = TRUE)

# 3. Identification des codes NC8 exportés (FLUX 2 et 4) ----------------------
cat("Identification des NC8 substituables (exportés en 2019)...\n")
f2 <- read_parquet("Desktop/MiF_py/flux2_2019.parquet") %>% filter(flux == 2)
f4 <- read_parquet("Desktop/MiF_py/flux4_2019.parquet") %>% filter(flux == 4)

nc8_exportes <- bind_rows(f2, f4) %>%
  mutate(nc8 = str_pad(as.character(nc8), 8, pad = "0")) %>%
  pull(nc8) %>%
  unique()

# 4. Chargement et filtrage des importations (FLUX 1 et 3) ---------------------
cat("Chargement et filtrage des importations (FLUX 1 et 3)...\n")
f1 <- read_parquet("Desktop/MiF_py/flux_2019.parquet") %>% 
  filter(flux == 1) %>% 
  rename(valeur = import)

f3 <- read_parquet("Desktop/MiF_py/flux3_2019.parquet") %>% 
  filter(flux == 3) %>% 
  rename(valeur = import)

imp_filtre <- bind_rows(f1, f3) %>%
  mutate(
    iso2 = str_to_upper(iso2),
    nc8  = str_pad(as.character(nc8), 8, pad = "0"),
    hs6  = str_sub(nc8, 1, 6)
  ) %>%
  # A) Exclusion UE + OCDE (sauf Turquie TR)
  filter(!iso2 %in% PAYS_A_EXCLURE) %>%
  # B) Appariement BEC
  left_join(bec, by = c("hs6" = "code_prod")) %>%
  # C) Consommation stricte (CONS) ET substituable (présent à l'export)
  filter(BEC5EndUse == "CONS", nc8 %in% nc8_exportes)

# 5. Agrégation par pays et calcul du Top 20 ----------------------------------
total_valeur_champ <- sum(imp_filtre$valeur, na.rm = TRUE)

top20_pays <- imp_filtre %>%
  group_by(iso2, country) %>%
  summarise(valeur_eur = sum(valeur, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    valeur_milliards_eur = valeur_eur / 1e9,
    part_pct = 100 * valeur_eur / total_valeur_champ
  ) %>%
  arrange(desc(valeur_eur)) %>%
  slice_head(n = 20) %>%
  mutate(Rang = row_number()) %>%
  select(Rang, Code_ISO2 = iso2, Pays = country, Valeur_Mds = valeur_milliards_eur, Part_Pct = part_pct)
#6.BIS --------
# 6. Génération directe du code LaTeX (à copier-coller) -----------------------
library(knitr)
library(dplyr)

# Préparation du dataframe avec les bons formats de texte (virgule décimale)
df_latex <- top20_pays %>%
  mutate(
    Valeur_Mds = sprintf("%.2f", Valeur_Mds) %>% gsub("\\.", ",", .),
    Part_Pct   = paste0(sprintf("%.2f", Part_Pct) %>% gsub("\\.", ",", .), " %")
  )

# Génération du code LaTeX brut
code_latex <- kable(
  df_latex,
  format = "latex",
  booktabs = TRUE,
  align = c("c", "c", "l", "r", "r"),
  col.names = c(
    "Rang", 
    "Code ISO", 
    "Pays Fournisseur", 
    "Valeur (Mds €)", 
    "Part dans le Champ"
  ),
  caption = "Top 20 des Pays Fournisseurs d'Importations Substituables (2019)"
)


# ==============================================================================
# CALCUL DU SURCOÛT DE RELOCALISATION PAR SECTEUR HS2 (2019)
# Suite directe du script — Méthodologie CEPII (Consommation Stricte & Substituable)
# ==============================================================================

cat("7. Chargement de la nomenclature HS2...\n")

hs2_dict <- read_excel("Desktop/MiF_py/HSCodeandDescription.xlsx", sheet = "HS12") %>%
  filter(as.character(Level) == "2") %>%
  mutate(
    hs2             = str_pad(as.character(Code), 2, pad = "0"),
    hs2_description = as.character(Description)
  ) %>%
  select(hs2, hs2_description) %>%
  distinct(hs2, .keep_all = TRUE)

# 8. Calcul des Prix/kg aux exports français (FOB) ----------------------------
cat("Calcul des prix/kg moyens des exportations françaises...\n")

exp_pk <- bind_rows(f2, f4) %>%
  mutate(nc8 = str_pad(as.character(nc8), 8, pad = "0")) %>%
  filter(indicmasse == 1, kgs > 0) %>%
  group_by(nc8) %>%
  summarise(
    exp_val_m = sum(export, na.rm = TRUE),
    exp_kg    = sum(kgs, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  mutate(pk_export = exp_val_m / exp_kg)

# 9. Appariement et calcul du surcoût au niveau transaction (NC8 x Pays) ------
cat("Calcul du surcoût ligne par ligne...\n")

imp_surcout <- imp_filtre %>%
  # Conservation des transactions avec masse valide
  filter(indicmasse == 1, kgs > 0, valeur > 0) %>%
  mutate(
    pk_import = valeur / kgs,
    hs2       = str_sub(nc8, 1, 2)
  ) %>%
  left_join(exp_pk %>% select(nc8, pk_export), by = "nc8") %>%
  filter(!is.na(pk_export), pk_export > 0, pk_import > 0) %>%
  mutate(
    ratio_pk    = pk_export / pk_import,
    # Surcoût unitaire = (Ratio * Valeur) - Valeur
    surcout_eur = (ratio_pk * valeur) - valeur
  ) %>%
  left_join(hs2_dict, by = "hs2")

# 10. Agrégation par secteur HS2 (Top 10) --------------------------------------
total_surcout_champ <- sum(imp_surcout$surcout_eur, na.rm = TRUE)

top10_hs2_surcout <- imp_surcout %>%
  group_by(hs2, hs2_description) %>%
  summarise(
    valeur_import_eur = sum(valeur, na.rm = TRUE),
    surcout_eur       = sum(surcout_eur, na.rm = TRUE),
    .groups           = "drop"
  ) %>%
  mutate(
    surcout_milliards_eur = surcout_eur / 1e9,
    part_surcout_pct      = 100 * surcout_eur / total_surcout_champ
  ) %>%
  arrange(desc(surcout_eur)) %>%
  slice_head(n = 10) %>%
  mutate(
    Rang = row_number(),
    Section_HS2 = paste0("HS2-", hs2, " : ", hs2_description)
  ) %>%
  select(Rang, Section_HS2, Surcout_Mds = surcout_milliards_eur, Part_Surcout_Pct = part_surcout_pct)

# 11. Génération directe du code LaTeX (à copier-coller) ----------------------
library(knitr)
library(dplyr)

# Préparation du dataframe avec mise en forme des nombres (virgule décimale)
df_hs2_latex <- top10_hs2_surcout %>%
  mutate(
    Surcout_Mds      = sprintf("%.2f", Surcout_Mds) %>% gsub("\\.", ",", .),
    Part_Surcout_Pct = paste0(sprintf("%.2f", Part_Surcout_Pct) %>% gsub("\\.", ",", .), " %")
  )

# Génération du code LaTeX brut
code_latex_hs2 <- kable(
  df_hs2_latex,
  format = "latex",
  booktabs = TRUE,
  align = c("c", "l", "r", "r"),
  col.names = c(
    "Rang", 
    "Secteur d'Activité (HS2)", 
    "Surcoût Estimé (Mds €)", 
    "Part dans le Surcoût Total"
  ),
  caption = "Top 10 des Secteurs HS2 par Surcoût de Relocalisation (Consommation Stricte, 2019)"
)

# Affichage direct dans la console R
cat("\n=== COPIE LE CODE CI-DESSOUS DANS TON FICHIER .TEX ===\n\n")
cat(code_latex_hs2, sep = "\n")
cat("\n=======================================================\n")

# ==============================================================================
# SECTION 12. CALCUL DU SURCOÛT TOTAL GLOBAL ET TEST DE ROBUSTESSE (FIRMESE)
# ==============================================================================

cat("Calcul du surcoût global Standard et du test de robustesse par exclusion des firmes haut de gamme...\n")

# A. Calcul de la référence française standard (Tous SIREN) par NC8
exp_std <- bind_rows(f2, f4) %>%
  filter(indicmasse == 1, kgs > 0, export > 0) %>%
  mutate(nc8 = str_pad(as.character(nc8), 8, pad = "0")) %>%
  group_by(nc8) %>%
  summarise(
    exp_val_total = sum(export, na.rm = TRUE),
    exp_kg_total  = sum(kgs, na.rm = TRUE),
    .groups       = "drop"
  ) %>%
  mutate(pk_export_std = exp_val_total / exp_kg_total)

# B. Calcul du prix au kilo par firme (SIREN) x NC8 côté français
exp_siren <- bind_rows(f2, f4) %>%
  filter(indicmasse == 1, kgs > 0, export > 0, !is.na(siren)) %>%
  mutate(nc8 = str_pad(as.character(nc8), 8, pad = "0")) %>%
  group_by(nc8, siren) %>%
  summarise(
    val_siren = sum(export, na.rm = TRUE),
    kg_siren  = sum(kgs, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  mutate(pk_siren = val_siren / kg_siren)

# C. Filtrage des 10% des firmes aux prix/kg les plus élevés pour chaque NC8
exp_robust_siren <- exp_siren %>%
  group_by(nc8) %>%
  filter(pk_siren <= quantile(pk_siren, 0.99, na.rm = TRUE)) %>%
  summarise(
    exp_val_robust = sum(val_siren, na.rm = TRUE),
    exp_kg_robust  = sum(kg_siren, na.rm = TRUE),
    .groups        = "drop"
  ) %>%
  filter(exp_kg_robust > 0) %>%
  mutate(pk_export_robust = exp_val_robust / exp_kg_robust)

# D. Appariement avec les importations et calcul des deux surcoûts
surcout_global_df <- imp_filtre %>%
  filter(indicmasse == 1, kgs > 0, valeur > 0) %>%
  mutate(pk_import = valeur / kgs) %>%
  left_join(exp_std %>% select(nc8, pk_export_std), by = "nc8") %>%
  left_join(exp_robust_siren %>% select(nc8, pk_export_robust), by = "nc8") %>%
  filter(!is.na(pk_export_std), pk_export_std > 0, pk_import > 0) %>%
  mutate(
    # Surcoût Standard
    surcout_std_eur = ((pk_export_std / pk_import) * valeur) - valeur,
    # Surcoût Robustesse (si le NC8 est conservé après retrait du top 10% des firmes)
    surcout_robust_eur = if_else(
      !is.na(pk_export_robust) & pk_export_robust > 0,
      ((pk_export_robust / pk_import) * valeur) - valeur,
      NA_real_
    )
  )

# E. Synthèse des résultats globaux en Milliards d'Euros
total_surcout_std_mds    <- sum(surcout_global_df$surcout_std_eur, na.rm = TRUE) / 1e9
total_surcout_robust_mds <- sum(surcout_global_df$surcout_robust_eur, na.rm = TRUE) / 1e9

nb_menages_2019 <- 29800000

surcout_par_menage_std    <- (total_surcout_std_mds * 1e9) / nb_menages_2019
surcout_par_menage_robust <- (total_surcout_robust_mds * 1e9) / nb_menages_2019

# F. Tableau récapitulatif 'gt'
res_recap <- tibble(
  Indicateur = c(
    "Surcoût Total Global (Standard)",
    "Surcoût Total Global (Robustesse - Top 10% Firmes Exclues)"
  ),
  `Surcoût Total (Mds €)` = c(total_surcout_std_mds, total_surcout_robust_mds),
  `Par Ménage / An (€)`   = c(surcout_par_menage_std, surcout_par_menage_robust),
  `Par Ménage / Mois (€)`  = c(surcout_par_menage_std / 12, surcout_par_menage_robust / 12)
)

tableau_recap_gt <- res_recap %>%
  gt() %>%
  tab_header(
    title = md("**Synthèse du Surcoût Total de Relocalisation (2019)**"),
    subtitle = "Consommation Stricte Substituable — Champ Pays de Délocalisation"
  ) %>%
  fmt_number(
    columns = `Surcoût Total (Mds €)`,
    decimals = 2,
    dec_mark = ",",
    sep_mark = " "
  ) %>%
  fmt_number(
    columns = c(`Par Ménage / An (€)`, `Par Ménage / Mois (€)`),
    decimals = 0,
    dec_mark = ",",
    sep_mark = " "
  ) %>%
  tab_options(
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(12),
    table.border.top.color = "#1F3A60",
    column_labels.background.color = "#1F3A60",
    column_labels.font.weight = "bold",
    table.font.names = "Arial",
    data_row.padding = px(8)
  )

print(tableau_recap_gt)
