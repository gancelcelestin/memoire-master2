
# Affichage direct dans la console R
cat("\n=== COPIE LE CODE CI-DESSOUS DANS TON FICHIER .TEX ===\n\n")
cat(code_latex, sep = "\n")
cat("\n=======================================================\n")

#Méthode 2 regarder les plus gros ratios -------

# ==============================================================================
# CALCUL DES RATIOS PRIX/KG D'IMPORTATION PAR PAYS (2019)
# CONSOMMATION STRICTE & SUBSTITUABLE - SEUIL 0,5%
# ==============================================================================

library(arrow)
library(dplyr)
library(readxl)
library(stringr)
library(gt) # Pour générer la table d'affichage (install.packages("gt") si besoin)

# 1. Chargement de la table de conversion BEC ---------------------------------
cat("Chargement de la table BEC...\n")
bec <- read_excel("Desktop/MiF_py/conversion_BEC.xlsx") %>%
  rename(code_prod = HS6) %>%
  mutate(
    code_prod  = str_pad(as.character(code_prod), 6, pad = "0"),
    BEC5EndUse = str_to_upper(as.character(BEC5EndUse))
  ) %>%
  select(code_prod, BEC5EndUse) %>%
  distinct(code_prod, .keep_all = TRUE)

# 2. Traitement des flux (Consommation Stricte 'CONS') ------------------------
cat("Traitement et filtrage des 4 flux douaniers 2019...\n")

traiter_flux <- function(path, val_col, flux_val, sens) {
  read_parquet(path) %>%
    filter(flux == flux_val) %>%
    rename(valeur = all_of(val_col)) %>%
    mutate(
      nc8 = str_pad(as.character(nc8), 8, pad = "0"),
      hs6 = str_sub(nc8, 1, 6)
    ) %>%
    left_join(bec, by = c("hs6" = "code_prod")) %>%
    mutate(BEC5EndUse = str_to_upper(BEC5EndUse)) %>%
    filter(BEC5EndUse == "CONS") %>% # Consommation stricte uniquement
    group_by(nc8, iso2, country) %>%
    summarise(
      prix       = sum(valeur, na.rm = TRUE),                     # Valeur totale
      poids      = sum(kgs[indicmasse == 1], na.rm = TRUE),        # Masse connue
      prix_masse = sum(valeur[indicmasse == 1], na.rm = TRUE),     # Valeur associée à la masse
      .groups    = "drop"
    ) %>%
    mutate(sens = sens)
}

f1 <- traiter_flux("Desktop/MiF_py/flux_2019.parquet",  "import", 1, "I")
f3 <- traiter_flux("Desktop/MiF_py/flux3_2019.parquet", "import", 3, "I")
f2 <- traiter_flux("Desktop/MiF_py/flux2_2019.parquet", "export", 2, "E")
f4 <- traiter_flux("Desktop/MiF_py/flux4_2019.parquet", "export", 4, "E")

dat <- bind_rows(f1, f3, f2, f4)

# 3. Univers substituable (NC8 présents en import ET export) ------------------
imp <- dat %>% 
  filter(sens == "I") %>% 
  group_by(nc8, iso2, country) %>%
  summarise(
    imp_val   = sum(prix), 
    imp_kg    = sum(poids),
    imp_val_m = sum(prix_masse), 
    .groups   = "drop"
  )

exp <- dat %>% 
  filter(sens == "E") %>% 
  group_by(nc8) %>%
  summarise(
    exp_kg    = sum(poids), 
    exp_val_m = sum(prix_masse), 
    .groups   = "drop"
  )

# Intersection des NC8
nc8_both <- intersect(unique(imp$nc8), unique(exp$nc8))
imp <- imp %>% filter(nc8 %in% nc8_both)
exp <- exp %>% filter(nc8 %in% nc8_both)

total_imp_sub <- sum(imp$imp_val, na.rm = TRUE)

# 4. Calcul des Prix au kilo et des Ratios (NC8 x Pays) -----------------------
# A) Prix/kg moyen des exportations françaises par NC8 (Numérateur)
pk_exp <- exp %>% 
  mutate(pk_export = if_else(exp_kg > 0, exp_val_m / exp_kg, NA_real_))

# B) Prix/kg des importations par couple (NC8 x Pays) (Dénominateur)
imp_pc <- imp %>%
  mutate(pk_import = if_else(imp_kg > 0, imp_val_m / imp_kg, NA_real_)) %>%
  left_join(pk_exp %>% select(nc8, pk_export), by = "nc8") %>%
  # Ratio = (Prix/kg export France) / (Prix/kg import du pays)
  mutate(ratio_pk = pk_export / pk_import)

# 5. Agrégation du Ratio par Pays (Pondération par la part du produit) --------
ratio_tous_pays <- imp_pc %>%
  filter(is.finite(ratio_pk), imp_val > 0) %>%
  group_by(iso2, country) %>%
  mutate(w = imp_val / sum(imp_val)) %>% # Pondération : part du produit dans le pays
  summarise(
    ratio_pk_agrege = sum(w * ratio_pk, na.rm = TRUE),
    imp_val         = sum(imp_val, na.rm = TRUE),
    .groups         = "drop"
  ) %>%
  mutate(part_pct = 100 * imp_val / total_imp_sub)

# 6. Classements : Top 20 Sans Filtre vs Top 10 Avec Seuil 0,5% ----------------
# A) Top 20 Ratios Globaux (Sans filtre de taille)
top20_ratio_brut <- ratio_tous_pays %>%
  arrange(desc(ratio_pk_agrege)) %>%
  slice_head(n = 20) %>%
  mutate(Rang = row_number(), Valeur_Mds = imp_val / 1e9)

# B) Top 10 Ratios des pays matériels (Part >= 0.5% du total)
SEUIL_PART <- 0.005 # Seuil à 0.5 %
top10_ratio_filtre <- ratio_tous_pays %>%
  filter(part_pct >= (SEUIL_PART * 100)) %>%
  arrange(desc(ratio_pk_agrege)) %>%
  slice_head(n = 10) %>%
  mutate(Rang = row_number(), Valeur_Mds = imp_val / 1e9)

# 7. Génération du code LaTeX pour la Table 1 (Top 10 Pays) -------------------
library(knitr)
library(dplyr)

# Préparation du dataframe avec mise en forme des nombres (virgule décimale)
df_top10_latex <- top10_ratio_filtre %>%
  select(
    Rang, 
    Code_ISO    = iso2, 
    Pays        = country, 
    Ratio_Poids = ratio_pk_agrege, 
    Part_Total  = part_pct, 
    Valeur_Mds
  ) %>%
  mutate(
    Ratio_Poids = sprintf("%.2f", Ratio_Poids) %>% gsub("\\.", ",", .),
    Part_Total  = paste0(sprintf("%.2f", Part_Total) %>% gsub("\\.", ",", .), " %"),
    Valeur_Mds  = sprintf("%.2f", Valeur_Mds) %>% gsub("\\.", ",", .)
  )

# Génération du code LaTeX brut
code_latex_top10 <- kable(
  df_top10_latex,
  format = "latex",
  booktabs = TRUE,
  align = c("c", "c", "l", "r", "r", "r"),
  col.names = c(
    "Rang", 
    "Code ISO", 
    "Pays Fournisseur", 
    "Ratio P/Kg Agrégé", 
    "Part Import", 
    "Valeur (Mds €)"
  ),
  caption = "Top 10 des Pays avec le Ratio Prix/Kg le plus Élevé (part $\\ge$ 0,5\\%, 2019)"
)

# Affichage direct dans la console R
cat("\n=== COPIE LE CODE CI-DESSOUS DANS TON FICHIER .TEX ===\n\n")
cat(code_latex_top10, sep = "\n")
cat("\n=======================================================\n")

# ==============================================================================
# CALCUL DU SURCOÛT DE RELOCALISATION PAR SECTEUR HS2 (2019)
# Top 10 des pays à plus fort ratio (Part >= 0,5%) — Consommation Stricte
# ==============================================================================

library(arrow)
library(dplyr)
library(readxl)
library(stringr)
library(gt)

# 1. Chargement de la nomenclature HS2 (onglet HS12) --------------------------
cat("Chargement de la nomenclature HS2...\n")

hs2_dict <- read_excel("Desktop/MiF_py/HSCodeandDescription.xlsx", sheet = "HS12") %>%
  filter(as.character(Level) == "2") %>%
  mutate(
    hs2             = str_pad(as.character(Code), 2, pad = "0"),
    hs2_description = as.character(Description)
  ) %>%
  select(hs2, hs2_description) %>%
  distinct(hs2, .keep_all = TRUE)

# 2. Chargement de la table BEC ------------------------------------------------
cat("Chargement de la table BEC...\n")

bec <- read_excel("Desktop/MiF_py/conversion_BEC.xlsx") %>%
  rename(code_prod = HS6) %>%
  mutate(
    code_prod  = str_pad(as.character(code_prod), 6, pad = "0"),
    hs2        = str_sub(code_prod, 1, 2),
    BEC5EndUse = str_to_upper(as.character(BEC5EndUse))
  ) %>%
  left_join(hs2_dict, by = "hs2") %>%
  select(code_prod, hs2, hs2_description, BEC5EndUse) %>%
  distinct(code_prod, .keep_all = TRUE)

# 3. Traitement des flux 2019 (Consommation Stricte) --------------------------
cat("Traitement et filtrage des flux douaniers...\n")

traiter_flux <- function(path, val_col, flux_val, sens) {
  read_parquet(path) %>%
    filter(flux == flux_val) %>%
    rename(valeur = all_of(val_col)) %>%
    mutate(
      nc8 = str_pad(as.character(nc8), 8, pad = "0"),
      hs6 = str_sub(nc8, 1, 6)
    ) %>%
    left_join(bec, by = c("hs6" = "code_prod")) %>%
    filter(BEC5EndUse == "CONS") %>%
    group_by(nc8, hs2, hs2_description, iso2, country) %>%
    summarise(
      prix       = sum(valeur, na.rm = TRUE),
      poids      = sum(kgs[indicmasse == 1], na.rm = TRUE),
      prix_masse = sum(valeur[indicmasse == 1], na.rm = TRUE),
      .groups    = "drop"
    ) %>%
    mutate(sens = sens)
}

f1 <- traiter_flux("Desktop/MiF_py/flux_2019.parquet",  "import", 1, "I")
f3 <- traiter_flux("Desktop/MiF_py/flux3_2019.parquet", "import", 3, "I")
f2 <- traiter_flux("Desktop/MiF_py/flux2_2019.parquet", "export", 2, "E")
f4 <- traiter_flux("Desktop/MiF_py/flux4_2019.parquet", "export", 4, "E")

dat <- bind_rows(f1, f3, f2, f4)

# 4. Restriction à l'univers substituable -------------------------------------
imp <- dat %>% 
  filter(sens == "I") %>% 
  group_by(nc8, hs2, hs2_description, iso2, country) %>%
  summarise(
    imp_val   = sum(prix), 
    imp_kg    = sum(poids),
    imp_val_m = sum(prix_masse), 
    .groups   = "drop"
  )

exp <- dat %>% 
  filter(sens == "E") %>% 
  group_by(nc8) %>%
  summarise(
    exp_kg    = sum(poids), 
    exp_val_m = sum(prix_masse), 
    .groups   = "drop"
  )

nc8_both <- intersect(unique(imp$nc8), unique(exp$nc8))
imp <- imp %>% filter(nc8 %in% nc8_both)
exp <- exp %>% filter(nc8 %in% nc8_both)

total_imp_sub <- sum(imp$imp_val, na.rm = TRUE)

# 5. Calcul des Prix/kg et Ratios par couple (NC8 x Pays) ----------------------
pk_exp <- exp %>% 
  mutate(pk_export = if_else(exp_kg > 0, exp_val_m / exp_kg, NA_real_))

imp_pc <- imp %>%
  mutate(pk_import = if_else(imp_kg > 0, imp_val_m / imp_kg, NA_real_)) %>%
  left_join(pk_exp %>% select(nc8, pk_export), by = "nc8") %>%
  mutate(ratio_pk = pk_export / pk_import)

# 6. Identification du Top 10 des pays à plus fort ratio (Part >= 0,5%) --------
SEUIL_PART <- 0.005

top10_iso2 <- imp_pc %>%
  filter(is.finite(ratio_pk), imp_val > 0) %>%
  group_by(iso2, country) %>%
  mutate(w = imp_val / sum(imp_val)) %>%
  summarise(
    ratio_pk_agrege = sum(w * ratio_pk, na.rm = TRUE),
    imp_val         = sum(imp_val, na.rm = TRUE),
    .groups         = "drop"
  ) %>%
  mutate(part_pct = 100 * imp_val / total_imp_sub) %>%
  filter(part_pct >= (SEUIL_PART * 100)) %>%
  arrange(desc(ratio_pk_agrege)) %>%
  slice_head(n = 10) %>%
  pull(iso2)

# 7. Calcul du Surcoût au niveau ligne et agrégation par HS2 ------------------
imp_top10_pays <- imp_pc %>%
  filter(iso2 %in% top10_iso2, is.finite(ratio_pk), ratio_pk > 0) %>%
  # Calcul du surcoût par transaction NC8 x Pays : (Ratio * Valeur) - Valeur
  mutate(surcout_eur = (ratio_pk * imp_val) - imp_val)

total_surcout_champ <- sum(imp_top10_pays$surcout_eur, na.rm = TRUE)

top10_hs2_surcout <- imp_top10_pays %>%
  group_by(hs2, hs2_description) %>%
  summarise(
    valeur_import_eur = sum(imp_val, na.rm = TRUE),
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

# 8. Génération du code LaTeX (à copier-coller) -------------------------------
library(knitr)
library(dplyr)

# Formatage des nombres avec la virgule décimale française
df_top10_hs2_latex <- top10_hs2_surcout %>%
  mutate(
    Surcout_Mds      = sprintf("%.2f", Surcout_Mds) %>% gsub("\\.", ",", .),
    Part_Surcout_Pct = paste0(sprintf("%.2f", Part_Surcout_Pct) %>% gsub("\\.", ",", .), " %")
  )

# Génération du code LaTeX
code_latex_top10_hs2 <- kable(
  df_top10_hs2_latex,
  format = "latex",
  booktabs = TRUE,
  align = c("c", "l", "r", "r"),
  col.names = c(
    "Rang", 
    "Secteur d'Activité (HS2)", 
    "Surcoût Estimé (Mds €)", 
    "Part dans le Surcoût Total"
  ),
  caption = "Top 10 des Secteurs HS2 par Surcoût (10 pays à plus fort ratio, part $\\ge$ 0,5\\%, 2019)"
)

# Impression dans la console R
cat("\n=== COPIE LE CODE CI-DESSOUS DANS TON FICHIER .TEX ===\n\n")
cat(code_latex_top10_hs2, sep = "\n")
cat("\n=======================================================\n")
