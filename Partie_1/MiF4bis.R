# ==============================================================================
# TOP 10 DES CHAPITRES SECTORIELS (HS2) IMPORTÉS DEPUIS LES 20 PAYS DE DÉLOCALISATION
# Hors UE/OCDE (Turquie incluse) - Consommation Stricte (CONS) & Substituable (2019)
# ==============================================================================

library(arrow)
library(dplyr)
library(readxl)
library(stringr)
library(gt)

# 1. Définition des listes de pays à exclure ----------------------------------
UE_ISO2 <- c(
  "AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", "FR", "GR", "HR",
  "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", 
  "SK", "GB"
)

OCDE_NON_UE_SANS_TR <- c(
  "AU", "CA", "CH", "CL", "CO", "CR", "IL", "IS", "JP", "KR", "MX", "NO", "NZ", "US"
)

PAYS_A_EXCLURE <- c(UE_ISO2, OCDE_NON_UE_SANS_TR)

# 2. Chargement structuré de la nomenclature HS2 via l'onglet HS12 --------------
cat("Chargement de la nomenclature HS2 (onglet HS12)...\n")

hs2_dict <- read_excel("Desktop/MiF_py/HSCodeandDescription.xlsx", sheet = "HS12") %>%
  # Filtrage strict sur les lignes de niveau HS2 (Level == 2)
  filter(as.character(Level) == "2") %>%
  mutate(
    hs2             = str_pad(as.character(Code), 2, pad = "0"),
    hs2_description = as.character(Description)
  ) %>%
  select(hs2, hs2_description) %>%
  distinct(hs2, .keep_all = TRUE)

# 3. Chargement de la table BEC ------------------------------------------------
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

# 4. Identification des NC8 substituables (exportés en 2019) ------------------
cat("Identification des NC8 substituables (exportés en 2019)...\n")

f2 <- read_parquet("Desktop/MiF_py/flux2_2019.parquet") %>% filter(flux == 2)
f4 <- read_parquet("Desktop/MiF_py/flux4_2019.parquet") %>% filter(flux == 4)

nc8_exportes <- bind_rows(f2, f4) %>%
  mutate(nc8 = str_pad(as.character(nc8), 8, pad = "0")) %>%
  pull(nc8) %>%
  unique()

# 5. Chargement et filtrage des importations (FLUX 1 et 3) ---------------------
cat("Chargement et filtrage des importations (FLUX 1 et 3)...\n")

f1 <- read_parquet("Desktop/MiF_py/flux_2019.parquet") %>% filter(flux == 1) %>% rename(valeur = import)
f3 <- read_parquet("Desktop/MiF_py/flux3_2019.parquet") %>% filter(flux == 3) %>% rename(valeur = import)

imp_filtre <- bind_rows(f1, f3) %>%
  mutate(
    iso2 = str_to_upper(iso2),
    nc8  = str_pad(as.character(nc8), 8, pad = "0"),
    hs6  = str_sub(nc8, 1, 6)
  ) %>%
  filter(!iso2 %in% PAYS_A_EXCLURE) %>%
  left_join(bec, by = c("hs6" = "code_prod")) %>%
  filter(BEC5EndUse == "CONS", nc8 %in% nc8_exportes)

# 6. Sélection des 20 premiers pays de délocalisation -------------------------
top20_iso2 <- imp_filtre %>%
  group_by(iso2) %>%
  summarise(valeur_eur = sum(valeur, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(valeur_eur)) %>%
  slice_head(n = 20) %>%
  pull(iso2)

# 7. Agrégation au niveau HS2 pour ces 20 pays --------------------------------
imp_top20_pays <- imp_filtre %>%
  filter(iso2 %in% top20_iso2)

total_valeur_top20_pays <- sum(imp_top20_pays$valeur, na.rm = TRUE)

top10_hs2 <- imp_top20_pays %>%
  group_by(hs2, hs2_description) %>%
  summarise(valeur_eur = sum(valeur, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    valeur_milliards_eur = valeur_eur / 1e9,
    part_pct = 100 * valeur_eur / total_valeur_top20_pays
  ) %>%
  arrange(desc(valeur_eur)) %>%
  slice_head(n = 10) %>%
  mutate(
    Rang = row_number(),
    Section_HS2 = paste0("HS2-", hs2, " : ", hs2_description)
  ) %>%
  select(Rang, Section_HS2, Valeur_Mds = valeur_milliards_eur, Part_Pct = part_pct)

# 8. Affichage du tableau 'gt' ------------------------------------------------
tableau_hs2_gt <- top10_hs2 %>%
  gt() %>%
  tab_header(
    title = md("**Top 10 des Secteurs HS2 Importés**"),
    subtitle = "Provenance : 20 principaux pays hors UE/OCDE — Consommation Stricte & Substituable (2019)"
  ) %>%
  cols_label(
    Rang = "Rang",
    Section_HS2 = "Secteur d'Activité / Chapitre Douanier (HS2)",
    Valeur_Mds = "Valeur (Mds €)",
    Part_Pct = "Part dans les 20 Pays (%)"
  ) %>%
  fmt_number(
    columns = Valeur_Mds,
    decimals = 2,
    dec_mark = ",",
    sep_mark = " "
  ) %>%
  fmt_number(
    columns = Part_Pct,
    decimals = 2,
    dec_mark = ",",
    suffix = " %"
  ) %>%
  cols_align(
    align = "center",
    columns = c(Rang, Part_Pct)
  ) %>%
  cols_align(
    align = "left",
    columns = Section_HS2
  ) %>%
  cols_align(
    align = "right",
    columns = Valeur_Mds
  ) %>%
  tab_options(
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(12),
    table.border.top.color = "#1F3A60",
    column_labels.background.color = "#1F3A60",
    column_labels.font.weight = "bold",
    table.font.names = "Arial",
    data_row.padding = px(6)
  )

print(tableau_hs2_gt)