# ==============================================================================
# REPLIQUE "MADE IN FRANCE" - IMPORTATIONS ET CONSOMMATION FINALE (2019)
# 4 NIVEAUX IMBRIQUES (Total, Consom. Élargie, Consom. Stricte, Substituable)
# ==============================================================================
library(arrow)
library(dplyr)
library(readxl)
library(stringr)
library(ggplot2)
library(ggforce)  # install.packages("ggforce") si besoin

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

# 2. Chargement des exportations 2019 (pour identifier les NC8 substituables) -
cat("Identification des codes NC8 exportés (FLUX 2 et 4)...\n")
f2 <- read_parquet("Desktop/MiF_py/flux2_2019.parquet") %>% filter(flux == 2)
f4 <- read_parquet("Desktop/MiF_py/flux4_2019.parquet") %>% filter(flux == 4)

nc8_exportes <- bind_rows(f2, f4) %>%
  mutate(nc8 = str_pad(as.character(nc8), 8, pad = "0")) %>%
  pull(nc8) %>%
  unique()

# 3. Chargement des importations 2019 (FLUX 1 et 3) ---------------------------
cat("Chargement et traitement des importations (FLUX 1 et 3)...\n")
f1 <- read_parquet("Desktop/MiF_py/flux_2019.parquet") %>% 
  filter(flux == 1) %>% 
  rename(valeur = import)

f3 <- read_parquet("Desktop/MiF_py/flux3_2019.parquet") %>% 
  filter(flux == 3) %>% 
  rename(valeur = import)

imp <- bind_rows(f1, f3) %>%
  mutate(
    nc8 = str_pad(as.character(nc8), 8, pad = "0"),
    hs6 = str_sub(nc8, 1, 6)
  ) %>%
  left_join(bec, by = c("hs6" = "code_prod"))

# 4. Calcul des 4 niveaux d'agrégats ------------------------------------------
# Niveau 1 : Total des importations
total_imp <- sum(imp$valeur, na.rm = TRUE) / 1e9

# Niveau 2 : Consommation Élargie (contient "CONS")
imp_elargi <- imp %>%
  filter(str_detect(BEC5EndUse, "CONS")) %>%
  summarise(v = sum(valeur, na.rm = TRUE) / 1e9) %>%
  pull(v)

# Niveau 3 : Consommation Stricte ("CONS" uniquement)
imp_strict <- imp %>%
  filter(BEC5EndUse == "CONS") %>%
  summarise(v = sum(valeur, na.rm = TRUE) / 1e9) %>%
  pull(v)

# Niveau 4 : Consommation Stricte ET Substituable (NC8 présent à l'export)
imp_substituable <- imp %>%
  filter(BEC5EndUse == "CONS", nc8 %in% nc8_exportes) %>%
  summarise(v = sum(valeur, na.rm = TRUE) / 1e9) %>%
  pull(v)

# 5. Table récapitulative -----------------------------------------------------
recap_table <- tibble(
  Périmètre = c(
    "1. Total des Importations",
    "2. Consommation Élargie (CONS + mixtes)",
    "3. Consommation Stricte (CONS uniquement)",
    "4. Consommation Stricte Substituable (NC8 exporté)"
  ),
  Valeur_Mds_EUR = round(c(total_imp, imp_elargi, imp_strict, imp_substituable), 2),
  Part_Total_Pct = round(100 * c(total_imp, imp_elargi, imp_strict, imp_substituable) / total_imp, 2)
)

cat("\n================ RECAPITULATIF IMPORTATIONS 2019 ================\n")
print(recap_table)

# 6. Représentation sous forme de 4 cercles emboîtés --------------------------
circles <- tibble(
  x0 = 0,
  # Alignement par le bas pour un emboîtement concentrique naturel
  y0 = c(
    0,
    sqrt(total_imp) - sqrt(imp_elargi),
    sqrt(total_imp) - sqrt(imp_strict),
    sqrt(total_imp) - sqrt(imp_substituable)
  ),
  r = sqrt(c(total_imp, imp_elargi, imp_strict, imp_substituable)),
  Categorie = factor(
    c(
      "1. Total Importations",
      "2. Consommation Élargie",
      "3. Consommation Stricte",
      "4. Substituable (NC8 exporté)"
    ),
    levels = c(
      "1. Total Importations",
      "2. Consommation Élargie",
      "3. Consommation Stricte",
      "4. Substituable (NC8 exporté)"
    )
  )
)

colors_map <- c(
  "1. Total Importations"         = "#1F3A60",  # Bleu nuit
  "2. Consommation Élargie"       = "#3B75B4",  # Bleu de France
  "3. Consommation Stricte"       = "#F2A900",  # Ambre
  "4. Substituable (NC8 exporté)" = "#D9534F"   # Rouge corail
)

plot_circles <- ggplot() +
  geom_circle(
    data = circles,
    aes(x0 = x0, y0 = y0, r = r, fill = Categorie),
    color = "white",
    size = 1.1,
    alpha = 0.85
  ) +
  scale_fill_manual(values = colors_map) +
  coord_fixed() +
  theme_void() +
  labs(
    title    = "Emboîtement des Importations Françaises (2019)",
    subtitle = "Du total des flux aux produits de consommation stricte substituables",
    fill     = "Périmètre :"
  ) +
  theme(
    plot.title      = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle   = element_text(size = 11, hjust = 0.5, margin = margin(b = 15)),
    legend.position = "bottom",
    legend.title    = element_text(face = "bold", size = 10),
    legend.text     = element_text(size = 9)
  )

print(plot_circles)

#--------------------------------
# ==============================================================================
# CALCUL DES PRODUITS NON SUBSTITUABLES (ABSENTS DES EXPORTATIONS EN 2019)
# ==============================================================================

library(arrow)
library(dplyr)
library(readxl)
library(stringr)

# 1. Chargement de la table BEC ------------------------------------------------
bec <- read_excel("Desktop/MiF_py/conversion_BEC.xlsx") %>%
  rename(code_prod = HS6) %>%
  mutate(
    code_prod  = str_pad(as.character(code_prod), 6, pad = "0"),
    BEC5EndUse = str_to_upper(as.character(BEC5EndUse))
  ) %>%
  select(code_prod, BEC5EndUse) %>%
  distinct(code_prod, .keep_all = TRUE)

# 2. Identification de TOUS les codes NC8 exportés en 2019 ---------------------
f2 <- read_parquet("Desktop/MiF_py/flux2_2019.parquet") %>% filter(flux == 2)
f4 <- read_parquet("Desktop/MiF_py/flux4_2019.parquet") %>% filter(flux == 4)

nc8_exportes <- bind_rows(f2, f4) %>%
  mutate(nc8 = str_pad(as.character(nc8), 8, pad = "0")) %>%
  pull(nc8) %>%
  unique()

# 3. Chargement des importations 2019 -----------------------------------------
f1 <- read_parquet("Desktop/MiF_py/flux_2019.parquet") %>% filter(flux == 1) %>% rename(valeur = import)
f3 <- read_parquet("Desktop/MiF_py/flux3_2019.parquet") %>% filter(flux == 3) %>% rename(valeur = import)

imp <- bind_rows(f1, f3) %>%
  mutate(
    nc8 = str_pad(as.character(nc8), 8, pad = "0"),
    hs6 = str_sub(nc8, 1, 6)
  ) %>%
  left_join(bec, by = c("hs6" = "code_prod"))

# 4. Identification et comptage des produits NON substituables -----------------
imp_analyse <- imp %>%
  mutate(est_substituable = nc8 %in% nc8_exportes)

# --- A) Sur la Consommation Strict (BEC == "CONS") ---
stats_non_sub_strict <- imp_analyse %>%
  filter(BEC5EndUse == "CONS") %>%
  group_by(est_substituable) %>%
  summarise(
    nb_produits_nc8 = n_distinct(nc8),
    valeur_mds_eur  = sum(valeur, na.rm = TRUE) / 1e9,
    .groups = "drop"
  ) %>%
  mutate(
    part_valeur_pct = 100 * valeur_mds_eur / sum(valeur_mds_eur),
    perimetre = "Consommation Stricte (CONS)"
  )

# --- B) Sur la Consommation Élargie (contient "CONS") ---
stats_non_sub_elargie <- imp_analyse %>%
  filter(str_detect(BEC5EndUse, "CONS")) %>%
  group_by(est_substituable) %>%
  summarise(
    nb_produits_nc8 = n_distinct(nc8),
    valeur_mds_eur  = sum(valeur, na.rm = TRUE) / 1e9,
    .groups = "drop"
  ) %>%
  mutate(
    part_valeur_pct = 100 * valeur_mds_eur / sum(valeur_mds_eur),
    perimetre = "Consommation Élargie"
  )

# 5. Affichage des résultats dans la console -----------------------------------
cat("\n================ PRODUITS NON SUBSTITUABLES EN 2019 ================\n")

cat("\n--- 1. Périmètre CONSOMMATION STRICTE ---\n")
print(
  stats_non_sub_strict %>%
    mutate(Statut = if_else(est_substituable, "Substituable (exporté)", "NON Substituable (non exporté)")) %>%
    select(Statut, nb_produits_nc8, valeur_mds_eur, part_valeur_pct)
)

cat("\n--- 2. Périmètre CONSOMMATION ÉLARGI ---\n")
print(
  stats_non_sub_elargie %>%
    mutate(Statut = if_else(est_substituable, "Substituable (exporté)", "NON Substituable (non exporté)")) %>%
    select(Statut, nb_produits_nc8, valeur_mds_eur, part_valeur_pct)
)
