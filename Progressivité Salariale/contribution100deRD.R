# DÉCOMPOSITION MARGINALE DU COÛT EMPLOYEUR POUR +100€ DE RD ####
#
# Pour chaque niveau de salaire (situation initiale), on cherche le salaire cible
# qui donne +100€ de RD, puis on calcule la variation de chaque composante
# entre situation initiale et situation cible.
#
# Décomposition du coût super brut additionnel :
#   Δsuper_brut = 100 (transfert logique)
#               + Δcotisations_patronales_taux_fixe  (retraite_pat + autres composantes hors exo)
#               + Δperte_allegements                 (les exo baissent → coût augmente)
#               + Δcotisations_salariales             (prélevées sur le brut)
#               + Δimpot_revenu                       (IR augmente)
#               - Δprime_activite                     (prime baisse → coût relatif augmente)

decompo_marginale <- final_FR %>%
  arrange(PTS_SMIC) %>%
  mutate(
    exonerations_pat = (assurance_maladie_pat + famille_pat + assurance_chomage_pat +
                          formation_pat + autres_pat + retraite_pat) - total_cotisations_pat,
    cot_pat_taux_fixe = assurance_maladie_pat + retraite_pat + famille_pat + assurance_chomage_pat +
      formation_pat + autres_pat,  # cotisations pat sans allègements
    RD_cible = salaire_net_impot_prime_activite + 100
  ) %>%
  mutate(
    # Interpolation de chaque variable au niveau du salaire cible
    brut_cible          = approx(salaire_net_impot_prime_activite, salaire_brut,          RD_cible, rule=2)$y,
    super_brut_cible    = approx(salaire_net_impot_prime_activite, salaire_super_brut,    RD_cible, rule=2)$y,
    cot_pat_fixe_cible  = approx(salaire_net_impot_prime_activite, cot_pat_taux_fixe,     RD_cible, rule=2)$y,
    exo_cible           = approx(salaire_net_impot_prime_activite, exonerations_pat,      RD_cible, rule=2)$y,
    cot_sal_cible       = approx(salaire_net_impot_prime_activite, total_cotisations_sal, RD_cible, rule=2)$y,
    ir_cible            = approx(salaire_net_impot_prime_activite, impot_revenu_total,    RD_cible, rule=2)$y,
    prime_cible         = approx(salaire_net_impot_prime_activite, prime_activite,        RD_cible, rule=2)$y
  ) %>%
  mutate(
    # Variations entre situation cible et situation initiale
    delta_super_brut    = super_brut_cible - salaire_super_brut,      # = coût total pour l'employeur
    
    delta_cot_pat_fixe  = cot_pat_fixe_cible  - cot_pat_taux_fixe,   # hausse des cot. pat. à taux fixe
    delta_perte_exo     = -(exo_cible - exonerations_pat),            # baisse des exo → coût en + (positif)
    delta_cot_sal       = cot_sal_cible - total_cotisations_sal,      # hausse des cot. salariales
    delta_ir            = ir_cible - impot_revenu_total,              # hausse de l'IR
    delta_perte_prime   = -(prime_cible - prime_activite),            # baisse de la prime → coût en + (positif)
    
    # Vérification : transfert logique = 100€ + ce que la prime "coûte" côté RD
    # delta_super_brut = 100 + delta_cot_pat_fixe + delta_perte_exo + delta_cot_sal + delta_ir + delta_perte_prime
    check = round(100 + delta_cot_pat_fixe + delta_perte_exo + delta_cot_sal + delta_ir + delta_perte_prime - delta_super_brut, 4)
  )

# Vérification
decompo_marginale %>%
  filter(PTS_SMIC %in% c(1.00, 1.20, 1.50, 2.00, 2.50, 3.00)) %>%
  select(PTS_SMIC, salaire_brut, delta_super_brut,
         delta_cot_pat_fixe, delta_perte_exo, delta_cot_sal, delta_ir, delta_perte_prime, check) %>%
  print()

# GRAPHIQUE ####
library(tidyr)

plot_data <- decompo_marginale %>%
  filter(PTS_SMIC >= 1 & PTS_SMIC <= 4) %>%
  mutate(transfert_logique = 100) %>%
  select(PTS_SMIC, transfert_logique, delta_cot_pat_fixe, delta_perte_exo,
         delta_cot_sal, delta_ir, delta_perte_prime) %>%
  pivot_longer(-PTS_SMIC, names_to = "composante", values_to = "valeur") %>%
  mutate(composante = factor(composante,
                             levels = c( "delta_perte_exo","delta_perte_prime", "delta_ir",
                                        "delta_cot_sal", "delta_cot_pat_fixe", "transfert_logique"),
                             labels = c(
                                        "Perte d'allègements ","Perte de prime d'activité",
                                        "Δ Impôt sur le revenu",
                                        "Δ Cotisations patronales (taux fixe)",
                                        "Δ Cotisations salariales",
                                        "Transfert logique (100€)")
  ))

couleurs <- c(
  "Transfert logique (100€)"             = "#ffffff",  # bleu foncé Rexecode
  "Δ Cotisations patronales (taux fixe)" = "#0071B9",  # bleu moyen
  "Δ Cotisations salariales"             = "#969696",  # bleu clair
  "Δ Impôt sur le revenu"                = "#D5B076",  # jaune/orange
  "Perte d'allègements "           = "#00A486",  # orange foncé
  "Perte de prime d'activité"            = "#E87511"   # rouge
)

ggplot(plot_data, aes(x = PTS_SMIC, y = valeur, fill = composante)) +
  geom_area(position = "stack", alpha = 0.88) +
  geom_line(
    data = decompo_marginale %>% filter(PTS_SMIC >= 1 & PTS_SMIC <= 4),
    aes(x = PTS_SMIC, y = delta_super_brut),
    inherit.aes = FALSE, color = "black", linewidth = 1.1
  ) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "grey40", linewidth = 0.7) +
  annotate("text", x = 3.5, y = 103, label = "100€ (transfert parfait)",
           color = "grey40", size = 3.2) +
  scale_fill_manual(values = couleurs, name = NULL) +
  scale_x_continuous(breaks = seq(1, 4, by = 0.5),
                     labels = function(x) paste0(x, " SMIC")) +
  scale_y_continuous(labels = function(x) paste0(round(x), "€")) +
  labs(
    title    = "Décomposition du coût employeur pour +100€ de revenu disponible",
    subtitle = "Chaque bande = variation effective de la composante entre situation initiale et cible",
    x        = "Salaire (en points de SMIC)",
    y        = "Coût super brut additionnel (€) pour +100€ de RD",
    caption  = "RD = salaire net après IR + prime d'activité\nLa ligne noire = coût total (somme des bandes)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        legend.key.size  = unit(0.5, "cm"))


openxlsx::write.xlsx(
  decompo_marginale %>%
    filter(PTS_SMIC >= 1 & PTS_SMIC <= 4) %>%
    select(
      PTS_SMIC, salaire_brut, salaire_super_brut,
      delta_super_brut,
      delta_cot_pat_fixe, delta_perte_exo,
      delta_cot_sal, delta_ir, delta_perte_prime,
      check
    ),  # même select que ci-dessus
  "coutemp010126bis.xlsx"
)
