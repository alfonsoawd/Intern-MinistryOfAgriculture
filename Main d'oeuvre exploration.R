#------------------------------------------------------------------------------#
# Author: Alfonso AWADALLA
# Descr: Exploration main d'oeuvre RICA
# Date: 10/06/2025
#------------------------------------------------------------------------------#


# ------------------------
# Libraries
# ------------------------
library(data.table)
library(janitor)
library(dplyr)
library(ggplot2)
library(viridis)

# ------------------------
# Data
# ------------------------

labour = fread("CERISE/02-Espace-de-Production/100_Comptes_Revenus/10010_RICA/RICA_2023/Donnees_brutes/Capibara/TEMPSTRAVAILR.csv")
labour_bis = fread("CERISE/02-Espace-de-Production/100_Comptes_Revenus/10010_RICA/RICA_2023/Donnees_brutes/Capibara/TEMPSTRAVAILS.csv")
caract = fread("CERISE/02-Espace-de-Production/100_Comptes_Revenus/10010_RICA/RICA_2023/Donnees_brutes/Capibara/CARACTERISTIQUES.csv")

# ------------------------
# Pre-processing 
# ------------------------

# Forme juridique
caract[, FJURI := factor(FJURI, levels=c(1,2,3,4,5,9), 
                         labels=c("Exploitation individuelle",
                                  "GAEC",
                                  "EARL",
                                  "Autre société civile (SCEA, GFA)",
                                  "SA, SARL, EURL, SNC",
                                  "Autres formes juridiques"))]

# Status
labour[, STATU := factor(STATU, levels=c(1,2,3), 
                         labels=c("Exploitant individuel",
                                  "Exploitant associé",
                                  "Aide non-salarié"))]

# Chef (non-salariés)
labour[, CHEFP := factor(CHEFP, levels=c(0,1), 
                         labels=c("", "Chef d'exploitation"))]

# Merge status + chef
labour[, full_status := paste(STATU, CHEFP, sep=" ")]

# Exploitations avec Chef salarié
labour_bis = unlist(labour_bis[!is.na(ANA09), .(Identifiant_dossier)])


# Removing NAs
labour = labour[!full_status %in% c("NA ", "NA NA"), ]
caract = caract[!is.na(FJURI), ]

# Removing exploitant individuel that is no chef d'exploitation (around 20)
# labour = labour[!full_status == "Exploitant individuel ", ]

# Select vars of interest
labour = labour[, .(Identifiant_dossier, full_status)]
caract = caract[, .(Identifiant_dossier, FJURI)]


# ------------------------
# Merging + Analysis
# ------------------------

# Merge forme juridique with labour info
df_final = merge(labour, caract, by = "Identifiant_dossier", all.x = TRUE)

# Collapse the full composition of each exploitation
#df_final[, all_status := paste(full_status[order(full_status)], collapse = " + "), by = Identifiant_dossier]
df_final[, all_status := paste(unique(full_status[order(full_status, decreasing = TRUE)]), collapse = " + "), by = Identifiant_dossier]

# Add when there is a chef d'exploitation salarié
df_final[Identifiant_dossier %in% labour_bis, all_status := paste(all_status, "Chef exploit. salarié", sep="+ ")]

# Deduplicate per exploitation
df_final = df_final[!duplicated(Identifiant_dossier), .(FJURI, all_status)]

# One NA in FJURI to remove
df_final = df_final[!is.na(FJURI), ]

# Frequency per forme juridique
# Compute frequency of each `all_status` per `FJURI`
freq_table = df_final[, .(freq = .N), by = .(FJURI, all_status)]

# Compute percentage share per `FJURI`
freq_table[, percent_share := round((freq / sum(freq)) * 100, 2), by = FJURI]
View(freq_table)

# ------------------------
# Visualisation
# ------------------------

# 1) Build per‐facet totals + global shares + label
total_table = freq_table[, .(total_freq = sum(freq)), by = FJURI]
total_table[, total_share := round(total_freq / sum(total_freq) * 100, 1)]
total_table[, label := paste0(total_freq, " (", total_share, "%)")]

# 2) Define palette
my_palette <- c(
  "#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e",
  "#e6ab02", "#a6761d", "#666666", "#1f78b4", "#b2df8a"
)

# 3) Plot
ggplot(freq_table, aes(x = "", y = freq, fill = all_status)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ FJURI, scales = "free") +
  scale_fill_manual(values = my_palette) +
  # one bold label per facet, nudged below the centre of the pie
  geom_text(
    data      = total_table,
    aes(x = 1, y = 0, label = label),
    inherit.aes = FALSE,
    size        = 3.5,          # smaller text
    fontface    = "bold",     # bold
    vjust       = 6         # push below the circle centre
  ) +
  labs(
    title = "Pie Chart RICA Labour Composition 2023",
    fill  = "Status"
  ) +
  theme_minimal() +
  theme(
    axis.text.x  = element_blank(),
    axis.ticks   = element_blank(),
    panel.grid   = element_blank()
  )