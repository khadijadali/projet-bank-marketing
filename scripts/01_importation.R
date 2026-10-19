# ================================================================
# 01_importation.R
# Importation et exploration initiale des données
# Projet Bank Marketing - Analyse Multivariée
# ================================================================

# 1. CHARGEMENT DES LIBRAIRIES ===================================
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(ggplot2)
library(kableExtra)

# 2. IMPORTATION DES DONNÉES =====================================
cat("📥 Importation des données...\n")

bank_data <- read.csv(
  "data/bank-additional-full.csv",
  sep = ";",
  header = TRUE,
  stringsAsFactors = TRUE
)

# 3. APERÇU DES DONNÉES ==========================================
cat("\n📊 Dimensions du dataset :", dim(bank_data), "\n")
cat("\n🏷️  Structure des données :\n")
str(bank_data)

cat("\n📝 Premières lignes :\n")
head(bank_data, 5)

cat("\n📈 Résumé statistique :\n")
summary(bank_data)

# 4. STATISTIQUES DESCRIPTIVES ===================================
# Variables numériques
numeric_vars <- names(bank_data)[sapply(bank_data, is.numeric)]
cat("\n🔢 Variables numériques :", length(numeric_vars), "\n")
print(numeric_vars)

# Variables catégorielles
categorical_vars <- names(bank_data)[sapply(bank_data, is.factor)]
cat("\n📋 Variables catégorielles :", length(categorical_vars), "\n")
print(categorical_vars)

# 5. STATISTIQUES DE LA VARIABLE CIBLE ===========================
cat("\n🎯 Distribution de la souscription (y) :\n")
table_y <- table(bank_data$y)
print(table_y)
cat("\nTaux de souscription :", round(table_y[2]/sum(table_y)*100, 2), "%\n")

# 6. SAUVEGARDE DES DONNÉES ======================================
saveRDS(bank_data, "data/bank_data_raw.rds")
cat("\n✅ Données sauvegardées dans 'data/bank_data_raw.rds'\n")

# 7. PREMIÈRES VISUALISATIONS ====================================
# Graphique de l'âge
p1 <- ggplot(bank_data, aes(x = age)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  labs(title = "Distribution de l'âge des clients",
       x = "Âge", y = "Nombre de clients") +
  theme_minimal()

print(p1)

# Souscription par âge
p2 <- ggplot(bank_data, aes(x = age, fill = y)) +
  geom_density(alpha = 0.6) +
  labs(title = "Souscription par âge",
       x = "Âge", y = "Densité") +
  theme_minimal() +
  scale_fill_manual(values = c("red", "green"))

print(p2)

# Sauvegarde des graphiques
ggsave("outputs/distribution_age.png", p1, width = 8, height = 5)
ggsave("outputs/souscription_age.png", p2, width = 8, height = 5)

cat("\n✅ Premières visualisations sauvegardées dans 'outputs/'\n")
cat("\n🎉 Fin de l'étape 1 - Importation réussie !\n")
