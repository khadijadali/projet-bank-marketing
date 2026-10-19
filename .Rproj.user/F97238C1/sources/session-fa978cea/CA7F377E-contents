# ================================================================
# 02_preparation.R
# Préparation des données : normalisation, détection outliers
# Projet Bank Marketing - Analyse Multivariée
# ================================================================

# 1. CHARGEMENT ==================================================
library(tidyverse)
library(corrplot)

cat("📂 Chargement des données...\n")
bank_data <- readRDS("data/bank_data_raw.rds")

# 2. IDENTIFICATION DES VARIABLES ================================
numeric_vars <- names(bank_data)[sapply(bank_data, is.numeric)]
categorical_vars <- names(bank_data)[sapply(bank_data, is.factor)]

cat("\n🔢 Variables numériques :", length(numeric_vars))
cat("\n📋 Variables catégorielles :", length(categorical_vars))

# 3. VALEURS MANQUANTES ==========================================
missing_count <- colSums(is.na(bank_data))
cat("\n\n❓ Valeurs manquantes par variable :\n")
print(missing_count[missing_count > 0])

if(sum(missing_count) == 0) {
  cat("✅ Aucune valeur manquante détectée\n")
}

# 4. ANALYSE DES "UNKNOWN" =======================================
unknown_counts <- bank_data %>%
  select(where(is.factor)) %>%
  map(~ sum(. == "unknown" | . == "nonexistent")) %>%
  as.data.frame() %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_unknown") %>%
  filter(n_unknown > 0)

cat("\n🔍 Variables avec modalité 'unknown' :\n")
print(unknown_counts)

# 5. DÉTECTION DES OUTLIERS (IQR) ================================
detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  return(sum(x < lower | x > upper, na.rm = TRUE))
}

outliers_count <- bank_data %>%
  select(all_of(numeric_vars)) %>%
  map_dfr(~ data.frame(n_outliers = detect_outliers(.))) %>%
  rownames_to_column("variable")

cat("\n📊 Nombre d'outliers par variable :\n")
print(outliers_count)

# 6. NORMALISATION ===============================================
cat("\n🔄 Normalisation des variables numériques...\n")
bank_num <- bank_data %>%
  select(all_of(numeric_vars))

bank_num_scaled <- scale(bank_num)

cat("✅ Normalisation centrée-réduite effectuée\n")
cat("Moyennes des variables normalisées :\n")
print(round(colMeans(bank_num_scaled), 2))
cat("Écarts-types :\n")
print(round(apply(bank_num_scaled, 2, sd), 2))

# 7. MATRICE DE CORRÉLATION ======================================
cat("\n📈 Calcul de la matrice de corrélation...\n")
cor_matrix <- cor(bank_num, use = "complete.obs")

# Visualisation
png("outputs/matrice_correlation.png", width = 800, height = 800)
corrplot(cor_matrix, method = "color", type = "upper",
         addCoef.col = "black", number.cex = 0.6,
         title = "Corrélations entre variables numériques",
         mar = c(0,0,2,0))
dev.off()

cat("✅ Matrice de corrélation sauvegardée dans outputs/\n")

# 8. SAUVEGARDE ==================================================
saveRDS(bank_num_scaled, "data/bank_num_scaled.rds")
saveRDS(bank_num, "data/bank_num.rds")
saveRDS(cor_matrix, "outputs/cor_matrix.rds")

# 9. RAPPORT DE PRÉPARATION ======================================
cat(paste0("\n", rep("=", 50), "\n"))
cat("📋 RAPPORT DE PRÉPARATION DES DONNÉES\n")
cat("\n", rep("=", 50), "\n")
cat("Observations totales :", nrow(bank_data), "\n")
cat("Variables totales :", ncol(bank_data), "\n")
cat("Variables numériques :", length(numeric_vars), "\n")
cat("Variables catégorielles :", length(categorical_vars), "\n")
cat("Valeurs manquantes :", sum(missing_count), "\n")
cat("Outliers détectés :", sum(outliers_count$n_outliers), "\n")
cat("Variables avec 'unknown' :", nrow(unknown_counts), "\n")
cat("\n✅ Données préparées et sauvegardées !\n")