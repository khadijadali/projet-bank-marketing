# ================================================================
# 03_acp.R
# Analyse en Composantes Principales (ACP)
# Projet Bank Marketing - Analyse Multivariée
# ================================================================

# 1. CHARGEMENT ==================================================
library(FactoMineR)
library(factoextra)
library(tidyverse)

bank_num_scaled <- readRDS("data/bank_num_scaled.rds")
bank_data <- readRDS("data/bank_data_raw.rds")
target_y <- bank_data$y

cat("📊 Réalisation de l'ACP...\n")

# 2. ACP ========================================================
res_pca <- PCA(bank_num_scaled, 
               scale.unit = FALSE,  # Déjà normalisé
               graph = FALSE)

# 3. RÉSULTATS ==================================================
cat("\n📈 Valeurs propres et variance expliquée :\n")
eigenvalues <- get_eigenvalue(res_pca)
print(eigenvalues)

# Graphique des valeurs propres
cat("\n💾 Sauvegarde du graphique de variance...\n")
png("outputs/acp_variance.png", width = 800, height = 500)
p1 <- fviz_eig(res_pca, addlabels = TRUE, 
               title = "Variance expliquée par les composantes principales",
               barfill = "steelblue", barcolor = "steelblue")
print(p1)
dev.off()
cat("✅ acp_variance.png sauvegardé\n")

# 4. CONTRIBUTIONS DES VARIABLES ================================

# Cercle des corrélations
cat("\n💾 Sauvegarde du cercle des corrélations...\n")
png("outputs/acp_cercle_correlations.png", width = 800, height = 700)
p2 <- fviz_pca_var(res_pca, 
                   col.var = "contrib",
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                   repel = TRUE,
                   title = "Cercle des corrélations - ACP")
print(p2)
dev.off()
cat("✅ acp_cercle_correlations.png sauvegardé\n")

# Contributions à l'axe 1
cat("\n💾 Sauvegarde des contributions Dim.1...\n")
png("outputs/acp_contrib_dim1.png", width = 800, height = 500)
p3 <- fviz_contrib(res_pca, choice = "var", axes = 1, 
                   title = "Contributions à la Dimension 1",
                   top = 10)
print(p3)
dev.off()
cat("✅ acp_contrib_dim1.png sauvegardé\n")

# Contributions à l'axe 2
cat("\n💾 Sauvegarde des contributions Dim.2...\n")
png("outputs/acp_contrib_dim2.png", width = 800, height = 500)
p4 <- fviz_contrib(res_pca, choice = "var", axes = 2,
                   title = "Contributions à la Dimension 2",
                   top = 10)
print(p4)
dev.off()
cat("✅ acp_contrib_dim2.png sauvegardé\n")

# 5. PROJECTION DES INDIVIDUS ===================================
cat("\n💾 Sauvegarde de la projection des individus...\n")
png("outputs/acp_individus_y.png", width = 800, height = 600)
p5 <- fviz_pca_ind(res_pca,
                   col.ind = as.factor(target_y),
                   palette = c("red", "green"),
                   addEllipses = TRUE,
                   ellipse.level = 0.95,
                   repel = TRUE,
                   title = "ACP - Individus par souscription")
print(p5)
dev.off()
cat("✅ acp_individus_y.png sauvegardé\n")

# 6. SAUVEGARDE =================================================
saveRDS(res_pca, "outputs/res_pca.rds")
cat("✅ Résultats ACP sauvegardés dans outputs/res_pca.rds\n")

# 7. SYNTHÈSE ==================================================
cat("\n==================================================\n")
cat("📋 SYNTHÈSE ACP\n")
cat("==================================================\n")
cat("Dimensions retenues (λ > 1) :", 
    sum(eigenvalues$eigenvalue > 1), "\n")
cat("Variance expliquée (2 premières dim) :",
    round(sum(eigenvalues$variance.percent[1:2]), 2), "%\n")
cat("\nTop 5 variables - Dimension 1 :\n")
contrib_dim1 <- res_pca$var$contrib %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  arrange(desc(Dim.1)) %>%
  head(5)
print(contrib_dim1)

cat("\nTop 5 variables - Dimension 2 :\n")
contrib_dim2 <- res_pca$var$contrib %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  arrange(desc(Dim.2)) %>%
  head(5)
print(contrib_dim2)

cat("\n🎉 ACP terminée ! Tous les graphiques sont dans outputs/\n")
