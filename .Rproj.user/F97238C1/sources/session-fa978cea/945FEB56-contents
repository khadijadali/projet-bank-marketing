# ================================================================
# 03_bis_acm.R
# Analyse des Correspondances Multiples (ACM)
# Projet Bank Marketing - Analyse Multivariée
# ================================================================

# 1. CHARGEMENT ==================================================
library(FactoMineR)
library(factoextra)
library(tidyverse)

cat("📊 Analyse des Correspondances Multiples (ACM)...\n")

# 2. CHARGER LES DONNÉES ========================================
bank_data <- readRDS("data/bank_data_raw.rds")

# 3. SÉLECTION DES VARIABLES CATÉGORIELLES ======================
# On sélectionne les variables catégorielles pertinentes
# On exclut 'y' (variable cible) pour une analyse non supervisée

cat_vars <- c("job", "marital", "education", "default", 
              "housing", "loan", "contact", "month", 
              "day_of_week", "poutcome")

# Créer le dataframe pour l'ACM
bank_cat <- bank_data %>%
  select(all_of(cat_vars))

cat("\n📋 Variables sélectionnées pour l'ACM :\n")
print(names(bank_cat))

cat("\n📊 Dimensions du jeu de données ACM :", dim(bank_cat), "\n")

# 4. RÉALISATION DE L'ACM ========================================
cat("\n🔄 Réalisation de l'ACM...\n")

res_mca <- MCA(bank_cat,
               graph = FALSE,
               ncp = 5)  # Nombre de dimensions à conserver

# 5. RÉSULTATS ==================================================
cat("\n📈 Valeurs propres et variance expliquée :\n")
eigenvalues_mca <- get_eigenvalue(res_mca)
print(eigenvalues_mca)

# 6. SAUVEGARDE =================================================
saveRDS(res_mca, "outputs/res_mca.rds")
cat("✅ res_mca.rds sauvegardé\n")

# 7. GRAPHIQUES =================================================
cat("\n💾 Sauvegarde des graphiques...\n")

# Variance expliquée
png("outputs/acm_variance.png", width = 800, height = 500)
fviz_eig(res_mca, addlabels = TRUE,
         barfill = "darkgreen", barcolor = "darkgreen",
         title = "Variance expliquée par l'ACM")
dev.off()
cat("✅ acm_variance.png\n")

# Graphique des variables (modalités)
png("outputs/acm_variables.png", width = 900, height = 700)
fviz_mca_var(res_mca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             title = "Projection des modalités - ACM")
dev.off()
cat("✅ acm_variables.png\n")

# Graphique des individus
png("outputs/acm_individus.png", width = 800, height = 600)
fviz_mca_ind(res_mca,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             title = "Projection des individus - ACM")
dev.off()
cat("✅ acm_individus.png\n")

# Individus colorés par souscription
png("outputs/acm_individus_y.png", width = 800, height = 600)
fviz_mca_ind(res_mca,
             col.ind = as.factor(bank_data$y),
             palette = c("red", "green"),
             addEllipses = TRUE,
             ellipse.level = 0.95,
             repel = TRUE,
             title = "ACM - Individus par souscription")
dev.off()
cat("✅ acm_individus_y.png\n")

# Biplot (variables + individus)
png("outputs/acm_biplot.png", width = 900, height = 700)
fviz_mca_biplot(res_mca,
                repel = TRUE,
                title = "Biplot - ACM")
dev.off()
cat("✅ acm_biplot.png\n")

# 8. CONTRIBUTIONS DES VARIABLES ================================
cat("\n📊 Contributions des variables aux dimensions :\n")

# Contributions à la dimension 1
contrib_dim1 <- res_mca$var$contrib %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  arrange(desc(Dim.1)) %>%
  head(10)

cat("\nTop 10 variables - Dimension 1 :\n")
print(contrib_dim1)

# Contributions à la dimension 2
contrib_dim2 <- res_mca$var$contrib %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  arrange(desc(Dim.2)) %>%
  head(10)

cat("\nTop 10 variables - Dimension 2 :\n")
print(contrib_dim2)

# 9. SYNTHÈSE ===================================================
cat("\n==================================================\n")
cat("📋 SYNTHÈSE ACM\n")
cat("==================================================\n")
cat("Variance expliquée (2 premières dim) :",
    round(sum(eigenvalues_mca$variance.percent[1:2]), 2), "%\n")
cat("Dimensions retenues (λ > 0.5) :",
    sum(eigenvalues_mca$eigenvalue > 0.5), "\n")

cat("\n🎉 ACM terminée ! Tous les graphiques sont dans outputs/\n")