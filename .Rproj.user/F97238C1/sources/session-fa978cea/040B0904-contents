# ================================================================
# 04_classification.R
# K-means et Classification Hiérarchique
# Projet Bank Marketing - Analyse Multivariée
# ================================================================

library(FactoMineR)
library(factoextra)
library(cluster)
library(tidyverse)

# 1. CHARGEMENT ==================================================
bank_num_scaled <- readRDS("data/bank_num_scaled.rds")
bank_data <- readRDS("data/bank_data_raw.rds")
target_y <- bank_data$y

cat("📊 Classification des clients...\n")

# 2. DÉTERMINATION DU NOMBRE OPTIMAL DE CLUSTERS ================

# Méthode de l'elbow
p1 <- fviz_nbclust(bank_num_scaled, kmeans, method = "wss",
                   k.max = 10, linecolor = "red",
                   title = "Méthode de l'elbow (K-means)")
print(p1)
ggsave("outputs/kmeans_elbow.png", p1, width = 8, height = 5)

# Méthode de la silhouette
p2 <- fviz_nbclust(bank_num_scaled, kmeans, method = "silhouette",
                   k.max = 10, linecolor = "red",
                   title = "Méthode de la silhouette (K-means)")
print(p2)
ggsave("outputs/kmeans_silhouette.png", p2, width = 8, height = 5)

# 3. EXÉCUTION DU K-MEANS =======================================
cat("\n🔄 Exécution de K-means...\n")
set.seed(123)
k_optimal <- 4  # Selon les graphiques
res_kmeans <- kmeans(bank_num_scaled, centers = k_optimal, 
                     nstart = 25, iter.max = 100)

# 4. VISUALISATION ==============================================
p3 <- fviz_cluster(res_kmeans, 
                   data = bank_num_scaled,
                   palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
                   ellipse.type = "norm",
                   geom = "point",
                   title = "Clusters K-means")
print(p3)
ggsave("outputs/kmeans_clusters.png", p3, width = 8, height = 6)

# 5. PROFILS DES CLUSTERS =======================================
bank_data_clusters <- bank_data %>%
  mutate(cluster = as.factor(res_kmeans$cluster))

# Profil des variables numériques
cluster_summary <- bank_data_clusters %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    age_moyen = round(mean(age), 1),
    duration_moy = round(mean(duration), 1),
    campaign_moy = round(mean(campaign), 1),
    pdays_moy = round(mean(pdays), 1),
    euribor_moy = round(mean(euribor3m), 3),
    taux_souscription = round(mean(y == "yes") * 100, 1)
  )

cat("\n📊 Profils des clusters :\n")
print(cluster_summary)

# 6. VALIDATION =================================================
# Comparaison avec la variable cible
confusion <- table(bank_data_clusters$cluster, target_y)
cat("\n🎯 Distribution de la souscription par cluster :\n")
print(prop.table(confusion, margin = 1) * 100)

# 7. CLASSIFICATION HIÉRARCHIQUE ================================
cat("\n🔄 Classification Hiérarchique...\n")
dist_matrix <- dist(bank_num_scaled, method = "euclidean")
res_hc <- hclust(dist_matrix, method = "ward.D2")

p4 <- fviz_dend(res_hc, 
                k = 4,
                cex = 0.5,
                k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
                rect = TRUE,
                rect_fill = TRUE,
                title = "Dendrogramme - CAH")
print(p4)
ggsave("outputs/cah_dendrogramme.png", p4, width = 10, height = 6)

# 8. SAUVEGARDE =================================================
saveRDS(res_kmeans, "outputs/res_kmeans.rds")
saveRDS(bank_data_clusters, "data/bank_data_with_clusters.rds")
saveRDS(res_hc, "outputs/res_hc.rds")

cat("\n✅ Classification terminée !\n")
cat("\n📋 RÉSUMÉ :\n")
cat("Nombre de clusters :", k_optimal, "\n")
cat("Taille des clusters :\n")
print(table(res_kmeans$cluster))
