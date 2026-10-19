# ================================================================
# 05_analyse_combinée.R
# Lien ACP + Classification
# Projet Bank Marketing - Analyse Multivariée
# ================================================================

library(FactoMineR)
library(factoextra)
library(tidyverse)
library(gridExtra)

# 1. CHARGEMENT ==================================================
bank_data <- readRDS("data/bank_data_with_clusters.rds")
res_pca <- readRDS("outputs/res_pca.rds")
res_kmeans <- readRDS("outputs/res_kmeans.rds")

cat("🔗 Analyse combinée ACP + Classification...\n")

# 2. PROJECTION DES CLUSTERS SUR L'ACP ==========================
p1 <- fviz_pca_ind(res_pca,
                   col.ind = bank_data$cluster,
                   palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
                   addEllipses = TRUE,
                   ellipse.level = 0.95,
                   title = "Clusters projetés sur l'ACP",
                   repel = TRUE)
print(p1)
ggsave("outputs/combi_clusters_acp.png", p1, width = 8, height = 6)

# 3. PROFILS DES CLUSTERS (VISUALISATION) =======================

# Variables numériques
profile_num <- bank_data %>%
  group_by(cluster) %>%
  summarise(across(c(age, duration, campaign, pdays, euribor3m), 
                   mean, .names = "mean_{.col}"))

print(profile_num)

# Heatmap des profils
profile_long <- profile_num %>%
  pivot_longer(cols = starts_with("mean_"),
               names_to = "variable",
               values_to = "value") %>%
  mutate(variable = str_remove(variable, "mean_"))

p2 <- ggplot(profile_long, aes(x = cluster, y = variable, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", mid = "white", high = "green") +
  labs(title = "Heatmap des profils par cluster",
       x = "Cluster", y = "Variable") +
  theme_minimal()
print(p2)
ggsave("outputs/combi_heatmap.png", p2, width = 8, height = 5)

# 4. TAUX DE SOUSCRIPTION PAR CLUSTER ===========================
cluster_target <- bank_data %>%
  group_by(cluster, y) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(cluster) %>%
  mutate(pct = n / sum(n) * 100) %>%
  filter(y == "yes")

p3 <- ggplot(cluster_target, aes(x = cluster, y = pct, fill = cluster)) +
  geom_col() +
  geom_text(aes(label = paste0(round(pct, 1), "%")), vjust = -0.5) +
  labs(title = "Taux de souscription par cluster",
       x = "Cluster", y = "Taux de souscription (%)") +
  theme_minimal() +
  theme(legend.position = "none")
print(p3)
ggsave("outputs/combi_taux_souscription.png", p3, width = 6, height = 5)

# 5. SYNTHÈSE FINALE ============================================
cat("\n" + rep("=", 60) + "\n")
cat("📋 SYNTHÈSE FINALE - PROFILS DES CLUSTERS\n")
cat(rep("=", 60) + "\n\n")

for(i in 1:4) {
  cat("🏷️  CLUSTER", i, "\n")
  cat(rep("-", 30), "\n")
  
  data_cluster <- bank_data %>% filter(cluster == i)
  
  cat("  • Taille :", nrow(data_cluster), 
      "clients (", round(nrow(data_cluster)/nrow(bank_data)*100, 1), "%)\n")
  cat("  • Âge moyen :", round(mean(data_cluster$age), 1), "ans\n")
  cat("  • Durée d'appel :", round(mean(data_cluster$duration), 1), "secondes\n")
  cat("  • Taux de souscription :", 
      round(mean(data_cluster$y == "yes") * 100, 1), "%\n")
  
  # Métier principal
  top_job <- names(sort(table(data_cluster$job), decreasing = TRUE))[1]
  cat("  • Métier principal :", top_job, "\n")
  
  # Éducation principale
  top_edu <- names(sort(table(data_cluster$education), decreasing = TRUE))[1]
  cat("  • Éducation principale :", top_edu, "\n")
  
  cat("\n")
}

cat("\n🎉 Projet terminé ! Tous les résultats sont dans 'outputs/'")
