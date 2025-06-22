library(tidyverse)
library(cluster)
library(factoextra)
library(corrplot)

data <- read.csv("~/Documents/IPB/Semester 4/Metode Kuantitatif/Final Project/customer_segmentation.csv")

# ====== Pra-Pemrosesan Data =====
# Hapus kolom tidak relevan
data <- data %>%
  select(Income, Recency, MntWines, MntFruits, MntMeatProducts, MntFishProducts,
         MntSweetProducts, MntGoldProds)

# Data Total Spending
data <- data %>%
  mutate(TotalSpend = MntWines + MntFruits + MntMeatProducts +
           MntFishProducts + MntSweetProducts + MntGoldProds)

# Penanganan Missing Value
data <- data %>%
  filter(!is.na(Income))

# Hapus Outlier Income
q99_income <- quantile(data$Income, 0.99)
data <- data %>%
  filter(Income <= q99_income)

# Hapus Spending < 20
data <- data %>%
  filter(TotalSpend >= 20)

# Standardisasi Z-Score
df <- data %>%
  select(Income, TotalSpend, Recency)

df_scaled <- scale(df)

# ====== Analisis Data =====
# Jumlah Cluster Optimal (Elbow Method)
fviz_nbclust(df_scaled, kmeans, method = "wss")

# Clustering K-Means (K=4)
set.seed(123)
kmeans_model <- kmeans(df_scaled, centers = 4, nstart = 25)

# Tambahkan hasil cluster ke data
data$Cluster4 <- kmeans_model$cluster  

# Analisis Karakter Tiap Cluster
cluster_summary <- data %>%
  group_by(Cluster4) %>%
  summarise(Avg_Income = mean(Income),
            Avg_TotalSpend = mean(TotalSpend),
            Avg_Recency = mean(Recency))

print(cluster_summary)

# Mapping Cluster ke Label Spender & Aktif/Dorman
data <- data %>%
  mutate(SpenderType = case_when(
    Cluster4 == "2" ~ "High Spender - Active",
    Cluster4 == "4" ~ "High Spender - Dormant",
    Cluster4 == "3" ~ "Low Spender - Active",
    Cluster4 == "1" ~ "Middle Spender - Dormant"
  ))

# Visualisasi Jumlah Pelanggan per Cluster
data %>%
  count(SpenderType) %>%
  ggplot(aes(x = SpenderType, y = n, fill = SpenderType)) +
  geom_col() +
  labs(title = "", x = "Cluster", y = "Jumlah")

# Visualisasi Scatter Plot (Income vs Spending)
ggplot(data, aes(x = Income, y = TotalSpend, color = SpenderType)) +
  geom_point(alpha = 0.7) +
  labs(title = "", x = "Income", y = "Total Spending")

# Hitung Silhouette Score
sil <- silhouette(kmeans_model$cluster, dist(df_scaled))

# Visualisasi Silhouette Plot
fviz_silhouette(sil)
mean(sil[, "sil_width"])