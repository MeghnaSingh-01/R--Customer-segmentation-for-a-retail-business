library(dplyr)
library(ggplot2)
library(cluster)
library(factoextra)
library(readxl)

data <- data.frame(
  CustomerID = sample(1:100, 500, replace = TRUE),
  InvoiceNo = sample(1000:2000, 500, replace = TRUE),
  PurchaseAmount = runif(500, 10, 500),
  PurchaseDate = sample(seq(as.Date('2023-01-01'), as.Date('2023-12-01'), by = "day"), 500, replace = TRUE)
)

data <- data %>% filter(!is.na(CustomerID))

rfm <- data %>%
  group_by(CustomerID) %>%
  summarise(
    Recency = as.numeric(Sys.Date() - max(PurchaseDate)),
    Frequency = n_distinct(InvoiceNo),
    Monetary = sum(PurchaseAmount)
  )

rfm_normalized <- scale(rfm[, -1])

fviz_nbclust(rfm_normalized, kmeans, method = "wss")

set.seed(123)
kmeans_model <- kmeans(rfm_normalized, centers = 3, nstart = 25)
rfm$Cluster <- as.factor(kmeans_model$cluster)

fviz_cluster(kmeans_model, data = rfm_normalized, geom = "point", ellipse.type = "norm")

summary <- rfm %>% group_by(Cluster) %>% summarise(
  AvgRecency = mean(Recency),
  AvgFrequency = mean(Frequency),
  AvgMonetary = mean(Monetary),
  Count = n()
)
print(summary)
