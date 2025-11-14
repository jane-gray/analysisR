################ Demension Reduction & Clustering #####################

install.packages("FactoMineR")
install.packages("factoextra")
library(FactoMineR)
library(factoextra)
library(dplyr)


################### Multiple Corresponding Analysis (MCA) (categorical types) ###############
# Making all variables as categorical types
mca_df <- mca_df %>%
  mutate(across(everything(), as.factor))

core_vars_names <- c("var1", "var2", "var3", "var4", "var5")

for (col_name in core_vars_names) {
  MCA[[col_name]] <- as.factor(MCA[[col_name]])
}

# Subset by condition (e.g., PAYS ==)
mca_fr <- subset(mca_df, PAYS == "France")
mca_ko <- subset(mca_df, PAYS == "Coree")

# Keep only core variables for MCA
mca_fr_core <- mca_fr[, core_vars_names]
mca_ko_core <- mca_ko[, core_vars_names]

# Remove missing values
mca_fr_core <- na.omit(mca_fr_core)
mca_ko_core <- na.omit(mca_ko_core)

# Run MCA (France)
res_MCA_fr <- FactoMineR::MCA(
  mca_fr_core,
  graph = FALSE,
  ncp = 5   # number of dimensions (usually 2 to 5)
)


print(res_MCA_fr$eig)       # Eigenvalues and explained variance
print(res_MCA_fr$var)       # Variable category coordinates/contributions
print(res_MCA_fr$ind$coord) # Individual (respondent) coordinates


################### MCA Visualization ###################

# Making MCA biplot (relationship between factor categories)
fviz_mca_var(res_MCA_fr, 
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE,        
             ggtheme = theme_minimal(),
             title = "ACM - Variables (France)")


# Making MCA biplot (individual coordinate distribution)
fviz_mca_ind(res_MCA_fr, 
             col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             pointsize = 0.5, 
             alpha.ind = 0.5, 
             repel = TRUE, 
             label = "none", 
             ggtheme = theme_minimal(),
             title = "MCA - Individuals (France)")



# V.test (z-score) significance  
# V-test is a statistic used to test whether each category’s contribution to a specific dimension (axis) is statistically significant

v_test_df <- as.data.frame(res_MCA_fr$var$v.test)
v_test_symbols_df <- v_test_df

for (col in names(v_test_symbols_df)) {
  for (row in rownames(v_test_symbols_df)) {
    z_score <- v_test_df[row, col]
    symbol <- ""
    if (!is.na(z_score)) { # 
      abs_z <- abs(z_score)
      if (abs_z >= 3.29) {
        symbol <- "***"
      } else if (abs_z >= 2.58) {
        symbol <- "**"
      } else if (abs_z >= 1.96) {
        symbol <- "*"
      }
    }
    v_test_symbols_df[row, col] <- paste0(sprintf("%.2f", z_score), symbol) 
  }
}

print(v_test_symbols_df)


################ Dimension reduction with MCA ################

# Create group variables based on MCA
dim1_scores_fr <- res_MCA_fr$ind$coord[, 1]
mca_analyzed_rows <- rownames(res_MCA_fr$ind$coord)
MCA_fr$dim1_score <- NA
MCA_fr[mca_analyzed_rows, "dim1_score"] <- dim1_scores_fr
MCA_fr$VI_group <- ifelse(MCA_fr$dim1_score >= 0, "Risk_yes", "Risk_no") 
MCA_fr$VI_group <- as.factor(MCA_fr$VI_group)

head(MCA_fr)
table(MCA_fr$VI_group, useNA = "always")


# (optional) MCA with all active variables
res_MCA_fr_socio <- MCA(mca_data_fr_socio,
                        graph = FALSE,
                        ncp = 5)


# (optional) MCA with fixed supplementary variables (e.g., VI_group)
vi_group_col_idx <- ncol(mca_data_fr_socio) # find VI_group column index
res_MCA_fr_socio <- MCA(mca_data_fr_socio,
                        quali.sup = vi_group_col_idx, # Assign the found index to quali.sup
                        graph = FALSE,
                        ncp = 5)


# (optional) Create MCA biplot with fixed dimensions
fviz_mca_var(res_MCA_fr_relation,
             axes = c(3, 4), # fix axes with dimensions 3 and 4
             col.var = "contrib", # Color by contribution to the dimensions
             gradient.cols = c("#9370DB", "#33CC99", "#66CC33"),
             repel = TRUE,
             ggtheme = theme_minimal(),
             title = "MCA (France)")




################### Principal Component Analysis (PCA) with numeric types ###############

library(dplyr)
library(FactoMineR)
library(factoextra)

core_vars_names <- c("var1", "var2", "var3", "var4", "var5")

# Subset by condition (e.g., PAYS == )
pca_fr <- subset(pca_df, PAYS == "France")
pca_ko <- subset(pca_df, PAYS == "Coree")   

# Keep only core variables for PCA
pca_fr_core <- pca_fr[, core_vars_names]
pca_ko_core <- pca_ko[, core_vars_names]

# Remove missing values
pca_fr_core <- na.omit(pca_fr_core)
pca_ko_core <- na.omit(pca_ko_core)

# (optional) transforming as numeric
pca_fr_core <- mutate_all(pca_fr_core, as.numeric)
pca_ko_core <- mutate_all(pca_ko_core, as.numeric)

# Run PCA (France)
res_PCA_fr <- FactoMineR::PCA(
  pca_fr_core,
  scale.unit = TRUE,  # Standardize variables (mean 0, variance 1)
  ncp = 5,            # number of dimensions (generally 2 to 5)
  graph = FALSE
)

# Summary 
print(res_PCA_fr$eig)           # Eigenvalues & explained variance
print(res_PCA_fr$var$coord)     # Loadings (variable coordinates)
print(res_PCA_fr$var$contrib)   # Variable contributions (%)
print(res_PCA_fr$var$cos2)      # Variable representation quality
print(res_PCA_fr$ind$coord)     # Individual coordinates
print(res_PCA_fr$ind$cos2)      # Individual representation quality



################### PCA Visualization ###################

# PCA variable plot: relationships among variables
fviz_pca_var(
  res_PCA_fr,
  col.var = "contrib", 
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
  repel = TRUE,
  ggtheme = theme_minimal(),
  title = "PCA - Variables (France)"
)

# PCA individual plot: distribution of individuals
fviz_pca_ind(
  res_PCA_fr,
  col.ind = "cos2", 
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  pointsize = 0.5,
  alpha.ind = 0.5,
  repel = TRUE,
  label = "none",
  ggtheme = theme_minimal(),
  title = "PCA - Individuals (France)"
)



##################### Clustering ###########################
install.packages("stringr")
install.packages("clustertend")
install.packages("NbClust")
install.packages("factoextra")
install.packages("ClusterR")
install.packages("fpc")
install.packages("clusterSim")
install.packages("psych")
install.packages("FactoMineR")
install.packages("clustMixType")
install.packages("hopkins")
install.packages("cluster")
install.packages("klaR")



# Assuming mydata is your dataset
# Replace "your_data.csv" with the actual file name or path
# mydata <- read.csv("your_data.csv")

# Perform k-modes clustering
kmodes_result <- kmodes(mydata_no_na, modes=mydata_no_na, iter.max = 10 ,weighte=FALSE, fast=TRUE)

# Display the cluster assignments for each observation
cluster_assignments <- kmodes_result$cluster
print(cluster_assignments)


# lambda calculated with variance to determine the number of k
Es <- numeric(10)
for(i in 1:10){
  kpres <- kproto(mydata_no_na, k = i)
  Es[i] <- kpres$tot.withinss
}

plot(1:10, Es, type = "b", ylab = "Total Within Cluster Sum Of Squares", xlab = "Number of clusters")


# lambda calculated with standard deviation to determine the number of k
Es2 <- numeric(10)
for(i in 1:10){
  kpres <- kproto(mydata_no_na, k = i, lambdaest(mydata_no_na, num.method = 2))
  Es2[i] <- kpres$tot.withinss
}

plot(1:10, Es2, type = "b", ylab = "Total Within Cluster Sum Of Squares", xlab = "Number of clusters")



# Silhouette score to determine the number of k

Essil <- numeric(10)
for (i in 2:10) {  # Starting from 2 clusters as silhouette is not defined for 1 cluster
  kpres <- kproto(mydata_no_na, k = i)
  cluster_assignments <- kpres$cluster
  dist_matrix <- dist(as.matrix(mydata_no_na))
  Essil[i] <- silhouette(cluster_assignments, dist_matrix)
}

print(Essil)
plot(1:10, Essil, type = "b", ylab = "Silhouette", xlab = "Number of clusters")


kmeanscat2 <- eclust(mydata_no_na, "kmeans", hc_metric="euclidean", k=2)
summary(mydata_no_na)


