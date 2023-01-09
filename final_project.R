## Name: Samantha Daoud
## ID: samda_859

# (0) Setup----
Sys.setenv(LANG="en")
library(FactoMineR)
library(explor)
library(magrittr)
library(dplyr)
library(factoextra)
library(kableExtra)
library(cluster)
library(NbClust)


data <- read.csv("/Users/samo/Documents/CSS program/DSSR/week 2/project2/data/human freedom index/hfi2008_2016.csv")

# (1) preparing data----

#Lets explore this dataset first
glimpse(data)

# filtering year 2016 using function
select_data <- function(data, num) {
  data %>% filter(year >= num)}

newdata <- select_data(data, 2016)

# transform the rownames and replace NAs with 0
rownames(newdata) <- make.names(newdata[,3], unique = TRUE) # countries column
newdata <- newdata[,-c(1:4)]
newdata[is.na(newdata)] <- 0
glimpse(newdata)

# filtering economic freedom index
newdata <- newdata[,c(60:114)]

# (2) perform PCA----

res<-PCA(newdata , scale.unit=T, graph = TRUE)
print(res) # results for PCA


plot(res$ind$coord[,1:2], xlab="Axis 1\n58.39%", ylab="12.04%", pch=20)

##Explore your results (using str)
str(res)

# to measure the amount of variation retained by each principal component, we examine eigenvalues..
##Eigenvalues
eig.val <- get_eigenvalue(res)
eig.val # The sum of all the eigenvalues give a total variance of 55.
# elbow method
plot(res$eig[,2], type="o")
# Scree plot
fviz_eig(res, addlabels = TRUE, ylim = c(0, 50))

# to extract results
var <- get_pca_var(res)

# Coordinates
head(var$coord)
# Cos2: quality on the factor map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)
# Coordinates of variables
head(var$coord, 5)
fviz_pca_var(res, col.var = "black")

head(var$cos2, 5)
library("corrplot")
corrplot(var$cos2, is.corr=FALSE)

# Color by cos2 values: quality on the factor map
fviz_pca_var(res, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)
#The most important (or, contributing) variables can be highlighted on the correlation plot as follow:
fviz_pca_var(res, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

#Let's explore this visually (explor())
explor(res)

# (3) K-mean clustering----
df <-  newdata
df <-  scale(df)
head(df)

k4 <- kmeans(df, centers = 4, nstart = 25)
str(k4)
k4

fviz_cluster(k4,
             repel = TRUE,
             show.clust.cent = TRUE,
             main = "Factor map",
             data = df)+
  ggtitle("k = 4")

# elbow method
set.seed(123)

# function to compute total within-cluster sum of square
clr <- function(k) {
  kmeans(df, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15
library(purrr)
# extract wss for 2-15 clusters
clr_values <- map_dbl(k.values, clr)

plot(k.values, clr_values,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

set.seed(123)

fviz_nbclust(df, kmeans, method = "wss")

