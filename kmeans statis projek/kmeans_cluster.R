library(datasets)
# Inspect data structure
# Membuat data frame
attitude <- data.frame(
  X = c(86.44, 78.96, 84.03, 82.81, 80.97, 84.3),
  Y = c(50.68, 41.28, 46.87, 51.88, 48.7, 57.93)
)

# Menampilkan data frame
print(attitude)

str(attitude)
# Summarise data
summary(attitude)

# Menggunakan kolom 1 (X), 2 (Y), dan 6 (Privilege) dari data frame 'attitude'
dat <- attitude[, c(1, 2)]

# Plot data
plot(dat, main = "% of favourable responses to Learning and Privilege", pch = 20, cex = 2)

# Melakukan K-Means dengan 3 clusters
set.seed(7)
km <- kmeans(dat, 3, nstart = 100)

# Menambahkan hasil klaster ke data frame 'attitude'
attitude$cluster <- km$cluster

# Plot hasil klaster dengan warna yang berbeda
plot(dat, col = km$cluster, main = "% of favourable responses to Learning and Privilege", pch = 20, cex = 2)

# Menampilkan pusat klaster
points(km$centers[, c(2, 3, 6)], col = 1:3, pch = 8, cex = 2, lwd = 2 ) 
print(points())
#=================================================================================

# Try to reduct the dimension of variables using PCA ---> do yourself
# Assume that the third and fourth variables are the result from PCA
# Subset the attitude data
#dat = attitude[,c(1,2,3)]

# Plot subset data
#plot(dat, main = "% of favourable responses to
    # Learning and Privilege", pch =20, cex =2)

# Perform K-Means with 2 clusters
set.seed(7)
km1 = kmeans(dat, 3, nstart=100)

# Plot results
plot(dat, col =(km1$cluster +1) , main="K-Means result with 2 clusters", pch=20, cex=2)

# Check for the optimal number of clusters given the data
mydata <-dat
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)


# Perform K-Means with the optimal number of clusters identified from the Elbow method
set.seed(7)
km2 = kmeans(dat, 6, nstart=100)

# Examine the result of the clustering algorithm
# Plot results
plot(dat, col =(km2$cluster +1) , main="K-Means result with 6 clusters", pch=20, cex=2)

