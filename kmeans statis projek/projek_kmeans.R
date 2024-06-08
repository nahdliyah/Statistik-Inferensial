library(readxl)
library(openxlsx)
# Inspect data structure
# Membuat data frame
attitude <- read_excel("C:\\Users\\ASUS\\Documents\\kmeans statis projek\\pengangguran.xlsx")
# Inspect data structure
str(attitude)
# Summarise data
summary(attitude)

# Try to reduct the dimension of variables using PCA ---> do yourself
# Assume that the third and fourth variables are the result from PCA
# Subset the attitude data
dat = attitude[,c(2,3,4,5)]

# Plot subset data
plot(dat, main = "% of favourable responses to
     Learning and Privilege", pch =20, cex =2)

# Perform K-Means with 4 clusters  
set.seed(7)
km1 = kmeans(dat, 4, nstart=100)

# Plot results
plot(dat, col =(km1$cluster +1) , main="K-Means result with 4 clusters", pch=20, cex=2)

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
km2 = kmeans(dat, 3, nstart=100)
km2
# Examine the result of the clustering algorithm
# Plot results
plot(dat, col =(km2$cluster +1) , main = "K-Means result with 3 clusters", pch = 20, cex = 2)
# Menambahkan pusat klaster ke dalam plot
#points(km2$centers, col = 1:k, pch = 4, cex = 3, lwd = 2)
# Membuat vektor warna untuk legenda
legend_colors <- rainbow(k)

# Menambahkan legenda ke bagian kanan bawah
legend("bottomright", legend = 1:k, col = legend_colors, pch = 20, title = "Clusters")
# Install dan memuat paket cluster jika belum terinstall
# install.packages("cluster")
library(cluster)

# Hitung indeks Silhouette
silhouette_values <- silhouette(km2$cluster, dist(dat))
# Tampilkan ringkasan indeks Silhouette
summary(silhouette_values)

# Plot Silhouette Plot
plot(silhouette_values, col = km2$cluster, border = NA)

#data$cluster_label <- km2$cluster
#write.xlsx(data, "C:/Users/ASUS/Documents/kmeans statis projek/nganggur_labels.xlsx", rowNames = FALSE)


