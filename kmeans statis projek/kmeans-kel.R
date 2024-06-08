# Membuat dataframe dari data yang diberikan
data <- data.frame(
  X = c(86.44, 78.96, 84.03, 82.81, 80.97, 84.3),
  Y = c(50.68, 41.28, 46.87, 51.88, 48.7, 57.93)
)

# Menampilkan data
print("Data:")
print(data)

# Menentukan jumlah cluster yang diinginkan
k <- 3

# Melakukan k-means clustering
kmeans_result <- kmeans(data, centers = k)

# Menampilkan hasil clustering
print("Hasil K-Means Clustering:")
print(kmeans_result)

# Menambahkan kolom Cluster ke dataframe asli
data$Cluster <- kmeans_result$cluster

# Menampilkan dataframe dengan kolom Cluster
print("Data dengan Kolom Cluster:")
print(data)
