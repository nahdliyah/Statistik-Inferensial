data <- data.frame(
  X = c(86.44,
        78.96,
        84.03,
        82.81,
        80.97,
        84.3),
  Y = c(50.68,
        41.28,
        46.87,
        51.88,
        48.7,
        57.93)
)

# Membuat matriks X dari data sample
X <- as.matrix(data)
rownames(X) <- c("KEPULAUAN RIAU",
                 "Karimun",
                 "Bintan",
                 "Natuna",
                 "Lingga",
                 "Kepulauan Anambas")
#Menghitung jarak euclidean
distance<-dist(X)
distance

# Hierarchical clustering: single linkage
hclust.s <- hclust(distance, method = "single")
round(hclust.s$height, 3)
plot(hclust.s)

# Hierarchical clustering: complete linkage
hclust.c <- hclust(distance, method = "complete")
round(hclust.c$height, 3)
plot(hclust.c)


