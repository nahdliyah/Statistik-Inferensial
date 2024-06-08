# Membuat data frame
DATA <- data.frame(
  group = c("puas", "puas", "puas", "puas", "puas", "puas", "puas", "puas", "tidak puas", "tidak puas", "tidak puas", "tidak puas", "tidak puas", "tidak puas", "tidak puas"),
  rank = c(11.5, 1.5, 14.5, 5, 11.5, 3, 11.5, 14.5, 8, 5, 5, 1.5, 8, 11.5, 8)
)

# Menampilkan data frame
View(DATA)

# Melakukan uji Wilcoxon signed-rank test
res <- wilcox.test(rank ~ group, data = DATA)

# Menampilkan hasil uji
print(res)

# Membuat data frame
DATA <- data.frame(
  group = c("puas", "puas", "puas", "puas", "puas", "puas", "puas", "puas", "tidak puas", "tidak puas", "tidak puas", "tidak puas", "tidak puas", "tidak puas", "tidak puas"),
  rank = c(5, 1, 6,1, 5, 2, 3, 6, 4, 3, 3, 1, 4, 5, 4)
)

# Menampilkan data frame
View(DATA)

# Melakukan uji Wilcoxon signed-rank test
res <- wilcox.test(rank ~ group, data = DATA)

# Menampilkan hasil uji
print(res)


################################
#### data wilcoxoxn rank sum test

#### contoh lain
low<-c(5, 1, 6, 1, 5, 2, 5, 6)
normal<-c(4, 3, 3, 1, 4, 5, 4)
group<-c("low","low","low","low","low",
         "low","low","low","normal","normal",
         "normal","normal","normal","normal","normal")
data_new <- data.frame( group, berat_badan = c(low,normal))

wilcox.test( berat_badan~group, data=data_new,exact=FALSE )

# Copy-paste these lines into the R command prompt.
# Lines that begin with the # character are taken as comment lines by R.     

A <- c(5, 1, 6, 3, 5, 2, 5, 6)

B <- c(4, 3, 3, 1, 4, 5, 4)

wilcox.test(A, B, paired = FALSE, alternative = "two.sided", mu = 0.0, 
            exact = FALSE, correct = TRUE, conf.int = TRUE, conf.level = 0.95)
