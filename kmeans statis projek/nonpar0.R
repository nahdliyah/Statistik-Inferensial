#####  How to analysis data using Wilcoxon signed Rank test
# Data in two numeric vectors
# ++++++++++++++++++++++++++
# the number of the cigarettes before pregnancy
before <-c(8,13,24,15,7,11,20,22,6,15,20)
# the number of the cigarettes after pregnancy
after <-c(5,15,11,19,0,12,15,0,0,6,20)
# Create a data frame
my_data <- data.frame( 
  group = rep(c("before", "after"), each = 11),
  rokok = c(before,  after)
)


# Compute t-test
res <- wilcox.test(rokok ~ group, data = my_data, paired = TRUE)
res


################################
#### data wilcoxoxn rank sum test

#### contoh lain
low<-c(3,0,4,0,1,2,3)
normal<-c(4,5,6,11,7,8,10,9)
group<-c("low","low","low","low","low",
         "low","low","normal","normal","normal",
         "normal","normal","normal","normal","normal")
data_new <- data.frame( group, berat_badan = c(low,normal))

wilcox.test( berat_badan~group, data=data_new ) 


