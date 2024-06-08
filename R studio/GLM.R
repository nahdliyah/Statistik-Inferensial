library(readxl)
# Read the Excel file
data <- read_excel("/Users/ASUS/Downloads/CVD_cleaned.xlsx")
head(data) ## in order to see the first row of the data
str(data) ## to see the structure of the data
#checking missing data using is.na
is.na(data)
summary(data)

data <- read_excel("/Users/ASUS/Downloads/CVD_cleaned.xlsx")
head(data)
str(data)

Heart_Disease <- ifelse(data$Heart_Disease== "Yes", 1, 0)
Depresi<-ifelse(data$Depression== "Yes", 1, 0)
Diabetes<-ifelse(data$Diabetes== "Yes", 1, 0)
Exercise<-ifelse(data$Exercise== "Yes", 1, 0)
Smoking_History<-ifelse(data$Smoking_History== "Yes", 1, 0)
Checkup<-ifelse(data$Checkup== "Never", 0, 1)
General_Health<- ifelse(tolower(data$General_Health) %in% c("good", "very good", "excellent"), 1, 0)
Skin_Cancer<-ifelse(data$Skin_Cancer== "Yes", 1, 0)
Arthritis<-ifelse(data$Arthritis== "Yes", 1, 0)
Alcohol_Consumption<-ifelse(data$Alcohol_Consumption== 0, 0, 1)
umur<-data$Age_Category
obesitas<-data$BMI
datanew<-data.frame(Heart_Disease,umur,obesitas,General_Health,Checkup,Depresi,Diabetes,Exercise,Smoking_History,Skin_Cancer,Arthritis, Alcohol_Consumption)
head(datanew)

# to find the summary result of the new data for the ratio predictor data
summary(obesitas)
## to find tabulation tabel for hypertensi vs all independent categorical predictor variables
xtabs(~ Heart_Disease+Depresi, data=datanew)
xtabs(~ Heart_Disease+Diabetes, data=datanew)
xtabs(~ Heart_Disease+Exercise, data=datanew)
xtabs(~ Heart_Disease+Smoking_History, data=datanew)
xtabs(~ Heart_Disease+Checkup, data=datanew)
xtabs(~ Heart_Disease+General_Health, data=datanew)
xtabs(~ Heart_Disease+Skin_Cancer, data=datanew)
xtabs(~ Heart_Disease+Arthritis, data=datanew)
xtabs(~ Heart_Disease+Alcohol_Consumption, data=datanew)
xtabs(~ Heart_Disease+umur, data=datanew)


### to find the correlation for used data
# Menyaring hanya variabel numerik dari datanew
datanew_numeric <- datanew[, sapply(datanew, is.numeric)]
# Menghitung matriks korelasi untuk variabel numerik
mydata.cor <- cor(datanew_numeric)
# Memvisualisasikan matriks korelasi menggunakan corrplot
library(corrplot)
corrplot(mydata.cor, method = "color", type = "full", tl.col = "black", tl.srt = 45)
# Menampilkan nilai-nilai korelasi
print("Matriks Korelasi:")
print(mydata.cor)

#### splitting 80 persen data for training and 20 data for testing
library(readr)
library(caret)
inTrain <- createDataPartition(y = datanew$Heart_Disease, p = .80, list = FALSE)
training <- datanew[inTrain,]
testing <- datanew[-inTrain,]
##===============================================================================================================================

hmodel01 <-glm(Heart_Disease ~ umur+obesitas+General_Health+Checkup+Depresi+Diabetes+Exercise+Smoking_History+Skin_Cancer+Arthritis+Alcohol_Consumption,
                   data=training,family=binomial)
summary(hmodel01)
install.packages("lmtest")
library(lmtest)
lrtest(hmodel01) ## to have overall test
#then checking whether there is multicolineritas between predictor variables using
#stepwise methods
### modelling using logistics regression with several models using stepwise
hmodel01 <-glm(Heart_Disease ~ umur+obesitas+General_Health+Checkup+Depresi+Diabetes+Exercise+Smoking_History+Skin_Cancer+Arthritis+Alcohol_Consumption,
                   data=training,family=binomial)
hmodel02 <-glm(Heart_Disease ~ umur+obesitas+General_Health+Checkup+Depresi+Diabetes+Exercise+Smoking_History+Skin_Cancer+Arthritis,
                   data=training,family=binomial)
hmodel03 <-glm(Heart_Disease ~ umur+obesitas+General_Health+Checkup+Depresi+Diabetes+Exercise+Smoking_History+Skin_Cancer,
                   data=training,family=binomial)
hmodel04 <-glm(Heart_Disease ~ umur+obesitas+General_Health+Checkup+Depresi+Diabetes+Exercise+Smoking_History,
                   data=training,family=binomial)
hmodel05 <-glm(Heart_Disease ~ umur+obesitas+General_Health+Checkup+Depresi+Diabetes+Exercise,
                   data=training,family=binomial)
hmodel06 <-glm(Heart_Disease ~ umur+obesitas+General_Health+Checkup+Depresi+Diabetes,
               data=training,family=binomial)
hmodel07 <-glm(Heart_Disease ~ umur+obesitas+General_Health+Checkup+Depresi,
               data=training,family=binomial)
hmodel08 <-glm(Heart_Disease ~ umur+obesitas+General_Health+Checkup,
               data=training,family=binomial)
hmodel09 <-glm(Heart_Disease ~ umur+obesitas+General_Health,
               data=training,family=binomial)
hmodel10 <-glm(Heart_Disease ~ umur+obesitas,
               data=training,family=binomial)
hmodel11 <-glm(Heart_Disease ~ umur,
               data=training,family=binomial)
hmodel01$aic
hmodel02$aic
hmodel03$aic
hmodel04$aic
hmodel05$aic
hmodel06$aic
hmodel07$aic
hmodel08$aic
hmodel09$aic
hmodel10$aic
hmodel11$aic

#========================================================================================================================================================

hmodel01prob<-glm(Heart_Disease ~ umur+obesitas+General_Health+Checkup+Depresi+Diabetes+Exercise+Smoking_History+Skin_Cancer+Arthritis+Alcohol_Consumption,
               data=training,family=binomial(link="probit"))
summary(hmodel01prob)
install.packages("lmtest")
library(lmtest)
lrtest(hmodel01prob) ## to have overall test
#then checking whether there is multicolineritas between predictor variables using
#stepwise methods
### modelling using logistics regression with several models using stepwise
hmodel01prob <-glm(Heart_Disease ~ umur+obesitas+General_Health+Checkup+Depresi+Diabetes+Exercise+Smoking_History+Skin_Cancer+Arthritis+Alcohol_Consumption,
               data=training,family=binomial(link="probit"))
hmodel02prob <-glm(Heart_Disease ~ umur+obesitas+General_Health+Checkup+Depresi+Diabetes+Exercise+Smoking_History+Skin_Cancer+Arthritis,
               data=training,family=binomial(link="probit"))
hmodel03prob <-glm(Heart_Disease ~ umur+obesitas+General_Health+Checkup+Depresi+Diabetes+Exercise+Smoking_History+Skin_Cancer,
               data=training,family=binomial(link="probit"))
hmodel04prob <-glm(Heart_Disease ~ umur+obesitas+General_Health+Checkup+Depresi+Diabetes+Exercise+Smoking_History,
               data=training,family=binomial(link="probit"))
hmodel05prob <-glm(Heart_Disease ~ umur+obesitas+General_Health+Checkup+Depresi+Diabetes+Exercise,
               data=training,family=binomial(link="probit"))
hmodel06prob <-glm(Heart_Disease ~ umur+obesitas+General_Health+Checkup+Depresi+Diabetes,
               data=training,family=binomial(link="probit"))
hmodel07prob <-glm(Heart_Disease ~ umur+obesitas+General_Health+Checkup+Depresi,
               data=training,family=binomial(link="probit"))
hmodel08prob <-glm(Heart_Disease ~ umur+obesitas+General_Health+Checkup,
               data=training,family=binomial(link="probit"))
hmodel09prob <-glm(Heart_Disease ~ umur+obesitas+General_Health,
               data=training,family=binomial(link="probit"))
hmodel10prob <-glm(Heart_Disease ~ umur+obesitas,
               data=training,family=binomial(link="probit"))
hmodel11prob <-glm(Heart_Disease ~ umur,
               data=training,family=binomial(link="probit"))
hmodel01prob$aic
hmodel02prob$aic
hmodel03prob$aic
hmodel04prob$aic
hmodel05prob$aic
hmodel06prob$aic
hmodel07prob$aic
hmodel08prob$aic
hmodel09prob$aic
hmodel10prob$aic
hmodel11prob$aic

#karena yang AIC model yang paling kecil adalah model hmodel01prob$aic 
summary(hmodel01prob$aic)

## using the best model,ie the second model from logistics regression
hip.prob = predict(hmodel01, testing, type="response")
y_pred_num <- ifelse(hip.prob > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- testing$Heart_Disease
table(y_pred, y_act)
mean(y_pred == y_act)


#============================================================================

# Replace "my_model" with the actual variable name you used when fitting the model
coef_summary <- summary(hmodel01prob)

# Extract significant predictors (p-value less than 0.05)
significant_predictors <- coef_summary$coefficients[coef_summary$coefficients[, "Pr(>|z|)"] < 0.05, ]

# Extract coefficient estimates and standard errors for significant predictors
coef_estimates <- significant_predictors[, "Estimate"]
se <- significant_predictors[, "Std. Error"]

# Calculate the odds ratio and 95% confidence interval
odds_ratio <- exp(coef_estimates)
lower_ci <- exp(coef_estimates - 1.96 * se)
upper_ci <- exp(coef_estimates + 1.96 * se)

# Create a data frame with the results
odds_ratio_table <- data.frame(
  Predictor = rownames(significant_predictors),
  Odds_Ratio = odds_ratio,
  Lower_CI = lower_ci,
  Upper_CI = upper_ci
)

# Print the results
print(odds_ratio_table)

install.packages("openxlsx")


# Install dan memuat paket
if (!require(openxlsx)) install.packages("openxlsx")
library(openxlsx)

# Tulis variabel ke file Excel
write.xlsx(odds_ratio_table, "C:/Users/ASUS/Downloads/nama_file.xlsx", sheetName = "Sheet1")




