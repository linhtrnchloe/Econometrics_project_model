
# Read excel file
install.packages("readxl")
library(readxl)
setwd("C:/Users/Admin/Downloads/Semester 5/Econometric Methods")
ca <- read_excel("backup2.xlsx") # suicide rate
head(ca)

# Clean the data

# Check stationary
if (!requireNamespace("tseries", quietly = TRUE)) {
  install.packages("tseries")
}
library(tseries)

adf.test(ca$Unemployment)
first_unemployment <- diff(ca$Unemployment)
adf.test(first_unemployment)

adf.test(ca$Suicide)
first_suicide <- diff(ca$Suicide)
second_suicide <- diff(first_suicide)
adf.test(second_suicide)

adf.test(ca$`Min wage`)
log_wage <- log(ca$`Min wage`)
adf.test(log_wage)

adf.test(ca$`Alcohol 2`)
first_alco <- diff(ca$`Alcohol 2`)
second_alco <- diff(first_alco)
adf.test(second_alco)

adf.test(ca$`Deaths from cancer (per 100 000 habitants)`)
first_cancer <- diff(ca$`Deaths from cancer (per 100 000 habitants)`)
second_cancer <- diff(first_cancer)
adf.test(second_cancer)

# data frame
ca_diff <- data.frame(
  second_suicide <- second_suicide,  
  first_unemployment <- first_unemployment[-((nrow(ca)-1):nrow(ca))],
  log_wage <- log_wage[-((nrow(ca)-1):nrow(ca))],
  second_alco <- second_alco,
  second_cancer <- second_cancer,
  education <- ca$`Education expenditure`[-((nrow(ca)-1):nrow(ca))]
)

# Check the correlation
cor_matrix <- cor(ca_diff)
print(cor_matrix)
heatmap(cor_matrix, main = "Correlation Heatmap", col = heat.colors(10))
# No correlation problem since there is no abs(r) that pass 0.7

# Descriptive Statistics
install.packages("psych")
library(psych)
decscribe(ca_diff)

# ols model
ols_model_suicide <- lm(second_suicide ~ second_cancer + first_unemployment + second_alco + log_wage, data = ca_diff)
summary(ols_model_suicide)
cor(second_suicide, first_unemployment, method = "pearson") # recheck relationship between unemployment and suicide rate

# test
# Multicollinearity
install.packages("car")
library(car)
vif(ols_model_suicide) # pass

# Heteroskedasticity
install.packages('lmtest')
library(lmtest)
bptest(ols_model_suicide) # Breusch-Pagan Test -> pass >0.05
install.packages("skedastic")
library(skedastic) # White Test
white_lm(ols_model_suicide)

#Normality o residuals
shapiro.test(residuals(ols_model_suicide)) # Shapiro-Wilk Test -> pass > 0.05
qqnorm(residuals(ols_model_suicide))
qqline(residuals(ols_model_suicide), col = "red")# Q-Q Plot

# Specification Test (Ramsey RESET Test)
resettest(ols_model_suicide, power = 2:3, type = "fitted") #> 0.05

# autocorrelation
install.packages("lmtest")
library(lmtest)
dwtest(ols_model_suicide) # Perform Durbin-Watson test >0.05



