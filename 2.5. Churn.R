# Data Input ----
rm(list = ls()); gc(); dev.off()
setwd(dir = "C:/Users/kazim/Documents/Udemy/2. Modelling/")
train <- fread(input = "2.5.1. Churn-Modelling.csv")[]
train[, ':='(Germany = ifelse(Geography == "Germany", 1, 0),
						 France  = ifelse(Geography == "France", 1, 0),
						 Spain   = ifelse(Geography == "Spain", 1, 0),
						 Gender  = as.factor(Gender))]
train <- train[,4:17]
train <- train[, -2]

test <- fread(input = "2.5.2. Churn-Modelling-Test-Data.csv")
test[, ':='(Germany = ifelse(Geography == "Germany", 1, 0),
						France  = ifelse(Geography == "France", 1, 0),
						Spain   = ifelse(Geography == "Spain", 1, 0),
						Gender  = as.factor(Gender))]
test <- test[,4:17]
test <- test[, -2]

# Data Transformation ----
train[, LogBalance := log10(Balance + 1)] # 1 is added since there are 0-balances.
train[, WealthAcc  := Balance / Age]

test[, LogBalance := log10(Balance + 1)] # 1 is added since there are 0-balances.
test[, WealthAcc  := Balance / Age]

# Analysis ----
lgtm1 <- step(glm(data = train, family = binomial(link='logit'), 
						  formula = Exited ~ CreditScore + Tenure + Balance + NumOfProducts + HasCrCard + IsActiveMember + EstimatedSalary + Germany + Spain + Gender + Age),
							direction = "backward") # logit model before data-transformation
summary(lgtm1)

lgtm2 <- glm(data = train, family = binomial(link = "logit"),
						 formula = Exited ~ CreditScore + Tenure + LogBalance + NumOfProducts + IsActiveMember + Germany + Gender + Age)
summary(lgtm2)
car::vif(lgtm2) # no problem with VIF

# Correlation Matrix ----
corrs <- Hmisc::rcorr(as.matrix(train[,-2]), type = "spearman")$r
cor_sigs <- Hmisc::rcorr(as.matrix(train[,-2]), type = "spearman")$P
corrplot(corrs, type = "upper", p.mat = cor_sigs, order = "hclust",
				 insig = "blank", sig.level = 0.05)

# library("PerformanceAnalytics") # To create a even better corr_plot
# chart.Correlation(train[, -2], histogram=TRUE, pch=19)

# Save -----
save.image(file = "ModelData.Rdata")
