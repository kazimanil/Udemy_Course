rm(list = ls()); gc(); dev.off()
setwd(dir = "C:/Users/kazim/Documents/Udemy/2. Modelling/")
email <- fread(input = "2.4. Email-Offer.csv")
email[, Gender := as.factor(Gender)]
logit <- glm(data = email, family=binomial(link='logit'), formula = TookAction ~ Age)
summary(logit)

mlogit <- glm(data = email, family=binomial(link='logit'), formula = TookAction ~ Age + Gender)
summary(mlogit)

# Confusion Matrix and Model Evaluation
email1 <- cbind(email, predict.glm(logit, type = "response"))
# when the type is response: it gets the predicted value (lets say x) and converts it to e^x / (1 + e^x ) to find the actual y value.
email1[V2 > 0.5, pred := 1]
email1[V2 <= 0.5, pred := 0]
email1[TookAction == 0 & pred == 1, 
			False_Pos := 1] # Type I
email1[TookAction == 1 & pred == 0,  
			False_Neg := 1] # Type II
email1[TookAction == 0 & pred == 0,
			True_Neg := 1]
email1[TookAction == 1 & pred == 1,
			True_Pos := 1]
perf1 <- rbind(c(email1[, sum(True_Neg, na.rm = T)], email1[,sum(False_Pos, na.rm = T)]),
							 c(email1[, sum(False_Neg, na.rm = T)], email1[,sum(True_Pos, na.rm = T)])) # confusion matrix

acc_rate <- (perf1[1,1] + perf1[2,2])/sum(perf1) # accuracy rate
err_rate <- (perf1[1,2] + perf1[2,1])/sum(perf1) # error rate

# Confusion Matrix and Model Evaluation for Model 2
email2 <- cbind(email, predict.glm(mlogit, type = "response"))
email2[V2 > 0.5,  pred := 1]
email2[V2 <= 0.5, pred := 0]
email2[TookAction == 0 & pred == 1, 
			False_Pos := 1] # Type I
email2[TookAction == 1 & pred == 0,  
			False_Neg := 1] # Type II
email2[TookAction == 0 & pred == 0,
			True_Neg := 1]
email2[TookAction == 1 & pred == 1,
			True_Pos := 1]
perf2 <- rbind(c(email2[, sum(True_Neg, na.rm = T)],  email2[,sum(False_Pos, na.rm = T)]),
							 c(email2[, sum(False_Neg, na.rm = T)], email2[,sum(True_Pos, na.rm = T)])) # confusion matrix

acc_rate2 <- (perf2[1,1] + perf2[2,2])/sum(perf2) # accuracy rate
err_rate2 <- (perf2[1,2] + perf2[2,1])/sum(perf2) # error rate
	
# Graph Building ----
email <- email[order(Age, TookAction)]
email[, Pass := cumsum(TookAction)]
email[, TotalPass := sum(TookAction)]
email[, prob := Pass / TotalPass]
email_age <- email[, tail(.SD, 1), .(Age)]
ggplot(data = email_age, aes(x = Age, y = prob)) + geom_smooth()
