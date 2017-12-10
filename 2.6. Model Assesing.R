# Data Input ----
rm(list = ls()); gc(); dev.off()
load("ModelData.Rdata")
train <- cbind(train, predict.glm(object = lgtm2, type = "response"))
train <- train[order(-V2)]

# CAP Preparation ----
CAP <- as.data.table(NULL)
for(i in 0:100){
	j <- i * 100
	CAP <- rbind(CAP, train[0:j,
													.(Total_Exited       = sum(Exited), 
														Total_Records      = .N,
														Hit_Ratio          = sum(Exited) / .N,
														Selected_Purchases = sum(Exited) / 2037,
														Expected_Purchases = (j / 10000), 
														Total_Contacted    = .N / 10000)])
}

# CAP Plot ----
ggplot(data = CAP, aes(x = Total_Contacted, y = Selected_Purchases)) + 
	geom_line(colour = "red", size = 2) + 
	geom_line(inherit.aes = F, aes(x = Total_Contacted, y = Expected_Purchases), colour = "blue", size = 2)

# Shortcut rather than calculating the area between 
# Go for %50 total_contacted line.
# If hit_ratio > 60% model is rubbish
# If btw 60%-70% it is poor
# If btw 70%-80% it is good
# If btw 80%-90% it is very good
# If btw 90%-100% it is too-good

# Test vs. Train ----
test[, pred := ifelse(predict.glm(newdata = test, object = lgtm2, type = "response") > 0.5, 1, 0)]
test[, prat := predict.glm(newdata = test, object = lgtm2, type = "response")]
test <- test[order(-prat)]

# Test CAP Preparation ----
CAP <- as.data.table(NULL)
for(i in 0:50){
	j <- i * 20
	CAP <- rbind(CAP, test[0:j,
													.(Total_Exited       = sum(Exited), 
														Total_Records      = .N,
														Hit_Ratio          = sum(Exited) / .N,
														Selected_Purchases = sum(Exited) / 260,
														Expected_Purchases = (j / 1000), 
														Total_Contacted    = .N / 1000)])
}
