rm(list = ls()); gc(); dev.off()
setwd(dir = "C:/Users/kazim/Documents/Udemy/2. Modelling/")
sd <- fread(input = "2.2. SalaryData.csv")
summary(sd)
linear_model <- lm(formula = Salary ~ YearsExperience, data = sd)
summary(linear_model)
sd <- cbind(sd, linear_model$fitted.values)
ggplot(data = sd, aes(x = YearsExperience, y = V2)) + 
	geom_line() + 
	geom_point(inherit.aes = FALSE, aes(x = YearsExperience, y = Salary), shape = "+", colour = "red", size = 2)
