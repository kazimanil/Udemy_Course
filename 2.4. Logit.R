rm(list = ls()); gc(); dev.off()
setwd(dir = "C:/Users/kazim/Documents/Udemy/2. Modelling/")
email <- fread(input = "2.4. Email-Offer.csv")
email[, Gender := as.factor(Gender)]
logit <- glm(data = email, family=binomial(link='logit'), formula = TookAction ~ Gender + Age)
summary(logit)

# Graph Building ----
email <- email[order(Age, TookAction)]
email[, Pass := cumsum(TookAction)]
email[, TotalPass := sum(TookAction)]
email[, prob := Pass / TotalPass]
email_age <- email[, tail(.SD, 1), .(Age)]
plot(x = email_age$Age, y = email_age$prob)
