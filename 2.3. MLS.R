rm(list = ls()); gc(); dev.off()
setwd(dir = "C:/Users/kazim/Documents/Udemy/2. Modelling/")
st <- fread(input = "2.3. 50-Startups.csv")
st[, State := as.factor(State)] #instead of building dummy variables, I will use it as a factor in my LM.
st <- `colnames<-`(st, c("R_D", "Adm", "MS", "State", "Profit")) # column names without extra-characters.

# full linear model
full_lm <- lm(formula = Profit ~ ., data = st)
# backwards - stepwise regression
bws_lm  <- step(lm(formula = Profit ~ ., data = st), direction = "backward")
# forward - stepwise regression
fws_lm  <- step(lm(formula = Profit ~ 1, data = st), direction = "forward", scope = ~ R_D + Adm + MS + State)
# bidirectional - stepwise regression
bis_lm  <- step(lm(formula = Profit ~ ., data = st), direction = "both")
# all possible models - i skip this.

# Lastly, to note, Kirill skips that R&D and Profit are endogenous. 
# A correlation test proves that there is a strong (%97.3), significant and positive relationship. 
cor.test(st$R_D, st$Profit)
# Also variables (Marketing Spend and R&D Spend) are highly correlated.
cor.test(st$R_D, st$MS)
# Those two points violate the assumptions of LM. Thereby, LM above is problematic.