setwd('H:/Biology/Statistics/Practical 7')
mammals <- read.csv('MammalData.csv')
# get logs of continuous variables
mammals$logLS <- log(mammals$LitterSize)
mammals$logBM <- log(mammals$AdultBodyMass_g)
mammals$logCvalue <- log(mammals$meanCvalue)
# reduce dataset to five key variables
mammals <- subset(mammals, select = c(logCvalue, logLS, logBM, TrophicLevel, GroundDwelling))
# remove the row with missing
mammals <- na.omit(mammals)
str(mammals)

model <- lm(formula = logCvalue ~ logLS * logBM * TrophicLevel * GroundDwelling, data = mammals)
anova(model)
summary(model)

model <- lm(logCvalue ~ (logLS + logBM + TrophicLevel + GroundDwelling)^2, data = mammals)
anova(model)
summary(model)
par(mfrow = c(2,2))
plot(model)

drop.scope(model)
drop.scope(y ~ a + b + c + a:b)
drop.scope(y ~ a + b + c + a:b + b:c + a:b:c

f <- y ~ a + b + c + b:c
# remove b:c from the current model
update(f, . ~ . - b:c)
# model g as a response using the same explanatory variables.
update(f, g ~ .)

model2 <- update(model, . ~ . - TrophicLevel:GroundDwelling)  # remove TrophicLevel:GroundDwelling
anova(model, model2)  # use anova to compare the two models
anova(model2)  # check what other variables can be removed 
summary(model2)
drop.scope(model2)

model3 <- update(model2, . ~ . - logBM:TrophicLevel)
anova(model2, model3)
anova(model3)
summary(model3)
drop.scope(model3)

model4 <- update(model3, . ~ . - logLS:TrophicLevel)
anova(model3, model4)
anova(model4)
summary(model4)
drop.scope(model4)

model5 <- update(model4, . ~ . - TrophicLevel)
anova(model4, model5)
anova(model5)
summary(model5)
drop.scope(model5)

model6 <- update(model5, . ~ . - logLS:logBM)
anova(model5, model6)
anova(model6)
summary(model6)
drop.scope(model6)

# Final model
# lm(formula = logCvalue ~ logLS + logBM + GroundDwelling + logLS:GroundDwelling + logBM:GroundDwelling)

save(model6, file = 'myFinalModel.Rda')