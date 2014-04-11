library(icda)
library(MASS)
data(horseshoecrabs)
horseshoecrabs

#########################
# R Code for Problem 3.13 
# (a)
# x = Weight, Y = Satellites
mod.fit<-glm(Satellites~Weight, data=horseshoecrabs, family=poisson(link = log))
summary(mod.fit)

# (b)

crab.poi<-glm(Satellites ~ Weight, data=horseshoecrabs, family=poisson())
summary(crab.poi)
predict(crab.poi, newdata=data.frame(Weight=2.44),type="response")

# (c)

confint(mod.fit, level = 0.90)

# (d)

crabs.WaldCI=confint.default(mod.fit)

# (e)

drop1(mod.fit, test="Chisq")

crab.poi<-glm(Satellites ~ Weight, data=horseshoecrabs, family=poisson())
summary(crab.poi)
predict(crab.poi, newdata=data.frame(Weight=2.44),type="response")

#########################
# R Code for Problem 3.14
# (a)
# Negative binomial fit of the model

crab.mod.nb=glm.nb(formula = Satellites ~ Weight, data = horseshoecrabs, link = log)
summary(crab.mod.nb)

# (b)
# Confidence interval for the negative binomial model
confint(crab.mod.nb, level = 0.95)

