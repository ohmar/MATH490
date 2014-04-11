library(icda)
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
