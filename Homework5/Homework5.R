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

#########################
# R Code for Problem 3.18
# (a)
Attendance=c(404,286,443,169,222,150,321,189,258,223,211,215,
             108,210,224,211,168,185,158,429,226,150,148)
Arrests=c(308,197,184,149,132,126,110,101,99,81,79,78,
          68,67,60,57,55,44,38,35,29,20,19)

atten.arrest=data.frame(Attendance, Arrests)

atten.fit2=glm(Arrests ~ Attendance, offset=log(Attendance), data=atten.arrest, family=poisson)
summary(atten.fit2)

atten.poi<-glm(Arrests ~ Attendance, offset=log(Attendance), data=atten.arrest, family=poisson())
summary(atten.poi)
predict.glm(atten.poi,type="terms")

# (b)
Attendance=c(404,286,443,169,222,150,321,189,258,223,211,215,
             108,210,224,211,168,185,158,429,226,150,148)
Arrests=c(308,197,184,149,132,126,110,101,99,81,79,78,
          68,67,60,57,55,44,38,35,29,20,19)
atten.arrest=data.frame(Attendance, Arrests)
atten.fit2=glm(Arrests ~ Attendance, offset=log(Attendance), data=atten.arrest, family=poisson)
summary(atten.fit2)
predict.glm(atten.fit2,type="terms")

# (c)

plot(x = atten.arrest$Attendance, y = atten.arrest$Arrests, xlab = "Attendance", ylab = "Number of arrests",
     main = "Attendance vs Arrest data set \n with poisson regression model fit", panel.first = grid(col = "gray", lty = "dotted"))
