#####################################################
# Problem 5.4(a)

library(icda)
data(MBdrink)
library(reshape2)
mb = melt(MBdrink)
mbwide = dcast(mb, ... ~Drink)
mbwide

mb.fit<-glm(cbind(Rarely,Often) ~ EI + SN + TF + JP, family=binomial, data=mbwide)
summary(mb.fit)

deviance(mb.fit)

df.residual(mb.fit)

pchisq(deviance(mb.fit), 1, lower.tail=FALSE)

#####################################################
# Problem 5.4(b)

# We use drop1 to see which variable we can drop.
drop1(mb.fit, test="Chisq")
# We see that we can take out the variable JP. 

#####################################################
# Problem 5.4(c)

mb.fit2<-glm(cbind(Rarely,Often) ~ EI + SN + TF, family=binomial, data=mbwide)


#####################################################
# Problem 5.15

#age=rep(c("13 or Less", "14 through 18", "19 and Above"),2)
#Gender=rep(c("Male", "Female"),each=3)
#Missing =c(33,63,157,38,108,159)
#Found = c(3271,7256,5065,2486,8877,3520)
#missing.dat=data.frame(Gender, age,Missing,Found)
#missing.dat
data(missingpersons)
is.data.frame(missingpersons)

library(reshape2)
missingP = melt(missingpersons)
missingWide = dcast(missingP, ... ~age)
missingWide

missing.fit=glm(cbind(stillmissing,total) ~ gender + age, family=binomial, data=missingpersons)
g=summary(missing.fit)
g

pchisq(deviance(missing.fit), 1, lower.tail=FALSE)

p_val=2*(1-pnorm(g$coef[1,2]))
p_val

