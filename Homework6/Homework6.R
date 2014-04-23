library(icda)
data(MBdrink)

# Accessing each vector in MBdrink to change values to 1's and 0's
a = MBdrink$EI
b = MBdrink$SN
c = MBdrink$TF
d = MBdrink$JP
Drink = MBdrink$Drink
Count = MBdrink$Count


# Changing E,S,T,J to 1s and I,N,F,P to 0s
EI = as.numeric(a == "E")
SN = as.numeric(b =="S")
TF = as.numeric(c == "T")
JP = as.numeric(d =="J")

# new data frame with dummy variables
drink = data.frame(EI, SN, TF, JP, Drink, Count)

summary(glm(Drink ~ EI+SN+TF+JP, data=drink, family=binomial(), weights=Count))


#problem 4.24

data(throat)
summary(glm((D > 0) ~ (Y + T), data=throat, family=binomial()))

summary(glm(cbind(Y, T) ~ D, data = throat, family = binomial()))

duration<-c(45,15,40,83,90,25,35,65,95,35,75,45,50,75,30,25,20,60,70,30,60,
            61,65,15,20,45,15,25,15,30,40,15,135,20,40)
type<-c(0,0,0,1,1,1,rep(0,5),1,1,1,0,0,1,1,1,rep(0,4),1,1,0,1,0,1,0,0,rep(1,4))
sore<-c(0,0,rep(1,10),0,1,0,1,0,rep(1,4),0,1,0,0,1,0,1,0,1,1,0,1,0,0)
sore.fr<-cbind(duration, type, sore)


# attempting (c)
sorethroat.lg<-glm(sore ~ type*scale(duration), family=binomial)
summary(sorethroat.lg)

sorethroat.lg2<-glm(sore ~ type + scale(duration), family=binomial)# no interaction
anova(sorethroat.lg2, sorethroat.lg, test = "Chisq")

summary(glm(cbind(T) ~ D, data = throat, family = binomial()))
summary(glm(cbind(Y) ~ D, data = throat, family = binomial()))

