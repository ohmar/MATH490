# PROBLEM 2.12

phs=matrix(c(193,19749,198,19934),byrow=TRUE,ncol=2)
phs

dimnames(phs)=list(Group=c("Placebo","Aspirin"),MI=c("Yes","No"))
phs


prop.test(phs)
prop.test(phs,correct=F)
#save output
phs.test=prop.test(phs)
str(phs.test)
phs.test$estimate
phs.test$conf.int

round(phs.test$conf.int,3)
phs.test$estimate[1]/phs.test$estimate[2] # relative risk

# str(phs.test) gives you a structure of your data.
# phs.test$p.value will give you that specific value.

#Odds ratio
phs.test$estimate
odds=phs.test$estimate/(1-phs.test$estimate)
odds

odds[1]/odds[2]

(phs[1,1]*phs[2,2])/(phs[2,1]*phs[1,2]) # as cross-prod ratio

# 95 percent Confidence Interval for Odds Ratio.
theta=odds[1]/odds[2]
ASE=sqrt(sum(1/phs))
ASE

logtheta.CI=log(theta) + c(-1,1)*1.96*ASE
logtheta.CI

exp(logtheta.CI)
