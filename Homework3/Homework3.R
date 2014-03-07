# Problem 2.22
# Chi-Squared Tests of Independence
problem22=matrix(c(105, 12, 18, 47, 0, 8, 2, 19, 52, 13),ncol=2)
dimnames(problem22)=list(Diagnosis=c("Schizophrenia","Affective Disorder","Neurosis",
                                     "Personality Disorder", "Special Symptoms"),
                         Table=c("Drugs", "No Drugs"))
problem22
chisq.test(problem22) #Pearson's Chi-square test
chisq.test(problem22,simulate.p.value=TRUE,B=10000)


# Problem 2.30
# Fisher Test

cancer <- c(21,15,  2,3)
cancer <- matrix(cancer,byrow=TRUE,nrow=2)
dimnames(cancer) <- list(Income=c("Surgery","Radiation Therapy"),
                           Problem230=c("Cancer Controlled","Cancer Not Controlled"))
cancer

fisher.test(cancer)
