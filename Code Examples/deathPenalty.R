#entering death penalty data

dp=c(53, 414, 11, 37, 0, 16, 4, 139)
dp=array(dp, dim=c(2,2,2))
dimnames(dp)=list(DeathPen=c("yes","no"),
Defendant=c("white","black"), Victim=c("white","black"))
df_flat=ftable(dp, row.vars=c("Victim","Defendant"), col.vars="DeathPen")
df_flat

# to get data from the library
library(icda)
data(deathpenalty)
deathpenalty
dp=xtabs(Freq ~ Victim + Defendant + DeathPenalty, data=deathpenalty)
dpflat=ftable(DeathPenalty ~ Victim + Defendant,data=dp)
dpflat

dp

round(100*prop.table(dpflat,1), 1)


Theta_XY_1=53*37/(414*11) #Z is white 
Theta_XY_2=0*37/(16*4) # Z is black
Theta_XY_2=(0+0.5)*(37+0.5)/((16+0.5)*(4+0.5))
Theta_XY=53*176/(15*430)

Theta_XY
Theta_XY_1
Theta_XY_2
