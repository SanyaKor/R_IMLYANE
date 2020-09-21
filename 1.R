library("IMTest") 
library("rlms") 
library("dplyr") 
library("GGally") 
library("car") 
library("sandwich") 
data2=swiss
data2 
help(swiss) 

#glimpse(data)

data=dplyr::select(data2,Fertility,Agriculture, Catholic, Infant.Mortality)
data
ggpairs(data)



data["Log_Agriculture"]=log(data$Agriculture)
data["Log_Catholic"]=log(data$Catholic)
data["Log_Infant.Mortality"]=log(data$Infant.Mortality)


Agr1<-as.character(data$Agriculture)
Agr2<-lapply(Agr1,as.integer)
Agr3<-as.numeric(unlist(Agr2))
data["Agr"]=(Agr3-mean(Agr3))/sqrt(var(Agr3))

Cat1<-as.character(data$Catholic)
Cat2<-lapply(Cat1,as.integer)
Cat3<-as.numeric(unlist(Cat2))
data["Cat"]=(Cat3-mean(Cat3))/sqrt(var(Cat3))

IM1<-as.character(data$Infant.Mortality)
IM2<-lapply(IM1,as.integer)
IM3<-as.numeric(unlist(IM2))
data["IM"]=(IM3-mean(IM3))/sqrt(var(IM3))



Ln_Agr1<-as.character(data$Log_Agriculture)
Ln_Agr2<-lapply(Ln_Agr1,as.integer)
Ln_Agr3<-as.numeric(unlist(Ln_Agr2))
data["Ln_Agr"]=(Ln_Agr3-mean(Ln_Agr3))/sqrt(var(Ln_Agr3))

Ln_Cat1<-as.character(data$Log_Catholic)
Ln_Cat2<-lapply(Ln_Cat1,as.integer)
Ln_Cat3<-as.numeric(unlist(Ln_Cat2))
data["Ln_Cat"]=(Ln_Cat3-mean(Ln_Cat3))/sqrt(var(Ln_Cat3))

LN_IM1<-as.character(data$Log_Infant.Mortality)
LN_IM2<-lapply(LN_IM1,as.integer)
LN_IM3<-as.numeric(unlist(LN_IM2))
data["Ln_IM"]=(LN_IM3-mean(LN_IM3))/sqrt(var(LN_IM3))


model1<-lm(Fertility~Agr+Cat+IM,data)
model1
summary(model1)#R^2=0.3449
#p -()(*)(**)

model2<-lm(Fertility~Cat+IM,data)
model2
summary(model2)#R^2=0.302
#p-(**)(**)

model3<-lm(Fertility~IM,data)
model3
summary(model3)#R^2=0.1562
#p-(**)

model4<-lm(Fertility~Cat,data)
model4
summary(model4)#R^2=0.1964
#p-(**)

model5<-lm(Fertility~Agr,data)
model5
summary(model5)#R^2=0.1057
#p-(*)

model6<-lm(Fertility~Ln_Agr+Ln_Cat+Ln_IM,data)
model6
summary(model6)#R^2=0.3766
#p-(**)(*)(**)

model7<-lm(Fertility~Ln_Agr+Ln_IM,data)
model7
summary(model7)#R^2=0.3146
#p-(**)(***)

model8<-lm(Fertility~Ln_IM+Cat,data)
model8
summary(model8)#R^2=0.3279
#p-(**)(***)

model9<-lm(Fertility~Ln_IM+Cat+Ln_Agr,data)
model9
summary(model9)#R^2=0.42317
#p-(***)(**)(**)

model10<-lm(Fertility~I(Ln_IM^2)+Cat+Ln_Agr,data)
model10
summary(model10)#R^2=0.4237
#p-(***)(**)(**)

model12<-lm(Fertility~Ln_IM+I(Cat^2)+I(Agr^2),data)#Best
model12
summary(model12)#R^2=0.564

model13<-lm(Fertility~I(Ln_IM*Cat)+Cat+I(Ln_Agr^2),data)
model13
summary(model13)#R^2=0.3536


model14<-lm(Fertility~I(Ln_IM*Cat)+I(Ln_Cat*Agr)+I(Ln_Agr*IM),data)
model14
summary(model14)#R^2=0.1801


model15<-lm(Fertility~I(IM*Cat)+I(Cat*Ln_Agr)+I(Agr^2),data)
model15
summary(model15)#R=0.1341


model16<-lm(Fertility~I(IM*Cat)+I(Cat*Ln_Agr)+I(Agr*Ln_IM),data)
model16
summary(model16)#R=0.1341
