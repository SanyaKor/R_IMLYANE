library("rlms") 
library("dplyr") 
library("GGally") 
library("car") 
library("sandwich") 
library("memisc")


data <- rlms_read("C:\\r19i_os26c.sav") 
glimpse(data) 
data2 = dplyr::select(data, idind, oj13.2, oh5, o_marst, o_educ, o_age, status, oj6.2) 
#ggpairs(data2)
data2 = na.omit(data2) 



data2["log_age"]=log(data2$o_age) 
data2["log_dur"]=log(data2$oj6.2) 


data2["wed1"]=data2$o_marst 
data2["wed1"]=0 
data2$wed1[which(data2$o_marst=='2')] <- 1
data2$wed1[which(data2$o_marst=='6')] <- 1
#data2$wed1 = as.numeric(data2$wed1) 


data2["wed2"]=data2$o_marst 
data2["wed2"]=0 
data2$wed2[which(data2$o_marst=='4')] <- 1
data2$wed2[which(data2$o_marst=='5')] <- 1
#data2$wed2 = as.numeric(data2$wed2) 


data2["wed3"]=data2$o_marst 
data2["wed3"]=0 
data2$wed3[which(data2$o_marst=='1')] <- 1
#data2$wed3 = as.numeric(data2$wed3) 

 
data2["sex"]=data2$oh5 
data2$sex[which(data2$sex!='1')] <- 0  
data2$sex[which(data2$sex=='1')] <- 1 
data2$sex = as.numeric(data2$sex) 


data2["city_status"]=data2$status 
data2["city_status"]=0 
data2$city_status[which(data2$status=='1')] <- 1 
data2$city_status[which(data2$status=='2')] <- 1 
#data2$city_status = as.numeric(data2$city_status) 

 
data2["higher_educ"]=data2$o_educ 
data2["higher_educ"]=0 
data2$higher_educ[which(data2$o_educ=='21')] <- 1 
data2$higher_educ[which(data2$o_educ=='22')] <- 1 
data2$higher_educ[which(data2$o_educ=='23')] <- 1 


sal = as.numeric(data2$oj13.2) 
sal1 = as.character(data2$oj13.2) 
sal2 = lapply(sal1, as.integer) 
sal = as.numeric(unlist(sal2)) 
data2["salary"] = (sal - mean(sal)) / sqrt(var(sal)) 




age1 = as.character(data2$o_age) 
age2 = lapply(age1, as.integer) 
age3 = as.numeric(unlist(age2)) 
data2["age"]= (age3 - mean(age3)) / sqrt(var(age3)) 

ln_age1 = as.character(data2$log_age) 
ln_age2 = lapply(ln_age1, as.integer) 
ln_age3 = as.numeric(unlist(ln_age2)) 
data2["ln_age"]= (ln_age3 - mean(ln_age3)) / sqrt(var(ln_age3)) 


dur1 = as.character(data2$oj6.2) 
dur2 = lapply(dur1, as.integer) 
dur3 = as.numeric(unlist(dur2)) 
data2["dur"] = (dur3 - mean(dur3)) / sqrt(var(dur3)) 

ln_dur1 = as.character(data2$log_dur) 
ln_dur2 = lapply(ln_dur1, as.integer) 
ln_dur3 = as.numeric(unlist(ln_dur2)) 
data2["ln_dur"] = (ln_dur3 - mean(ln_dur3)) / sqrt(var(ln_dur3)) 



model1 = lm(salary~wed1+wed2+wed3+sex+city_status+higher_educ+age+dur, data2) 
model1 
summary(model1) #R^2 = 0.137 
vif(model1) 

model2 = lm(salary~sex+city_status+higher_educ+age+dur, data2) 
model2 
summary(model2) #R^2 = 0.135
vif(model2) 
 
model397 = lm(salary~wed2+wed3+sex+city_status+higher_educ+age+dur, data2) 
model397 #
summary(model397) #R^2 = 0.1371 
vif(model397) 


model3 = lm(salary~wed3+sex+city_status+higher_educ+age+dur, data2) 
model3 #
summary(model3) #R^2 = 0.1372
vif(model3) 


model4 = lm(salary~wed2+wed3+sex+city_status+higher_educ+dur, data2) 
model4 #
summary(model4) #R^2 = 0.1345 
vif(model4) 

model5 = lm(salary~wed3+sex+city_status+higher_educ+dur, data2) 
model5 # 
summary(model5) #R^2 = 0.1031 
vif(model5)  

model21 = lm(salary~wed2+wed3+sex+city_status+higher_educ+age+ln_dur, data2) 
model21 # 
summary(model21) #R^2 = 0.1385
vif(model21) 

model33 = lm(salary~wed2+wed3+sex+city_status+higher_educ+ln_age+ln_dur, data2) 
model33 # 
summary(model33) #R^2 = 0.141 
vif(model33) 

model34 = lm(salary~wed2+wed3+sex+city_status+higher_educ+ln_age+ln_dur, data2) 
model34 # 
summary(model34) #R^2 = 0.141 
vif(model34)  



model35 = lm(salary~wed2+wed3+sex+city_status+higher_educ+I(age^2)+dur, data2) 
model35  
summary(model35) #R^2 = 0.148 
vif(model35) 

model36 = lm(salary~wed2+I(wed3^2)+sex+city_status+higher_educ+age+dur, data2) 
model36
summary(model36) #R^2 = 0.1371
vif(model36) 

model37 = lm(salary~wed1+higher_educ, data2) 
model37   
summary(model37) #R^2=0.04646
vif(model37)  

model38 = lm(salary~wed3+city_status, data2) 
model38 #  
summary(model38) #R^2=0.0268
vif(model38) 

model51=lm(salary~wed2+city_status, data2) 
model51
summary(model51) 
vif(model51) 
#R^2=0.04
model52=lm(salary~wed2+higher_educ, data2) 
model52
summary(model52) 
vif(model52) 
#R^2=0.03
compare5 = mtable(model34,model33,model21,model52,model51)
compare5
