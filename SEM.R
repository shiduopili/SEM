#SEM
struc_model<- '
# measurement model
connect=~ccs_connectedness1+ccs_connectedness2+ccs_connectedness7+ccs_connectedness10
learn=~ccs_learning2+ccs_learning4+ccs_learning5+ccs_learning8
sl=~social_loneli1_recode+social_loneli2_recode+social_loneli3_recode
el=~emotion_loneli1_recode+emotion_loneli3_recode
#regressions
connect~Total_hours+age_group_recode1+education_recode1+gender
learn~Total_hours+age_group_recode1+education_recode1+gender
sl~learn+Total_hours+age_group_recode1+education_recode1+gender
el~connect+learn+Total_hours+age_group_recode1+education_recode1+gender
happiness~sl+connect+learn+el+Total_hours+age_group_recode1+education_recode1+gender
#covariance
sl~~el
connect~~learn
Total_hours~~education_recode1
Total_hours~~gender
gender~~age_group_recode1
gender~~education_recode1'
struc_fit<-lavaan::sem(struc_model,data=data,estimator="MLM")
summary(struc_fit,fit.measures=TRUE,standardized=TRUE)
fitMeasures(struc_fit)



conn_happ<-"
# measurement model
connect=~ccs_connectedness1+ccs_connectedness2+ccs_connectedness7+ccs_connectedness10
learn=~ccs_learning2+ccs_learning4+ccs_learning5+ccs_learning8
sl=~social_loneli1_recode+social_loneli2_recode+social_loneli3_recode
el=~emotion_loneli1_recode+emotion_loneli3_recode

#a
el~a1*connect+a2*learn+a3*Total_hours+age_group_recode1+education_recode1+gender
sl~aa1*learn+aa3*Total_hours+age_group_recode1+education_recode1+gender
connect~m1*Total_hours+age_group_recode1+education_recode1+gender
learn~m2*Total_hours+age_group_recode1+education_recode1+gender
#b and cp
happiness~b1*el+b2*sl+cp1*connect+cp2*learn+cp3*Total_hours+age_group_recode1+education_recode1+gender

#covariance
sl~~el
connect~~learn
Total_hours~~education_recode1+gender
gender~~age_group_recode1+education_recode1

contohapp_EL := a1*b1
leatohapp_SL := aa1*b2
leatohapp_EL :=a2*b1
hourstoEL_con :=m1*a1
hourstoEL_lea :=m1*a2
hourstoSL_lea :=m2*aa1
hourstohapp_EL :=a3*b1
hourstohapp_SL :=aa3*b2"

model1<-lavaan::sem(conn_happ,data=data,estimator="MLM",bootstrap=10000)
summary(model1, standardized = T, fit.measures = T, rsq = T)
