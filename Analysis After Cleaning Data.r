# To be used for analysis AFTER cleaning data.r

#Turn Columns into factors if non-numeric

Cleaned$`Student Gender`<-factor(Cleaned$`Student Gender`)
Cleaned$URM<-factor(Cleaned$URM)
Cleaned$`Citizenship Status`<-factor(Cleaned$`Citizenship Status`)
Cleaned$`Bachelor's Major`<-factor(Cleaned$`Bachelor's Major`)
Cleaned$`Admission Year Range`<-factor(Cleaned$`Admission Year Range`)
Cleaned$`Bacherlor's Institution Grouping`<-factor(Cleaned$`Bacherlor's Institution Grouping`)
Cleaned$`Finished Within 7 Years`<-factor(Cleaned$`Finished Within 7 Years`)

#Rename two word columns:
Cleaned$Gender<-Cleaned$`Student Gender`
Cleaned$Citizenship<-Cleaned$`Citizenship Status`
Cleaned$Major<-Cleaned$`Bachelor's Major`
Cleaned$AdmitYearRange<-Cleaned$`Admission Year Range`
Cleaned$SchoolTier<-Cleaned$`Bacherlor's Institution Grouping`
Cleaned$Success<-Cleaned$`Finished Within 7 Years`
Cleaned$Ethnicity <- Cleaned$`Student Ethnicity`
Cleaned$Ethnicity[is.na(Cleaned$Ethnicity)]<-"Unknown"

levels(Cleaned$Success)


logit.all<-glm(Success ~ SchoolTier + Citizenship + UGGPA + MUGGPA + GREV + GREQ + AdmitYearRange + Major + Gender+ URM, data=Cleaned, family="binomial")
summary(logit.all)

logit.Tier.GPA<-glm(Success ~ SchoolTier + UGGPA + Citizenship, data=Cleaned, family="binomial")
summary(logit.Tier.GPA)
newdata3 <- with(Cleaned, data.frame(GREV,GREQ,Major,MUGGPA,UGGPA,Citizenship,Gender,URM,SchoolTier,AdmitYearRange,Ethnicity))
newdata3$rankP <- predict(logit.Tier.GPA,newdata=newdata3,type="response")
ggplot(newdata3,aes(UGGPA,rankP,color=SchoolTier,shape=Citizenship))+geom_jitter()




library(ggplot2)

newdata3 <- with(Cleaned, data.frame(GREV,GREQ,Major,MUGGPA,UGGPA,Citizenship,Gender,URM,SchoolTier,AdmitYearRange))
newdata3$rankP <- predict(logit.kitchensink,newdata=newdata3,type="response")
ggplot(newdata3,aes(UGGPA,rankP,color=Gender,shape=URM))+geom_jitter()

newdata3 <- with(Cleaned, data.frame(GREV,GREQ,Major,MUGGPA,UGGPA,Citizenship,Gender,URM,SchoolTier,AdmitYearRange))
newdata3$rankP <- predict(logit.gender,newdata=newdata3,type="response")
ggplot(newdata3,aes(UGGPA,rankP,color=Gender,shape=URM))+geom_jitter()


logit.gender.URM<-glm(Success ~ Gender+ URM, data=Cleaned, family="binomial")
summary(logit.gender.URM)
newdata3$rankP <- predict(logit.gender.URM,newdata=newdata3,type="response")
ggplot(newdata3,aes(UGGPA,rankP,color=Gender,shape=URM))+geom_jitter()

