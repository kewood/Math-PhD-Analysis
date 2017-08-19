## Starting Almost from Scratch. Data came with courses for students, but for now we will not be examining them. We Cleared Courses from Excel First.


#Import Data from Excel:
library(readxl)
Cleaned <- read_excel("~/Math PhD Analysis/Cleaned.xlsx")

#Check Data for Gender
table(Cleaned$`Student Gender`) #looks good, no unknowns

table(Cleaned$`Student Ethnicity`) #Messy. We are currenly interested if the student is a URM (under represented minority) or not. 

#Create new column 'URM' to determin their staus
Cleaned$`URM`[Cleaned$`Student Ethnicity`=="African American"]<-"URM"
Cleaned$`URM`[Cleaned$`Student Ethnicity`=="Asian or Pacific Islander"]<-"Non-URM"
Cleaned$`URM`[Cleaned$`Student Ethnicity`=="Caucasian"]<-"Non-URM"
Cleaned$`URM`[Cleaned$`Student Ethnicity`=="Chicano"]<-"URM"
Cleaned$`URM`[Cleaned$`Student Ethnicity`=="Chinese American"]<-"Non-URM"
Cleaned$`URM`[Cleaned$`Student Ethnicity`=="East Indian"]<-"URM"
Cleaned$`URM`[Cleaned$`Student Ethnicity`=="East Indian/Pakistani"]<-"URM"
Cleaned$`URM`[Cleaned$`Student Ethnicity`=="Hispanic"]<-"URM"
Cleaned$`URM`[Cleaned$`Student Ethnicity`=="Indian"]<-"URM"
Cleaned$`URM`[Cleaned$`Student Ethnicity`=="Other"]<-"Unknown"
Cleaned$`URM`[Cleaned$`Student Ethnicity`=="Other-Chinese/ American"]<-"Non-URM"
Cleaned$`URM`[Cleaned$`Student Ethnicity`=="Other-see note section"]<-"Unknown"
Cleaned$`URM`[Cleaned$`Student Ethnicity`=="Unknown"]<-"Unknown"
Cleaned$`URM`[is.na(Cleaned$`Student Ethnicity`)]<-"Unknown"




table(Cleaned$`URM`) #Better

sum(is.na(Cleaned$`URM`))


table(Cleaned$`Citizenship Status`) #Looks Good

# Categorize as Math and Not Math
unique(Cleaned$`Bachelor's Major`) #Some of these are not spelled correctly, but most are
Cleaned$`Bachelor's Major`[grepl("Math",Cleaned$`Bachelor's Major`,ignore.case=TRUE)]<-"Math" #Find "math" inside a statement and replace with "math"
Cleaned$`Bachelor's Major`[grepl("Mahematics",Cleaned$`Bachelor's Major`,ignore.case=TRUE)]<-"Math"
Cleaned$`Bachelor's Major`<-factor(Cleaned$`Bachelor's Major`) #Create factor
levels(Cleaned$`Bachelor's Major`)
levels(Cleaned$`Bachelor's Major`)[c(seq(from=1, to=18),seq(from=20, to=25))]<- "Not Math"
table(Cleaned$`Bachelor's Major`) #Looks Good
Cleaned$`Bachelor's Major`[is.na(Cleaned$`Bachelor's Major`)]<-"Unknown"
table(Cleaned$`Bachelor's Major`)

#Update GRE Scores, some scores are from the old GRE scoring method
library(plyr)
Cleaned$`GREQ` <- mapvalues(Cleaned$`GRE Quantitative Score`, from = c(800,790,780,770,760,750,740,730,720,710,700,690,680,670,660,650,640,630,620,610,600,590,580,570,560,550),
                            to = c(166, 164, 163, 161, 160, 159, 158, 157, 156, 155, 155, 154, 153, 152, 152, 151, 151, 150, 149, 149, 148, 148, 147, 147, 146, 146))


Cleaned$`GREV` <- mapvalues(Cleaned$`GRE Verbal Score`, from = c(800,790,780,770,760,750,740,730,720,710,700,690,680,670,660,650,640,630,620,610,600,590,580,570,560,550,540,530,520,510,500,490,480,470,460,450,440,430,420,410,400,390,380,370,360,350,340,330,320,310,300,290,280,270,260,250,240,230,220,210,200),
                            to = c(170,170,170,170,170,169,169,168,168,167,166,165,165,164,164,163,162,162,161,160,160,159,158,158,157,156,156,155,154,154,153,152,152,151,151,150,149,149,148,147,146,146,145,144,143,143,142,141,140,139,138,137,135,134,133,132,131,130,130,130,130))
summary(Cleaned$`GREQ`)
summary(Cleaned$`GREV`)



#Examine GPA
unique(Cleaned$`Bachelor's GPA`)
Cleaned$`Bachelor's GPA`[Cleaned$`Bachelor's GPA`>4.0]<-NA
summary(Cleaned$`Bachelor's GPA`)

#Examine Math GPA
unique(Cleaned$`Math Undergrad GPA`)
Cleaned$`Math Undergrad GPA`[is.na(Cleaned$`Math Undergrad GPA`)]
table(Cleaned$`Math Undergrad GPA`)
Cleaned$`MUGGPA`<-as.numeric(Cleaned$`Math Undergrad GPA`)
Cleaned$`MUGGPA`[Cleaned$`MUGGPA`==0]<-NA
summary(Cleaned$MUGGPA)


#Examine Institutions
unique(Cleaned$`Bacherlor's Institution Grouping`)
Cleaned$`Bacherlor's Institution Grouping`[is.na(Cleaned$`Bacherlor's Institution Grouping`)]<-"Other"
table(Cleaned$`Bacherlor's Institution Grouping`)


# PHD finished in 7 years?
Cleaned$`Finished Within 7 Years`<-as.numeric(Cleaned$`Time to PhD`)
unique(Cleaned$`Finished Within 7 Years`)
Cleaned$`Finished Within 7 Years`[is.na(Cleaned$`Time to PhD`)] <-"N"
Cleaned$`Finished Within 7 Years`[as.numeric(Cleaned$`Time to PhD`)<=7]<-"Y"
Cleaned$`Finished Within 7 Years`[as.numeric(Cleaned$`Time to PhD`)>7]<-"N"
table(Cleaned$`Finished Within 7 Years`)
