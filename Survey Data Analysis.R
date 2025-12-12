#"intall"
install.packages("gtsummary")
install.packages("readxl")
install.packages("ggplot2")

#library
library(survey)
library(readxl)
library(gtsummary)
library(dplyr)
library(ggplot2)

#set
setwd("./Desktop")
club<-read_excel("New club.xlsx",skip = 1)

#Factors
club$love_dep <- as.factor(club$love_dep)
#club$carr <- as.factor(club$carr)
club$hobby<- as.factor(club$hobby)
club$idea <- as.factor(club$idea)
club$sport <- as.factor(club$sport)
club$activity<- as.factor(club$activity)
club$class<- as.factor(club$class)
club$cgpa<- as.factor(club$cgpa)
club$fam_sup<- as.factor(club$fam_sup)
club$fri_sup<- as.factor(club$fri_sup)
#club$fac<-as.factor(club$fac)
summary(club)



#Table
club[,c(2:20)] %>% tbl_summary()

#Plots
ggplot(club, aes(carr )) + geom_bar()
ggplot(club, aes(hobby)) + geom_bar()
ggplot(club, aes(sport)) + geom_bar()
ggplot(club, aes(idea)) + geom_bar()
ggplot(club, aes(sex)) + geom_bar()
ggplot(club, aes(x="0", sex)) + geom_boxplot()

mean(club2$carr
     )
f <- data.frame(faculty=c("Eğitim", "Fen Edebiyat", "İktisadi ve İdari Bilimler", "Mimarlık", "Mühendislik"), fpc=c(1806,4423,2162,1412,9294))

club$fac<-ifelse(club$fac=="Fen Edebiyet","Fen Edebiyat", club$fac)
club$fac<-ifelse(club$fac=="İktisadi ve İdari Bilimler Fakültesi","İktisadi ve İdari Bilimler", club$fac)
#club$fac<-as.factor(club$fac)
#summary(club)

club2 <- left_join(club, f, by = c("fac"="faculty"))

table(club2$fac)



strat_design <- svydesign(id=~1, strata = ~fac, fpc =~fpc , data = club2)
strat_design

svymean(~carr,strat_design)

model1<-svyglm(carr~class, strat_design)
summary(model1)
table(club2$class)

model2<-svyglm(carr~live, strat_design)
summary(model2)
table(club2$class)
levels(club2$class)


model3<-svyglm(carr~love_dep, strat_design)
summary(model)
table(club2$class)

model4<-svyglm(carr~love_dep, strat_design)
summary(model)
table(club2$class)

-----------
data=1
genderdata <- data.frame(
  gender=c("Diğer","Erkek","Kadın","PNS"),
  value=c(table(club$sex)))
vvl<-val=c(table(club$sex))

students <- data.frame( names = c( "Bill", "Stacey", "Fred", "Jane", "Sarah" ), 
                        gender = c( "M", "F", "M", "F", "F" ),
                        stringsAsFactors = FALSE )
genderdata %>% 
  group_by(gender) %>% 
  summarise( percent = 100 * n() / nrow( genderdata$value ) )


# Basic piechart
ggplot(data, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)


#Means
group_by(club,sex)%>%summarise(n())
group_by(club2,fac)%>%summarise(n())
mhobby<-mean(club$hobby)
mcarr<-mean(club$carr)
msport<-mean(club$sport)
midea<-mean(club$idea)

mns<-c(mcarr,mhobby,msport,midea)
mns

#ggplot(apiclus2, aes(x="0", growth)) + geom_boxplot()#

rlan
