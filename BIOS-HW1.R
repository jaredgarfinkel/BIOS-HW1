exercise<-read.csv(C:\\Users\\jared\\OneDrive\\Fall 2019\\Biostatistics\\HW1\\BIOS-HW1\\Exercise.xlsx)
?read.xl
?read.table
exercise<-read.table(C:\\Users\\jared\\OneDrive\\Fall 2019\\Biostatistics\\HW1\\BIOS-HW1\\Exercise.xlsx)
?read.csv
exercise<-read.delim(C:\Users\jared\OneDrive\Fall 2019\Biostatistics\HW1\BIOS-HW1\Exercise.xlsx)
read.xl(C:\Users\jared\OneDrive\Fall 2019\Biostatistics\HW1\BIOS-HW1\Exercise.xlsx)
?read.xl
??read.xl
library(tidyverse)
exercise<-read.csv("C:\\Users\\jared\\OneDrive\\Fall 2019\\Biostatistics\\HW1\\BIOS-HW1\\Exercise.csv")
summary(exercise)
?read.csv
exercise<-read.csv("C:\\Users\\jared\\OneDrive\\Fall 2019\\Biostatistics\\HW1\\BIOS-HW1\\Exercise.csv", header = FALSE)
summary(exercise)
exercise<-read.csv("C:\\Users\\jared\\OneDrive\\Fall 2019\\Biostatistics\\HW1\\BIOS-HW1\\Exercise.csv", col.names = ifelse(is.character(1),ifelse(is.character(2),3)))
?ifelse
exercise<-read.csv("C:\\Users\\jared\\OneDrive\\Fall 2019\\Biostatistics\\HW1\\BIOS-HW1\\Exercise.csv", skip = 1)
?row()
?row.names
?col
summary(exercise)
exercise<-read.csv("C:\\Users\\jared\\OneDrive\\Fall 2019\\Biostatistics\\HW1\\BIOS-HW1\\Exercise.csv", skip = 1)
?table
library(arsenal)
library(dplyr)
mean(exercise$Age)
mean(~age+gender+race, data=exercise)
tab1<-tableby( ~ Age + Gender + Race, data=exercise)
summary(tab1)
exercise<-read.csv("C:\\Users\\jared\\OneDrive\\Fall 2019\\Biostatistics\\HW1\\BIOS-HW1\\Exercise.csv", skip = 1)
tab1<-tableby( ~ Age + Gender + Race, data=exercise)
summary(tab1)
tab1<-tableby(~Age+Gender+Race+HTN+T2DM+Depression, data=exercise)
summary(tab1)
gender<-exercise %>% 
  mutate(Race=factor(Race, labels=c("African American","Hispanic","African American","Caucasian","Other","Other")))
tab1<-tableby(~Age+Gender+Race+HTN+T2DM+Depression+Smokes, data=exercise)
gender<-exercise %>%
  mutate(Gender=factor(Gender, labels=c("Male", "Female")))
summary(gender)
race<-gender %>%
  mutate(Race=factor(Race, labels=c("African American", "Hispanic", "Caucasian")))
htn<-race %>%
  mutate(HTN=factor(HTN, labels=c("Hypertensive","Not Hypertensive")))
t2dm<-htn %>%
  mutate(T2DM=factor(T2DM, labels=c("Type 2 Diabetes", "No Type 2 Diabetes")))
last_tbl<-t2dm %>%
  mutate(Depression=factor(Depression, labels=c("Depression","No Depression"))) %>%
  mutate(Smokes=factor(Smokes,labels=c("Smokes","Doesn't smoke")))
summary(last_tbl)
tbl2<-tableby(~Age+Gender+Race+HTN+T2DM+Depression+Smokes, data=last_tbl)
summary(tbl2)
