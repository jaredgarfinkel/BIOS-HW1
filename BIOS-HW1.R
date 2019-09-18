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
?summary
table1<-exercise %>%
  mutate(Group=factor(Group, labels=c("Intervention","Control"))) %>%
  mutate(Gender=factor(Gender, labels=c("Male", "Female"))) %>%
  mutate(Race=factor(Race, labels=c("African American", "Hispanic", "Caucasian"))) %>%
  mutate(HTN=factor(HTN, labels=c("Yes","No"))) %>%
  mutate(T2DM=factor(T2DM, labels=c("Yes", "No"))) %>%
  mutate(Depression=factor(Depression, labels=c("Yes","No"))) %>%
  mutate(Smokes=factor(Smokes,labels=c("Yes","No")))
summary(table1)
solution1a<-tableby(~Age+Gender+Race+HTN+T2DM+Depression+Smokes, data=table1,control = my_controls)
summary(exercise)
library(tableHTML)
tableHTML(df, 
          rownames = FALSE,
          second_header = list(c(1, 2, 2), c('Metabolic Parameters', 'Intervention', 'Control')))
install.packages(tableHTML)
names(df) <- c(" ", "Baseline", "6-month", "Baseline", "6-month")
table(df, 
      rownames = FALSE,
      second_header = list(c(1, 2, 2), c('Metabolic Parameters', 'Intervention', 'Control')))
names(exercise) <- c(" ", "Baseline", "6-month", "Baseline", "6-month")
table(exercise,rownames=FALSE,second_header = list(c(1,2,3),c("Metablic Parameters","Intervention","Control")))
summary(exercise)
exercise<-read.csv("C:\\Users\\jared\\OneDrive\\Fall 2019\\Biostatistics\\HW1\\BIOS-HW1\\Exercise.csv", skip = 1)
summary(exercise)
tableB<-tableby(Group ~ PRE, POST, PRE.1, POST.1,PRE.2,POST.2)
?tableby.control
abs(pull(exercise, PRE))
?cbind
?pull
mp_df <- tibble(
  Sys_u = mean(pull(GROUP~table2,PRE)),
  Sys_m = median(pull(GROUP~table2,PRE)),
  Sys_sd = sd(pull(GROUP~table2,PRE)),
  Sys_iqr = iqr(GROUP~pull(table2,PRE)),
  Sys_u2 = mean(GROUP~pull(table2,POST)),
  Sys_m2 = median(GROUP~pull(table2,POST)),
  Sys_sd2 = sd(GROUP~pull(table2,POST)),
  Sys_iqr2 = iqr(GROUP~pull(table2,POST))
)
mp_df <- tibble(
    Sys_u = mean(pull(table2,PRE)),
    Sys_m = median(pull(table2,PRE)),
    Sys_sd = sd(pull(table2,PRE)),
    Sys_iqr = iqr(pull(table2,PRE)),
    Sys_u2 = mean(pull(table2,POST)),
    Sys_m2 = median(pull(table2,POST)),
    Sys_sd2 = sd(pull(table2,POST)),
    Sys_iqr2 = iqr(pull(table2,POST))
)
mp_sys <- tableby(Group ~ PRE, data = exercise)
summary(mp_sys)
library(arsenal)
library(tidyverse)
library(dplyr)
summary(mp_sys)
df_sys <- table(
  int_u = mean(mp_sys),
  con_u = mean(mp_sys)
)
int_u
df_sys
?mean
mean(PRE)
mean(pull(table2,PRE))
mean(pull(exercise,POST))
data <- c(Group ~ PRE, data=exercise)
summary(data)
data <- c(Group ~ PRE, data=exercise)
data <- c(PRE ~ Group, data=exercise)
summary(data)
data <- col(exercise,Group ~ PRE)
?col
data <- col(c(Group~PRE))
?factor
factor(Group)
factor(pull(exercise,Group))
exp <- factor(pull(exercise,Group))
levels(exp)
group_labels <- list('0' = "Control", '1' = "Intervention")
Group_exp <- c(exp, labelTranslations = group_labels)
Group_exp
PRE2 <- factor(c(pull(exercise, PRE),(pull(exercise,Group))))
PRE2
tablex <- tableby(Group ~ PRE, data=exercise)
tablex
summary(tablex, total=FALSE, test=FALSE)
tbl <- table(pull(exercise,Group),pull(exercise,PRE))
summary(tbl)
prop.table(tbl,1)
prop.table(tbl,2)
tbl
table(tbl)
hist(tbl)
pre <- split(pull(exercise,PRE),exp)
pre
pull(pre,0)
pre$'0'
pull(pre,'0')
df_good <- tibble(
  pre = split(pull(exercise,PRE),exp),
  post = split(pull(exercise,POST),exp),
  pre.1 = split(pull(exercise,PRE.1),exp),
  post.1 = split(pull(exercise,POST.1),exp),
  pre.2 = split(pull(exercise,PRE.2),exp),
  post.2 = split(pull(exercise,POST.2),exp),
  pre.3 = split(pull(exercise,PRE.3),exp),
  post.3 = split(pull(exercise,POST.3),exp),
  pre.4 = split(pull(exercise,PRE.4),exp),
  post.4 = split(pull(exercise,POST.4),exp),
  pre.5 = split(pull(exercise,PRE.5),exp),
  post.5 = split(pull(exercise,POST.5),exp),
)
mp_df <- tibble(
  Sys_u = mean(df_good$pre$'0'),
  Sys_m = median(df_good$pre$'0'),
  Sys_sd = sd(df_good$pre$'0'),
  Sys_u2 = mean(df_good$post$'0'),
  Sys_m2 = median(df_good$post$'0'),
  Sys_sd2 = sd(df_good$post$'0'),
)
df_good$post$'0'
pre
pre.1
df_good
summary(df_good,total=FALSE,test=FALSE)
mp_df$Sys_m$'0'
?median
??median
df_good$post$'0'
mp_df$Sys_iqr
?quartile
?quantile
quantile(df_good$pre$'0')
sys_m <- median(df_good$pre$'0')
sys_m
my_labels2 <-
summary(df_good)
my_labels2 <- list(PRE = "Systolic Blood Pressure (mm Hg)", POST = " ",
                   PRE.1 = "Diastolic Blood Pressure (mm Hg)", POST.1 = " ",
                   PRE.2 = "Body Mass Index", POST.2 = " ",
                   PRE.3 = "HDL Cholesterol", POST.3 = " ",
                   PRE.4 = "LDL Cholesterol", POST.4 = " ",
                   PRE.5 = "Blood Glucose (mmol/L)", POST.5 = " ")
summary(df_good, labelTranslations = my_labels2)
my_controls2 <- tableby.control(
  total = FALSE,
  test = FALSE,  # No test p-values yet
  numeric.stats = c("meansd", "medianq1q3"),
  cat.stats = c("countpct"),
  stats.labels = list(
    meansd = "Mean (SD)",
    medianq1q3 = "Median (Q1, Q3)",
    countpct = "N (%)"))
solution1b <- tableby(Group ~ PRE + POST + PRE.1 + POST.1 + 
                        PRE.2 + POST.2 + PRE.3 + POST.3 + 
                        PRE.4 + POST.4 + PRE.5 + POST.5,
                      data = exercise, control = my_controls2)
summary(solution1b, title="Metabolic Parameters", labelTranslations = my_labels2, text=TRUE)
exp <- factor(pull(exercise,Group))

df_good <- tibble(
  pre = split(pull(solution1b,PRE),exp),
  post = split(pull(solution1b,POST),exp),
  pre.1 = split(pull(solution1b,PRE.1),exp),
  post.1 = split(pull(solution1b,POST.1),exp),
  pre.2 = split(pull(solution1b,PRE.2),exp),
  post.2 = split(pull(solution1b,POST.2),exp),
  pre.3 = split(pull(solution1b,PRE.3),exp),
  post.3 = split(pull(solution1b,POST.3),exp),
  pre.4 = split(pull(solution1b,PRE.4),exp),
  post.4 = split(pull(solution1b,POST.4),exp),
  pre.5 = split(pull(solution1b,PRE.5),exp),
  post.5 = split(pull(solution1b,POST.5),exp),
)
summary(df_good)
library(stargazer)
?mean
?mutate
grouping %>%
group_by(Group)
           mutate(mean(PRE), na.rm=TRUE)
?group_by
group_by(solution1b,Group)           
group_by(exercise,exercise$Group)

mutate(exercise$PRE = factor(exercise$PRE, labels = c("Intervention","Control")))
exercise %>%
  group_by(Group) %>%
  mutate(PRE = split(PRE, Group))
exercise<-read.csv("C:\\Users\\jared\\OneDrive\\Fall 2019\\Biostatistics\\HW1\\BIOS-HW1\\Exercise.csv", skip = 1)
grouped_data <- exercise %>% group_by(Group)
grouped_data %>% summarize(
  pre = mean(PRE),
  post = mean(POST),
  pre.1 = mean(PRE.1),
  post.1 = mean(POST.1),
  pre.2 = mean(PRE.2),
  post.2 = mean(POST.2),
  pre.3 = mean(PRE.3),
  post.3 = mean(POST.3),
  pre.4 = mean(PRE.4),
  post.4 = mean(POST.4),
  pre.5 = mean(PRE.5),
  post.5 = mean(POST.5),
)
grouped_data %>% s
df_good <- exercise %>% group_by(Group, add = TRUE)
df_good
df_good2 <- tibble(exercise %>%
                      mutate(
  pre = mean(PRE),
  post = mean(POST),
  pre.1 = mean(PRE.1),
  post.1 = mean(POST.1),
  pre.2 = mean(PRE.2),
  post.2 = mean(POST.2),
  pre.3 = mean(PRE.3),
  post.3 = mean(POST.3),
  pre.4 = mean(PRE.4),
  post.4 = mean(POST.4),
  pre.5 = mean(PRE.5),
  post.5 = mean(POST.5)))
summary(df_good2)
exercise<-read.csv("C:\\Users\\jared\\OneDrive\\Fall 2019\\Biostatistics\\HW1\\BIOS-HW1\\Exercise.csv", skip = 1)
library(tidyverse)
library(dplyr)
library(arsenal)
grouped_data2 <- exercise %>% group_by(Group, add = TRUE)
summary(grouped_data2)
grouped_data2 <- tibble(exercise %>%
                          mutate(PRE = group_by(Group))
)
grouped_data2 <- exercise %>% 
  group_by(Group)
grouped_data2 %>% summarize(
  pre = mean(pull(grouped_data2,PRE)),
  pre1 = mean(PRE$'1'),
  post = mean(PRE$'0'),
  post1 = mean(POST$'1')
)
summary(pull(grouped_data,PRE)
exercise$PRE
?cbind
grouped_data %>% tibble(
  mutate(PRE = factor(PRE))
)
group1 <- tableby(Group ~ PRE, data=exercise)
summary(group1)
groupsys1 <- tableby(Group ~ PRE, data=exercise, control = my_controls2)


  --- | Intervention |         | Control |     |    
  --- | ------------ | ------- | ------- | --- |
  --- | Baseline | 6-months | Baseline | 6-months
Systolic Blood Pressure (mm Hg) | `r mean(groupsys1$'0')` +/- `r sd(groupsys1$'0')` | `r mean(groupsys1$'1')` +/- `r sd(groupsys1$'1')` | `r mean(groupsys2$'0')` +/- `r sd(groupsys2$'0')` | `r mean(groupsys2$'1')` +/- `r sd(groupsys2$'1')`
?list
?round
medianq1q3(exercise$PRE)
exercise<-read.csv("C:\\Users\\jared\\OneDrive\\Fall 2019\\Biostatistics\\HW1\\BIOS-HW1\\Exercise.csv", skip = 1)
?q1q3
?q1q3
?filter
?filter
?dplyr$filter
?filter
?difference
?subtract
?mar
??mar
?par
?boxplot
?mfrow
?ggplot
library(ggplot2)
?ggplot
?subset
?group
low_birth_all<-read.csv("C:\\Users\\jared\\OneDrive\\Fall 2019\\Biostatistics\\Recitation\\20191011\\lowbwt_ALL.csv")
low_birth_all$smoking <- ifelse(low_birth_all$smoke=="0","Non-Smoker","Smoker")
ggplot(low_birth_all, aes(x = race, y = bwt, fill = smoking)) +
  geom_boxplot() +
  scale_fill_manual(values = c("blue", "red"),
                    labels = expression("Non-Smokers", "Smokers")) +
  xlab("") +
  ylab("Birthweight (g)") +
  theme(legend.position = "top", legend.title = element_blank())
boxplot(bwt~smoke, data=low_birth_all, col=c("blue","red"))
?cbind
?subset
?select
??select
?boxplot
?par
?c
?rownames
?pin
?par('pin')
?prod
?quotient
?divide
library(tidyverse)
exercise <- read.csv(file = "./Exercise.csv", skip = 1)
?colnames
?cbind
?col
?list?
?labels
?labels
