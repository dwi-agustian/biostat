#coding biostatistika dasar
# Load example data 
mydata<-read.csv("https://raw.githubusercontent.com/plotly/datasets/master/diabetes.csv")
library(tidyverse)
#mempelajari struktur dan variable dataset
str(mydata)
glimpse(mydata)
#Pregnancies: Represents the number of pregnancies.
#Glucose: Indicates the glucose level in the blood.
#BloodPressure: Measures blood pressure.
#SkinThickness: Represents the thickness of the skin.
#Insulin: Indicates the insulin level in the blood.
#BMI: Represents the body mass index.
#DiabetesPedigreeFunction: Measures the likelihood of diabetes based on genetic factors.
#Age: Represents the age of the individual.
#Outcome: Indicates the final diagnosis (1 = Yes, diabetic; 0 = No, non-diabetic).

#melihat variable di dataset my data
names(mydata)
table(mydata$Outcome)

# normality test for small sample (3-5000)
shapira.test()
shapiro.test(mydata$Age)
shapiro.test(mydata$BMI)
shapiro.test(mydata$Glucose)

#for big sample
library(nortest)
lillie.test(mydata$Age)

#p-value is the probability that the H0 is true
#H0 in this test: data = normally distributed
#If the p-value is greater than 0.05: 
#You fail to reject the null hypothesis. 
#There is no statistically significant evidence to conclude
#that the data deviates from a normal distribution.

#If the p-value is less than or equal to 0.05: 
#You reject the null hypothesis. 
#There is statistically significant evidence to conclude 
#that the data are not normally distributed.

#checking histogram of variable age
hist(mydata$Age)

#creating new variable for age : Lansia or not
mydata$lansia  = 1
mydata$lansia [mydata$Age < 60] = 0

table(mydata$Outcome)
# Create a contingency table
contingency_table <- table(mydata$lansia, mydata$Outcome)
print(contingency_table)

#labeling & factoring variable
mydata$lansia <- factor(mydata$lansia, 
                      levels = c(0, 1), 
                      labels = c("Non-Lansia","Lansia"))

mydata$Outcome <- factor(mydata$Outcome, 
                      levels = c(0, 1), 
                      labels = c("No-Diabetes","Diabetes"))

# Calculate proportions relative to the entire table
total_proportions <- prop.table(contingency_table)
print(total_proportions*100)

# Calculate proportions by row
row_proportions <- prop.table(contingency_table, margin = 1)
print(row_proportions*100)

# Calculate proportions by column
col_proportions <- prop.table(contingency_table, margin = 2)
print(col_proportions*100)

#chi square test
chisq.test(contingency_table)

#boxplot
#membuat grafik perbedaan distribusi umur/Age vs kelompok Outcome
boxplot(mydata$Age~mydata$Outcome)

#membuat grafik perbedaan distribusi BMI vs kelompok Outcome
boxplot(mydata$BMI~mydata$Outcome)

#parametrict test (data yg terdistribusi normal)
#t test
t.test(Age~Outcome, data=mydata)

#non parametric test
#independent
wilcox.test(Age ~ Outcome, data=mydata)
wilcox.test(BMI ~ Outcome, data=mydata)

#check ulang kategory variable outcome
table(mydata$Outcome)

#repeated measures (paired)
wilcox.test(V1, V2, paired=TRUE)

#uji statistik untuk lebih dari 2 kelompok
kruskal.test(Ozone ~ Month, data = airquality)

#mensave object menjadi file csv di komputer kita
write.csv(mydata,"dmdata.csv")

#melihat nama2 variabel
names(mydata)

#korelasi antar variabel numerik
#grafik scatter plot
#install package ggplot bila belum ada
install.packages("ggplot2")
#aktivasi package ggplot
library(ggplot2)

#membuat grafik scatter plot
ggplot(mydata, aes(x=Age, y=Glucose)) + 
  geom_point()

ggplot(mydata, aes(x=Insulin, y=Glucose)) + 
  geom_point()

#menambahkan garis linear
ggplot(mydata, aes(x=Age, y=Glucose)) + 
  geom_point() + geom_smooth(method=lm, color="red", se=FALSE)

ggplot(mydata, aes(x=Insulin, y=Glucose)) + 
  geom_point() + geom_smooth(method=lm, color="magenta", se=FALSE)

#garis linear + confidence interval(standar error)+point size
ggplot(mydata, aes(x=Age, y=Glucose)) + 
  geom_point(size=0.5) + geom_smooth(method=lm, 
                                     color="red", se=TRUE)

ggplot(mydata, aes(x=Insulin, y=Glucose)) + 
  geom_point() + geom_smooth(method=lm, 
                             color="magenta", se=TRUE)

#garis linear + confidence interval(standar error)+point size
ggplot(mydata, aes(x=Age, y=Glucose)) + 
  geom_point(size=0.5) + geom_smooth(method=loess, 
                                     color="red", se=TRUE)

#Kedua variabel numerik terdistribusi normal
cor.test(mydata$Age, mydata$Glucose, 
         method = "pearson", use = "complete.obs")

cor.test(mydata$Insulin, mydata$Glucose, 
         method = "pearson", use = "complete.obs")

#Minimal salah satu var numerik tidak terdistribusi normal
cor.test(mydata$Age, mydata$Glucose, 
         method = "spearman", use = "complete.obs")

cor.test(mydata$Insulin, mydata$Glucose, 
         method = "spearman", use = "complete.obs")

cor.test(mydata$SkinThickness, mydata$BloodPressure, 
         method = "spearman", use = "complete.obs")

library("ggpubr")

ggscatter(mydata, x = "SkinThickness", y = "BloodPressure", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Skin Thickness (mm)", ylab = "Blood Pressure (cmHg)")
names(mydata)


#multivariable model
#single linier regression y=pef, x=age, y~x1
smod1 = lm (Glucose~Age , data=mydata)

#melihat hasil analisis
summary(smod1)

plot(mydata$Age,mydata$Glucose)
AIC(smod1)

smod2 = lm (Glucose~BloodPressure , data=mydata)
summary(smod2)
AIC(smod2)

smod3 = lm (Glucose~DiabetesPedigreeFunction,
            data=mydata)
summary(smod3)
AIC(smod3)

#multiple linear regression
mmod = lm(Glucose~Age+BloodPressure+DiabetesPedigreeFunction, data=mydata)
summary(mmod)
AIC(mmod)
