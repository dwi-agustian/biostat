#load library needed
library(gridExtra)
library(knitr)
library(mosaic)
library(xtable)
library(pscl) 
library(multcomp)
library(pander)
library(MASS)
library(tidyverse)
#data reading
library(readr)
fHH1 <- read_delim("https://raw.githubusercontent.com/dwi-agustian/biostat/refs/heads/main/fHH1.csv", 
                   delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(fHH1)
#Explaratory Data analysis
names(fHH1)

#visualisasi histogram dengan plot
hist(fHH1$total)

#visualisasi histogram dengan ggplot
ggplot(fHH1, aes(total)) + 
  geom_histogram(binwidth = .3, color = "red", 
                 fill = "blue") + 
  xlab("Number in the house excluding head of household") +
  ylab("Count of households")

#kategorisasi umur kepala keluarga
cuts = cut(fHH1$age,
           breaks=c(15,20,25,30,35,40,45,50,55,60,65,70))
ageGrps <- data.frame(cuts,fHH1)

#visualisasi per golongan umur kepala keluarga
ggplot(data = ageGrps, aes(x = total)) +
  geom_histogram(binwidth = .25, color = "black", 
                 fill = "white") +
  facet_wrap(cuts) +
  xlab("Household size")

## Checking linearity assumption: Empirical log of the means plot
sumStats <- fHH1 %>% group_by(age) %>% 
  summarise(mntotal = mean(total),
            logmntotal = log(mntotal), n=n())

ggplot(sumStats, aes(x=age, y=logmntotal)) +
  geom_point()+
  geom_smooth(method = "loess", size = 1.5)+
  xlab("Age of head of the household") +
  ylab("Log of the empirical mean number in the house") 

sumStats2 <- fHH1 %>% group_by(age, location) %>% 
  summarise(mntotal = mean(total),
            logmntotal = log(mntotal), n=n())

ggplot(sumStats2, aes(x=age, y=logmntotal, color=location,
                      linetype = location, shape = location)) +
  geom_point()+
  geom_smooth(method = "loess", se=FALSE)+
  xlab("Age of head of the household") +
  ylab("Log empirical mean household size") 

#poisson regression / count regression
model_p1 = glm(total ~ age, family = poisson, data = fHH1)

summary(model_p1)

#coefficient value
tabel_est = coef(summary(model_p1))

#mengeluarkan hasil estimate poisson regression dalam bentuk file csv
#yang bisa dibaca di excel
write.csv(tabel_est,"tabel_est.csv")

#identifikasi home folder tempat file terexport (write)
getwd()


#menghitung jumlah anggota kel dgn usia kepala keluarga baseline (0)
exp(-0.004705881*0) #hasilnya 1

#menghitung jumlah anggota kel dgn usia kepala keluarga lebih tua 1 tahun (1)
exp(-0.004705881*1) #hasilnya 0.9953052

#menghitung jumlah anggota kel dgn usia kepala keluarga  25 tahun (25 th)
exp(-0.004705881*25) #hasilnya 0.8890098

#menghitung jumlah anggota kel dgn usia kepala keluarga 20 tahun (20 th)
exp(-0.004705881*20) #hasilnya 0.9101757

#log jumlah anggota kel/ dengan KK usia 20 = 1.450876
(-0.0047059*20) + 1.54499422
# jumlah angg kel dgn KK usia 20 = exp(1.450876) = 4.266851
exp(1.450876)

#log jumlah anggota kel/ dengan KK usia 25 = 1.427347
(-0.0047059*25) + 1.54499422
# jumlah angg kel dgn KK usia 25 = exp(1.427347) = 4.167628
exp(1.427347)
# prediksi selisih jumlah angg kel kk usia 25 vs 20 = 3.257447
4.167628 - 4.266851
25-20

#selisih per tahunnya
-0.099223 /5

# identifikasi variabel lain
str(fHH1$location)
table(fHH1$location)

model_p3 = glm(total ~ age + location, family = poisson, data=fHH1)
summary(model_p3)

#residual deviance & dispersion
cat(" Residual deviance = ", summary(modela)$deviance, " on ",
    summary(modela)$df.residual, "df", "\n",
    "Dispersion parameter = ", summary(modela)$dispersion)

# Wald type CI by hand
#ekstraksi nilai Beta0 dari model 
beta0hat <- summary(model_p1)$coefficients[1,1] 
beta0hat

#ekstraksi nilai Beta1 dari model
beta1hat <- summary(model_p1)$coefficients[2,1]
beta1hat

#Kalkulasi rate ratio
exp(beta1hat)

#ekstraksi dari SE Beta1
beta1se <- summary(model_p1)$coefficients[2,2]

#penghitungan 95% CI dari Beta 1
beta1hat - 1.96*beta1se   # lower bound 
beta1hat + 1.96*beta1se   # upper bound 

#penghitungan 95% CI dari Rate Ratio
exp(beta1hat - 1.96*beta1se)
exp(beta1hat + 1.96*beta1se)

# CI for betas using profile likelihood
confint(model_p1)

#calculating CI for relative risk
exp(confint(model_p1))

# model0 is the null/reduced model
model0 <- glm(total ~ 1, family = poisson, data = fHH1)

# perform testing for comparing models
drop_in_dev <- anova(model0, model_p1, test = "Chisq")

drop_in_dev

# second order variable (quadratic term)
# creating variable (age square)
fHH1 <- fHH1 %>% mutate(age2 = age*age)

# model with quadratic term
model_p2 = glm(total ~ age + age2, family = poisson, 
              data = fHH1)

# comparing models
drop_in_dev <- anova(model_p1, model_p2, test = "Chisq")

drop_in_dev

# Adding covariate (location)
model_p3 = glm(total ~ age + age2 + location, 
               family = poisson, data = fHH1)

# comparing models
drop_in_dev <- anova(model_p2, model_p3, test = "Chisq")

drop_in_dev





