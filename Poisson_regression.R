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
#Explaratory Data analysis
names(fHH1)

#visualisasi
ggplot(fHH1, aes(total)) + 
  geom_histogram(binwidth = .25, color = "black", 
                 fill = "white") + 
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

#poisson regression
modela = glm(total ~ age, family = poisson, data = fHH1)

#coefficient value
coef(summary(modela))

#summary of the model
summary(modela)

#residual deviance & dispersion
cat(" Residual deviance = ", summary(modela)$deviance, " on ",
    summary(modela)$df.residual, "df", "\n",
    "Dispersion parameter = ", summary(modela)$dispersion)

# Wald type CI by hand
#ekstraksi dari Beta0
beta0hat <- summary(modela)$coefficients[1,1] 
beta0hat

#ekstraksi dari Beta1
beta1hat <- summary(modela)$coefficients[2,1]
beta1hat

#Kalkulasi rate ratio
exp(beta1hat)

exp(-0.0047)

#ekstraksi dari SE Beta1
beta1se <- summary(modela)$coefficients[2,2]
beta1hat - 1.96*beta1se   # lower bound 
beta1hat + 1.96*beta1se   # upper bound 
exp(beta1hat - 1.96*beta1se)
exp(beta1hat + 1.96*beta1se)

# CI for betas using profile likelihood
confint(modela)

#calculating CI for relative risk
exp(confint(modela))

# model0 is the null/reduced model
model0 <- glm(total ~ 1, family = poisson, data = fHH1)

# perform testing for comparing models
drop_in_dev <- anova(model0, modela, test = "Chisq")

drop_in_dev


# second order variable (quadratic term)
# creating variable (age square)
fHH1 <- fHH1 %>% mutate(age2 = age*age)

# model with quadratic term
modela2 = glm(total ~ age + age2, family = poisson, 
              data = fHH1)

# comparing models
drop_in_dev <- anova(modela, modela2, test = "Chisq")

drop_in_dev

# Adding covariate (location)
modela2L = glm(total ~ age + age2 + location, 
               family = poisson, data = fHH1)

# comparing models
drop_in_dev <- anova(modela2, modela2L, test = "Chisq")

drop_in_dev





