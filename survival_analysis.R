library(survival)
# Check out the help page for this dataset
help(GBSG2, package = "TH.data")

# Load the data
data(GBSG2, package = "TH.data")

# Look at the summary of the dataset
summary(GBSG2)

# Count censored and uncensored data
num_cens <- table(GBSG2$cens)
num_cens

# Create barplot of censored and uncensored data
barplot(num_cens)

# Create time and event data
time <- c(5, 6, 2, 4, 4)
event <- c(1, 0, 0, 1, 1)

# Compute Kaplan-Meier estimate
km <- survfit(Surv(time, event) ~ 1)
km

# Take a look at the structure
str(km)

# Create data.frame
data.frame(time = km$time, n.risk = km$n.risk, n.event = km$n.event,
           n.censor = km$n.censor, surv = km$surv)


# Create dancedat data
dancedat <- data.frame(
  name = c("Chris", "Martin", "Conny", "Desi", "Reni", "Phil", 
           "Flo", "Andrea", "Isaac", "Dayra", "Caspar"),
  time = c(20, 2, 14, 22, 3, 7, 4, 15, 25, 17, 12),
  obs_end = c(1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0))

# Estimate the survivor function pretending that 
# all censored observations are actual observations.
km_wrong <- survfit(Surv(time) ~ 1, data = dancedat)

# Estimate the survivor function from this dataset 
# via kaplan-meier.
km <- survfit(Surv(time, obs_end) ~ 1, data = dancedat)

library(survminer)
# Plot the two and compare
ggsurvplot_combine(list(correct = km, wrong = km_wrong))


# Kaplan-Meier estimate
km <- survfit(Surv(time, cens) ~ 1, data = GBSG2)

# Plot of the Kaplan-Meier estimate
ggsurvplot(km)

# Add the risk table to plot
ggsurvplot(km, risk.table = TRUE)

# Add a line showing the median survival time
ggsurvplot(km, risk.table =TRUE,   surv.median.line= "hv")

# Weibull model
wb <- survreg(Surv(time, cens) ~ 1, data = GBSG2)

# Compute the median survival from the model
predict(wb, type = "quantile", p = 1-0.5, 
        newdata = data.frame(1))

# 70 Percent of patients survive beyond time point...
predict(wb, type = 'quantile', p = 1 - 0.7, 
        newdata = data.frame(1))


# Retrieve survival curve from model probabilities 
surv <- seq(.99, .01, by = -.01)

# Get time for each probability
t <- predict(wb, type = "quantile", p = 1 - surv, newdata = data.frame(1))

# Create data frame with the information
surv_wb <- data.frame(time = t, surv = surv)

# Look at first few lines of the result
head(surv_wb)

# Weibull model
wb <- survreg(Surv(time, cens) ~ 1, data=GBSG2)

# Retrieve survival curve from model
surv <- seq(.99, .01, by = -.01)

# Get time for each probability
t <- predict(wb, type = "quantile", p = 1- surv, newdata = data.frame(1))

# Create data frame with the information needed for ggsurvplot_df
surv_wb <- data.frame(time = t, surv = surv, 
                      upper = NA, lower = NA, std.err = NA)



# Correct way to set line size and linetype
library(ggplot2)

ggplot(surv_wb, aes(x = time, y = surv)) +
  geom_line(size = 1, linetype = 1, color='red')



# Weibull model
wbmod <- survreg(Surv(time, cens) ~ horTh, data = GBSG2)
coef(wbmod)
table(GBSG2$horTh)
# Retrieve survival curve from model
surv <- seq(.99, .01, by = -.01)
t_yes <- predict(wbmod, type = "quantile", p = 1-surv,
                 newdata = data.frame(horTh = 'yes'))

# Take a look at survival curve
str(t_yes)

# Weibull model
wbmod <- survreg(Surv(time, cens) ~ horTh + tsize, data = GBSG2)

# Imaginary patients
newdat <- expand.grid(
  horTh = levels(GBSG2$horTh),
  #menset up 3 ukuran tumor dengan q0.25, q0.5, & q0.75
  tsize = quantile(GBSG2$tsize, probs = c(0.25, 0.5, 0.75)))

# Compute survival curves by predict the time
surv <- seq(.99, .01, by = -.01)
t <- predict(wbmod, type = "quantile", p = 1-surv,
             newdata = newdat)

# How many rows and columns does t have?
dim(t)

# Use cbind() to combine the information in newdat with t
surv_wbmod_wide <- cbind(newdat, t)
install.packages("reshape2")
library(reshape2)
# Use melt() to bring the data.frame to long format
surv_wbmod <- melt(surv_wbmod_wide, id.vars = c("horTh", "tsize"),
                   variable.name = "surv_id", value.name = "time")

# Use surv_wbmod$surv_id to add the correct survival probabilities surv
surv_wbmod$surv <- surv[as.numeric(surv_wbmod$surv_id)]

# Add columns upper, lower, std.err, and strata to the data.frame
surv_wbmod[, c("upper", "lower", "std.err", "strata")] <- NA

# Plot the survival curves
ggsurvplot_df(surv_wbmod, surv.geom = geom_line,
              linetype = "horTh", color = "tsize", 
              legend.title = NULL)


# Weibull model
wbmod <- survreg(Surv(time, cens) ~ horTh, data = GBSG2)

# Log-Normal model
lnmod <- survreg(Surv(time, cens) ~ horTh, data = GBSG2, 
                 dist = "lognormal")

# Newdata
newdat <- data.frame(horTh = levels(GBSG2$horTh))

# Surv
surv <- seq(.99, .01, by = -.01)

# Survival curve from Weibull model and log-normal model
wbt <- predict(wbmod, type = "quantile", p = 1 - surv, newdata = newdat)
lnt <- predict(lnmod, type = "quantile", p = 1- surv, newdata = newdat)



# Cox model
cxmod <- coxph(Surv(time, cens) ~ horTh + tsize,
               data = GBSG2)

# Imaginary patients
newdat <- expand.grid(
  horTh = levels(GBSG2$horTh),
  tsize = quantile(GBSG2$tsize, probs = c(0.25, 0.5, 0.75)))
rownames(newdat) <- letters[1:6]

# Compute survival curves
cxsf <- survfit(cxmod, data = GBSG2, newdata = newdat,
                conf.type = "none")
head(cxsf$surv)

# Compute data.frame needed for plotting
surv_cxmod0 <- surv_summary(cxsf)

# Get a character vector of patient letters (patient IDs)
pid <- as.character(surv_cxmod0$strata)

# Multiple of the rows in newdat so that it fits with surv_cxmod0
m_newdat <- newdat[pid, ]

# Add patient info to data.frame
surv_cxmod <- cbind(surv_cxmod0, m_newdat)

# Plot
ggsurvplot_df(surv_cxmod, linetype = "horTh", color = "tsize",
              legend.title = NULL, censor = FALSE)

#COX PH Model
names(GBSG2)
#menganalisis pengaruh hormonal terapi
cox_th = coxph(Surv(time, cens) ~ horTh, data=GBSG2)

print(cox_th)

#Menghitung Hazard Ratio (HR)
exp(-0.3640)

#check PH assumption: 
# a non-significant relationship between residuals and time
cox.zph(cox_th)

#check outliers
ggcoxdiagnostics(cox_th, type = "dfbeta",
                 linear.predictions = FALSE, 
                 ggtheme = theme_bw())

#menganalisis pengaruh usia
cox_age = coxph(Surv(time, cens) ~ age, data=GBSG2)
print(cox_age)

#multivariable model
mod1 = coxph(Surv(time, cens) ~ age + horTh + tsize + menostat + tgrade + 
               pnodes + progrec + estrec, data=GBSG2)
print(mod1)


#check PH assumption: 
# a non-significant relationship between residuals and time
cox.zph(mod1)

#memperlihatkan AIC dari model
print(paste0("mod1 AIC = ", round(AIC(mod1), 1)))

#check outliers
ggcoxdiagnostics(mod1, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())

