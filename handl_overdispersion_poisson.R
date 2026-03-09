#handling over dispersion
#poisson model
modelP <- glm(total ~ age , 
               family = poisson,
               data = fHH1)
summary(modelP)
AIC(modelP)


# Check for overdispersion

# A ratio significantly > 1 suggests overdispersion
dispersion_ratio <- modelP$deviance / modelP$df.residual
print(dispersion_ratio)

#handling over dispersion

modelQP <- glm(total ~ age , 
               family = quasipoisson,
               data = fHH1)

summary(modelQP)
AIC(modelQP)

# negatif binomial
library(MASS)
modelNB <- glm.nb(total ~ age , 
                  data = fHH1)
AIC(modelNB)
summary(modelNB)

#fit the ZIP model
zip_model <- zeroinfl(total ~ age | age, data = fHH1)

# Fit the ZINB model
# count is the dependent variable (number of fish caught)
# child and camper are predictors for both the count and zero-inflation parts

zinb_model <- zeroinfl(total ~ age | age, data = fHH1, dist = "negbin")
summary(zinb_model)

#comparing model fit
AIC(modelP,modelNB,zip_model,zinb_model)
