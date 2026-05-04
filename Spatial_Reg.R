# install.packages(c("spdep", "spatialreg", "sf"))
library(spdep)
library(spatialreg)
library(sf)

# Load an example shapefile (columbus dataset built into spdep)
columbus_data <- st_read("~/Downloads/columbus.shp", quiet = TRUE)

# 1. Create a list of neighbors based on Queen contiguity (shared borders/vertices)
neighbors <- poly2nb(columbus_data)

# 2. Convert the neighbors list into a spatial weights list (W style standardizes row sums)
weights_list <- nb2listw(neighbors, style="W", zero.policy=TRUE)

# 3. Let's model crime against housing values (income and housing values)
ols_model <- lm(CRIME ~ HOVAL + INC, data = columbus_data)
summary(ols_model)

# 4. Lagrange Multiplier tests for spatial lag and spatial error dependence
lm.LMtests(ols_model, weights_list, test="all") #older code
lm.RStests(ols_model, weights_list, test="all")

# If the tests are significant, proceed to Step 5 or 6.

# 5. Fit the Spatial Error Model
error_model <- errorsarlm(CRIME ~ HOVAL + INC, data = columbus_data, listw = weights_list)
summary(error_model)

# 6. Fit the Spatial Lag Model
lag_model <- lagsarlm(CRIME ~ HOVAL + INC, data = columbus_data, listw = weights_list)
summary(lag_model)

# 7. comparing AIC
AIC(ols_model, error_model, lag_model)

