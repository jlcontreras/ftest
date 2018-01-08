# The dataset contains info about pollution levels in different cities of the USA.
df <- read.csv("air_pollution.csv")

# Lets see if we can model SO2 as dependent on the population of each city and
# the number of raining days
model.full <- lm(SO2 ~ popul + precip, data = df) 

# TODO: Interpret the p-values of each of the variables
summary(model.full)

# Now let us consider the reduced model, that is, b1 = b2 = 0
# (so we only have an intercept, which is the mean)

# COMMENTED OUT BECAUSE NOW THE REDUCED MODEL IS ONLY THE INTERCEPT
# TODO: DOES IT MAKE SENSE? OR SHOULD THE REDUCED MODEL BE THE ONE BELOW?
#model.reduced <- lm(SO2 ~ popul, data = df)
#fitted.reduced <- predict(model.reduced, df[,c("popul","precip")])
#sser <- sum((df$SO2 - fitted.reduced)^2) # Reduced model

# Fit both models to our data, in order to get the error sum of squares of both models
fitted.full    <- predict(model.full,    df[,c("popul","precip")])

ssef <- sum((df$SO2 - fitted.full)^2)        # Full model
sser <- sum((df$precip - mean(df$precip))^2) # Reduced model, which is only Y = mean(y)


# Calculate the F-statistic
k <- 2        # Number of vars used in the full model
n <- nrow(df) # Number of observations
F <- (ssef / k) / (sser / (n - k - 1))

# Now, how to get from the F-statistic to a p-value??