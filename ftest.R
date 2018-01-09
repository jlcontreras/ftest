setwd("repos/uni/f-test/")

# Part 1: Testing overall significance of regression model

# The dataset contains info about pollution levels in different cities of the USA.
df <- read.csv("air_pollution.csv")

# Lets see if we can model SO2 as dependent on the population of each city and
# the number of raining days
model.full <- lm(SO2 ~ popul + predays, data = df) 

# TODO: Interpret the p-values of each of the variables
summary(model.full)

# Now let us consider the reduced model, that is, b1 = b2 = 0
# (so we only have an intercept, which is the mean)

# Fit the model to our data, in order to get the error sum of squares of both models
fitted.full <- predict(model.full,  df[,c("popul","precip")])

ssr <- sum((fitted.full - mean(df$SO2))^2)  # Regression sum of squares
sse <- sum((df$SO2 - fitted.full)^2)        # Error (residual) sum of squares

# Calculate the F-statistic
k <- 2        # Number of vars used in the full model
n <- nrow(df) # Number of observations
F <- (ssr / k) / (sse / (n - k - 1))
F

# Now, how to get from the F-statistic to a p-value??
pvalue <- 1 - pf(F, k, n-k-1)
pvalue

# Part 2: Variable selection
full <- lm(SO2 ~ popul + wind + precip + temp + predays, data = df) 
summary(full)
reduced <- lm(SO2 ~ wind + precip + predays, data = df) 
anova(reduced, full) # Low pval -> Reject H0 (H0=the vars we took out were useless)

reduced2 <- lm(SO2 ~ popul + temp, data = df) 
anova(reduced2, full)# High pval -> not enough evidence to reject H0 (so the vars we took out were useless)

yhat.full <- predict(full, df[,c("popul", "wind", "precip", "temp", "predays")])
yhat.red <-  predict(reduced2, df[, c("popul", "temp")])

rss1 <- sum((df$SO2 - yhat.full)^2) 
rss0 <- sum((df$SO2 - yhat.red)^2)

p1 <- 5 # Number of variables included in the full model
p0 <- 2 # Number of variables included in the reduced(nested) model

n <- nrow(df) # Number of observations
F0 <- ((rss0 - rss1)/(p1 - p0)) / (rss1 / (n - p1 - 1))
F0

pvalue <- 1 - pf(F0, p1-p0, n-p1-1)
pvalue
