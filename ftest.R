setwd("repos/uni/f-test/")
library(car)
# Part 1: Testing overall significance of regression model

# The dataset contains info about pollution levels in different cities of the USA.
df <- read.csv("air_pollution.csv")

# Part 1, example 1

model <- lm(SO2 ~ temp, data = df) 

# Plot: model
plot(df$temp, df$SO2, main="S02 level vs population", xlab="Population", ylab="SO2")
abline(model$coefficients[1], model$coefficients[2], col="red")
abline(mean(df$SO2), 0, col="blue")
# Plot: Mean (reduced model)
plot(df$popul, df$SO2, main="S02 level vs population", xlab="Population", ylab="SO2")


# TODO: Interpret the p-values of each of the variables
summary(model)

# Now let us consider the reduced model, that is, b1 = b2 = 0
# (so we only have an intercept, which is the mean)

# Fit the model to our data, in order to get the error sum of squares of both models
new <- data.frame(popul = df$popul)
fitted <- predict(model, newdata = new)

ssr <- sum((fitted - mean(df$SO2))^2)  # Regression sum of squares
sse <- sum((df$SO2 - fitted)^2)        # Error (residual) sum of squares

# Calculate the F-statistic
k <- 1        # Number of vars used in the full model
n <- nrow(df) # Number of observations
F <- (ssr / k) / (sse / (n - k - 1))
F

# Now, how to get from the F-statistic to a p-value??
pvalue <- 1 - pf(F, k, n-k-1)
pvalue


# Part 1, example 2

# Lets see if we can model SO2 as dependent on the population of each city and
# the number of raining days
model.full <- lm(SO2 ~ popul + predays, data = df) 

# TODO: Interpret the p-values of each of the variables
summary(model.full)

# Now let us consider the reduced model, that is, b1 = b2 = 0
# (so we only have an intercept, which is the mean)

# Fit the model to our data, in order to get the error sum of squares of both models
fitted.full <- predict(model.full,  df[,c("popul","predays")])

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

# Part 2: Partial F-Test for variable selection
#-----------------

full <- lm(income ~ education + women + prestige + census, data = Prestige) 
summary(full)
reduced <- lm(income ~ education + prestige, data = Prestige)
anova(reduced, full) # Low pval -> Reject H0 (H0=the vars we took out were useless)

yhat.full <- predict(full, Prestige[,c("education", "women", "prestige", "census")])
yhat.red <-  predict(reduced, Prestige[, c("education", "prestige")])

rss1 <- sum((Prestige$income - yhat.full)^2) 
rss0 <- sum((Prestige$income - yhat.red)^2)

p1 <- 4 # Number of variables included in the full model
p0 <- 2 # Number of variables included in the reduced(nested) model

n <- nrow(Prestige) # Number of observations
F0 <- ((rss0 - rss1)/(p1 - p0)) / (rss1 / (n - p1 - 1))
F0

pvalue <- 1 - pf(F0, p1-p0, n-p1-1)
pvalue
