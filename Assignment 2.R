#Assignment 2
#Tyler

library(tidyverse)

#Develop a function (based on the above) that will generate, for a range of each of ‘magnitude of B1’, ‘sample size’ and ‘error’, a measure of the quality of the parameter estimate. (Hint, that measure of quality is the t-value – the estimate scaled by the standard error of the estimate).

gen.mod <- function(n = 100, b0 = 2, b1 = 3, sd.err = 1) {
  predictor <- runif(n) - 0.5
  response <- b0 + b1 * predictor + rnorm(n, mean = 0, sd = sd.err)
  return(data.frame(response = response, predictor = predictor))}

n.time <- 1:10
n <- c(10, 50, 100, 200)
b1 <- c(0.5, 1, 3.3, 5)
sd.err <- c(0.25, 1.25, 3, 5)

first.df <- expand.grid(n.time = n.time, n = n, b1 = b1, sd.err = sd.err)
View(first.df)

# Use that function to produce a data frame, with various combinations of each of the three variables as ‘factors’ and the quality of the estimate as the response.

for (i in 1:length(first.df$n.time)) {
  temp.df <- gen.mod(n = first.df$n[i], b0 = 2, b1 = first.df$b1[i], sd.err = first.df$sd.err[i])
  temp.lm <- lm(response ~ predictor, data = temp.df)
  first.df$tval[i] <- summary(temp.lm)$coefficients["predictor", "t value"]}

#Show the results visually.

ggplot(data = first.df, aes(tval, group = b1, color = b1)) + 
  geom_density() + 
  xlab("t value") + 
  ylab("Density") + 
  facet_grid(~sd.err)

# Do the same thing as above, but use a single parameter estimate of B1 = 0 (e.g. no effect). (Set B0 = 0). Use a sample size of 100. Instead of using the ‘t’ value as your ‘quality’ estimate, have the function output the ‘p-value’.

gen.mod2 <- function(n = 100, b0 = 0, b1 = 0, sd.err = 1) {
  predictor <- runif(n) - 0.5
  response <- b0 + b1 * predictor + rnorm(n, mean = 0, sd = sd.err)
  return(data.frame(response = response, predictor = predictor))}

n.time <- 1:10
n <- c(10, 50, 100, 200)
b1 <- c(0)
sd.err <- c(0.25, 1.25, 3, 5)

second.df <- expand.grid(n.time = n.time, n = n, b1 = b1, sd.err = sd.err)
View(second.df)

for (i in 1:length(second.df$n.time)) {
  temp.df2 <- gen.mod2(n = second.df$n[i], b0 = 0, b1 = second.df$b1[i], sd.err = second.df$sd.err[i])
  temp.lm2 <- lm(response ~ predictor, data = temp.df2)
  second.df$pval[i] <- summary(temp.lm2)$coefficients["predictor", "Pr(>|t|)"]}

#For a given sample size and error, what does the distribution of p-values look like?
  
ggplot(data = second.df, aes(pval, group = n, color = n)) + 
  geom_density() + 
  facet_grid(~sd.err) + 
  xlab("p value") + 
  ylab("Density")

#What proportion of p-values are <= 0.05?

pvalues <- which(second.df$p.val <= 0.05)
tabulate(pvalues)

sum(tabulate(pvalues))

proportion <- (sum(tabulate(pvalues))/120) * 100
print(proportion)

#How does that change when the amount of error changes?

ggplot(data = second.df, aes(pval, group = n, color = n)) +
  geom_density() + 
  facet_grid(~sd.err) +
  xlab("p value") + 
  ylab("Density")
