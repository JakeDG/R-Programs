# Question 2

library(MCMCpack)

# Load data from pendulum data file
df <- read.table("http://omega.albany.edu:8008/ex4TLg.txt", header = TRUE)

# M0 is T ~ 1
m0 <- MCMCregress(T~1,data=df,B0=1,marginal.likelihood="Chib95")

# M1 is T ~ 0+sqrt(L/g) (no intercept)
m1 <- MCMCregress(T~0+sqrt(L/g),data=df,B0=1,marginal.likelihood="Chib95")

# M1 is T ~ sqrt(L/g)
m2 <- MCMCregress(T~sqrt(L/g),data=df,B0=1,marginal.likelihood="Chib95")

# Get the posterior probabilities
BF <- BayesFactor(m0,m1,m2)
PostProbMod(BF)

