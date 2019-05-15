# Question 1

library(MCMCpack)

# Load R women data
data(women)
names(women) <- c("h","w")

# Set priors
bb0 <- 1
BB0 <- 1e-4
cc0 <- 1e5
dd0 <- 1e5

# M1 is line through origin
m1 <- MCMCregress(w~0+h,data=women,b0=bb0,c0=cc0,d0=dd0,B0=BB0,marginal.likelihood="Chib95")

# M2 is an arbitray line
m2 <- MCMCregress(w~h,data=women,b0=bb0,c0=cc0,d0=dd0,B0=BB0,marginal.likelihood="Chib95")


# M3 is a quadratic line
m3 <- MCMCregress(w~h+I(h^2),data=women,b0=bb0,c0=cc0,d0=dd0,B0=BB0,marginal.likelihood="Chib95")

# Get Bayes factors
BF <- BayesFactor(m1,m2,m3)

# Prints logs of all the models
print(BF)
