# Question 6

library(MCMCpack)

# Reshapes data into binary form
# Returns reshaped data frame 
myresh <- function(y,n,x)
{
  L <- sum(n)
  X <- rep(0,L)
  Y <- rep(0,L)
  t <- 0
  k <- length(n)
  
  for(i in 1:k)
  {
    X[(t+1):(t+n[i])] <- x[i]
    
    if (y[i] > 0)
    {
      Y[(t+1):(t+y[i])] <- 1
    }
    
    t = t + n[i]
  }
  
  df = data.frame(x=X, y=Y)
  df
}

# Load beetles data file
d <- read.table("http://omega.albany.edu:8008/beetles.txt", header = TRUE)

ns = length(d[,1]) # number of responses
ni <- d[,2] # numbers of beetles
yi <- d[,3] # numbers of dead beetles after exposure
xi <- d[,1] # doses

# Reshape data into binary form: No = 0, Yes = 1
newdf <- myresh(yi,ni,xi)

# Fit a Bayes logistic regression model
mc1 <- MCMClogit(y~x, data=newdf)

# Print bayes model summary
summary(mc1)

# Draw 100 points from the posterior
ri <- sample(1:dim(mc1)[1], 1000)
s <- mc1[ri,] # Sample matrix
plot(s)

# Print summary of sample
sampleStats <- summary(s)
sampleStats