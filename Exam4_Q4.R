# Question 4

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

# Load baby data file
d <- read.table("http://omega.albany.edu:8008/logisticBabies.txt", header = TRUE)

x <- d$Age # Matrix of Ages  
n <- d$No + d$Yes # The total # of babies
y <- d$Yes # Matrix of yes's

# Reshape data into binary form: No = 0, Yes = 1
newdf <- myresh(y,n,x)

# Create the gen linear model
glm1 <- glm(y ~ x,family="binomial",data=newdf)
glm1
