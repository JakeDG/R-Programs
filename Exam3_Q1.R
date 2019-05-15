# Question 1

# Cloud function to generate a cloud of 1 million random points
# Returns a data frame containing the 1 million
cloud <- function(n, avex, avey, sdx, sdy, r)
{
  x = rnorm(n,avex,sdx)
  y = rnorm(n,avey + (x-avex)/sdx*r*sdy, sqrt(1-r^2)*sdy)
  
  # Create a dataframe called "data.frame(x=x,y=y)"
  df <- data.frame(x = x, y = y)
  
  return(df)
}

# Information supplied by the problem
b0 <- 2.0
b1 <- 2.0
sde <- 2.0
cod <- 0.85
avey <- -1

# Calculate average of x and SD of x and y
avex <- (b0 - avey) / -b1
sdy <- sde / sqrt(1-0.85)
sdx <- (sqrt(0.85)*sdy)/2 

# Output average of x and SD of x and y
print(avex)
print(sdx)
print(sdy)

# Print answer
df <- cloud(1e6,avex,avey,sdx,sdy,sqrt(cod))
round(mean((df$x * df$y)^3)/1e4)