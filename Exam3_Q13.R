# Question 13

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

# Store 1 million points returned from cloud function
df <- cloud(1e6,0.25,1.0,2.98,6.45,-0.92)

# Print answer
round(mean((df$x * df$y)^3)/1e4)
