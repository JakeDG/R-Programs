# Question 11

# Pull data from heights data file
hf <- heights$data.frame$Father
hm <- heights$data.frame$Mother
hs <- heights$data.frame$Son

# Put all data heights into data frame
df <- data.frame(hs, hf, hm)

# Create linear model to calculate the variance of the heights of 
# sons explained by the regression on the heights of their mothers and fathers
lm0 <- lm(hs~.,df)

# Print R^2
summary(lm0)$r.squared