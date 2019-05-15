# Question 5

# load muscle mass data file
df <- read.table("http://omega.albany.edu:8008/MuscleMass.txt", header = TRUE)

# Store age and mass data
age <- df$Age
mass <- df$MuscleMass

# plot mass in terms of age
plot(mass ~ age)

# Create linear model of mass in
lm1 = lm(mass ~ age,data = df)

# Print summary of linear model
summary(lm1)

# Find the confidence interval of the linear model
confint(lm1,"age", level = 0.5)