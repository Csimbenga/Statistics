# load libraries 

library("rcompanion")
library("car")
library("effects")
library("multcomp")

# load data 

cellphone <- read.csv("/Users/christinasimbenga/Desktop/cellPhone.csv")

# Question
#How does the presence or absence of an international phone plan (International.Plan)
#influence the use of nighttime minutes (Night.Mins), 
#holding whether or not the client has a voicemail plan (vMail.Plan) constant?

# Data wrangling 



# Testing Assumptions
# Normal
plotNormalHistogram(cellphone$Night.Mins)

# This is normal moving on 

### Homogeneity of Variance
leveneTest(Night.Mins~International.Plan, data=cellphone)

# This is not significant, it has passed the assumption 

# Homogeneity of Regression Slopes

Homogeneity_RegrSlp = lm(Night.Mins~vMail.Plan, data=cellphone)
anova(Homogeneity_RegrSlp)

# This is not significant, it has passed the assumption 

# Sample size is met

## Running the Analysis


ANCOVA = lm(Night.Mins~vMail.Plan + International.Plan*vMail.Plan, data=cellphone)
anova(ANCOVA)

# We conclude that wether you have international plan or not it does not affects the number of night minutes used
# even if the you have a voice mail plan 












