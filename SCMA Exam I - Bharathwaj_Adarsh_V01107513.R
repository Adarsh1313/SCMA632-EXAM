## Current Working Directory of the project
getwd()

## Loading the data
cancer <- read.csv("cancer_reg.csv")
cancer -> cancer1

## Function to auto-install and load 
packagesinstall_and_load <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
  }
}

# List of packages to install and load
packages <- c("nortest", "DataExplorer", "MASS", "lmtest", "car")

# Call the function
install_and_load(packages)
## Data Cleaning
## Data Preparation and Understanding
summary(cancer1)
names(cancer1)
head(cancer1)
tail(cancer1)

## Using plots to understand the data
plot_str(cancer1)
plot_missing(cancer1)

# Identify columns with missing values
cols_with_na <- sapply(cancer1, function(x) any(is.na(x)))

# Remove columns with missing values
cancer1_cleaned <- cancer1[, !cols_with_na]

## Using plots to understand the data
plot_str(cancer1_cleaned)
plot_missing(cancer1_cleaned)

## Data Understanding
plot_histogram(cancer1_cleaned)
plot_density(cancer1_cleaned)
plot_qq(cancer1_cleaned)

## Test for Normality
ad.test(cancer1_cleaned$TARGET_deathRate)

## Multiple regression analysis
## Building a regression model
set.seed(123)
cancer1_cleaned_mixeddata <- cancer1_cleaned[order(runif(3047)),]
head(cancer1_cleaned_mixeddata, 5)

# Select only numerical variables
cancer1_cleaned_mixeddata <- cancer1_cleaned_mixeddata[, sapply(cancer1_cleaned_mixeddata, is.numeric)]

# Split into 80:20 ratio
cancer1_training = cancer1_cleaned_mixeddata[1:2459,]
cancer1_testing = cancer1_cleaned_mixeddata[2460:3047,]

# Running the initial regression model
cancer1_lm_full <- lm(TARGET_deathRate~., data = cancer1_training)
summary(cancer1_lm_full)



## Build reduced models
le_step <- stepAIC(cancer1_lm_full,direction = "backward")

# Highest AIC
le1_lm_reduced1 = lm(TARGET_deathRate ~ avgAnnCount + avgDeathsPerYear + incidenceRate + 
                       medIncome + popEst2015 + povertyPercent + studyPerCap + MedianAge + 
                       MedianAgeMale + MedianAgeFemale + AvgHouseholdSize + PercentMarried + 
                       PctNoHS18_24 + PctHS18_24 + PctBachDeg18_24 + PctHS25_Over + 
                       PctBachDeg25_Over + PctUnemployed16_Over + PctPrivateCoverage + 
                       PctEmpPrivCoverage + PctPublicCoverage + PctPublicCoverageAlone + 
                       PctWhite + PctBlack + PctAsian + PctOtherRace + PctMarriedHouseholds + 
                       BirthRate, data = cancer1_training)
summary(le1_lm_reduced1)

# Lowest AIC
le1_lm_reduced2 = lm(TARGET_deathRate ~ avgAnnCount + avgDeathsPerYear + incidenceRate + 
                       popEst2015 + povertyPercent + MedianAgeMale + PercentMarried + 
                       PctNoHS18_24 + PctHS18_24 + PctHS25_Over + PctBachDeg25_Over + 
                       PctUnemployed16_Over + PctPrivateCoverage + PctEmpPrivCoverage + 
                       PctWhite + PctOtherRace + PctMarriedHouseholds + BirthRate, data = cancer1_training)
summary(le1_lm_reduced2)



## Model Diagnostics
# To check for linearity
plot(le1_lm_reduced2, which = 1)
# To test for serial independence (autocorrelation) of errors
dwtest(le1_lm_reduced2)
# To detect heteroskedasticity (unequal variance of residuals)
plot(le1_lm_reduced2, which = 3)
# To check the normality assumption of residuals
plot(le1_lm_reduced2, which = 2)
# To assess multi-collinearity (high correlation among predictor variables)
vif(le1_lm_reduced2)
