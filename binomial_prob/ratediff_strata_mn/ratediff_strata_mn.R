###### The results is consistent with paper results:

# Cochran-Mantel-Haenszel Weighted Miettinen & Nurminen Method 
# for Confidence Intervals of the Difference in Binomial Proportions 
# from Stratified 2x2 Samples.

# The x1 means response in the treatment group while the n1 is the total patient in treatment group.
# The x2 means response in the control group while the n2 is the total patient in the control group.
# The 3 column means 3 different strata.

library(ratesci)
a <- scoreci(
  x1 = c(3, 4, 18),
  x2 = c(1, 3, 2),
  n1 = c(5, 10, 35),
  n2 = c(15, 10, 25), 
  
  stratified = TRUE,
  weighting = "MH",
  skew = FALSE
  # ,bcf = FALSE
  # ,plot = TRUE
)
a