# Do not include this into the acual example.
source("./R/avgDistort.R")
source("./R/utils.R")

# Create the data that will be checked
nObs <- 600
half <- round(nObs/2)
data <- data.frame(
  id = rep(1:half + 500, 2),
  year = rep(c(2000, 2001), each=half),
  weight = round(rnorm(nObs, 50, 10)),
  revenue = round(rnorm(nObs, 100000, 5000)),
  cost = round(rnorm(nObs, 50000, 5000)),
  labor_input = round(rnorm(nObs, 2, 0.5), 1)
)
data <- within(data, {
  region <- id %% 3 + 1
  type <- id %% 12 + 1
  income <- revenue - cost
  `I(income/labor_input)` <- income/labor_input
})

# Define the variables (column in data.frame) which should be checked.
# For each variable, give the weight for later aggregation into one score.
# The absolute value of the weights doesn't matter. What matters are the relative diffrences between the weights.
variables <- as.data.frame(matrix(c(
  # weight, variable name
  40, "I(income/labor_input)",
  20, "income",
  10, "revenue",
  10, "cost"
), ncol=2, byrow=TRUE), stringsAsFactors = FALSE)

colnames(variables) <- c("weight","name")
variables[,"weight"] <- as.numeric(variables[,"weight"])
variables <- variables[,ncol(variables):1]
print(variables)

# Define the levels on which the influence of observations should be calculated.
# These levels can be weighted. E.g. the influence on the year level is more important than the influence on a lower level like "region x year".
# The absolute value of the weights doesn't matter. What matters are the relative diffrences between the weights.
indexList <- list(yearLevel =  list(name =             "year",  weight = 1),
                  regionYear = list(name = c("region", "year"), weight = 1/3),
                  typeYear =   list(name = c("type",   "year"), weight = 1/12))

# Calculate the influence of one observation on the sample average.
infl <- influenceOfGivenObservation(data = data, weightCol = "weight", yearCol = "year", year0 = 2000, year1 = 2001,
                                    variables = variables, indexList = indexList, ids = c(501, 502), idCol = "id", method = "delta", nCores = 1)
infl <- influenceOfGivenObservation(data = data, weightCol = "weight", yearCol = "year", year0 = 2000, year1 = 2001,
                                    variables = variables, indexList = indexList, ids = c(501, 502), idCol = "id", method = "oneYear", nCores = 1)

# Now calculate the influence of observations on the sample averages.
ranking <- findInfluencialObservations(data = data, weightCol = "weight", idCol = "id", yearCol = "year", year0 = 2000, year1 = 2001,
                                       variables = variables, indexList = indexList, weightAndScore = TRUE, condense = TRUE, nCores = 1,
                                       method = "oneYearAndDelta")
# Look at the score.
score <- ranking[["score"]]
head(score)
# Find the most influencial observations
# The rownames of ranking[["score"]] shows the ID.
mostInflIds <- rownames(score)[ order(score[,"sum"], decreasing = TRUE)[1:10]]
{
  cat("\n10 most influencial observations.\n")
  print(score[mostInflIds,])
  cat("\nDetails why these obserations are considered influencial on the delta.\n")
  print(ranking[["deltaDetails"]][mostInflIds])
}
