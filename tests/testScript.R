{
  #### *** START *** ####
  source("./tests/testFunc.R")
  source("./R/avgDistort.R")
  source("./R/utils.R")
  errorLvl <- 0

  message(
    "*************\n",
    "*** Start ***\n",
    "*************\n")

  #### Date preparation ####
  require("zaUtils")

  # Daten einlesen
  data <- load.spe.gb()
  data[,"TypS3"] <- data[,"TypS3"] - 1500
  data[,"Typ"] <- data[,"TypS3"]

  # Einstellung zu nCores max.
  maxNCores <- if (as.numeric(substr(Sys.time(), 12, 13)) >= 20) 15 else 12
  nCores <- min(maxNCores, parallel::detectCores())

  # Filtern - Einstellung
  year0 <- 2016
  year1 <- year0 + 1
  yearCol <- "Jahr"
  idCol <- "ID"
  weightCol <- "Gewicht"

  # Filtern - Anwendung
  yearFilt <- data[,yearCol] %in% c(year0, year1)
  data <- data[yearFilt,]

  # Weitere settings
  variables <- char.cols.to.num(matrix(c(
    # Gewicht, Variablenname
    20,    "LE"
    , 40,  "I(Arbeitsverdienst/JAE_FamAK)"
    , 10,  "er_Ertrag_tot"
    , 2.5, "er_HauptprodAckerFutter"
    , 2.5, "er_Tierhaltung"
    , 5,   "au_Aufwand_tot"
  ), ncol=2, byrow=TRUE))
  colnames(variables) <- c("weight","name")
  variables <- variables[,ncol(variables):1]
  #
  indexList <- list(Jahr=list(name=yearCol, weight = 1)
                    ,Region_Jahr=list(name=c("Region", yearCol), weight = 1/3)
                    ,Typ_Jahr=list(name=c("Typ", yearCol), weight = 1/12)
  )
  indexNames <- lapply(indexList, function(x) {
    y <- x[["name"]]
    return (y[!y %in% yearCol])
    })


  .calcDelta(data, weightCol=weightCol, yearCol=yearCol, year0=year0, year1=year1, variables=variables, indexNames=indexNames, outRow=data[1:2, , drop=FALSE], y0Value=NULL, type = "relative")

  #### TEST_NAME_HERE ####
  errorLvl = errorLvl + local({
    testResult <- assertthat::are_equal(.calcDelta(data, weightCol=weightCol, yearCol=yearCol, year0=year0, year1=year1, variables=variables, indexNames=indexNames, outRow=data[1:2, , drop=FALSE], y0Value=.calcYear1(data = data, weightCol = weightCol, yearCol = yearCol, year=year0, variables = variables, indexNames = indexNames)),
                                        .calcDelta(data, weightCol=weightCol, yearCol=yearCol, year0=year0, year1=year1, variables=variables, indexNames=indexNames, outRow=data[1:2, , drop=FALSE], y0Value=NULL))
    rightResult <- TRUE
    return (
      testAgainstAndReport(testName = ".calcDelta() -> Compare result when `y0Value=NULL` and `y0Value` was given.", testResult = testResult, rightResult = rightResult, testNo = 1.01, warn = TRUE)
    )
  })



  #### *** END *** ####

  message("")

  if (errorLvl != 0)
    stop (
      "***************\n",
      "*** FAILURE ***\n",
      "***************\n",
      "See warnings above and check which tests were not successful.")

  message(
    "***************\n",
    "*** SUCCESS ***\n",
    "***************\n",
    "All tests passed successfully."
  )
}
