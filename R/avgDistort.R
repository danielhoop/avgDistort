#' Calculate average of one year
#' @author Daniel Hoop
#' @keywords internal
#' @inheritParams influenceOfGivenObservation
#' @param year The year to calculate the average for.
#' @param outRow The special row for which the index level should be filtered.
#' @param indexNames The names of the columns that contain the indices.
#' @return The average values for all variable and index combinations. Can be a matrix or a vector depending on the inputs.
.calcYear1 <- function(data, weightCol, yearCol, year, variables, indexNames, outRow=NULL) {
  if (length(year) != 1)
    stop ("length(year) != 1")
  return (
    lapply(indexNames, function(index){
      filt <- data[,yearCol] == year
      if (length(index) != 0) {
        indexVec <- paste.cols(data[,index])
        index.of.result <- sort(unique(indexVec[filt]))
        if (!is.null(outRow)){
          filt <- filt & indexVec%in%unlist(unname(paste.cols(outRow[index])))
        }
      } else {
        indexVec <- rep("", nrow(data))
        index.of.result <- sort(unique(indexVec[filt]))
      }
      mean.weight(data[filt,,drop=FALSE], cols=variables[,"name"], weights=data[filt,weightCol], index=indexVec[filt], fixed.index=TRUE, index.of.result=index.of.result)
    })
  )
}
#' Calculate delta between averages of two years.
#' @author Daniel Hoop
#' @keywords internal
#' @inheritParams influenceOfGivenObservation
#' @inheritParams .calcYear1
#' @param year0 The reference year to which the delta should be calculated.
#' @param year1 The subsequent year.
#' @param y0Value Optional precalculated value for the reference year, in case the calculations should be carried out many times (to speed up the process).
#' @param type The type of delta. If \code{"relative"}, then the relative difference will be calculated as: \code{(year1 / year0) - 1}
#' @return The delta for all variable and index combinations. Can be a matrix or a vector depending on the inputs.
.calcDelta <- function(data, weightCol, yearCol, year0, year1, variables, indexNames, outRow = NULL, y0Value = NULL, type = c("absolute", "relative")) {

  type <- match.arg(type)

  if (!is.null(y0Value)) {
    if (length(indexNames) != length(y0Value))
      stop("indexNames and y0Value must have the same length.")
  } else {
    y0Value <- lapply(indexNames, function(x) return (NULL))
  }

  return (
    mapply(
      index = indexNames,
      y0Value = y0Value,
      FUN = function(index, y0Value) {
        year0Filt <- data[,yearCol] == year0
        year1Filt <- data[,yearCol] == year1
        if (length(index) != 0) {
          indexVec <- paste.cols(data[,index])
        } else {
          indexVec <- rep("", nrow(data))
        }
        index.of.result <- sort(unique(indexVec[year0Filt | year1Filt]))
        # Year 0
        if (is.null(y0Value)) {
          filt <- year0Filt
          y0 <- mean.weight(data[filt,], cols=variables[,"name"], weights=data[filt,weightCol], index=indexVec[filt], fixed.index=TRUE, index.of.result=index.of.result)
        } else {
          y0 <- y0Value
        }
        # Year 1
        filt <- year1Filt
        if (length(index) != 0 && !is.null(outRow)){
          filt <- filt & (indexVec %in% unlist(unname(paste.cols(outRow[index]))))
        }
        y1 <- mean.weight(data[filt,], cols=variables[,"name"], weights=data[filt,weightCol], index=indexVec[filt], fixed.index=TRUE, index.of.result=index.of.result)
        # Delta
        errorMsg <- tryCatch({
          if (type == "absolute") {
            res <- y1 - y0
          } else if (type == "relative") {
            res <- (y1 / y0) - 1
          }
          "" # Return
        }, error=function(e){
          e$message
        })
        if (errorMsg != "") {
          message("y0")
          print(y0)
          message("y1")
          print(y1)
          stop ("An error happened when trying to calculate `y1 - y0`: \"", errorMsg, "\"",
                "\nNotice that `y0` and `y1` must be calculated for the same indices and columns, (i.e. their rownames and colnames must be equal).",
                "\nAbove this error you should see `y0` and `y1` printed.")
        }
        rownames(res) <- paste0("d-", rownames(res))
        return (res)
      })
  )

}

#' @title Condense results.
#' @description  Condense results such that indexes levels that aren't relevant for an observation aren't displayed. If variables are given, then they will be weighted in order to further condense.
#' @author Daniel Hoop
#' @keywords internal
#' @param x The data structure containing the results to be condensed. A return value of the function \code{\link{influenceOfGivenObservation}}.
#' @inheritParams influenceOfGivenObservation
#' @inheritParams .calcYear1
#' @inheritParams .calcDelta
#' @param weightResults Logical value indicating if the influence of the observation on the different variables should be weighted.
#' @param giveNames Logical indicating if the result should be given rownames equal to the index levels.
#'
.condenseResults <- function(x, indexList, variables=NULL, weightResult=FALSE, giveNames=FALSE) {

  # Test und Verarbeitung der Funktionsparameter
  if (weightResult) {
    if (is.null(variables))
      stop("if `weightResult==TRUE`, then `variables` must not be NULL.")
    .testVariablesValidity(variables)
    rownames(variables) <- variables[,"name"]
  }
  .testIndexListValidity(indexList)
  indexWeights <- sapply(indexList, function(x)x[["weight"]])

  # if (!is.null(weights)) {
  #   if (!is.list(weights) || length(weights) != 2 || !all(c("index","vars")%in%names(weights)))
  #     stop ("weights must be a list of length 2, names index and vars")
  #   if (length(weights[["index"]]) != length(indexNames) || any(names(weights[["index"]]) != names(indexList)))
  #     stop ("all entries in weights[['index']] must be equal to names(indexList).")
  #   if (length(dim(weights[["vars"]])) != 2 || any(dim(weights[["vars"]]) != dim(variables)) || any(colnames(weights[["vars"]]) != colnames(variables)) || any(rownames(weights[["vars"]]) != rownames(variables)))
  #     stop ("weights[['vars']] must be identical to variables")
  # }

  return (
    lapply(x, function(a){
      res <- lapply(a, function (b) {
        if (is.null(dim(b)))
          return (b)
        return (apply(b, 2, function(x)x[!is.na(x)]))
      })
      if (giveNames) {
        names(res) <- names(indexList)
      }
      res <- do.call("rbind", res)
      if (weightResult) {
        res <- res * indexWeights
        res <- t(t(res) * variables[,"weight"])
      }
      return (res)
    })
  )
}

#' @keywords internal
.testVariablesValidity <- function (variables) {
  if (ncol(variables) != 2 || !all(c("name", "weight") %in% colnames(variables)))
    stop ("variables must be a matrix/data.frame with 2 columns called 'name' and 'weight'")
  return (invisible(NULL))
}

#' @keywords internal
.testIndexListValidity <- function (indexList, data=NULL) {
  lapply(indexList, function(x) {
    if (!is.list(x) || length(x) != 2 || !all(c("name","weight") %in% names(x)))
      stop ("All entries in indexList must be a list of length 2, with two entries called 'name' and 'weight'")
  })
  if (!is.null(data)) {
    indexNames <- lapply(indexList, function(x)x[["name"]])
    indexNotAvail <- unique(unlist(indexNames))
    indexNotAvail <- indexNotAvail[!indexNotAvail%in%colnames(data)]
    if (length(indexNotAvail) > 0)
      stop (paste0("Some indices given in indexList are not available in the colnames of data: ", paste0(indexNotAvail, collapse=", ")))
  }
  return (invisible(NULL))
}

#' @keywords internal
.testColsInData <- function(data, idCol=NULL, weightCol=NULL, yearCol=NULL) {
  checkArgs <- c("idCol", "weightCol", "yearCol")
  for(arg in checkArgs) {
    if (!is.null(get(arg)) && !get(arg) %in% colnames(data))
      stop (paste0(arg, " is not contained in the colnames of data."))
  }
}

#' @keywords internal
.scaleIndices <- function (indexList) {
  maxIndexWeight <- max(sapply(indexList, function(x)max(x[["weight"]])))
  return (lapply(indexList, function(x){
    x[["weight"]] <- x[["weight"]] / maxIndexWeight
    return (x)
  }))
}

#' @keywords internal
.scaleVariableWeights <- function (variables) {
  # Gewichtung auf 1 normieren. Diese aendern!
  variables[,"weight"] <- variables[,"weight"] / max(variables[,"weight"])
  return (variables)
}

#' Define function to get index of observations.
#' @author Daniel Hoop
#' @keywords internal
#' @inheritParams influenceOfGivenObservation
#' @param missingIn A character defining the method to apply. Either "anyYear", "year0" or "year1".
#' @return A function that will can be used to perform the task inside a call to \code{\link{tapply}}.
.createRowFinderFunc <- function (missingIn, data, idCol, yearCol, year0, year1) {
  if (missingIn == "anyYear") {
    getIdFunc <- function (id) {
      return (which(data[,idCol] == id[1]))
    }
  } else if (missingIn == "year0") {
    getIdFunc <- function (id) {
      y <- which(data[,idCol] == id[1])
      if (length(y) == 1 && data[y, yearCol] == year0)
        return (y)
      return (NULL)
    }
  } else if (missingIn == "year1") {
    getIdFunc <- function (id) {
      y <- which(data[,idCol] == id[1])
      if (length(y) == 1 && data[y, yearCol] == year1)
        return (y)
      return (NULL)
    }
  } else stop ("Internal error. Wrong `missingIn`.")
}

#' Calculate the influence of a given observation on the average of a sample
#' @author Daniel Hoop
#' @export
#' @param data A data.frame containing all columns specified in \code{weightCol}, \code{yearCol}, \code{indexList}, and \code{variables}.
#' @param weightCol The column in \code{data} that contains the weights.
#' @param yearCol The column in \code{data} that contains the year.
#' @param year0 Optional: The reference year to which the delta should be calculated. Only has to be given if \code{method = "oneYear"} is chosen.
#' @param year1 Optional: The subsequent year. Only has to be given if \code{method = "oneYear"} is chosen.
#' @param variables The variables for which the calculation should be executed. Consult the example in \code{\link{findInfluencialObservations}} to see how this argument must be structured.
#' @param indexList A list which holds all index levels which should be checked. Consult the example in \code{\link{findInfluencialObservations}} to see how this argument must be structured.
#' @param ids Optional: The ids of the observations of which the influence should be calculated. If \code{NULL}, then \code{whichRows} has to be specified.
#' @param idCol Optional: The name of the column in \code{data} which contains the ids. Must be specified if \code{ids} is not \code{NULL}.
#' @param whichRows Optional: The row numbers of the observations of which the influence should be calculated. If \code{NULL}, then \code{ids} has to be specified.
#' @param reference Optional: Reference average values for year0 against which the comparison should be made. If \code{NULL}, then the average values of \code{year0} will be calculated inside the function.
#' @param method Either \code{"oneYear"}, \code{"delta"} or \code{"relativeDelta"}, specifying if the influence on one year or on the absolulte/relative delta between two years should be calculated.
#' @param compareFunc The function to compare averages when the observation is part of the sample (\code{inside}) or outside the sample (\code{outside}).
#' @param nCores The number of cores to use for the computations.
#' @param parallelCluster Optional parallel cluster created with \code{\link[snow:makeCluster]{snow::makeCluster}} and registered with \code{\link[doSNOW:registerDoSNOW]{doSNOW::registerDoSNOW}}.
#' @return A list containing the results for each individual observation that was checked.
#' @examples # See \link{findInfluencialObservations}
#'
influenceOfGivenObservation <- function (data, weightCol, yearCol, year0=NULL, year1=NULL, variables, indexList, ids=NULL, idCol=NULL, whichRows=NULL, reference=NULL,
                                         method=c("oneYear", "delta", "relativeDelta"), missingIn=c("anyYear", "year0", "year1"),
                                         compareFunc=function(outside, inside) return (inside/outside - 1), nCores=1, parallelCluster=NULL, verbose=FALSE, calledInternally=FALSE) {
  # Error checks only when called directly
  if (!calledInternally) {
    if (is.null(ids) && is.null(whichRows))
      stop ("Either specify ids or whichRows.")
    if (!is.null(ids) && !is.null(whichRows))
      stop ("Either specify ids or whichRows but not both.")
    if (!is.null(ids) && is.null(idCol))
      stop ("If ids is specified, then also idCol has to be given")
    if (!is.null(whichRows) && (!is.list(whichRows) || is.null(names(whichRows))))
      stop ("wichRows must be a list named with the ids of the observations.")
    tryCatch({
      compareFunc(outside=1, inside=1)
    }, error=function(e){
      stop ("compareFunc must be a function with 2 arguments named 'outside' and 'inside'. 'outside' stands for the new value that was calculated without (some) observation(s). 'inside' stands for the full sample to compare against.")
    })
    method <- match.arg(method)
    if (method == "delta") {
      missingIn <- match.arg(missingIn)
    }
    .testColsInData(data, idCol=idCol, weightCol=weightCol, yearCol=yearCol)
    .testIndexListValidity(indexList, data=data)
    # Verarbeitung Funktionsparameter
    indexList <- .scaleIndices(indexList)
    variables <- .scaleVariableWeights(variables)
  }

  rownames(variables) <- variables[,"name"]
  indexNames <- lapply(indexList, function(x) x[["name"]][!x[["name"]] %in% yearCol] )

  # Prepare whichRows, if it was not given.
  if (is.null(whichRows)) {
    if (method %in% c("delta", "relativeDelta")) {
      if (is.null(year0) || is.null(year1))
        stop ("If `is.null(whichRows)`, then `year0` and `year1` have to be given.")
      suYear <- sort(unique(data[,yearCol]))
      if (length(suYear) != 2 || any(suYear != c(year0, year1)))
        stop ("`data` must be filtered to contain only two years: year0 and year1. Otherwise the calculation logic has to be adapted by the package maintainer.")
      # Get the right rows for given IDs.
      findRow <- .createRowFinderFunc(missingIn = missingIn, data = data, idCol = idCol, yearCol = yearCol, year0 = year0, year1 = year1)
      whichRows <- tapply(ids, ids, findRow)
      whichRows[sapply(whichRows, is.null)] <- NULL # Like this, NULL values are removed from the list.
    } else if (method == "oneYear") {
      whichRows <- tapply(ids, ids, function(x) {
        which(data[,idCol] == x[1])
      })
    } else stop ("Internal error. Wrong argument `method`.")
  }
  if (length(unlist(whichRows)) == 0)
    stop ("No observation matches the criteria given in `ids` or `whichRows`.")
  idNames <- names(whichRows)

  # Initialize the cluster if necessary
  multiCoreSettings <- multiCorePrep(nCores = nCores, parallelCluster = parallelCluster)

  # Make the right expression to evaluate.
  # Then calculate
  installFromCRAN("foreach")
  require("foreach")

  result <- vector("list", length(whichRows))
  errorMsg <- tryCatch({

    if (method == "oneYear") {
      if (is.null(reference)) {
        reference <- .calcYear1(data,                    weightCol=weightCol, yearCol=yearCol,              year=year1, variables=variables, indexNames=indexNames)
      }
      result <- foreach (i = whichRows, .export=lsf.str(envir=parent.frame(n=99999), all.names=TRUE)) %dopar% { # unique(c(ls(all.names=TRUE), lsf.str(envir=parent.frame(n=99999), all.names=TRUE)))
        #for (i in whichRows) { # i <- whichRows[[6]]
        if (FALSE) i <- NULL # For RStudio diagnostics
        if (verbose) message(paste0("Executing sequentially, i = ", paste0(i, collapse=", ")))
        mapply(compareFunc,
               outside = .calcYear1(data[-i,,drop=FALSE], weightCol=weightCol, yearCol=yearCol,              year=year1, variables=variables, indexNames=indexNames, outRow=data[i,,drop=FALSE]),
               inside = reference)
      }
    } else if (method %in% c("delta", "relativeDelta")) {

      deltaType <- if (method == "delta") "absolute" else if (method == "relativeDelta") "relative"
      if (is.null(reference)) {
        reference <- .calcDelta(data,                     weightCol=weightCol, yearCol=yearCol, year0=year0, year1=year1, variables=variables, indexNames=indexNames, type=deltaType)
      }
      result <- foreach (i = whichRows, .export=lsf.str(envir=parent.frame(n=99999), all.names=TRUE)) %dopar% { # unique(c(ls(all.names=TRUE), lsf.str(envir=parent.frame(n=99999), all.names=TRUE)))
        #for (i in whichRows) { # i <- whichRows[[6]]
        if (verbose) message(paste0("Executing sequentially, i = ", paste0(i, collapse=", ")))
        mapply(compareFunc,
               outside = .calcDelta(data[-i,,drop=FALSE], weightCol=weightCol, yearCol=yearCol, year0=year0, year1=year1, variables=variables, indexNames=indexNames, outRow=data[i,,drop=FALSE], type = deltaType),
               inside = reference)
      }
    } else stop ("Internal error, wrong `method`.")
    ""

  }, error = function (e) {
    e$message
  })
  if (errorMsg == "invalid connection")
    stop ("An error happened, probably because a cluster was registered and closed. Either try to set `nCores` to something greater than 1 or try to restart the R session.")
  if (errorMsg != "")
    stop (errorMsg)

  names(result) <- idNames

  # Stop the cluster if it was initialized
  multiCorePost(multiCoreSettings)

  # Return
  return (result)
}


#' Find observations that heavily influence the averages of a sample
#' @export
#' @author Daniel Hoop
#' @inheritParams influenceOfGivenObservation
#' @param weightAndScore Logical value indicating if the influence on different variables should be weighted according to the settings given in \code{variables}, and if the observations should subsequently be given an overall distortion score.
#' @param condense Logical value indicating if the results should be condensed, i.e. if index levels which are not relevant for an observation should be excluded from the results.
#' @param method Either \code{"oneYearAndDelta"} (default) which will calculate the influence on one year and the delta between years or \code{"oneYear"} or  \code{"delta"} which will calculate one of either.
#' @param deltaType The type of difference: \code{"absolute"} or \code{"relative"}
#' @param verbose Logical value indicating if some calculation steps should be commented.
#' @return A list with names \code{"score"}, \code{"year1Details"}, and \code{"deltaDetails"}. The first of which contains the weighted results for each observation in a data.frame format. The second and third contain the details of the calculations.
#' @examples
#' # Create the data that will be checked
#' nObs <- 600
#' half <- round(nObs/2)
#' data <- data.frame(
#'   id = rep(1:half + 500, 2),
#'   year = rep(c(2000, 2001), each=half),
#'   weight = round(rnorm(nObs, 50, 10)),
#'   revenue = round(rnorm(nObs, 100000, 5000)),
#'   cost = round(rnorm(nObs, 50000, 5000)),
#'   labor_input = round(rnorm(nObs, 2, 0.5), 1)
#' )
#' data <- within(data, {
#'   region <- id %% 3 + 1
#'   type <- id %% 12 + 1
#'   income <- revenue - cost
#'   `I(income/labor_input)` <- income/labor_input
#' })
#'
#' # Define the variables (column in data.frame) which should be checked.
#' # For each variable, give the weight for later aggregation into one score.
#' # The absolute value of the weights doesn't matter. What matters are the relative diffrences between the weights.
#' variables <- as.data.frame(matrix(c(
#'   # weight, variable name
#'   40, "I(income/labor_input)",
#'   20, "income",
#'   10, "revenue",
#'   10, "cost"
#' ), ncol=2, byrow=TRUE), stringsAsFactors = FALSE)
#'
#' colnames(variables) <- c("weight","name")
#' variables[,"weight"] <- as.numeric(variables[,"weight"])
#' variables <- variables[,ncol(variables):1]
#' print(variables)
#'
#' # Define the levels on which the influence of observations should be calculated.
#' # These levels can be weighted. E.g. the influence on the year level is more important than the influence on a lower level like "region x year".
#' # The absolute value of the weights doesn't matter. What matters are the relative diffrences between the weights.
#' indexList <- list(yearLevel =  list(name =             "year",  weight = 1),
#'                   regionYear = list(name = c("region", "year"), weight = 1/3),
#'                   typeYear =   list(name = c("type",   "year"), weight = 1/12))
#'
#' # Calculate the influence of one observation on the sample average.
#' infl <- influenceOfGivenObservation(data = data, weightCol = "weight", yearCol = "year", year0 = 2000, year1 = 2001,
#'                                     variables = variables, indexList = indexList, ids = c(501, 502), idCol = "id", method = "delta", nCores = 2)
#' infl <- influenceOfGivenObservation(data = data, weightCol = "weight", yearCol = "year", year0 = 2000, year1 = 2001,
#'                                     variables = variables, indexList = indexList, ids = c(501, 502), idCol = "id", method = "oneYear", nCores = 2)
#'
#' # Now calculate the influence of observations on the sample averages.
#' ranking <- findInfluencialObservations(data = data, weightCol = "weight", idCol = "id", yearCol = "year", year0 = 2000, year1 = 2001,
#'                                        variables = variables, indexList = indexList, weightAndScore = TRUE, condense = TRUE, nCores = 12,
#'                                        method = "oneYearAndDelta")
#' # Look at the score.
#' score <- ranking[["score"]]
#' head(score)
#' # Find the most influencial observations
#' # The rownames of ranking[["score"]] shows the ID.
#' mostInflIds <- rownames(score)[ order(score[,"sum"], decreasing = TRUE)[1:10]]
#' {
#'   cat("\n10 most influencial observations.\n")
#'   print(score[mostInflIds,])
#'   cat("\nDetails why these obserations are considered influencial on the delta.\n")
#'   print(ranking[["deltaDetails"]][mostInflIds])
#' }
#'
findInfluencialObservations <- function (data, weightCol, idCol, yearCol, year0, year1, variables, indexList,
                                         compareFunc = function(outside, inside) return(inside/outside - 1),
                                         weightAndScore = TRUE, condense = TRUE, nCores=1, parallelCluster=NULL,
                                         method = c("oneYearAndDelta", "oneYear", "delta"),
                                         deltaType = c("absolute", "relative"),
                                         missingIn=c("anyYear", "year0", "year1"),
                                         verbose=FALSE) {

  # Tests der Einstellungen
  method <- match.arg(method)
  deltaType <- match.arg(deltaType)
  missingIn <- match.arg(missingIn)
  calcOneYear <- method %in% c("oneYearAndDelta", "oneYear")
  calcDelta <- method %in% c("oneYearAndDelta", "delta")
  if (sum(calcOneYear, calcDelta) == 0)
    stop ("One of `calcOneYear` or `calcDelta` must be TRUE.")
  .testColsInData(data, weightCol=weightCol, idCol=idCol, yearCol=yearCol)
  .testVariablesValidity(variables)
  .testIndexListValidity(indexList, data=data)
  tryCatch({
    cfr <- compareFunc(outside=1, inside=1)
    if (!is.numeric(cfr))
      stop ("The result is not numeric.")
    rm(cfr)
  }, error=function(e){
    stop ("compareFunc must be a function with 2 arguments named 'outside' and 'inside'.",
          " 'outside' stands for the new value that was calculated without (some) observation(s).",
          " 'inside' stands for the full sample to compare against.",
          " The function must return a numeric result if two numeric inputs were given.")
  })
  # Verarbeitung der Funktions-Parameter
  rownames(variables) <- variables[,"name"]
  indexList <- .scaleIndices(indexList)
  variables <- .scaleVariableWeights(variables)
  indexNames <- lapply(indexList, function(x) x[["name"]][!x[["name"]] %in% yearCol] )

  #### Vorbereitungen Datensatz
  # if (FALSE) {
  #   indexNames_c <-vector("list", length(indexNames))
  #   names(indexNames_c) <- names(indexNames)
  #
  #   for (i in 1:length(indexNames)) {
  #     if (!all(indexNames[[i]] %in% colnames(data)))
  #       stop (paste0("At least some indices are not available in data: ", paste0(indexNames[[i]], collapse=", ")))
  #     newNames. <- paste0(indexNames[[i]], ".")
  #     data[,newNames.] <- lapply(indexNames[[i]], function(x) paste0(x, data[,x]) )
  #     newNamesCombined <- paste0(newNames., collapse="_")
  #
  #     indexNames_c[[i]] <- newNamesCombined
  #     data[,newNamesCombined] <- paste.cols(data[,newNames., drop=FALSE])
  #   }
  #
  #   rm(i, newNames., newNamesCombined)
  # }

  # Get the right observations to check
  if (calcDelta) {
    suYear <- sort(unique(data[,yearCol]))
    if (length(suYear) != 2 || any(suYear != c(year0, year1)))
      stop ("data must be filtered to contain only two years: year0 and year1. Otherwise the calculation logic has to be adapted.")
  }
  # Get the right rows for given IDs.
  findRow <- .createRowFinderFunc(missingIn = missingIn, data = data, idCol = idCol, yearCol = yearCol, year0 = year0, year1 = year1)
  whichRows <- tapply(data[,idCol], data[,idCol], findRow)
  whichRows[sapply(whichRows, is.null)] <- NULL # Like this, NULL values are removed from the list.

  # Prepare cluster for parallel computing.
  # Initialize the cluster if necessary
  multiCoreSettings <- multiCorePrep(nCores = nCores, parallelCluster = parallelCluster)

  # Calculate influence on two different stages (1 year and delta)
  if (calcOneYear) {
    if (verbose) message("Calculating influence on year1...")
    year1Ref <- .calcYear1(data, weightCol=weightCol, yearCol=yearCol, year=year1, variables=variables, indexNames=indexNames)
    year1Details <- influenceOfGivenObservation(data=data, weightCol=weightCol, yearCol=yearCol, year0=year0, year1=year1, variables=variables, indexList=indexList,
                                                ids=NULL, whichRows=whichRows, reference=year1Ref,
                                                method="oneYear", missingIn=missingIn,
                                                compareFunc=compareFunc,
                                                nCores=nCores, parallelCluster=multiCoreSettings[["parallelCluster"]],
                                                calledInternally = TRUE, verbose = verbose)
  } else {
    year1Details <- NULL
  }
  if (calcDelta) {
    if (verbose) message("Calculating influence on delta...")
    deltaRef <- .calcDelta(data, weightCol=weightCol, yearCol=yearCol, year0=year0, year1=year1, variables=variables, indexNames=indexNames, type = deltaType)
    deltaDetails <- influenceOfGivenObservation(data=data, weightCol=weightCol, yearCol=yearCol, year0=year0, year1=year1, variables=variables, indexList=indexList,
                                                ids=NULL, whichRows=whichRows, reference=deltaRef,
                                                method = if (deltaType == "absolute") "delta" else if (deltaType == "relative") "relativeDelta",
                                                missingIn=missingIn,
                                                compareFunc=compareFunc,
                                                nCores=nCores, parallelCluster=multiCoreSettings[["parallelCluster"]],
                                                calledInternally = TRUE, verbose = verbose)
  } else {
    deltaDetails <- NULL
  }
  if (verbose) message("Done.")

  # Stop the cluster if it was initialized
  multiCorePost(multiCoreSettings)

  # Condensing the results.
  if (weightAndScore || condense) {
    if (calcOneYear)
      year1Details <- .condenseResults(year1Details, indexList=indexList, variables=variables, weightResult=weightAndScore, giveNames=TRUE)
    if (calcDelta)
      deltaDetails <- .condenseResults(deltaDetails, indexList=indexList, variables=variables, weightResult=weightAndScore, giveNames=TRUE)
    # Create pseudo condensed deltaDetails or year1Details, in case it was not even calculated.
    if (calcOneYear && !calcDelta)
      deltaDetails <- lapply(year1Details, function (x){ x[] <- 0; return (x) })
    if (!calcOneYear && calcDelta)
      year1Details <- lapply(deltaDetails, function (x){ x[] <- 0; return (x) })
  }

  # Don't weight and score
  if (!weightAndScore) {
    return (list(
      year1Details = year1Details,
      deltaDetails = deltaDetails
    ))
  }

  # Calculating the final scores for all observations
  score <- data.frame(
    year1 = sapply(year1Details, function(x) sum(apply(x, 1, function(x)sum(abs(x))))), # abs() is important.
    delta = sapply(deltaDetails, function(x) sum(apply(x, 1, function(x)sum(abs(x)))))) # abs() is important.
  score <- cbind(
    score,
    sum = rowSums(score))

  # Pack everything together and return
  return (list(
    score = score,
    year1Details = year1Details,
    deltaDetails = deltaDetails
  ))
}
