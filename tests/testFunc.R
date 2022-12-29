#### Test functions ####

areEqual <- function (x, y, tolerance=1e-6) {
  if (is.null(x) && is.null(y))
    return (TRUE)
  if (!all(suppressWarnings(class(x) == class(y))))
    return (FALSE)

  # Round to tolerance. Unfortunately, the tolerance argument in assertthat::are_equal() does not work.
  round2 <- function (x, digits) {
    if (is.numeric(x))
      return (round(x, digits))
    return (x)
  }
  invTol <- -log10(tolerance)

  # Case vector (no list)
  if (is.vector(x) & !is.list(x)) {
    # if (!is.vector(y) || is.list(y))
    #   return (FALSE)
    if (mode(x) != mode(y))
      return (FALSE)
    if (length(x) != length(y))
      return (FALSE)
    if (length(names(x)) != length(names(y))) # will also catch NULL, not nULL
      return (FALSE)
    if (!is.null(names(x)) && any(names(x) != names(y)))
      return (FALSE)
    if (any(is.na(x) & !is.na(y)))
      return (FALSE)
    if (any(!is.na(x) & is.na(y)))
      return (FALSE)
    x <- round2(x, digits=invTol)
    y <- round2(y, digits=invTol)
    return (length(which(x != y)) == 0)
  }

  # Case data.frame / matrix
  if (is.data.frame(x) || is.matrix(x)) {
    if (!(is.data.frame(y) || is.matrix(y)))
      return (FALSE)
    if (!areEqual(dim(x), dim(y)))
      return (FALSE)
    if (!areEqual(dimnames(x), dimnames(y)))
      return (FALSE)
    for (col in 1:ncol(x)) {
      if (!areEqual(unname(x[,col]), unname(y[,col]))) {
        return (FALSE)
      }
    }
    return (TRUE)
    # if (is.matrix(x)) x[] <- apply(x, 2, round2, digits=invTol)
    # if (is.matrix(y)) y[] <- apply(y, 2, round2, digits=invTol)
    # if (is.data.frame(x)) x[] <- lapply(x, round2, digits=invTol)
    # if (is.data.frame(y)) y[] <- lapply(y, round2, digits=invTol)
    #
    # errors <- matrix(FALSE, nrow=nrow(x), ncol=ncol(x))
    # for (col in 1:ncol(x)) {
    #   errors[,col] <- x[,col] != y[,col]
    #   errors[,col] <- errors[,col] | (is.na(x[,col]) & !is.na(y[,col]))
    #   errors[,col] <- errors[,col] | (!is.na(x[,col]) &  is.na(y[,col]))
    #   # In places where there is a NA it results from NA in both data sets, therefore it is no error.
    #   errors[is.na(errors[,col]),col] <- FALSE
    #   if (any(errors[,col]))
    #     return (FALSE)
    # }
    # return (TRUE)
  }

  # Case list (no data.frame, was catched above)
  if (is.list(x)) {
    # if (!is.list(y))
    #   return (FALSE)
    # if (!is.data.frame(x)) {
    #   if (is.data.frame(y))
    #     return (FALSE)
    # Recursively for all list entries. Catch warnings/errors and return FALSE.
    returnValue <- tryCatch({
      return (areEqual(names(x), names(y)) && all(unlist(mapply(function(x, y) return (areEqual(x,y)), x=x, y=y))))
    }, error=function(e){
      return (FALSE)
    }, warning=function(w){
      return (FALSE)
    })
    return (returnValue)
    # }
  }

  # Other cases
  return (assertthat::are_equal(x=x, y=y, tolerance=tolerance))
}

testAgainstAndReport <- function (testResult, rightResult, testNo, testName, warn=FALSE, silent=FALSE, tolerance=1e-9) {

  if (areEqual(x=rightResult, y=testResult, tolerance=tolerance)) {
    if (!silent)
      message(paste0("Test no. ", testNo, " passed successfully: ", testName))
    return (0)
  }
  if (silent)
    return (1)
  msg <- paste0("*** FAIL *** - Test no. ", testNo, " failed: ", testName)
  if (warn) {
    cat("------------------------------------\n")
    cat(msg,"\n",sep="") #, call.=FALSE, immediate.=TRUE)
  }
  cat("------------------------------------\n")
  cat("Expected result:\n"); print(rightResult)
  cat("------------------------------------\n")
  cat("Wrong result:\n"); print(testResult)
  cat("------------------------------------\n")
  if (!warn)
    stop (msg, call.=FALSE)
  return (1)
}

expectError <- function (expr, errorMsg, testNo, testName, warn=FALSE, silent=FALSE) {
  tryCatch(eval(expr),
           error=function(e){
             return (testAgainstAndReport(testResult=e$message, rightResult=errorMsg, testNo=testNo, testName=testName, warn=warn, silent=silent))
           }, warning=function(w){
             return (testAgainstAndReport(testResult=paste0("WARNING: ", w$message), rightResult=paste0("ERROR: ", errorMsg), testNo=testNo, testName=testName, warn=warn, silent=silent))
           })
}

expectWarning <- function (expr, errorMsg, testNo, testName, warn=FALSE, silent=FALSE) {
  tryCatch(eval(expr),
           error=function(e){
             return (testAgainstAndReport(testResult=paste0("ERROR: ", e$message), rightResult=paste0("WARNING: ", errorMsg), testNo=testNo, testName=testName, warn=warn, silent=silent))
           }, warning=function(w){
             return (testAgainstAndReport(testResult=w$message, rightResult=errorMsg, testNo=testNo, testName=testName, warn=warn, silent=silent))
           })
}

printTestSkeleton <- function (name = "TEST_NAME_HERE", number = "NUMBER_HERE", warn = TRUE) {
  cat("
      #### ", name ," ####
      errorLvl = errorLvl + local({
      # Prepare data & calculate results
      testResult <- TEST_RESULT_HERE

      # Define right results
      rightResult <- RIGHT_RESULT_HERE

      # Test
      return (
      testAgainstAndReport(testName = \"", name, "\", testResult = testResult, rightResult = rightResult, testNo = ", number, ", warn = ", warn, ")
      )
      })", sep="")
}

printTestScriptSkeleton <- function () {
  cat("
      {
      #### *** START *** ####

      errorLvl <- 0

      message(
      \"*************\\n\",
      \"*** Start ***\\n\",
      \"*************\\n\")




      printTestSkeleton()




      #### *** END *** ####

      message(\"\")

      if (errorLvl != 0)
      stop (
      \"***************\\n\",
      \"*** FAILURE ***\\n\",
      \"***************\\n\",
      \"See warnings above and check which tests were not successful.\")

      message(
      \"***************\\n\",
      \"*** SUCCESS ***\\n\",
      \"***************\\n\",
      \"All tests passed successfully.\"
      )
      }
      ", sep="\n")
      }

#### dput2 - more compact than dput ####
dput2 <- function (x) {
  if (is.data.frame(x)) {
    str0 <- sapply(x, function (x) {
      if (is.factor(x))
        return (paste0("factor(c('", paste0(x, collapse="', '"), "')",
                       ", levels=c('", paste0(levels(x), collapse="', '"), "')",
                       ")"))
      notChar <- !is.character(x)
      if (is.integer(x))
        x <- paste0(x, "L")
      if (notChar)
        return (paste0("c(", paste0(x, collapse=", "), ")"))
      return (paste0("c('", paste0(x, collapse="', '"), "')"))
    })
    classes <- sapply(x, class)
    classes <- paste0("c(\"", paste0(classes, collapse="\",\""), "\")")

    str <- paste0(str0, collapse=", ") #paste0( paste0("`",names(str0), "`=", str0), collapse=", ")
    str <- paste0("local({ asdofiue <- data.frame(", str, ", stringsAsFactors=FALSE); ",
                  "classes <- ",  classes, ";",
                  "for(i in 1:ncol(asdofiue)) class(asdofiue[,i]) <- classes[i];",
                  "colnames(asdofiue) <- c(", paste0(paste0("'", names(str0), "'", sep=""), collapse=", "), "); ",
                  "rownames(asdofiue) <- c(", paste0(paste0("'", rownames(x), "'", sep=""), collapse=", "), "); ",
                  "return (asdofiue); })\n", sep="")
    cat(str)
      } else {
        dput(x)
      }
}