#### mean.weight and helper functions ####
# See also utils in package avgImpute

mean.weight <- function(data, weights=NULL, index=NULL, cols=NULL,
                        fixed.index=FALSE, index.of.result=NULL, index.sep="_",
                        trunc.probs=NULL, calc.sum=FALSE, digits=NULL, na.rm=TRUE,
                        edit.I.colnames=FALSE, del.I.help.columns=FALSE, I.help.columns=NULL,
                        change.output.str=FALSE, called.internally=FALSE){
  # This function calculates the weighted mean of all variables in a possibly indexed data.frame or matrix.

  # Arguments
  # data = data of which the weighted means should be calculated. Can be data.frame, matrix or vector
  #        If any colname of data contains an expression like I(Var_A/Var_B), then the the "weighted mean of the ratio" is calculated.
  #           This is done by building a model.matrix() of the result matrix.
  #           Use function extract.I.vars() to add all variables to your data frame that are used in the formula
  # weights =         weights for the weighted mean calculation
  # index =           index in the same structure as used in tapply(). Can be a vector or list of vectors.
  # trunc.probs = The probability of quantiles within which the truncated mean should be calculated.
  # calc.sum =        Should sum(data*weights) should be calculated, rather than weighted means?
  # digits =          digits for rounding the results
  # na.rm =           na action
  # edit.I.colnames = Should the colnames containing expressions with I() be edited, such that I() won't be there anymore? TRUE/FALSE

  # Wenn innerhalb eines Indexes mehrere Indexe als Listen abgelegt sind, wird die Berechnung fuer alle Indexe gemacht.
  #if(is.list(index)){
  #  if(any(sapply(index,function(x)is.list(x)))){
  #    return(do.call("rbind", lapply(index, function(x)mean.weight(data=data, weights=weights, index=x, digits=digits, na.rm=na.rm, edit.I.colnames=edit.I.colnames, del.I.help.columns=del.I.help.columns, I.help.columns=I.help.columns))))
  #  }
  #}

  if(is.list(data) && !is.data.frame(data)) stop("data must be matrix or data.frame but not a list.")

  if(!is.null(cols)) {
    # Check if some cols (possibly in I(a+b) columns) are missing. If yes, throw an error.
    .checkMissingICols(cols, colnames(data))
    # Extract all columns from I() cols and create I()-cols if they don't yet exist in data.
    cols_add <- extract.I.vars(cols, keep.only.necessary=TRUE)
    data <- create.cols(data, cols)
    cols_all <- c(cols, cols_add)
    data <- data[,cols_all,drop=FALSE]
  } else {
    cols <- colnames(data)
    cols_all <- colnames(data)
  }

  if (!is.null(trunc.probs)){
    if (length(trunc.probs) != 2)
      stop ("If trunc.probs is not NULL, then it must be a numeric vector of length 2.")
    if (min(trunc.probs) < 0 || max(trunc.probs) > 1)
      stop ("If trunc.probs is not NULL, then it must be a numeric vector of length 2 with values ranging from 0 to 1 (inclusive).")
    trunc.probs <- sort(trunc.probs)
  }

  # Fixed index ausschalten, wenn Index ein Vektor ist. Dann bringt es nichts.
  if(fixed.index && is.null(index.of.result) && !is.list(index)) stop("fixed.index & is.null(index.of.result) & !is.list(index)   -> fixed.index doesn't have any effect this way. Give index as a list!")

  # Im Falle, dass der index fixiert sein soll, hier die rohe Ergebnisstruktur erstellen.
  if(fixed.index){
    if (is.null(dim(data))) {
      rawResult <- .prepare.fixed.index.result(data=data, index=index, names.result=index.of.result, edit.I.colnames=edit.I.colnames)
    } else {
      # In this case it more efficient to create the result here. Recurive function call will be with fixed.index=FALSE!
      rawResult <- .prepare.fixed.index.result(data=data[,cols,drop=FALSE], index=index, names.result=index.of.result, edit.I.colnames=edit.I.colnames)
    }
    index <- .paste.elements(index, sep="_", errorMsg="All indices must have same length!")
  }

  # Index muss eine List mit folgender Struktur sein:
  isNullIndex <- is.null(index)
  if(!is.list(index)) index <- list(index)

  # Im Falle, dass !is.null(dim(data)) folgt eine rekursive Funktionsdefinition!
  if(!is.null(dim(data))) {
    if (!isNullIndex) {
      all.na.index <- sapply(index,function(x)all(is.na(x)))
      if(any(all.na.index)) stop("The following indices contain only NA values. Please change to a different value (not NA): ", paste0(names(all.na.index)[all.na.index],collapse=","))
    }

    if (!change.output.str && length(index) > 1) {
      index <- list(.paste.elements(index, sep="_", errorMsg="All indices must have same length!"))
    }
    # Wenn !is.null(dim(data))
    # & es keinen oder nur einen Index gibt:
    if(is.null(index) || length(index)==1) {

      # Berechnung rekursiv fuer matrix / data.frame
      if(is.matrix(data)) {
        if(nrow(data)==0) stop("nrow of data is 0")
        result <- apply(data[,cols_all,drop=FALSE], 2, function(x)mean.weight(data=x, weights=weights, index=index, fixed.index=FALSE, index.of.result=NULL, index.sep=index.sep, trunc.probs=trunc.probs, calc.sum=calc.sum, digits=digits, na.rm=na.rm, edit.I.colnames=edit.I.colnames, del.I.help.columns=del.I.help.columns, I.help.columns=I.help.columns))
      } else if(is.data.frame(data)) {
        if(nrow(data)==0) stop("nrow of data is 0")
        #result <- sapply(data, function(x)  mean.weight(data=x, weights=weights, index=index, fixed.index=FALSE, index.of.result=index.of.result, index.sep=index.sep, calc.sum=calc.sum, digits=digits, na.rm=na.rm, edit.I.colnames=edit.I.colnames, del.I.help.columns=del.I.help.columns, I.help.columns=I.help.columns))
        result <- as.matrix(as.data.frame(lapply(data[,cols_all,drop=FALSE], function(x)  mean.weight(data=x, weights=weights, index=index, fixed.index=FALSE, index.of.result=NULL, index.sep=index.sep, trunc.probs=trunc.probs, calc.sum=calc.sum, digits=digits, na.rm=na.rm, edit.I.colnames=edit.I.colnames, del.I.help.columns=del.I.help.columns, I.help.columns=I.help.columns)),stringsAsFactors=FALSE))
      }
      # Wieder zu Marix machen, falls es ein Vektor ist
      if(is.null(dim(result))) {
        result <- t(as.matrix(result))
        if(!isNullIndex && length(index)==1 && length(unique(index[[1]]))==1)
          rownames(result) <- index[[1]][1]
      }
      if (nrow(result) == 1 && is.null(rownames(result)) && !is.null(index)) {
        rownames(result) <- sort(unique(index[[1]]))
      }
      # Wieder die alten Colnames vergeben
      colnames(result) <- cols_all

      # Falls eine Expression mit I() in einem der colnames ist, werden diese Kennzahlen neu berechnet.
      # Konkret wird statt "weighted mean of ratio" das "ratio of weighted means" berechnet.
      cn.res <- colnames(result) # cn.res.orig
      icols <- substr(cn.res,1,2)=="I("
      if(any(icols)){
        if(!is.null(digits)) stop("When rounding (digts!=NULL) and using I() columns, the results might not be accurate")
        # Wert der I() columns berechnen
        result <- calc.I.cols(result, edit.I.colnames=edit.I.colnames, del.I.help.columns=del.I.help.columns, I.help.columns=I.help.columns)
      }

      if(edit.I.colnames){
        cols <- .rm.I.from.names(cols)
      }
      # Ergebnis in fixierte Ergebnisstrutkur einfuegen.
      if(fixed.index){
        result <- result[rownames(result)%in%rownames(rawResult),cols,drop=FALSE]
        rawResult[match(rownames(result),rownames(rawResult)),cols] <- result
        result <- rawResult
      }
      # Resultat ausgeben.
      if(nrow(result)==1 && isNullIndex) {
        return(result[1,cols]) #rownames(result) <- NULL
      } else {
        return(result[,cols,drop=FALSE])
        #return (result[, cols])
      }

      # Wenn !is.null(dim(data))
      # & 2 Indexe eingegeben wurden:
    } else if (length(index)==2) {
      # res1 <- mean.weight(data=data[,1], weights=weights, index=index, calc.sum=calc.sum, digits=digits, na.rm=na.rm)

      # Hier keine Fallunterscheidung zwischen matrix und data.frame einfuegen, sonst funktioniert es nicht!!
      res.prov <- apply(data[,cols_all,drop=FALSE], 2, function(x) mean.weight(data=x, weights=weights, index=index, calc.sum=calc.sum, digits=digits, na.rm=na.rm, trunc.probs=trunc.probs) )
      if (!"matrix" %in% class(res.prov))
        res.prov <- t(as.matrix(res.prov))

      res.list <- list()
      su.index1 <- sort(unique(index[[1]]))
      su.index2 <- sort(unique(index[[2]]))
      for(i in 1:ncol(res.prov)){
        res.list[[i]] <- matrix(res.prov[,i],nrow=length(su.index1), ncol=length(su.index2))
        dimnames(res.list[[i]]) <- list(su.index1, su.index2)
      }
      names(res.list) <- cols_all

      # Falls eine Expression mit I() in einem der colnames ist, werden diese Kennzahlen neu berechnet.
      # Konkret wird statt "weighted mean of ratio" das "ratio of weighted means" berechnet.
      cn.res <- names(res.list)
      icols <- grepl("I\\(", cn.res)
      if(any(icols)){
        if(!is.null(digits)) stop("When rounding (digts!=NULL) and using I() columns, the results might not be accurate")
        #if(any(cn.res%in%c("_","."))) stop("When using I() colnames _ and . are not allowed.")
        res.list <- calc.I.cols(res.list, edit.I.colnames=edit.I.colnames, del.I.help.columns=del.I.help.columns, I.help.columns=I.help.columns)
        if(edit.I.colnames){
          cols <- .rm.I.from.names(cols)
        }
      }
      return(res.list[cols])

    } else if(length(index)>2) {
      stop("more than 2 indexes not possible if data is a matrix/data.frame. Please enter data as vector.")
    }
  }

  # Tatsaechliche mean.weight() Funktion.
  # Falls es keine numerische Variable ist (weil z.B. ein durchmischter data.frame eingegeben wird),
  # wird daraus eine 0 gemacht, damit die Funktion trotzdem funktioniert.
  if (!(is.numeric(data) || is.logical(data)))
    data <- rep(0, length(data))
  if (is.null(weights))
    weights <- rep(1,length(data))

  # Falls kein index gegeben wurde, einfache Berechnung (mit weighted.mean)
  if (is.null(index) || is.null(index[[1]])) {
    if (!is.null(trunc.probs)) {
      q12 <- quantile.weight(data, weights = weights, probs = trunc.probs, na.rm=na.rm)
      filter <- q12[1] <= data & data <= q12[2]
      data <- data[filter]
      weights <- weights[filter]
    }
    if(calc.sum){
      result <- sum(data * weights, na.rm=na.rm)
    } else {
      result <- weighted.mean(data, weights, na.rm=na.rm)
    }

    # Sonst muss mit index und tapply() gerechnet werden.
  } else {
    index <- lapply(index, function(x)
      if (length(x)==1) return (rep(x,length(weights))) else return (x))
    length.index <- sapply(index, length)
    if (any(length.index!=length.index[1]))
      stop ("All vectors in the index have to have the same length!")
    if (!all(length(weights)==length.index))
      stop ("length(weights)!=length(index)")

    # Daten auf quantile einschraenken.
    if (!is.null(trunc.probs)) {
      if (length(index) > 1)
        stop ("If trunc.probs is not NULL, then only one dimensional indices can be chosen.")
      newData <- do.call("rbind", by(data.frame(data=data, weights=weights, index=index[[1]]), index[[1]], function (x){
        q12 <- quantile.weight(x[,"data"], weights = x[,"weights"], probs = trunc.probs, na.rm=na.rm)
        filter <- q12[1] <= x[,"data"] & x[,"data"] <= q12[2]
        return (x[filter,])
      }))
      data <- newData[,"data"]
      weights <- newData[,"weights"]
      index <- newData[,"index"]
    }

    # NA Werte in weights uebertragen. Muss so sein, nicht mit na.rm innerhalb der Funktionen, da sonst data und weights evtl. nicht korrespondieren!!
    dataweights <- data*weights
    weights[is.na(dataweights)] <- NA

    if(calc.sum){
      # Resultat = Summe ( Werte * Gewichte )
      result <-  tapply(dataweights,index,  sum,na.rm=na.rm)
    } else {
      # Resultat = Summe ( Werte * Gewichte )                             / Summe( Gewichte )
      result <-  tapply(dataweights,index,  sum,na.rm=na.rm) / tapply(weights,index,  sum,na.rm=na.rm)
    }

    # Resultat in vorgefertige fixierte Index-Struktur einfuegen
    if(fixed.index){
      result <- result[names(result)%in%names(rawResult)]
      rawResult[match(names(result),names(rawResult))] <- result
      result <- rawResult
    }
  }

  # Falls gewuenscht, runden, dann Ergebnis ausgeben.
  if(!is.null(digits)) result <- round(result, digits)
  return(result)
}


#vars <- c("asd","efe+p", "c-1", "f*c", "a/ b", "A^B", "c,d", "a==b", "ifelse(a==b, 1, 2)")
extract.I.vars <- function(vars, keep.original=FALSE, keep.only.necessary=TRUE, original.single.vars.only=FALSE){
  # This function extracts all Variables in a I(a+b*c) formula seperately. This is useful in combination with the function mean.weight()
  if (any(grepl("ifelse", vars)))
    message("extract.I.vars: ifelse is defined as an operator and should not be used in variable names.")
  operatorRegex <- "\\(|\\)|\\-|/|\\*|\\+|\\^|,|=|ifelse"
  vars_all <- vars[grep(operatorRegex,vars)]
  vars_all <- unlist(strsplit(vars_all,"-|/|\\*|\\+|\\^|,|=|ifelse"))
  vars_all <- gsub("I\\(|\\(|\\)| ","",vars_all)
  vars_all <- unique(vars_all[vars_all!=""])
  vars_all <- vars_all[is.na(suppressWarnings(as.numeric(vars_all)))] # Only non-numbers.
  if(keep.only.necessary) vars_all <- vars_all[!vars_all%in%vars]
  if(keep.original) {
    if (original.single.vars.only) {
      vars_all <- unique(c(vars[!grepl(operatorRegex, vars)], vars_all))
    } else {
      vars_all <- unique(c(vars, vars_all))
    }
  }
  return(vars_all)
}

#gb <- load.gb(); data <- gb; cols <- c("I(ArbVerdFamilie/JAE_FamAK)", "I(LE+NE_tot)", "JAE_FamAK"); non.I.value=NA
create.cols <- function(data, cols, non.I.value=NA_integer_){
  # This function creates columns in a data.frame that do not exist yet.
  # If they are written as I(), their value is filled with the right value.
  # Arguments
  # data         data.frame to which columns should be added
  # cols         all new colums that should be created in data.frame. Can also be without I().
  # non.I.value  The value that is filled into columns that don't have colnames I().

  # Create new columns
  cols_new <- cols[!cols%in%colnames(data)]
  if (length(cols_new)>0)
    data[,cols_new] <- non.I.value
  # Calculate the value of the new columns with I()
  if(any(substr(cols,1,2)=="I("))
    data <- calc.I.cols(data)
  # Return result
  return(data)
}

#gb <- load.gb(); data <- gb; data[,c("I(ArbVerdFamilie/JAE_FamAK)", "I(LE+NE_tot)","NA")] <- 1
#h(calc.I.cols(data))
#data <- matrix(c(1:30),ncol=3); colnames(data) <- c("a","b","I(a+asdf)")
#data <- as.list(as.data.frame(data))
calc.I.cols <- function(data, edit.I.colnames=FALSE, del.I.help.columns=FALSE, I.help.columns=NULL) {
  # This function calculates the value of all columns with colnames looking like I(a+b) in a matrix/data.frame/list.
  # The whole matrix/data.frame/list is returned after having finished the caluclations.
  # Arguments
  # data               matrix/data.frame/list. Columns to be calculated must have names like I(a+b), I(a/b). Only "a+b" does not work.
  # edit.I.colnames    Should the brackets I() in the colnames be removed after the calculations?
  # del.I.help.colums  If e.g. I(a+b) is calculated. Should the columns "a" and "b" be removed after the calculation because they are of no interest?
  # I.help.columns     If del.I.help.colums=TRUE: The list of the columns to be deleted can be specified.
  #                    Otherwise the help columns are dectected automatically.

  ismat <- is.matrix(data)
  if(ismat) data <- as.data.frame(data)

  i_cols <- names(data)
  i_cols <- i_cols[startsWith(i_cols, "I(") & endsWith(i_cols,")")]
  # If there are no i_cols then return the original data without calculations.
  if(length(i_cols)==0) return(data)

  # Calc only if there are elements. Otherwise this would yield an error.
  if(length(data[[1]])>0){
    for(i in 1:length(i_cols)){
      tryCatch({
        data[[i_cols[i]]][] <- as.vector(with(data, eval(parse(text=i_cols[i]))))
      }, error=function(e){
        if(grepl("unexpected", e$message)) {
          stop (paste0("The calculation syntax in a column like 'I(a+b)' is errorneous. See the error message below.\n", gsub("<[^u]+: ","",e$message)), call.=FALSE)
        } else {
          stop (paste0("The calculation in a column like 'I(a+b)' is not possible, probably because a variable is missing. See the error message below.\n", i_cols[i], ", ", e$message), call.=FALSE)
        }
      })

    }
  }

  if(del.I.help.columns){
    i_cols <- startsWith(names(data), "I(") & endsWith(names(data),")")
    if(!is.null(I.help.columns)){
      delnames <- I.help.columns
    } else {
      delnames <- unlist(strsplit(names(data)[i_cols],"-|/|\\*|\\+"))
      delnames <- gsub("I\\(|\\(|)","",delnames)
    }
    data <- data[!names(data)%in%delnames] # ALT, geht nicht fuer Listen: #data <- data[,!names(data)%in%delnames,drop=FALSE]
  }
  if(edit.I.colnames){
    names(data) <- .rm.I.from.names(names(data))
  }

  if(ismat) data <- as.matrix(data)
  return(data)
}

.rm.I.from.names <- function(x){
  i_x <- startsWith(x, "I(") & endsWith(x,")")
  x[which(i_x)] <- substr( x[which(i_x)], 3, nchar(x[which(i_x)])-1 )
  return(x)
}

.checkMissingICols <- function (colsToCheck, colsAvailable) {
  if (!is.null(dim(colsAvailable)))
    colsAvailable <- colnames(colsAvailable)
  if (is.null(colsAvailable))
    stop ("colsAvailable must not be NULL")
  colsMissing <- extract.I.vars(colsToCheck, keep.original=TRUE, keep.only.necessary=FALSE, original.single.vars.only=TRUE)
  colsMissing <- colsMissing[!colsMissing%in%colsAvailable]
  if (length(colsMissing)>0) {
    iToShow <- colsToCheck[startsWith(colsToCheck, "I(") & grepl(paste0(colsMissing, collapse="|"), colsToCheck)]
    addTxt <- if (length(iToShow) == 0) NULL else paste0("\nSome of them are located in 'I()' formulas: ", paste0(iToShow, collapse=", "))
    stop (paste0("Some columns don't exist in the given data.frame/matrix: ",
                 paste0(colsMissing, collapse=", "),
                 addTxt))
  }
  return (invisible(NULL))
}

paste.cols <- function(dat, cols=NULL, sep="_") {
  # Paste Values of columns of a data frame
  if(is.null(dim(dat))) return(dat)
  if(is.matrix(dat)) dat <- as.data.frame(dat)
  if(is.null(cols)) cols <- colnames(dat)
  return( eval(parse(text= paste0( "paste(", paste( paste0("dat[,'",cols,"']"), collapse=", "), ", sep='",sep,"')") )) )
}

.paste.elements <- function(l, sep="_", errorMsg="All list places must have same length!"){
  if(!is.list(l) && !is.matrix(l)){
    return(l)
  }
  if(is.matrix(l) | is.data.frame(l)) {
    return(paste.cols(l, sep=sep))
  }
  if(length(unique(unlist(lapply(l,function(x)length(x)))))>1) {
    stop(errorMsg)
  }
  paste_own <- function(...) paste(..., sep=sep)
  return( do.call("paste_own", l) )
}

.prepare.fixed.index.result <- function(data, index, names.result, index.sep="_", edit.I.colnames=FALSE){
  # This function is internally used in mean.weight and variance.estimate
  if(!is.null(dim(data))){
    rawResult <- tapply.fixed(X=data[,1], INDEX=index, FUN=sum, names.result=names.result, vector.result=TRUE)
    nam <- names(rawResult)
    rawResult <- matrix(NA, nrow=length(rawResult), ncol=ncol(data))
    rownames(rawResult) <- nam;
    if(!edit.I.colnames) {
      colnames(rawResult) <- colnames(data)
    } else {
      colnames(rawResult) <- .rm.I.from.names(colnames(data))
    }
  } else {
    rawResult <- tapply.fixed(X=data, INDEX=index, FUN=sum, names.result=names.result, sep.sign=index.sep, vector.result=TRUE)
    rawResult[] <- NA
  }
  return(rawResult)
}

tapply.fixed <- function(X, INDEX, FUN, names.result=NULL, missing.value=NA, vector.result=FALSE, sep.sign="_", warn=FALSE){
  # This function puts the result of tapply(X,INDEX,FUN) into a fixed given vector with names=names.result.
  # This is especially useful if some entries are missing in INDEX but you want them to be displayed as well!
  # Otherwise they would be missing in the result of the tapply() function.
  #
  # Arguments:
  # X, INDEX, FUN: See help page for tapply()
  # names.result = The names of the resulting vector (including all possible 0/NA entries).
  # missing.value = Which value should be put into the resulting vector if there were no entries in INDEX?
  # sep.sign = The sign to separate the new vector index and names.result if INDEX is a list.
  # vector.result = Should the result be presentet in a vector (one dimension) or in a multidimensional array?

  if(!is.null(dim(X))) stop("This function only works for vectors! dim(X) must be NULL!")
  if(length(INDEX)==1) vector.result=TRUE
  if(is.matrix(INDEX)) INDEX <- as.data.frame(INDEX, stringsAsFactors=FALSE)

  # Falls names.result eine Liste ist, werden alle moeglichen Kombinationen der Listenplaetze
  # zusammengestelt und damit ein Vektor erstellt
  if(is.list(names.result) & vector.result){
    if(length(names.result)==1) {
      names.result <- sort(unique(names.result[[1]]))
    } else {
      su.index1 <- sort(unique(names.result[[1]]))
      su.index2 <- sort(unique(names.result[[2]]))
      nres1 <- paste( rep.1b1(su.index1, length(su.index2)) , rep(su.index2, length(su.index1)) , sep=sep.sign)
      if(length(names.result)>2){
        for(i in 3:length(names.result)){
          su.index1 <- nres1
          su.index2 <- sort(unique(names.result[[i]]))
          nres1 <- paste( rep.1b1(su.index1, length(su.index2)) , rep(su.index2, length(su.index1)) , sep=sep.sign)
        }
      }
    }
  }

  # Vorbereiten von INDEX und names.result
  # Wenn der Index eine Liste ist...
  if(is.list(INDEX)) {
    l.INDEX <- sapply(INDEX, function(x)if(!is.null(dim(x))) nrow(x) else length(x))
    if(any(l.INDEX!=l.INDEX[1])) stop(paste0("All indexes must have the same length! ", paste0(l.INDEX,collapse=" ") ))
    if(l.INDEX[1]==0 && length(X)>0) stop(paste0("INDEX(es) has/have length=0 or nrow=0."))

    # Wenn das Resultat 1 Dimension haben soll.
    if(vector.result){
      if(is.null(names.result)){
        if(length(l.INDEX)==1) {
          names.result <- sort(unique(INDEX[[1]]))
        } else {
          su.index1 <- sort(unique(INDEX[[1]]))
          su.index2 <- sort(unique(INDEX[[2]]))
          names.result <- paste( rep.1b1(su.index1, length(su.index2)) , rep(su.index2, length(su.index1)) , sep=sep.sign)
          if(length(INDEX)>2){
            for(i in 3:length(INDEX)){
              su.index1 <- names.result
              su.index2 <- sort(unique(INDEX[[i]]))
              names.result <- paste( rep.1b1(su.index1, length(su.index2)) , rep(su.index2, length(su.index1)) , sep=sep.sign)
            }
          }
        }
      }
      pasteown <- function(...) paste(..., sep=sep.sign)
      INDEX <- do.call(pasteown, INDEX)

      # Wenn das Resultat mehrere Dimensionen haben soll.
    } else {
      if(is.null(names.result)){
        names.result <- lapply(INDEX,function(x)sort(unique(x)))
      }
    }
    # Wenn der Index keine Liste ist.
  } else {
    if(is.null(names.result)) names.result <- sort(unique(INDEX))
  }


  # Resultate-Berechnung im Falle mehrdimensionaler Ergebnisstruktur.
  if(is.list(INDEX) & !vector.result){
    # Hier Ergebnis-Berechnung
    res0 <- tapply(X,INDEX,FUN)
    if(!is.na(missing.value)) res0[is.na(res0)] <- missing.value

    nres0 <- unlist(dimnames(res0))
    nres1 <- unlist(names.result)
    # Warnung ausgeben, falls nicht alle Ergebnisse ausgegeben werden wegen zu kurzem names.result
    if(warn) if(any(!nres0%in%nres1))
      warning(paste0("For some entries in X no corresponding entries in names.result were given. The resulting array is incomplete!\n", paste(nres0[!nres0%in%nres1], collapse=" ") ))
    # Aufwaendige Uebertragung nur machen, wenn es wirklich fehlende Eintraege in res0 gibt
    # ALTE Bedingung: if(length(res0)!=length(nres1) || names(res0)!=nres1){
    if(length(res0)!=length(nres1) || length(names(res0))==0 || any(names(res0)!=nres1)){
      # Ergebnis-Strukturen fuer Matching vorbereiten.
      su.index1 <- sort(unique(INDEX[[1]]))
      su.index2 <- sort(unique(INDEX[[2]]))
      nres0 <- paste( rep.1b1(su.index1, length(su.index2)) , rep(su.index2, length(su.index1)) , sep=sep.sign)
      if(length(INDEX)>2){
        for(i in 3:length(INDEX)){
          su.index1 <- nres0
          su.index2 <- sort(unique(INDEX[[i]]))
          nres0 <- paste( rep.1b1(su.index1, length(su.index2)) , rep(su.index2, length(su.index1)) , sep=sep.sign)
        }
      }
      su.index1 <- sort(unique(names.result[[1]]))
      su.index2 <- sort(unique(names.result[[2]]))
      nres1 <- paste( rep.1b1(su.index1, length(su.index2)) , rep(su.index2, length(su.index1)) , sep=sep.sign)
      if(length(names.result)>2){
        for(i in 3:length(names.result)){
          su.index1 <- nres1
          su.index2 <- sort(unique(names.result[[i]]))
          nres1 <- paste( rep.1b1(su.index1, length(su.index2)) , rep(su.index2, length(su.index1)) , sep=sep.sign)
        }
      }
      res1 <- array(missing.value, dim=lapply(names.result,function(x)length(x)), dimnames=names.result)
      res1[nres1%in%nres0] <- res0[nres0%in%nres1]
      if(is.null(dim(res1))) res1 <- as.array(res1); return(res1)
      # Sonst Original-Ergebnis ausgeben
    } else {
      if(is.null(dim(res0))) res0 <- as.array(res0); return(res0)
    }

    # Resultate-Berechnung im Falle von Vektor-Ergebnisstruktur
  } else{
    res0 <- tapply(X,INDEX,FUN)
    if(!is.na(missing.value)) res0[is.na(res0)] <- missing.value

    # Warnung ausgeben, falls nicht alle Ergebnisse ausgegeben werden wegen zu kurzem names.result
    if(warn) if(any(!names(res0)%in%names.result))
      warning(paste0("For some entries in X no corresponding entries in names.result were given. The resulting array is incomplete!\n", paste(names(res0)[ !names(res0)%in%names.result ], collapse=" ") ))
    # Aufwaendige Uebertragung nur machen, wenn es wirklich fehlende Eintraege in res0 gibt
    if(length(res0)!=length(names.result) || any(names(res0)!=names.result)){
      res1 <- rep(missing.value, length(names.result))
      names(res1) <- names.result
      ind <- match(names(res0),names(res1))
      if(any(is.na(ind))){
        res0 <- res0[!is.na(ind)]
        ind <- ind[!is.na(ind)]
      }
      res1[ ind ] <- res0
      if(is.null(dim(res1))) res1 <- as.array(res1); return(res1)
      # Sonst Original-Ergebnis ausgeben
    } else {
      if(is.null(dim(res0))) res0 <- as.array(res0); return(res0)
    }
  }
}

#X=round(runif(100,1,15)); names.result=1:5;
#table.fixed(X, names.result=1:5)
#table.fixed(round(runif(100,1,15)), round(runif(100,1,15)), names.result=list(1:20, 5:15), vector.result=FALSE)
table.fixed <- function(..., names.result=NULL, vector.result=FALSE, sep.sign="_") {
  # This function puts the result of table(X) into a fixed given vector with names=names.result.
  # It is a wrapper function for tapply.fixed().
  # For information on the arguments see tapply.fixed()
  INDEX <- list(...)
  if(is.list(INDEX[[1]])) INDEX <- INDEX[[1]]
  # Kuenstlich Daten erzeugen, falls keine vorhanden sind. Ist noetig, damit es keinen Fehler in tapply.fixed gibt.
  #if(length(INDEX[[1]])==0){
  #  INDEX[[1]] <-
  #}
  tapply.fixed(X=rep(1,length(INDEX[[1]])), INDEX=INDEX, FUN=function(x)length(x), names.result=names.result, missing.value=0, vector.result=vector.result, sep.sign=sep.sign)
}

rep.1b1 <- function(vector,times){
  if(length(times)==1) {
    return(rep(vector, each=times))
  } else {
    if(length(vector)!=length(times)) stop("If length(times)>1 then condition length(vector)==length(times) must hold.")
    return(rep(vector, times))
  }
}


#### weighted quantiles ####
median.weight <- function(...) return(quantile.weight(..., probs=0.5))

#x <- gb[,"ArbVerd_jeFJAE"]; weights <- gb[,"Gewicht"];index <- gb[,c("Jahr","Region","Betriebstyp_S3")]; probs=0.5; na.rm=TRUE
#quantile.weight(x=gb[,c("LE","ArbVerd_jeFJAE")], weights=gb[,"Gewicht"], index=gb[,c("Jahr")], probs=c(0.25,0.5))
quantile.weight <- function(x, weights=NULL, index=NULL, probs=0.5, cols=NULL, na.rm=TRUE) {
  # This function calculates weighted quantiles.
  # Arguments:
  # x       = Vector of numbers of which quantiles should be calculated
  # weights = vector of weights
  # probs   = probabilities of quantiles

  # Original function was cwhmisc::w.median. Alternative function with same result is reldist::wtd.quantile
  # Differents result calculated by these functions: Hmisc::wtd.quantile, matrixStats::weightedMedian (test with RefB, Jahr 2012, gb[,"ArbVerd_jeFJAE"])

  # Recursive function definition if x is given as matrix, data.frame or list
  if(!is.null(dim(x))) {
    if(!is.null(cols)) {
      # Check if some cols (possibly in I(a+b) columns) are missing. If yes, throw an error.
      .checkMissingICols(cols, colnames(x))
      # Extract all columns from I() cols and create I()-cols if they don't yet exist in data.
      cols_add <- extract.I.vars(cols, keep.only.necessary=TRUE)
      x <- create.cols(x, cols)
      cols_all <- c(cols, cols_add)
      x <- x[,cols_all,drop=FALSE]
    } else {
      cols <- colnames(x)
      cols_all <- colnames(x)
    }

    #cn_x <- colnames(x)
    if(is.matrix(x)) res <- apply(
      x[,cols_all,drop=FALSE],
      2,
      function(x)quantile.weight(x=x,weights=weights,index=index,probs=probs,na.rm=na.rm))
    if(is.data.frame(x)) res <- as.data.frame(lapply(
      x[,cols_all,drop=FALSE],
      function(x)quantile.weight(x=x,weights=weights,index=index,probs=probs,na.rm=na.rm)),
      stringsAsFactors=FALSE)
    #colnames(res) <- cn_x
    colnames(res) <- cols_all
    return (res[,cols,drop=FALSE])
  }
  if(is.list(x)) {
    if (!is.null(cols))
      stop ("x must not be a list, when cols are specified. Use a data.frame or a matrix as input.")
    return( lapply(x,function(x)quantile.weight(x=x,weights=weights,index=index,probs=probs,na.rm=na.rm)) )
  }

  # Recursive function definition if index is given
  if(!is.null(index)){
    if (is.factor(x))
      x <- as.character(x)
    res <- by(cbind(x=x, weights=weights),
              index,
              function(x) quantile.weight(x=x[,"x"], weights=if(is.null(weights)) NULL else x[,"weights"], probs=probs, na.rm=na.rm))
    attr(res,"call") <- NULL
    if(length(res[[1]])>1) res <- do.call("rbind", res) else if(length(dim(res))==1) res <- c(res) else class(res) <- "array"
    return(res)
  }

  if(is.null(weights)) weights <- rep(1, length(x))
  w <- weights

  # Recursive function definition for more than 1 probs
  if(length(probs)>1){
    res <- apply(matrix(probs),1,function(probs)  quantile.weight(x=x, weights=w, probs=probs, na.rm=na.rm) )
    names(res) <- paste0(round(probs*100,2),"%")
    return(res)

    # Now follows the actual function to calculate weighted means
  } else {
    if (!is.numeric(x))
      x <- rep(0, length(x))
    if (!is.numeric(w))
      w <- rep(1, length(x))
    if(na.rm) {
      ok <- complete.cases(x, w)
      x <- x[ok]
      w <- w[ok]
    }
    w_not0 <- w!=0
    x <- x[w_not0]
    w <- w[w_not0]

    if(length(x)==0) return(NA)

    ind <- sort.list(x)
    x <- x[ind]
    w <- w[ind]
    ind1 <- min(which(cumsum(w)/sum(w) >= probs))
    ind2 <- if ((w[1]/sum(w)) > probs) {
      1
    } else {
      max(which(cumsum(w)/sum(w) <= probs))
    }
    max(x[ind1], x[ind2])
  }
}

####
quantile.inverse <- function(x, value){ # quantile.reverse inverse.quantile reverse.quantile
  # This funciton acts as a inverse quantile function.
  # However, it does not yield exactly the same results as you would get using the quantile() function because the distribution functions differ.
  # Arguments
  # x     = All values in the sample.
  # value = The value of which the probability in the cumulative distribution should be calculated.
  invProb <- ecdf(x)(value)
  invProbs <- c(invProb-0.01,invProb,min(1,invProb+0.01))
  values <- quantile(x,invProbs)
  return(approx(x=values,y=invProbs,xout=value)$y)
}


#### Other ####
#' Install a package from CRAN, but only if it is not yet installed
#' @keywords internal
#' @author Daniel Hoop
#' @param x The name(s) of the package(s).
#' @param ... Further arguments passed into \code{\link[utils:install.packages]{utils::install.packages}}.
#' @return Invisible \code{NULL}
installFromCRAN <- function (pkgs, ...) {
  pkgs <- pkgs[!pkgs%in%rownames(installed.packages())];
  if (length(pkgs>0))
    install.packages(pkgs, ...)
}

#' @title Multicore preparations
#' @description Installs required packages, registers a parallel cluster and returns it as value, if on Windows operating systems.
#' @author Daniel Hoop
#' @param nCores The number of cores to register.
#' @param parallelCluster Optionally: A parallel cluster that was already registered.
#' @return A list with names \code{"clusterWasRegistered"} and \code{"parallelCluster"}.
#' @keywords internal
multiCorePrep <- function (nCores, parallelCluster) {
  nCores <- max(1, nCores)
  clusterWasRegistered <- FALSE # Flag is needed to close cluster after finishing the calculations.

  if (nCores > 1) {
    if (is.null(parallelCluster)) {
      installFromCRAN("doSNOW") # require("doSNOW")
      parallelCluster <- snow::makeCluster(nCores)
      doSNOW::registerDoSNOW(parallelCluster)
      clusterWasRegistered <- TRUE
    } else if (!"cluster"%in%class(parallelCluster)) {
      stop ("parallelCluster must be a cluster created by the function `snow::makeCluster` and registered with function `doSNOW::registerDoSNOW`.")
    }
  }
  return (list("clusterWasRegistered" = clusterWasRegistered,
               "parallelCluster" = parallelCluster))
}

#' @title Multicore post operations
#' @description Stops the parallel cluster if it was initialized.
#' @author Daniel Hoop
#' @param settings The return value of \code{\link{multiCorePrep}}.
#' @keywords internal
multiCorePost <- function (settings) {
  if (!all(c("clusterWasRegistered", "parallelCluster") %in% names(settings)))
    stop ("Internal error. Settings don't have required names.")
  if (settings[["clusterWasRegistered"]]) {
    snow::stopCluster(settings[["parallelCluster"]])
  }
}