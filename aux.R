

load.dependencies <- function() {
  library(data.table)
  library(ggplot2)
  library(plotly)
  library(flexdashboard)
  library(htmltools)
  library(psych)
  library(Hmisc)
}


load.data <- function() {

}


get.descriptive.table <- function(dt = NULL) {
  results <- data.table(
    "i" = integer(),
    "j" = integer(),
    "variable" = character(),
    "mean" = numeric(),
    "sd" = numeric(),
    "level" = character(),
    "n" = integer(),
    "%" = numeric()
  )

  i <- 0
  for (var in colnames(dt)) {
    i <- i + 1
    if (class(dt[, get(var)]) %in% c("integer", "numeric")) {
      tmp <- dt[, .(
        "i" = i,
        "j" = NA_integer_,
        "variable" = var,
        "mean" = round(mean(get(var)), 3),
        "sd" = round(sd(get(var)), 3),
        "level" = "",
        "n" = NA_integer_,
        "%" = NA_real_
      )]
    } else
    if (class(dt[, get(var)]) == "factor") {
      tmp <- dt[, .(
        "i" = i,
        "variable" = var,
        "mean" = NA_real_,
        "sd" = NA_real_,
        "n" = .N
      ), by = var][order(get(var))]
      colnames(tmp)[[1]] <- "level"
      tmp[, "%" := 100 * round(n / sum(n), 3)]
      tmp2 <- data.table(
        "i" = i,
        "j" = 1:length(levels(dt[, get(var)])),
        "variable" = var,
        "mean" = NA_real_,
        "sd" = NA_real_,
        "level" = levels(dt[, get(var)])
      )

      tmp <- merge(tmp, tmp2, by = intersect(colnames(tmp), colnames(tmp2)), all = T)
      tmp[is.na(n), n := 0]
    }

    results <- merge(results, tmp, by = colnames(results), all = T)
  }

  results[order(i, j), 3:8]
}


as.date <- function(input = NULL, tryFormat = NULL) {
  if (class(input) %in% c("numeric", "integer")) {
    input <- as.character(input)
    if (is.null(tryFormat)) {
      tryFormat <- c("%Y%m%d")
    }
  } else {
    if (class(input) %in% c("factor", "character")) {
      input <- as.character(input)
      if (is.null(tryFormat)) {
        tryFormat <- c("%Y-%m-%d", "%Y%m%d")
      }
    }
  }
  return(as.Date(input, tryFormats = tryFormat))
}

date.to.char <- function(date = NULL, format = NULL) {
  if (is.null(format)) {
    format <- "%Y-%m-%d"
  }
  return(as.character(date, format = format))
}

sanitize <- function(input = NULL) {
  input <- gsub("\\.(xlsx|XLSX)", "", input)
  input <- gsub("[\\?\\!]+", "", input)
  input <- gsub("ü", "ue", input)
  input <- gsub("ä", "ae", input)
  input <- gsub("ö", "oe", input)
  input <- gsub("ß", "ss", input)
  input <- gsub("[\r\n]+", ".", input)
  input <- gsub("[[:space:][:punct:]]+", ".", input)
  input <- gsub("\\.$", "", input)
  return(input)
}

repair.duplicates <- function(input = NULL) {
  for (i in 1:length(input))
  {
    if (input[i] %in% input[!((1:length(input)) %in% i)]) {
      J <- which(input == input[i])
      for (j in J) {
        input[j] <- paste0(input[j], ".", j)
      }
    }
  }
  return(input)
}


assign.col.classes <- function(dataset = NULL, fac.cols = NULL, num.cols = NULL, int.cols = NULL, dat.cols = NULL,
                               chr.cols = NULL, log.cols = NULL, cust.fac.cols = NULL) {
  if (is.null(dataset)) {
    return(dataset)
  }

  if (!is.null(fac.cols)) {
    dataset[, fac.cols] <- dataset[, lapply(.SD, factor), .SDcols = fac.cols]
  }

  if (!is.null(cust.fac.cols)) {
    for (i in 1:length(cust.fac.cols)) {
      var <- names(cust.fac.cols[i])
      dataset[, var] <- factor(dataset[, get(var)], levels = cust.fac.cols[[i]])
    }
  }
  if (!is.null(dat.cols)) {
    dataset[, dat.cols] <- dataset[, lapply(.SD, as.POSIXct), .SDcols = dat.cols]
  }

  if (!is.null(int.cols)) {
    dataset[, int.cols] <- dataset[, lapply(.SD, as.integer), .SDcols = int.cols]
  }

  if (!is.null(num.cols)) {
    dataset[, num.cols] <- dataset[, lapply(.SD, as.numeric), .SDcols = num.cols]
  }

  if (!is.null(chr.cols)) {
    dataset[, chr.cols] <- dataset[, lapply(.SD, as.character), .SDcols = chr.cols]
  }

  if (!is.null(log.cols)) {
    for (i in 1:length(log.cols)) {
      var <- names(log.cols[i])

      dataset[get(var) == log.cols[[i]], var] <- "TRUE"
      dataset[is.na(get(var)) | get(var) != "TRUE", var] <- "FALSE"

      dataset[, var] <- as.logical(dataset[, get(var)])
    }
  }

  return(dataset)
}

read.csv <- function(filename = NULL, var.name = NULL, first.row.col.names = FALSE, col.names = NULL, 
                     fac.cols = NULL, num.cols = NULL, int.cols = NULL, dat.cols = NULL, chr.cols = NULL, 
                     log.cols = NULL, cust.fac.cols = NULL, 
                     sep.between.cols = "auto", sep.within.cols = "auto", ...) {
  
  args <- list(...)
  
  if (is.null(var.name)) {
    var.name <- sanitize(filename)
  }

  assign("dt.tmp", fread(filename, header = first.row.col.names, 
                         sep = sep.between.cols, sep2 = sep.within.cols, ...))

  if (!is.null(col.names) && length(col.names) == dim(dt.tmp)[2]) {
    colnames(dt.tmp) <- col.names
  } else {
    sat.cln <- repair.duplicates(sanitize(colnames(dt.tmp)))
    setnames(dt.tmp, colnames(dt.tmp), sat.cln)
  }

  # delete rows that contain only NAs
  dt.tmp <- dt.tmp[!Reduce(`&`, lapply(dt.tmp, is.na))]

  # assign specified column classes
  dt.tmp <- assign.col.classes(
    dataset = dt.tmp, fac.cols = fac.cols, num.cols = num.cols, int.cols = int.cols,
    dat.cols = dat.cols, chr.cols = chr.cols, log.cols = log.cols, cust.fac.cols = cust.fac.cols
  )

  assign(var.name, dt.tmp, envir = .GlobalEnv)
  return(list(
    "fac.cols" = fac.cols, "num.cols" = num.cols, "int.cols" = int.cols, "dat.cols" = dat.cols,
    "chr.cols" = chr.cols, "log.cols" = log.cols, "cust.fac.cols" = cust.fac.cols
  ))
}
