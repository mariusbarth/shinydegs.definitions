#' @export

setGeneric(
  name = "describe"
  , def = function(x, ...){
    standardGeneric("describe")
  }
)

#' @export

setMethod(
  f = "describe"
  , signature = "annotated_numeric"
  , definition = function(x, na.rm = TRUE, ...){
    list(
      "Mittelwert" = mean(x, na.rm = na.rm)
      , "Standardabweichung" = sd(x, na.rm = na.rm)
    )
  }
)

#' @export

setMethod(
  f = "describe"
  , signature = "annotated_integer"
  , definition = function(x, na.rm = TRUE, ...){
    list(
      "Mittelwert" = mean(x, na.rm = na.rm)
      , "Standardabweichung" = sd(x, na.rm = na.rm)
    )
  }
)

#' @export

setMethod(
  f = "describe"
  , signature = "annotated_factor"
  , definition = function(x, ...){
    
    x_tab <- table(x)
    x_prop <- x_tab/sum(x_tab)
    k <- as.integer(nlevels(x))
    
    list(
      "Modus" = names(x_tab[x_tab==max(x_tab)])
      , "Anzahl Kategorien" = k
      , "Relativer Informationsgehalt" = -1/log(k) * sum(x_prop * log(x_prop))
    )
  }
)

#' @export

setGeneric(
  name = "descriptives_table"
  , def = function(x, by = list(), ...){
    standardGeneric("descriptives_table")
  }
)


#' @export

setMethod(
  f = "descriptives_table"
  , signature = c("annotated_vector", "missing")
  , definition = function(x, by, ...){
    
    as.data.frame(lapply(X = describe(x), papaja::printnum))
    
  }
)

#' @export
 
setMethod(
  f = "descriptives_table"
  , signature = c("annotated_vector", "list")
  , definition = function(x, by, ...){
    
    # y <- tapply(X = x, INDEX = by, FUN = function(x){unlist(lapply(X = describe(x), papaja::printnum))}, simplify = FALSE)
    # z <- do.call("rbind", y)
    y <- aggregate(x = x, by = by, FUN = describe)
    # tmp <- as.data.frame(y$x)
    # tmp <- lapply(tmp, unlist)
    # # tmp <- lapply(tmp, papaja::printnum)
    # # tmp <- lapply(X = tmp, FUN = function(x){lapply(X = x, papaja::printnum)})
    # y$x <- NULL
    # colnames(y) <- names(by)
    # cbind(y, tmp)
    y
  }
)



