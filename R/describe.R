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