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
    x <- droplevels(x)
    x_tab <- table(x)
    x_prop <- x_tab/sum(x_tab)
    k <- as.integer(nlevels(x))
    
    list(
      "Modus" = paste(names(x_tab[x_tab==max(x_tab)]), collapse = ", ")
      , "Anzahl Kategorien" = k
      , "Relativer Informationsgehalt" = -1/log(k) * sum(x_prop * log(x_prop))
    )
  }
)

#' @export

setGeneric(
  name = "descriptives_table"
  , def = function(x, by, ...){
    standardGeneric("descriptives_table")
  }
)


#' @export

setMethod(
  f = "descriptives_table"
  , signature = c(x = "annotated_vector", by = "missing")
  , definition = function(x, by, ...) {
    
    as.data.frame(
      lapply(
        X = describe(x)
        , FUN = function(x) {
          if(is.numeric(x)){
            papaja::printnum(x)
          } else{
        x
      }
    }
  )
  , stringsAsFactors = FALSE
  , check.names = FALSE)
  }
)

#' @export
 
setMethod(
  f = "descriptives_table"
  , signature = c(x = "annotated_vector", by = "list")
  , definition = function(x, by, ...){
    
    y <- aggregate(x = x, by = by, FUN = descriptives_table)
    tmp <- as.data.frame(y$x, check.names = FALSE)

    y$x <- NULL
    colnames(y) <- unlist(lapply(by, variable_label))
    cbind(y, tmp)
  }
)



