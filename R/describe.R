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
  
    y <- list(
      "Anzahl Beobachtungen" = sum(!is.na(x))
      , "Mittelwert" = mean(x, na.rm = na.rm)
      , "Standardfehler" = sd(x, na.rm = na.rm) / sum(!is.na(x))
      , "Standardabweichung" = sd(x, na.rm = na.rm)
    )
    attr(y$`Anzahl Beobachtungen`, "digits") <- 0
    attr(y$`Mittelwert`, "digits") <- 2
    attr(y$`Standardfehler`, "digits") <- 2
    attr(y$`Standardabweichung`, "digits") <- 2
    y
  }
)

#' @export

setMethod(
  f = "describe"
  , signature = "annotated_integer"
  , definition = function(x, na.rm = TRUE, ...){
    y <- as(x, "annotated_numeric")
    describe(y)
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
    
    y <- list(
      "Anzahl Beobachtungen" = sum(!is.na(x))
      , "Modus" = paste(names(x_tab[x_tab==max(x_tab)]), collapse = ", ")
      , "Anzahl Kategorien" = k
      , "Relativer Informationsgehalt" = -1/log(k) * sum(x_prop * log(x_prop))
    )
    
    attr(y$`Anzahl Beobachtungen`, "digits") <- 0
    attr(y$`Modus`, "digits") <- 0
    attr(y$`Anzahl Kategorien`, "digits") <- 0
    attr(y$`Relativer Informationsgehalt`, "digits") <- 2
    
    y
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
  , definition = function(x, by, digits = NULL, ...) {
    
    as.data.frame(
      lapply(
        X = describe(x)
        , FUN = function(x) {
          if(inherits(x, c("numeric", "integer"))){
            papaja::printnum(x, digits = attr(x, "digits"))
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
    
    
    y <- aggregate(x = x, by = by, FUN = describe)
    tmp <- as.data.frame(y$x, check.names = FALSE, stringsAsFactors = FALSE)
    tmp <- as.data.frame(
      lapply(
        tmp
        , FUN = function(x) {
          if(inherits(x[[1]], what = c("numeric", "integer"))){
            papaja::printnum(
              unlist(x)
              , digits = attr(x[[1]], "digits")
            )
          } else {
            unlist(x)
          }
        }
      )
      , check.names = FALSE
      , stringsAsFactors = FALSE
    )
    y$x <- NULL
    colnames(y) <- unlist(lapply(by, variable_label))
    cbind(y, tmp)
  }
)



