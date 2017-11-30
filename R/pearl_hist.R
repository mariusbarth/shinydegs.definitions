#' @import papaja broom afex
NULL

#' @export

setGeneric(
  name = "pearl_hist"
  , def = function(x, y, ...){
    standardGeneric("pearl_hist")
  }
)

#' @export

setMethod(
  f = "pearl_hist"
  , signature = c(x = "numeric")
  , definition = function(x, breaks, highlight = FALSE, pch = 21, ci = NULL, true_x = FALSE, ...){
    # clean `x` for some robustness:
    x <- x[!is.na(x)]

    # Most probably, `hist` implements an efficient way of calculating counts
    test <- hist(x = x, plot = FALSE, breaks = breaks)

    last_x <- x[length(x)]

    plot.new()
    plot.window(xlim = test$breaks[c(1, length(test$breaks))], ylim = c(0, max(c(test$counts, 8))))
    col <- getOption("shinydegs.theme")$col
    border <- getOption("shinydegs.theme")$border

    if(!true_x){
      x <- test$mids
    }
    y <- test$counts

    if(length(x)>0){
      for (i in 1:length(x)){
        if(y[i]!=0){
          points(x = rep(x[i], length.out = y[i]), y = 1:y[i]-.5, pch = pch, bg = col, col = border, cex = 2)
        }
      }

      if(highlight){
        bigger <- last_x > test$breaks
        
        if(!is.null(ci)){
          arrows(
            x0 = x[sum(bigger)]
            , x1 = x[sum(bigger)] + c(ci, -ci)
            , y0 = y[sum(bigger)]-.5
            , y1 = y[sum(bigger)]-.5
            , length = .05
            , angle = 90
            , lwd = 2
            , col = "indianred3"
          )
        }
        points(x = x[sum(bigger)], y = y[sum(bigger)]-.5, pch = pch, bg = "indianred3", col = border, cex = 3)
      }
    }

    axis(side = 1)
    axis(side = 2, las = 1)
  }
)

#' @export

setMethod(
  f = "pearl_hist"
  , signature = c(x = "logical")
  , definition = function(x, breaks, highlight = FALSE, ...){
    plot.new()
    plot.window(xlim = c(min(breaks), max(breaks)), ylim = c(0, 8))

    axis(side = 1)
    axis(side = 2, las = 1)
  }
)

