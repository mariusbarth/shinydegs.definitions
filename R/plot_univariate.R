#' @import papaja
NULL

#' Plot univariate data
#'
#' Convenience function for plotting univariate data. Method dispatch ensures
#' correct plotting of factor, integer, numeric, and logical vectors.
#'
#' @examples
#' annotation <- new("vector_annotation", label = "Test Label", unit = "Category")
#'
#' int_ <- new("annotated_integer", .Data = (-5:4)[-3], annotation = annotation, label = "Test Label")
#' fac_ <- new("annotated_factor", .Data = 1:4, levels = letters[1:14], annotation = annotation, label = "Test Label")
#'
#' plot_univariate(int_)
#' plot_univariate(fac_)
#'
#' @rdname plot_univariate
#' @export

setGeneric(
  name = "plot_univariate"
  , def = function(x, ...){
    standardGeneric("plot_univariate")
  }
)


#' @rdname plot_univariate
#' @export

setMethod(
  f = "plot_univariate"
  , "annotated_numeric"
  , definition = function(x, ...){
    lab_ <- variable_label(x)
    hist(
      x
      , probability = FALSE
      , col = getOption("shinydegs.theme")$col
      , border = getOption("shinydegs.theme")$border
      , xlab = lab_
      , ylab = "Häufigkeit"
      , las = 1
      , main = character(0)
    )
    # lines(density(x), col = "indianred3", lwd = 2)
  }
)

#' @rdname plot_univariate
#' @export

setMethod(
  f = "plot_univariate"
  , "annotated_integer"
  , definition = function(x, main = "", ylab = "Häufigkeit", freq = TRUE, ...){

    if(freq==TRUE){
      frequencies <- table(x, useNA = "ifany")
    } else {
      tmp_ <- table(x, useNA = "ifany")
      frequencies <- tmp_/sum(tmp_)
    }
    x_values <- min(x):max(x)
    height <- rep(0, length(x_values))
    names(height) <- x_values
    height[names(frequencies)] <- frequencies

    xlim <- c(min(x)-.5, max(x) + .5)
    ylim <- c(0, max(frequencies)*1.05)

    plot.new()
    plot.window(xlim = xlim, ylim = ylim)
    title(xlab = variable_label(x), ylab = ylab, main = main)
    rect(xleft = x_values-.4, xright = x_values+.4, ybottom = rep(0, length(x_values)), ytop = height, col = getOption("shinydegs.theme")$col, border = getOption("shinydegs.theme")$border)
    axis(side = 1, at = c(min(x)-.5, x_values+.5), labels = rep("", length(x_values)+1), pos = 0)
    axis(side = 1, at = x_values, labels = x_values, pos = 0, tick = FALSE)
    axis(side = 2, las = 1)
  }
)

#' @rdname plot_univariate
#' @export

setMethod(
  f = "plot_univariate"
  , "annotated_factor"
  , definition = function(x, main = "", ylab = "Häufigkeit", freq = TRUE, ...){

    if(freq==TRUE){
      frequencies <- table(x, useNA = "ifany")
    } else {
      tmp_ <- table(x, useNA = "ifany")
      frequencies <- tmp_/sum(tmp_)
    }
    x_values <- 1:length(x@levels)
    names(x_values) <- x@levels

    height <- rep(0, length(x_values))
    names(height) <- names(x_values)
    height[names(frequencies)] <- frequencies

    xlim <- c(min(x_values)-.5, max(x_values) + .5)
    ylim <- c(0, max(frequencies)*1.05)

    plot.new()
    plot.window(xlim = xlim, ylim = ylim)
    title(xlab = variable_label(x), ylab = ylab, main = main)
    rect(xleft = x_values-.4, xright = x_values+.4, ybottom = rep(0, length(x_values)), ytop = height, col = getOption("shinydegs.theme")$col, border = getOption("shinydegs.theme")$border)
    axis(side = 1, at = c(-.5, x_values+.5), labels = rep("", length(x_values)+1), pos = 0)
    axis(side = 1, at = x_values, labels = names(x_values), pos = 0, tick = FALSE)
    axis(side = 2, las = 1)
  }
)

#' @rdname plot_univariate
#' @export

setMethod(
  f = "plot_univariate"
  , "annotated_logical"
  , definition = function(x, main = "", ylab = "Häufigkeit", freq = TRUE, ...){

    if(freq==TRUE){
      frequencies <- table(x, useNA = "ifany")
    } else {
      tmp_ <- table(x, useNA = "ifany")
      frequencies <- tmp_/sum(tmp_)
    }


    height <- rep(0, length(x_values))
    names(height) <- names(x_values)
    height[names(frequencies)] <- frequencies

    xlim <- c(min(x_values)-.5, max(x_values) + .5)
    ylim <- c(0, max(frequencies)*1.05)

    plot.new()
    plot.window(xlim = xlim, ylim = ylim)
    title(xlab = variable_label(x), ylab = ylab, main = main)
    rect(xleft = x_values-.4, xright = x_values+.4, ybottom = rep(0, length(x_values)), ytop = height, col = getOption("shinydegs.theme")$col, border = getOption("shinydegs.theme")$border)
    axis(side = 1, at = c(-.5, x_values+.5), labels = rep("", length(x_values)+1), pos = 0)
    axis(side = 1, at = x_values, labels = names(x_values), pos = 0, tick = FALSE)
    axis(side = 2, las = 1)
  }
)
