#' @import papaja broom afex
NULL

#' Plot bivariate data
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
#'
#' @rdname plot_bivariate
#' @export

setGeneric(
  name = "plot_bivariate"
  , def = function(x, y, ...){
    standardGeneric("plot_bivariate")
  }
)

# ------------------------------------------------------------------------------
# numeric -- numeric -- OK
# numeric -- integer
# numeric -- factor
#
# integer -- numeric
# integer -- integer
# integer -- factor

# factor -- numeric -- OK
# factor -- integer -- OK
# factor -- factor  -- OK




# ----
# x = "annotated_numeric"

#' @rdname plot_bivariate
#' @export

setMethod(
  f = "plot_bivariate"
  , signature = c(x = "annotated_numeric", y = "annotated_numeric")
  , definition = function(x, y, xlim = NULL, ylim = NULL, model = TRUE, ...){
    data <- data.frame(as.numeric(x), as.numeric(y))
    data <- data[complete.cases(data), ]
    N <- nrow(data)

    if(is.null(xlim)) xlim <- range(x, na.rm = TRUE)
    if(is.null(ylim)) ylim <- range(y, na.rm = TRUE)

    plot(
      x = x
      , y = y
      , xlab = annotate4::variable_label(x)
      , ylab = annotate4::variable_label(y)
      , pch = 21
      , col = getOption("shinydegs.theme")$border
      , cex = (1/(1+exp(1/200*(N-500))))+.5
      , bg = getOption("shinydegs.theme")$col
      , frame.plot = FALSE
      , las = 1
      , xlim = xlim
      , ylim = ylim
    )
    if(model){
      model_object <- lm(formula = y ~ x, data = data)
      abline(model_object, lwd = 2, col = "skyblue4")
      invisible(model_object)
    }
  }
)

#' @rdname plot_bivariate
#' @export

setMethod(
  f = "plot_bivariate"
  , signature = c(x = "annotated_numeric", y = "annotated_factor")
  , definition = function(x, y, model = TRUE, ...){

    response <- rep(0, length(y))
    response[y!=levels(y)[1]] <- 1

    data <- data.frame(as.numeric(x), y = response)
    data <- data[complete.cases(data), ]

    plot(
      x = data$x
      , y = data$y
      , xlab = annotate4::variable_label(x)
      , ylab = latex2exp::TeX(paste0("$\\mathit{P}(", annotate4::variable_label(y), "=", levels(y)[2], ")$"))
      , frame.plot = FALSE
      , las = 1
      , pch = 21
      , bg = getOption("shinydegs.theme")$col
      , col = getOption("shinydegs.theme")$border
    )

    # run a logistic regression model
    if(model){
      model_object <- glm(formula = y ~ x, data = data, family = binomial(link = "probit"))

      curve(expr = predict.glm(
        object = model_object
        , data.frame(x = x)
        , type = "response"
      )
      , add = TRUE
      , lwd = 2
      , col = "skyblue4"
      )
      invisible(model_object)
    }
  }
)

#' @rdname plot_bivariate
#' @export

setMethod(
  f = "plot_bivariate"
  , signature = c(x = "annotated_numeric", y = "annotated_integer")
  , definition = function(x, y, model = TRUE, ...){
    # just return the opposite arrangement, this is too stupid
    plot_bivariate(x = y, y = x, model = model, ...)
  }
)


# ----
# x = "annotated_integer"

#' @rdname plot_bivariate
#' @export

setMethod(
  f = "plot_bivariate"
  , signature = c(x = "annotated_integer", y = "annotated_numeric")
  , definition = function(x, y, model = TRUE, ...){
    data <- data.frame(pred = annotate4::as.factor(x), dv = as.numeric(y))
    data <- data[complete.cases(data), ]

    data$id <- 1:nrow(data)
    papaja::apa_barplot(
      data = data
      , id = "id"
      , dv = "dv"
      , factors = "pred"
      , args_rect = list(col = "#75AADB", border = "white")
      , las = 1
    )
    if(model){
      model_object <- aov_ez(data = data, id = "id", dv = "dv", between = "pred", return = "Anova")
      invisible(model_object)
    }
  }
)

#' @rdname plot_bivariate
#' @export

setMethod(
  f = "plot_bivariate"
  , signature = c(x = "annotated_integer", y = "annotated_integer")
  , definition = function(x, y, model = TRUE, ...){
    x <- droplevels(as(x, "annotated_factor"))
    par(mfrow = c(nlevels(x), 1))
    for (i in levels(x)){
      plot_univariate(x = y[x==i], main = i)
    }

    if(model){
      model_object <- glm.fit(x = x, y = y, family = binomial(link = "logit"))
      invisible(model_object)
    }
  }
)

#' @rdname plot_bivariate
#' @export

setMethod(
  f = "plot_bivariate"
  , signature = c(x = "annotated_integer", y = "annotated_factor")
  , definition = function(x, y, ...){
    plot_bivariate(x = y, y = x, ...)
  }
)

# ----
# x = "annotated_factor"

#' @rdname plot_bivariate
#' @export

setMethod(
  f = "plot_bivariate"
  , signature = c(x = "annotated_factor", y = "annotated_factor")
  , definition = function(x, y, model = TRUE, ...){
    x <- droplevels(as(x, "annotated_factor"))
    par(mfrow = c(nlevels(x), 1))
    for (i in levels(x)){
      plot_univariate(x = y[x==i], main = i)
    }
    if(model){
      model_object <- chisq.test(x = table(x, y))
      invisible(model_object)
    }
  }
)

#' @rdname plot_bivariate
#' @export

setMethod(
  f = "plot_bivariate"
  , signature = c(x = "annotated_factor", y = "annotated_numeric")
  , definition = function(x, y, xlim = NULL, ylim = NULL, model = TRUE, ...){
    data <- data.frame(pred = annotate4::as.factor(x), dv = as.numeric(y))
    data <- data[complete.cases(data), ]
    data$id <- 1:nrow(data)
    papaja::apa_barplot(
      data = data
      , id = "id"
      , dv = "dv"
      , factors = "pred"
      , args_rect = list(col = "#75AADB", border = "white")
      , las = 1
      , xlab = annotate4::variable_label(x)
      , ylab = annotate4::variable_label(y)
      # , xlim = xlim # don't let user  specify
      , ylim = ylim
    )

    if(model){
      model_object <- afex::aov_ez(data = data, id = "id", dv = "dv", between = "pred", na.rm = TRUE, return = "Anova")
      invisible(model_object)
    }
  }
)

#' @rdname plot_bivariate
#' @export

setMethod(
  f = "plot_bivariate"
  , signature = c(x = "annotated_factor", y = "annotated_integer")
  , definition = function(x, y, model = TRUE, ...){
    x <- droplevels(x)
    par(mfrow = c(nlevels(x), 1))
    for (i in levels(x)){
      plot_univariate(x = y[x==i], main = i)
    }

    if(model) {
      model_object <- chisq.test(x = table(x, y))
      invisible(model_object)
    }
  }
)
