#
# load("~/git/papaja_man/data/cosmetic_surgery.rdata")
# data <- cosmetic_surgery
# iv <- c("BDI", "Reason", "Surgery", "Gender", "Clinic", "Age")
# iv <- c("BDI", "Base_QoL")
# dv <- "Post_QoL"
# dv <- "Reason"


#' @export

generalized_linear_model <- function(data, dv, iv, standardize = FALSE){
  
  # cleanup and NA handling
  data <- data[, c(iv, dv)]
  data <- data[complete.cases(data), ]
  

  formula <- formula(paste0(dv, " ~ ", paste(iv, collapse = "+ ")))

  if(is.null(standardize)) standardize <- FALSE

  if(is(data[[dv]], "annotated_integer")){
    data[[dv]] <- as(data[[dv]], "annotated_numeric")
  }
  
  for(i in iv){
    if(is(data[[i]], "annotated_factor")){
      data[[i]] <- as(data[[i]], "factor")
    }
  }

  if(is(data[[dv]], "annotated_numeric")){
    if(standardize){
      numeric_variables <- unlist(lapply(data[, c(dv, iv)], is, "annotated_numeric"))
      
      data[, c(dv, iv)][, numeric_variables] <- scale(data[, c(dv, iv)][, numeric_variables])
    }
    lm(formula = formula, data = data, model = TRUE, x = TRUE, y = TRUE, qr = TRUE)
  } else {
    if(is(data[[dv]], "annotated_factor")){
      
      response <- rep(0, length(data[[dv]]))
      response[data[[dv]]!=levels(data[[dv]])[1]] <- 1
      
      data[[dv]] <- as.factor(response)
      
      glm(formula = formula, data = data, family = binomial(link = "logit"))
    }
  }
}

# par(mfrow = c(1, 3))
# iv <- c("BDI")
# test <- generalized_linear_model(data = data, iv = iv, dv = dv)
# apa_print(test)$table

#' @export

setGeneric(
  name = "plot_marginal"
  , def = function(object, x1, ...){
    standardGeneric("plot_marginal")
  }
)

#' @export

setMethod(
  f = "plot_marginal"
  , signature = "lm"
  , function(object, x1, xlab = "X", ylab = "Y (corrected for all but X)", ...){
    plot(
      x = object$x[, x1]
      , y = residuals(object) + coefficients(object)[x1] * object$x[, x1]
      , pch = 21
      , col = "white"
      , bg = "skyblue4"
      , xlab = xlab
      , ylab = ylab
      , las = 1
      , frame.plot = FALSE
    )
    lines(x = object$x[, x1], y = coefficients(object)[x1] * object$x[, x1], lwd = "2", col = "indianred3")
  }
)

# plot_marginal(test, x1 = iv[1])


# iv <- c("BDI", "Base_QoL")
# test <- generalized_linear_model(data = data, iv = iv, dv = dv)
# apa_print(test)$table
# plot(x = test$x[, iv[[1]]], y = residuals(test) + coefficients(test)[iv[[1]]] * test$x[, iv[[1]]], pch = 21, col = "white", bg = "skyblue4")
# lines(x = test$x[, iv[[1]]], y = coefficients(test)[iv[[1]]] * test$x[, iv[[1]]], lwd = "2", col = "indianred3")
#
# iv <- c("BDI", "Base_QoL", "Surgery", "Age", "Reason")
# test <- generalized_linear_model(data = data, iv = iv, dv = dv)
# apa_print(test)$table
# plot(x = test$x[, iv[[1]]], y = residuals(test) + coefficients(test)[iv[[1]]] * test$x[, iv[[1]]], pch = 21, col = "white", bg = "skyblue4")
# lines(x = test$x[, iv[[1]]], y = coefficients(test)[iv[[1]]] * test$x[, iv[[1]]], lwd = "2", col = "indianred3")

# library(rockchalk)
#
# plotCurves(test, plotx = "BDI", modx = "Surgery")

# plot(x = test$x[, iv[[1]]], y = test$y, pch = 21, col = "white", bg = "skyblue4")
# points(x = test$x[, iv[[1]]], y = test$fitted.values, col = "white", pch = 22, bg = "indianred3")


