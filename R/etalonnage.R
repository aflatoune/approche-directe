#' etalonnage
#'
#' @param name A character indicating a name for the analysis.
#' @param X
#' @param y
#' @param model
#' @param train_index
#' @param test_index
#' @param seed
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
etalonnage <- function(name, X, y, model, train_index, test_index, seed = 313, ...) {
    fitted_values <- c()
    predicted_values <- c()
    call <- as.list(match.call()[-1])
    regressor <- switch(model,
                        "randomForest" = randomForest::randomForest,
                        "glmnet" = glmnet::glmnet,
                        "xgboost" = xgboost::xgboost)

    for (i in seq_along(train_index)) {
        #do.call(regressor, call)
        fit <- regressor(X[train_index[[i]],], y[train_index[[i]]], ...)
        if (identical(i, 1)) {
            fitted_values <- c(fitted_values, pred(fit, X[train_index[[i]],]))
        }
        predicted_values <- c(predicted_values, pred(fit, X[test_index[[i]],]))
    }

    test_rmse <- sqrt(mean((predicted_values - y[test_index[[1]]])^2))
    test_mae <- mean(abs(predicted_values - y[test_index[[1]]]))
    test_mda <- mda(predicted_values - y[test_index[[1]]])
    out <- list(
        name = name,
        model = model,
        fitted_values = fitted_values,
        predicted_values = predicted_values,
        test_rmse = test_rmse,
        test_mae = test_mae,
        test_mda = test_mda,
        call = call
    )
    structure(out, class = "etalonnage")
}
