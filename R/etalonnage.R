#' etalonnage
#'
#' @param name A character indicating a name for the analysis.
#' @param X
#' @param y
#' @param model_type
#' @param train_index
#' @param test_index
#' @param scale
#' @param seed
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
etalonnage <-
    function(name,
             X,
             y,
             model_type,
             train_index,
             test_index,
             scale = c(NULL, "center", "scale"),
             seed = 313,
             ...) {
        scale <- match.arg(scale)
        call <- as.list(match.call()[-1])
        regressor <- switch(
            model_type,
            "randomForest" = randomForest::randomForest,
            "glmnet" = glmnet::glmnet,
            "xgboost" = xgboost::xgboost
        )
        fitted_values <- c()
        predicted_values <- c()

        set.seed(seed)
        for (i in seq_along(train_index)) {
            y_ <- y[train_index[[i]]]

            if (identical(scale, "center")) {
                X_ <- X[train_index[[i]], ]
                X_ <- X_ %>%
                    standardize_data(scale = FALSE)
            } else if (identical(scale, "scale")) {
                X_ <- X[train_index[[i]], ]
                X_ <- X_ %>%
                    standardize_data(scale = TRUE)
            } else {
                X_ <- X[train_index[[i]], ]
            }

            fit <-
                regressor(X_, y_, ...)
            if (identical(i, 1)) {
                fitted_values <- c(fitted_values, predict(fit, X_, ))
            }
            predicted_values <-
                c(predicted_values, predict(fit, X[test_index[[i]], ]))
        }

        test_rmse <-
            sqrt(mean((predicted_values - y[-train_index[[1]]]) ^ 2))
        test_mae <- mean(abs(predicted_values - y[-train_index[[1]]]))
        test_mda <- mda(y[-train_index[[1]]], predicted_values)
        out <- list(
            name = name,
            model_type = model_type,
            fit = fit,
            fitted_values = fitted_values,
            predicted_values = predicted_values,
            test_rmse = test_rmse,
            test_mae = test_mae,
            test_mda = test_mda,
            call = call
        )
        structure(out, class = "etalonnage")
    }
