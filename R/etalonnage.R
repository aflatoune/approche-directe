
#' etalonnage
#'
#' @param name A character indicating a name for the analysis.
#' @param X A tibble/df containing the regressors. Must contain a date column.
#' @param y A vector containing the target variable.
#' @param first_date A character indicating the first date to keep, must be of
#' the form `"YYYY-MM-01"`.
#' @param forecast_origin A character indicating the first forecast origin, it
#' must be of the form `"YYYY-MM-01"`.
#' @param model_type A character. For now, only `"randomForest"`, `"xgboost"`
#' `"glmnet"` are accepted.
#' @param scale Indicates  whether to leave unchanged, center or scale `X`.
#' Must be one of `NULL`, `"center"` or `"scale"`.
#' @param seed A numeric value interpreted as an integer.
#' @param ...
#'
#' @return An object of the S3 class `etalonnage`.
#' @export
#'
#' @examples
#' To train a Random Forest from 1990Q2 to 2019Q4 and predict all samples
#' after 2018Q4 using a rolling-origin-update evaluation :
#' model_1 <- etalonnage(name = "Random Forest en M+1",
#' X = X,
#' y = y$target,
#' model_type = "randomForest",
#' first_date = "1990-04-01",
#' ast_date = "2018-10-01",
#' scale = "center")
#'
etalonnage <-
    function(name,
             X,
             y,
             first_date,
             forecast_origin,
             model_type = c("randomForest", "xgboost", "glmnet"),
             scale = c(NULL, "center", "scale"),
             seed = 313,
             ...) {
        if (!("date" %in% names(X)) | !("Date" %in% sapply(X, class))) {
            stop("X must contain a column \"date\" containing Date objects.")
        }
        model_type <- match.arg(model_type)
        message("Note :The date column is used to build the train/test scheme.",
                " It is not used when fitting the model.")

        scale <- match.arg(scale)
        call <- as.list(match.call()[-1])
        fitted_values <- c()
        predicted_values <- c()
        regressor <- switch(
            model_type,
            "randomForest" = randomForest::randomForest,
            "glmnet" = glmnet::glmnet,
            "xgboost" = xgboost::xgboost
        )
        indexes <- train_test_index(
            X, date1 = first_date, date2 = forecast_origin
            )
        train_index <- indexes$train
        test_index <- indexes$test

        X <- X %>%
            dplyr::filter(date >= first_date)
        firstdate <- min(X$date)
        X <- X %>%
            dplyr::select(-date)

        if (identical(regressor, "xgboost")) {
            X <- as.matrix(X)
        }

        set.seed(seed)
        for (i in seq_along(train_index)) {
            print(i)
            y_ <- y[train_index[[i]]]

            if (identical(scale, "center")) {
                X_ <- X[train_index[[i]],]
                X_ <- X_ %>%
                    standardize_data(scale = FALSE)
            } else if (identical(scale, "scale")) {
                X_ <- X[train_index[[i]],]
                X_ <- X_ %>%
                    standardize_data(scale = TRUE)
            } else {
                X_ <- X[train_index[[i]],]
            }

            fit <- regressor(X_, y_, ...)
            if (identical(i, 1L)) {
                fitted_values <- c(fitted_values, predict(fit, X_,))
            }
            predicted_values <-
                c(predicted_values, predict(fit, X[test_index[[i]],]))
        }

        n_test <- length(predicted_values)
        test_rmse <-
            sqrt(mean((predicted_values[-n_test] - y[-train_index[[1]]])^2))
        test_mae <-
            mean(abs(predicted_values[-n_test] - y[-train_index[[1]]]))
        test_mda <- mda(y[-train_index[[1]]], predicted_values[-n_test])

        out <- list(
            name = name,
            first_date = firstdate,
            target = y,
            model_type = model_type,
            last_fit = fit,
            fitted_values = fitted_values,
            predicted_values = predicted_values,
            test_rmse = test_rmse,
            test_mae = test_mae,
            test_mda = test_mda,
            call = call
        )
        structure(out, class = "etalonnage")
    }

