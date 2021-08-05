#' etalonnage
#'
#' To train and evaluate the models, a rolling-origin-update evaluation (ROUE)
#' is implemented, meaning that the forecast origin rolls ahead in time.
#' At each step, ROUE increments the traning set by one observation of the test
#' set. The date of the first sample to predict is given by `forecast_origin`
#' + 1 quarter. Note that the training set always starts at the first sample
#' of `X`.
#' To take into account the non-synchronicity of data publications, use the
#' `cols` argument to indicate which series need to be extended. In this way
#' the forecast accuracy can be assessed on the basis of a pseudo real-time
#' experiment i.e. replicating the timeliness of the releases of the series
#' by taking into account their publications lags. This ensures to consider
#' only those values of the series that would have been available on the date
#' on which the forecasts were calculated.
#'
#' @param name A character indicating a name for the analysis.
#' @param X A tibble/df containing the regressors. Must contain a date column.
#' @param y A vector containing the target variable.
#' @param forecast_origin A character indicating the first forecast origin, it
#' must be of the form `"YYYY-MM-01"`.
#' @param regressor A character. For now, only `"randomForest"`, `"xgboost"`
#' `"glmnet"` and `"lm"` are accepted.
#' @param cols A vector of characters indicating columns to extend. Series
#' are extended with an ARIMA(p,d,q) model - if missing defaults to `NULL`.
#' @param scale Indicates  whether to leave unchanged, center or scale `X`.
#' Must be one of `"none`, `"center"` or `"scale"`.
#' @param frequency A character indicating the date frequency - if missing
#' defaults to `"quarter"`.
#' @param seed A numeric value interpreted as an integer.
#' @param ... Aditionnal arguments to pass to the regressor.
#'
#' @return An object from S3 class `etalonnage`.
#' @export
etalonnage <-
    function(name,
             X,
             y,
             forecast_origin,
             regressor = c("randomForest", "xgboost", "glmnet", "lm"),
             cols = NULL,
             scale = c("none", "center", "scale"),
             frequency = "quarter",
             seed = 313,
             ...) {
        if (!("date" %in% names(X)) | !("Date" %in% sapply(X, class))) {
            stop("X must contain a column \"date\" containing Date objects.")
        }
        regressor <- match.arg(regressor)
        scale <- match.arg(scale)
        message(
            "Note: The date column is used to build the train/test scheme.",
            " It is not used when fitting the model."
        )

        forecast_origin <- as.Date(forecast_origin)
        first_date <- min(X$date)
        call <- match.call()
        fitted_values <- c()
        predicted_values <- c()
        regressor <- switch(
            regressor,
            "randomForest" = randomForest::randomForest,
            "glmnet" = glmnet::glmnet,
            "xgboost" = xgboost::xgboost,
            "lm" = "lm"
        )
        indexes <- train_test_index(X,
                                    date_start = first_date,
                                    date_end = forecast_origin,
                                    frequency = frequency)
        train_index <- indexes$train
        test_index <- indexes$test
        X <- X %>%
            dplyr::select(-date)

        pb <- try(utils::txtProgressBar(min = 1,
                                    max = max(seq_along(train_index)),
                                    style = 3),
                  silent = TRUE)
        set.seed(seed)
        for (i in seq_along(train_index)) {
            y_ <- y[train_index[[i]]]
            X_ <- X[train_index[[i]],]

            if (identical(scale, "center")) {
                X_ <- X_ %>%
                    standardize_data(scale = FALSE) %>%
                    extend_series(cols = cols) %>%
                    as.matrix()
            } else if (identical(scale, "scale")) {
                X_ <- X_ %>%
                    standardize_data(scale = TRUE) %>%
                    extend_series(cols = cols) %>%
                    as.matrix()
            } else if (identical(scale, "none")) {
                X_ <- X_ %>%
                    extend_series(cols = cols) %>%
                    as.matrix()
            }

            if (identical(regressor, "lm")) {
                fit <- lm(y_ ~ ., data = as.data.frame(X_))
            }
            else {
                fit <- regressor(X_, y_, ...)
            }

            if (identical(i, 1L) & !identical(regressor, "lm")) {
                fitted_values <- c(fitted_values, predict(fit, X_,))
            } else if (identical(i, 1L) & identical(regressor, "lm")) {
                fitted_values <- c(fitted_values, predict(fit, as.data.frame(X_),))
            }

            if (identical(regressor, "lm")) {
                predicted_values <-
                    c(predicted_values,
                      predict(fit, X[test_index[[i]],]))
            } else {
                predicted_values <-
                    c(predicted_values,
                      predict(fit, as.matrix(X[test_index[[i]],])))
            }

            try(utils::setTxtProgressBar(pb, i), silent = TRUE)
        }

        n_test <- length(predicted_values)
        test_rmse <- sqrt(mean((predicted_values[-n_test] - y[-train_index[[1]]]) ^ 2))
        test_mae <-
            mean(abs(predicted_values[-n_test] - y[-train_index[[1]]]))
        test_mda <-
            mda(y[-train_index[[1]]], predicted_values[-n_test])
        out <- list(
            name = name,
            first_date = first_date,
            forecast_origin = forecast_origin,
            target = y,
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
