#' etalonnage
#'
#' To train and evaluate the models, a rolling-origin-update evaluation (ROUE)
#' is implemented, meaning that the forecast origin rolls ahead in time.
#' At each step, ROUE increments the traning set by one observation of the test
#' set. The date of the first sample to predict is given by `forecast_origin`
#' plus 1 quarter. Note that the training set always starts at the first sample
#' of `X`.
#' To take into account the non-synchronicity of data publications, use the
#' `extend` argument to indicate which series need to
#' be extended. This way the forecast accuracy can be assessed on the basis of
#' a pseudo real-time experiment i.e. replicating the timeliness of the
#' releases of the series by taking into account their publication lags.
#' This ensures to consider only those values of the series that would have
#' been available on the date on which the forecasts were calculated.
#'
#' @param name A character indicating a name for the analysis.
#' @param X A tibble/df containing the regressors at a quarterly frequency.
#' Must contain a date column.
#' @param y A vector containing the target variable.
#' @param forecast_origin A character indicating the first forecast origin, it
#' must be of the form `"YYYY-MM-01"`.
#' @param regressor A character. For now, only `"randomForest"`, `"xgboost"`
#' `"glmnet"` and `"lm"` are accepted.
#' @param extend A list of 2 elements. The 1st one contains a vector of
#' characters indicating columns to extend when fitting models, the 2nd one
#' contains a vector indicating the number of samples to remove and predict for
#' each column - if missing defaults to `NULL`.
#' @param extend_mode Indicates whether to extend columns in `extend_cols[[1]]`
#' using an ARMA(p,d,q) model or by replacing missing values with the last
#' observed value. Must be one of  `"ARIMA"` or `"constant"`.
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
             extend = NULL,
             extend_mode = c("ARIMA", "constant"),
             scale = c("none", "center", "scale"),
             frequency = "quarter",
             seed = 313,
             ...) {
        if (!("date" %in% names(X)) | !("Date" %in% sapply(X, class))) {
            stop("X must contain a column \"date\" containing Date objects.")
        }
        regressor <- match.arg(regressor)
        extend_mode <- match.arg(extend_mode)
        scale <- match.arg(scale)
        message(
            "Note: The date column is used to build the train/test scheme.",
            " It is not used when fitting the models."
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
            y_train <- y[train_index[[i]]]
            X_train <- X[train_index[[i]], ]
            X_test <- X[test_index[[i]], ]

            if (identical(scale, "center")) {
                X_train <- X_train %>%
                    standardize(mode = scale)
            } else if (identical(scale, "scale")) {
                X_train <- X_train %>%
                    standardize(mode = scale)
            } else if (identical(scale, "none")) {
                X_train <- X_train
            }

            if (!is.null(extend)) {
                X_train <- X_train %>%
                    extend_series(columns = extend[[1]],
                                  n = extend[[2]],
                                  mode = extend_mode)  %>%
                    as.matrix()
            } else {
                X_train <- X_train %>% as.matrix()
            }

            if (identical(regressor, "lm")) {
                fit <- lm(y_train ~ ., data = as.data.frame(X_train))
                predicted_values <- c(predicted_values, predict(fit, X_test))
            }
            else {
                fit <- regressor(X_train, y_train, ...)
                predicted_values <- c(predicted_values,
                                      predict(fit, as.matrix(X_test)))
            }

            if (identical(i, 1L) & identical(regressor, "lm")) {
                fitted_values <- c(fitted_values, predict(fit, as.data.frame(X_train),))
            } else if (identical(i, 1L) & !identical(regressor, "lm")) {
                fitted_values <- c(fitted_values, predict(fit, X_train,))
            }
            if (identical(i, 1L)) {
                first_fit <- fit
                in_sample_rmse <- sqrt(mean((fitted_values - y_train) ^2))
                in_sample_mae <- mean(abs((fitted_values - y_train) ^2))
                in_sample_mda <- mda(y_train, fitted_values)
            }

            try(utils::setTxtProgressBar(pb, i), silent = TRUE)
        }

        n_test <- length(predicted_values)
        oot_rmse <-
            sqrt(mean((predicted_values[-n_test] - y[-train_index[[1]]]) ^ 2))
        oot_mae <- mean(abs(predicted_values[-n_test] - y[-train_index[[1]]]))
        oot_mda <- mda(y[-train_index[[1]]], predicted_values[-n_test])
        out <- list(
            name = name,
            first_date = first_date,
            forecast_origin = forecast_origin,
            target = y,
            first_fit = first_fit,
            last_fit = fit,
            fitted_values = fitted_values,
            predicted_values = predicted_values,
            in_sample_rmse = in_sample_rmse,
            in_sample_mae = in_sample_mae,
            in_sample_mda = in_sample_mda,
            oot_rmse = oot_rmse,
            oot_mae = oot_mae,
            oot_mda = oot_mda,
            call = call
        )
        structure(out, class = "etalonnage")
    }
