#' etalonnage
#'
#` To train and evaluate our models, a rolling-origin-update (ROUE) evaluation
#` is implemented, meaning that the forecast origin "rolls" ahead in time.
#` At each step, ROUE increments the traning set by one observation of the test
#' set. The date of the first sample to predict is given by `forecast_origin`
#' + 1 quarter. Note that the training set always starts at the first sample
#' of `X`.
#'
#' @param name A character indicating a name for the analysis.
#' @param X A tibble/df containing the regressors. Must contain a date column.
#' @param y A vector containing the target variable.
#' @param forecast_origin A character indicating the first forecast origin, it
#' must be of the form `"YYYY-MM-01"`.
#' @param regressor A character. For now, only `"randomForest"`, `"xgboost"`
#' `"glmnet"` are accepted.
#' @param scale Indicates  whether to leave unchanged, center or scale `X`.
#' Must be one of `NULL`, `"center"` or `"scale"`.
#' @param seed A numeric value interpreted as an integer.
#' @param ... Aditionnal arguments to pass to the regressor.
#'
#' @return An object from S3 class `etalonnage`.
#' @export
#'
#' @examples
#' To train a Random Forest from the first date in `X` to 2019Q4 and predict
#' all samples after 2018Q4 using a rolling-origin-update evaluation :
#' model_1 <- etalonnage(name = "Random Forest en M+1",
#' X = X,
#' y = y$target,
#' regressor = "randomForest",
#' forecast_origin = "2018-10-01",
#' scale = "center")
etalonnage <-
    function(name,
             X,
             y,
             forecast_origin,
             regressor = c("randomForest", "xgboost", "glmnet"),
             scale = c(NULL, "center", "scale"),
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

        model_family <- regressor
        forecast_origin <- as.Date(forecast_origin)
        first_date <- min(X$date)
        call <- match.call()
        fitted_values <- c()
        predicted_values <- c()
        regressor <- switch(
            regressor,
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
            dplyr::select(-date)

        pb <- utils::txtProgressBar(
            min = 1, max = max(seq_along(train_index)), style = 3
            )
        set.seed(seed)
        for (i in seq_along(train_index)) {
            y_ <- y[train_index[[i]]]
            X_ <- X[train_index[[i]], ]
            if (identical(scale, "center")) {
                X_ <- X_ %>%
                    standardize_data(scale = FALSE) %>%
                    as.matrix()
            } else if (identical(scale, "scale")) {
                X_ <- X_ %>%
                    standardize_data(scale = TRUE) %>%
                    as.matrix()
            }
            # if (identical(model_family, "glmnet")) {
            #     cv <- glmnet::cv.glmnet(X_, y_, nfolds = 5, ...)
            #     fit <- regressor(X_, y_, lambda = cv$lambda.1se, ...)
            # } else {
            #     fit <- regressor(X_, y_, ...)
            # }
            fit <- regressor(X_, y_, ...)
            if (identical(i, 1L)) {
                fitted_values <- c(fitted_values, predict(fit, X_, ))
            }
            predicted_values <-
                c(predicted_values,
                  predict(fit, as.matrix(X[test_index[[i]], ])))
            utils::setTxtProgressBar(pb, i)
        }

        n_test <- length(predicted_values)
        test_rmse <- sqrt(
            mean((predicted_values[-n_test] - y[-train_index[[1]]]) ^ 2)
            )
        test_mae <- mean(abs(predicted_values[-n_test] - y[-train_index[[1]]]))
        test_mda <- mda(y[-train_index[[1]]], predicted_values[-n_test])
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
