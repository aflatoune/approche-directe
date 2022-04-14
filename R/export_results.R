#' @export
export_results <- function(x, ...) {
    UseMethod("export_results", x)
}

#' @export
export_results.default <- function(x, ...) {
    stop("Method not implemented for this class of object.")
}

#' export_results
#'
#' Export results to a csv file. The files are automatically named according to
#' the `name` attribute of the object or to its name if the attribute is `NULL`.
#'
#' @param x An object of class etalonnage.
#' @param coef A logical indicating whether to export estimated coefficients to
#' a csv file - if missing defaults to `TRUE`.
#' @param metrics A logical indicating whether to export some performances
#' metrics to a csv file - if missing defaults to `TRUE`.
#'
#' @export
export_results.etalonnage <-
    function(x,
             coef = TRUE,
             metrics = TRUE) {
        if (!inherits(x, "etalonnage")) {
            stop("x is not of class etalonnage.")
        }
        if (!identical(x$call$regressor, "lm")) {
            stop("`export_results()` only works when `regressor == \"lm\"`.",
                 " To check your regressor run `x$call$regressor`.")
        }

        tidy_coef <- broom::tidy(summary(x$first_fit))
        adj_rsquared <- summary(lm_insee$first_fit)$adj.r.squared
        metrics <- data.frame(
            adj_rsquared = adj_rsquared,
            in_sample_rmse = x$in_sample_rmse,
            in_sample_mae = x$in_sample_mae,
            in_sample_mda = x$in_sample_mda,
            oot_rmse = x$oot_rmse,
            oot_mae = x$oot_mae,
            oot_mda = x$oot_mda
        )

        if (!is.null(x$name)) {
            path_coef <- paste0("coef_", x$name, ".csv")
            path_metrics <- paste0("metrics_", x$name, ".csv")
            coef <- readr::write_excel_csv2(tidy_coef, file = path_coef)
            metrics <- readr::write_excel_csv2(metrics, file = path_metrics)
        } else {
            name <- deparse(substitute(x))
            path_coef <- paste0("coef_", name, ".csv")
            path_metrics <- paste0("metrics_", name, ".csv")
            coef <- readr::write_excel_csv2(tidy_coef, file = path_coef)
            metrics <- readr::write_excel_csv2(metrics, file = path_metrics)
        }
    }
