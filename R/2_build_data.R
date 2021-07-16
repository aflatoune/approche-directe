#' Build monthly dataset
#'
#' @param X A tibble/df with a `"date"` column at monthly frequency.
#' @param start A character indicating the first date to keep, it must be of
#' the form "YYYY-MM-01" - if missing defaults to `NULL`.
#' @param end A character indicating the last date to keep, it must be of
#' the form "YYYY-MM-01" - if missing defaults to `NULL`.
#'
#' @return A wider tibble/df with quarterly data. The new df contains one
#' column for each month value (e.g. X_month1, X_month2, X_month3).
#'
#' @export
build_monthly_data <-
    function(X, start = NULL, end = NULL) {
        if (!is.null(start) & !is.null(end)) {
            X <- X %>%
                dplyr::filter(date >= start & date <= end)
        } else if (is.null(start) & !is.null(end)) {
            X <- X %>%
                dplyr::filter(date <= end)
        } else if (!is.null(start) & is.null(end)) {
            X <- X %>%
                dplyr::filter(date >= start)
        }

        excluded_vars <- c("year", "quarter", "month")
        prepared_X <- X %>%
            dplyr::mutate(
                year = lubridate::year(date),
                quarter = lubridate::quarter(date),
                month = lubridate::month(date)
            ) %>%
            dplyr::mutate(month = dplyr::case_when(
                month %in% c(1, 4, 7, 10) ~ 1,
                month %in% c(2, 5, 8, 11) ~ 2,
                month %in% c(3, 6, 9, 12) ~ 3
            )) %>%
            dplyr::select(-date) %>%
            dplyr::group_by(year, quarter) %>%
            tidyr::pivot_wider(names_from = month,
                               values_from = -dplyr::all_of(excluded_vars)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(quarter = dplyr::case_when(quarter == 1 ~ 1,
                                                     quarter == 2 ~ 4,
                                                     quarter == 3 ~ 7,
                                                     quarter == 4 ~ 10)) %>%
            dplyr::mutate(date = lubridate::make_date(year, quarter)) %>%
            dplyr::relocate(date) %>%
            dplyr::select(-year, -quarter)
        return(prepared_X)
    }


#' Build target dataset
#'
#' @param Y A tibble/df with two columns : date and target variable.
#' @param date_freq A character `"month"` or `"quarter"`, indicates
#' the frequency of the date column - if missing defaults to `"month"`.
#' @param growth_rate A logical, indicates whether to compute the growth rate
#' or not - if missing defaults to `FALSE`.
#'
#' @return A tibble/df with quarterly data.
#' @export
build_target <-
    function(y,
             growth_rate,
             start = NULL,
             end = NULL,
             date_freq = "month") {
        if (!(identical(date_freq, "month") |
              identical(date_freq, "quarter"))) {
            stop("\"date_freq\" must be one of \"month\" or \"quarter\".",
                 call. = FALSE)
        }

        prepared_y <- y %>%
            dplyr::rename("target" = 2)

        if (identical(date_freq, "month")) {
            prepared_y <- prepared_y %>%
                dplyr::mutate(month = lubridate::month(date)) %>%
                dplyr::filter(month %in% c(1, 4, 7, 10)) %>%
                dplyr::select(-month)
        } else {
            prepared_y <- y
        }

        if (isTRUE(growth_rate)) {
            prepared_y <- prepared_y %>%
                dplyr::mutate(
                    target = (target - dplyr::lag(target)) / dplyr::lag(target)
                    )
        }

        if (!is.null(start) & !is.null(end)) {
            prepared_y <- prepared_y %>%
                dplyr::filter(date >= start & date <= end)
        } else if (is.null(start) & !is.null(end)) {
            prepared_y <- prepared_y %>%
                dplyr::filter(date <= end)
        } else if (!is.null(start) & is.null(end)) {
            prepared_y <- prepared_y %>%
                dplyr::filter(date >= start)
        }

        return(prepared_y)
    }
