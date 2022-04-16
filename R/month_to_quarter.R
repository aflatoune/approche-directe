#' Build quarterly dataset from monthly dataset
#'
#' @param X A tibble/df with a `"date"` column at monthly frequency.
#' @param start A character indicating the first date to keep, it must be of
#' the form `"YYYY-MM-01"` - if missing defaults to `NULL`.
#' @param end A character indicating the last date to keep, it must be of
#' the form `"YYYY-MM-01"` - if missing defaults to `NULL`.
#'
#' @return A wider tibble/df with quarterly data. The new df contains one
#' column for each month value (e.g. X_month1, X_month2, X_month3).
#'
#' @export
month_to_quarter <-
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
