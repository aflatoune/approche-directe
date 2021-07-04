#' Build monthly dataset
#'
#' @param data A tibble/df with monthly data.
#'
#' @return A wider tibble/df with quarterly data. The new df contains one
#' column for each month value (e.g. X_month1, X_month2, X_month3).
#' @export
build_monthly_data <-
    function(data) {
        excluded_vars <- c("year", "quarter", "month")
        prepared_data <- data %>%
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
            dplyr::ungroup()

        return(prepared_data)
    }


#' Build target dataset
#'
#' @param data A tibble/df with two columns : date and target variable.
#' @param date_freq A character `"month"` or `"quarter"`, indicates
#' the frequency of the date column - if missing defaults to `"month"`.
#' @param growth_rate A logical, indicates whether to compute the growth rate
#' or not - if missing defaults to `FALSE`.
#'
#' @return A tibble/df with quarterly data.
#' @export
build_target <-
    function(data,
             date_freq = "month",
             growth_rate = FALSE) {
        if (!(identical(date_freq, "month") |
              identical(date_freq, "quarter"))) {
            stop("\"date_freq\" must be one of \"month\" or \"quarter\".")
        }

        prepared_data <- data %>%
            dplyr::rename("y" = 2)

        if (identical(date_freq, "month")) {
            prepared_data <- prepared_data %>%
                dplyr::mutate(month = lubridate::month(date)) %>%
                dplyr::filter(month %in% c(1, 4, 7, 10)) %>%
                dplyr::select(-month)
        } else {
            prepared_data <- data
        }

        if (isTRUE(growth_rate)) {
            prepared_data <- prepared_data %>%
                dplyr::mutate(y = (y - dplyr::lag(y)) / dplyr::lag(y))
        }
        return(prepared_data)
    }


#' Add first-difference to the dataset
#'
#' @param data A df/tibble
#' @param exclude A vector of characters, indicates columns to ignore when
#' computing the fd - if missing defaults to `c("year", "quarter")`.
#'
#' @return A df/tibble augmented with fd
add_firstdiff <- function(data, exclude = c("year", "quarter")) {
    data <- data %>%
        dplyr::mutate(
            dplyr::across(-exclude, list(d1 =  ~ . - dplyr::lag(., n = 1L)))
            )
    return(data)
}
