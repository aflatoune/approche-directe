#' Build target dataset
#'
#' @param Y A tibble/df with two columns : date and target variable.
#' @param growth_rate A logical, indicates whether to compute the growth rate
#' or not - if missing defaults to `FALSE`.
#' @param date_freq A character `"month"` or `"quarter"`, indicates
#' the frequency of the date column.
#' @param start A character indicating the first date to keep, it must be of
#' the form `"YYYY-MM-01"` - if missing defaults to `NULL`.
#' @param end A character indicating the last date to keep, it must be of
#' the form `"YYYY-MM-01"` - if missing defaults to `NULL`.
#'
#' @return A tibble/df with quarterly data.
#' @export
build_target <-
    function(y,
             growth_rate,
             date_freq = c("month", "quarter"),
             start = NULL,
             end = NULL) {
        date_freq <- match.arg(date_freq)
        prepared_y <- y %>%
            dplyr::rename("target" = 2)

        if (identical(date_freq, "month")) {
            prepared_y <- prepared_y %>%
                dplyr::mutate(month = lubridate::month(date)) %>%
                dplyr::filter(month %in% c(1, 4, 7, 10)) %>%
                dplyr::select(-month)
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
