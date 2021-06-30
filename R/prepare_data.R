#' Title
#'
#' @param df A tibble/df
#' @param position
#' @param mode A string indicating the aggregation mode of the data.
#' (\code{"mean"}, \code{first}, \code{second} or \code{"last"})
#'
#' @return A tibble/df
#' @export
#'
#' @examples
preapre_data <- function(df, mode) {
    if (identical(mode, "mean")) {
        aggregate_data <- df %>%
            dplyr::group_by(year = lubridate::year(date),
                            quarter = lubridate::quarter(date)) %>%
            dplyr::summarise(dplyr::across(where(is.numeric), list(mean), .names = {
                "col"
            }),
            .groups = "drop")
        return(prepared_data)
    } else if (identical(mode, "last")) {
        aggregate_data <- df %>%

    }
}



dg_data <- dg_data %>%
    filter(lubridate::year(date) > 1989)


temp <- dg_data %>%
    group_by(
        year = lubridate::year(date),
        quarter = lubridate::quarter(date),
        month = lubridate::month(date)
    ) %>%
    mutate(
        month = case_when(
            month %in% c(1, 4, 7, 10) ~ 1,
            month %in% c(2, 5, 8, 11) ~ 2,
            month %in% c(3, 6, 9, 12) ~ 3
        )
    )







sub <- temp %>%
    select(-date)
rere = sub %>% group_by(year, quarter) %>% pivot_wider(names_from = month, values_from = names(sub)[1:14])








