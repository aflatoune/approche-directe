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
            dplyr::summarise(
                dplyr::across(where(is.numeric), list(mean), .names = {"col"}),
                .groups = "drop"
            )
        return(prepared_data)
    } else if (identical(mode, "last")) {
        aggregate_data <- df %>%

    }
}
