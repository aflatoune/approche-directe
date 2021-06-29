#' Initialize y and X objects
#'
#' @param path A path to the xlsx file containing all the data. The file must
#' not contain a date column, nor date indexes
#' @param target_variable A string, target variable name.
#' @param frequency A string (\code{"month"} or \code{"quarter"})
#' @param start A string indicating the date of observation of the first sample,
#' must be of the form "YYYY-MM-01"
#' @sheet A string, sheet in the xlsx file - if missing defaults to NULL
#'
#' @return
#' A list with elements
#' \item{y}{A tibble with dates and \code{target_variable} serie}
#' \item{X}{A tibble with dates and all series except \code{target_variable}}
#'
#' @examples
#' @export
#'
#' @examples
load_data <-
    function(path,
             target_variable,
             frequency,
             start,
             sheet = NULL) {
        if (is.null(sheet)) {
            dataset <- read_xlsx(path = path, sheet = sheet)
        } else {
            dataset <- read_xlsx(path = path)
        }
        dataset <- dataset %>%
            dplyr::mutate(date = seq.Date(
                from = lubridate::ymd(start),
                by = frequency,
                length.out = dim(dataset)[1]
            )) %>%
            dplyr::relocate(date)
        y <- dataset %>%
            dplyr::select(date, target_variable)
        X <- dataset %>%
            dplyr::select(-target_variable)
        return(list(y = y, X = X))
    }
