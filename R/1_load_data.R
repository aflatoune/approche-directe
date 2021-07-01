#' Load data
#'
#' @param path A path to the xlsx file containing all the data. The file must
#' not contain a date column, nor date indexes.
#' @param date A logical, indicates whether the first column of the file is
#' a date column. The date must be of the form "YYYY-MM-01"
#' - if missing defaults to TRUE.
#' @param start A character indicating the date of observation of the first
#' sample, must be of the form "YYYY-MM-01" - if missing defaults to NULL.
#' @param frequency A character (\code{"month"} or \code{"quarter"})
#' - if missing defaults to NULL.
#' @sheet A character, sheet in the xlsx file - if missing defaults to NULL
#'
#' @return A df/tibble with a date column and loaded data
#'
#' @examples
#' @export
#'
#' @examples
load_data <-
    function(path,
             date = TRUE,
             start = NULL,
             frequency = NULL,
             sheet = NULL) {
        if (is.null(start) & isFALSE(date)) {
            stop("One of (\"date\", \"start\") must be set.")
        }
        if (!is.null(start) & is.null(frequency)) {
            stop("You must set \"frequency\" when \"start\" is not null.")
        }
        if (!is.null(frequency)) {
            if (!(identical(frequency, "month") |
                  identical(frequency, "quarter"))) {
                stop("Frequency must be one of \"month\" or \"quarter\".")
            }
        }

        if (is.null(sheet)) {
            dataset <- readxl::read_xlsx(path = path, sheet = sheet)
        } else {
            dataset <- readxl::read_xlsx(path = path)
        }

        if (isTRUE(date) & is.null(start)) {
            dataset <- dataset %>%
                dplyr::rename("date" = 1)
        } else if (isFALSE(date) & !is.null(start)) {
            dataset <- dataset %>%
                dplyr::mutate(date = seq.Date(
                    from = lubridate::ymd(start),
                    by = frequency,
                    length.out = dim(dataset)[1]
                )) %>%
                dplyr::relocate(date)
        }
        return(dataset)
    }
