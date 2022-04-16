#' Load data
#'
#' Load data from an xlsx file.
#'
#' @param path A path to the xlsx file containing all the data.
#' @param date A logical indicating whether the first column of the file is
#' a date column. The date must be of the form `"YYYY-MM-01"`
#' - if missing defaults to `TRUE`.
#' @param first_date A character indicating the date of observation of the first
#' sample when `date` is `FALSE`, must be of the form `"YYYY-MM-01"`
#' - if missing defaults to `NULL`.
#' @param frequency A character `"month"` or `"quarter"` used to generate
#' dates when `date = FALSE` - if missing defaults to `NULL`.
#' @param sheet A character, sheet in the xlsx file - if missing defaults
#' to `NULL`.
#'
#' @return A df/tibble with a date column and loaded data
#'
#' @export
load_data <-
    function(path,
             date = TRUE,
             first_date = NULL,
             frequency = NULL,
             sheet = NULL) {

        if (isFALSE(date) & is.null(first_date)) {
            stop("One of (\"date\", \"first_date\") must be set.", call. = FALSE)
        }
        if (isTRUE(date) & !is.null(first_date)) {
            warning("The argument \"first_date\" has been ignored as ",
            "\"date\" is not NULL", call. = FALSE)
        }
        if (isTRUE(date) & !is.null(frequency)) {
            warning("The argument \"frequency\" has been ignored as ",
            "\"date\" is not NULL", call. = FALSE)
        }
        if (!is.null(first_date) & is.null(frequency)) {
            stop("You must set \"frequency\" when \"first_date\" is not null.",
                 call. = FALSE)
        }
        if (!is.null(frequency)) {
            frequency <- match.arg(frequency, c("month", "quarter"))
        }
        if (!is.null(sheet)) {
            dataset <- readxl::read_xlsx(path = path, sheet = sheet)
        } else {
            dataset <- readxl::read_xlsx(path = path)
        }
        if (isTRUE(date) & is.null(first_date)) {
            dataset <- dataset %>%
                dplyr::rename("date" = 1) %>%
                dplyr::mutate(date = as.Date(date))
        } else if (isFALSE(date) & !is.null(first_date)) {
            dataset <- dataset %>%
                dplyr::mutate(date = seq.Date(
                    from = lubridate::ymd(first_date),
                    by = frequency,
                    length.out = dim(dataset)[1]
                )) %>%
                dplyr::relocate(date)
        }
        return(dataset)
    }
