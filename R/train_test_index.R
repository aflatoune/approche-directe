#' train_test_index
#'
#' Generate train/test indexes to train and evaluate the models using
#' rolling-origin-update evaluation (ROUE).
#'
#' Note: if `X` has non-consecutive dates, it is better to generate train/test
#' indexes using `n` rather than `date_start` and `date_end` to avoid unexpected
#' results.
#'
#' @param X A df/tibble containing the predictors.
#' @param n A numeric indicating the size of the initial window. Cannot be
#' used with `date_start` and `date_end` - if missing defaults to `NULL`.
#' @param date_start A character indicating the starting date of the initial window.
#' It must be of the form `"YYYY-MM-01"`. Cannot be used with `n`
#' - if missing defaults to `NULL`.
#' @param date_end A character indicating the last date of the initial window.
#' It must be of the form `"YYYY-MM-01"`. Cannot be used with `n`
#' - if missing defaults to `NULL`.
#' @param horizon A integer indicating the number of consecutive values in test
#' set sample - if missing defaults to 1.
#' @param frequency A character indicating the date frequency. Must be one of
#' `"month` or `"quarter"`.
#'
#' @return A list with 2 items: one for training indexes, one for test indexes.
#' @export
train_test_index <-
    function(X,
             n = NULL,
             date_start = NULL,
             date_end = NULL,
             horizon = 1,
             frequency = c("month", "quarter")) {
        frequency <- match.arg(frequency)

        if (!is.null(n)) {
            if (!is.null(date_start) | !is.null(date_end)) {
                stop("date_start and date_end must be NULL when n is used.")
            }
            timeSlices <-
                caret::createTimeSlices(
                    y = 1:dim(X)[1],
                    initialWindow = n,
                    horizon = horizon,
                    fixedWindow = FALSE
                )
        } else if (!is.null(date_start) & !is.null(date_end)) {
            date_start <- as.Date(date_start)
            date_end <- as.Date(date_end)
            if (!(date_start %in% X$date)) {
                stop("date_start must be present in X.", call. = FALSE)
            }
            if (!(date_end %in% X$date)) {
                stop("date_end must be present in X.", call. = FALSE)
            }
            initialWindow <-
                length(seq.Date(from = date_start, to = date_end, by = frequency))
            timeSlices <-
                caret::createTimeSlices(
                    y = 1:dim(X)[1],
                    initialWindow = initialWindow,
                    horizon = horizon,
                    fixedWindow = FALSE
                )
        } else {
            stop("You must fill n or dates.")
        }
        return(timeSlices)
    }
