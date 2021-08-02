#' train_test_index
#'
#' Generate train/test indexes to train and evaluate the models using
#' rolling-origin-update evaluation (ROUE).
#'
#' Note: if `X` has non-consecutive dates, it is better to generate train/test
#' indexes using `n` rather than `date1` and `date2` to avoid unexpected
#' results.
#'
#' @param X A df/tibble containing the predictors.
#' @param n A numeric indicating the size of the initial window. Cannot be
#' used with `date1` and `date2` - if missing defaults to `NULL`.
#' @param date1 A character indicating the starting date of the initial window.
#' It must be of the form `"YYYY-MM-01"`. Cannot be used with `n`
#' - if missing defaults to `NULL`.
#' @param date2 A character indicating the last date of the initial window.
#' It must be of the form `"YYYY-MM-01"`. Cannot be used with `n`
#' - if missing defaults to `NULL`.
#' @param horizon A integer indicating the number of consecutive values in test
#' set sample - if missing defaults to 1.
#'
#' @return A list with 2 items: one for training indexes, one for test indexes.
#' @export
train_test_index <-
    function(X,
             n = NULL,
             date1 = NULL,
             date2 = NULL,
             horizon = 1) {
        date1 <- as.Date(date1)
        date2 <- as.Date(date2)

        if (!is.null(n)) {
            if (!is.null(date1) | !is.null(date2)) {
                stop("date1 and date2 must be NULL when n is used.")
            }
            timeSlices <-
                caret::createTimeSlices(
                    y = 1:dim(X)[1],
                    initialWindow = n,
                    horizon = horizon,
                    fixedWindow = FALSE
                )
        } else if (!is.null(date1) & !is.null(date2)) {
            if (!(date1 %in% X$date)) {
                stop("date1 must be present in X.", call. = FALSE)
            }
            if (!(date2 %in% X$date)) {
                stop("date2 must be present in X.", call. = FALSE)
            }
            initialWindow <-
                length(seq.Date(from = date1, to = date2, by = "quarter"))
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
