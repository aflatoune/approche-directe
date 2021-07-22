#' train_test_index
#'
#' @param X A df/tibble containing the predictors.
#' @param date1 A character indicating the starting date of the initial window.
#' It must be of the form `"YYYY-MM-01"`.
#' @param date2 A character indicating the last date of the initial window.
#' It must be of the form `"YYYY-MM-01"`.
#' @param horizon A integer indicating the number of consecutive values in test
#' set sample - if missing defaults to 1.
#'
#' @return A list with 2 items: one for training indexes, one for test indexes.
#' @export
train_test_index <-
    function(X,
             date1,
             date2,
             horizon = 1) {
        date1 <- as.Date(date1)
        date2 <- as.Date(date2)

        if (!(date1 %in% X$date)) {
            stop("The date must be present in X.", call. = FALSE)
        }
        if (!(date2 %in% X$date)) {
            stop("The date must be present in X.", call. = FALSE)
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
        return(timeSlices)
    }
