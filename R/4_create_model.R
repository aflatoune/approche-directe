#' train_test_index
#'
#' @param y A df/tibble containing the target variable.
#' @param date1 A character indicating the starting date of the initial window.
#' It must be of the form "YYYY-MM-01".
#' @param date2 A character indicating the last date of the initial window.
#' It must be of the form "YYYY-MM-01".
#' @param horizon A integer indicating the number of consecutive values in test
#' set sample - if missing defaults to 1.
#'
#' @return A list with 2 items: one for training indexes, one for test indexes.
#' @export
train_test_index <-
    function(y,
             date1,
             date2,
             horizon = 1) {
        date1 <- as.Date(date1)
        date2 <- as.Date(date2)
        initialWindow <-
            length(seq.Date(from = date1, to = date2, by = "quarter"))
        timeSlices <-
            caret::createTimeSlices(
                y = 1:dim(y)[1],
                initialWindow = initialWindow,
                horizon = horizon,
                fixedWindow = FALSE
            )
        train_index <- timeSlices[[1]]
        test_index <- timeSlices[[2]]
        return(list(train_index = train_index, test_index = test_index))
    }


#' train_model
#'
#' @param model
#' @param truncate
#' @param train_index
#' @param test_index
#'
#' @return
#' @export
#'
#' @examples
train_model <- function(y, X, model, train_index, test_index) {
    #TODO
}
