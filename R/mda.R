#' mda (Mean Directional Accuracy)
#'
#' @param actual A numeric vector indicating the ground truth values.
#' @param predicted A numeric vector indicating the predicted values.
#' @param lag A numeric - if missing defaults to 1
#'
#' @return
#' @export
mda <- function(actual, predicted, lag = 1L) {
    mean(sign(diff(actual, lag = lag)) == sign(diff(predicted, lag = lag)))
}
