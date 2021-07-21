graph <- function(x, ...) {
    UseMethod("graph", x)
}

graph.default <- function(x, ...) {
    stop("Method is not implemented for this class of object.")
}

graph.etalonnage <- function(x, ...) {
    if (!inherits(x, "etalonnage")) {
        stop("x is not of class etalonnage")
    }
    #TODO
}
