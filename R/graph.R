graph <- function(x, ...) {
    UseMethod("graph", x)
}


graph.etalonnage <- function(x, ...) {
    if (!inherits(x, "etalonnage")) {
        stop("x is not of class etalonnage")
    }
    #TODO
}
