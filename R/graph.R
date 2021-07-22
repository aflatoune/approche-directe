#' @import ggplot2

graph <- function(x, ...) {
    UseMethod("graph", x)
}

graph.default <- function(x, ...) {
    stop("Method not implemented for this class of object.")
}

#' Title
#'
#' @param x
#' @param start A character indicating the date of observation of the first
#' sample, must be of the form "YYYY-MM-01"
#' @param title
#' @param legend_text_size
#' @param axis_text_size
#' @param date_breaks
#' @param ...
#'
#' @return A ggplot2 graphic.
#' @export
#'
#' @examples
graph.etalonnage <-
    function(x,
             start,
             title = NULL,
             legend_text_size = 12,
             axis_text_size = 11,
             date_breaks = "1 year",
             ...) {
        if (!inherits(x, "etalonnage")) {
            stop("x is not of class etalonnage.")
        }

        if (is.null(title)) {
            name <- x$name
            title <-
                paste0("Prévision du taux de croissance (", name, ")")
        }

        actual <- x$target
        actual <- append(actual, NA)
        pred <- c(x$fitted_values, x$predicted_values)
        date <- seq.Date(
            from = lubridate::ymd(start),
            by = "quarter",
            length.out = length(pred)
        )
        data <- data.frame(date, actual, pred)
        try(data <- tibble::tibble(data))
        data <- data %>%
            tidyr::pivot_longer(
                data,
                cols = c(actual, pred),
                names_to = "type",
                values_to = "value"
            )
        upper <- max(data$date)

        g <- ggplot(data) +
            aes(x = date, y = value, colour = type) +
            geom_line(size = 1) +
            scale_color_manual(
                labels = c("Croissance observée", "Croissance prévue"),
                values = c("darkblue", "darkorange")
            ) +
            theme_minimal() +
            labs(
                title = title,
                x = "",
                y = "",
                caption = paste("Source : DG Trésor.", "Dernier point :", upper)
            ) +
            theme(
                legend.position = "bottom",
                legend.title = element_blank(),
                legend.text = element_text(size = legend_text_size),
                plot.title = element_text(
                    color = "#000080",
                    hjust = 0.5,
                    face = "bold"
                ),
                axis.title.x = element_blank(),
                axis.text.x = element_text(
                    color = "black",
                    angle = 90,
                    size = axis_text_size,
                    vjust = 0
                ),
                axis.title.y = element_blank(),
                axis.text.y = element_text(color = "black",
                                           size = axis_text_size),
                panel.grid.minor.x = element_blank(),
                panel.grid.minor.y = element_blank(),
                panel.border = element_rect(
                    color = "black",
                    fill = NA,
                    size = 1.2
                )
            ) +
            scale_x_date(
                date_labels = "%d-%m-%Y",
                date_breaks = date_breaks,
                expand = c(0, 0)
            ) +
            scale_y_continuous(labels = scales::percent)
        return(g)
    }
