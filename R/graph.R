#' @import ggplot2
NULL

graph <- function(x) {
    UseMethod("graph", x)
}

graph.default <- function(x) {
    stop("Method not implemented for this class of object.")
}

#' graph
#'
#' S3 method for class `etalonnage`.
#'
#' @param x An object of class etalonnage.
#' @param start_graph A character indicating the first date to plot, must be of
#' the form `"YYYY-MM-01"` - if missing defaults to `x$first_date`.
#' @param title A character indicating a title for the plot - if missing
#' defaults to `NULL`.
#' @param legend_text_size A numeric - if missing defaults to 12.
#' @param axis_text_size A numeric - if missing defaults to 11.
#' @param annotation_size A numeric - if missing defaults to 5.5.
#' @param date_breaks A character indicating date breaks on the x axis of the
#' plot - if missing defaults to `"1 year"`.
#' @param annotation_x A numeric indicating the position of the annonation on
#' x-axis - if missing defaults to 45 (days).
#' @param annotation_y A numeric indicating the position of the annonation on
#' y-axis - if missing defaults to -0.08 (8 %).
#'
#' @return A ggplot2 plot.
#' @export
graph.etalonnage <-
    function(x,
             start_graph = x$first_date,
             title = NULL,
             legend_text_size = 12,
             axis_text_size = 11,
             annotation_size = 5.5,
             date_breaks = "1 year",
             annotation_x = 45,
             annotation_y = -.08) {
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
            from = lubridate::ymd(x$first_date),
            by = "quarter",
            length.out = length(pred)
        )
        data <- data.frame(date, actual, pred)
        data <- data %>%
            tidyr::pivot_longer(
                cols = c(actual, pred),
                names_to = "type",
                values_to = "value"
            ) %>%
            dplyr::filter(date > start_graph)
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
            scale_y_continuous(labels = scales::percent) +
            geom_vline(xintercept = x$forecast_origin,
                       col = "gray40",
                       lwd = .5) +
            annotate(
                "text",
                x = x$forecast_origin - annotation_x,
                y = annotation_y,
                label = "Prévision",
                angle = 90,
                col = "gray40",
                size = annotation_size
                )
        return(g)
    }
