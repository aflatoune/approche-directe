#' Compare models predictions
#'
#' @param ... Objects of class etalonnage.
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
compare_models <- function(...,
                    start_graph,
                    title = NULL,
                    legend_text_size = 12,
                    axis_text_size = 11,
                    annotation_size = 5.5,
                    date_breaks = "1 year",
                    annotation_x = 45,
                    annotation_y = -.08) {
    objects <- list(...)
    data <- objects %>%
        purrr::map(prepare_comp) %>%
        purrr::reduce(dplyr::full_join, by = "date") %>%
        tidyr::pivot_longer(-date,
                            names_to = "model",
                            values_to = "value")

    if (is.null(title)) {
        title <-
            paste0("Prévision du taux de croissance")
    }

    upper <- max(data$date)

    g <- ggplot(data) +
        aes(x = date, y = value, colour = model) +
        geom_line(size = 1) +
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


#' @keywords internal
prepare_comp <- function(x) {
    if (!inherits(x, "etalonnage")) {
        stop("x is not of class etalonnage.")
    }

    pred <- c(x$fitted_values, x$predicted_values)
    date <- seq.Date(
        from = lubridate::ymd(x$first_date),
        by = "quarter",
        length.out = length(pred)
    )
    col_name <- x$name
    data <- data.frame(date,  pred) %>%
        dplyr::rename({{ col_name }} := pred)
    return(data)
}
