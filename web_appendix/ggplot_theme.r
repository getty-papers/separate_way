library(latex2exp)
library(ggplot2)


theme_Publication <- function(base_size = 14, ...) {
    library(grid)
    library(ggthemes)
    library(ggtext)
    (theme_foundation(base_size = base_size)
    + theme(
            plot.title = element_text(
                face = "bold",
                size = rel(1.2), hjust = 0.5
            ),
            plot.subtitle = element_markdown(size = rel(1), hjust = 0.5),
            plot.caption = element_markdown(size = rel(0.7), color = "grey"),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_markdown(face = "bold", size = rel(1)),
            axis.title.y = element_text(angle = 0, vjust = 0.5),
            axis.title.x = element_markdown(vjust = -0.2),
            axis.text = element_markdown(),
            axis.line = element_line(colour = "black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour = "#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size = unit(0.2, "cm"),
            legend.margin = margin(0, "cm"),
            legend.title = element_markdown(face = "italic"),
            plot.margin = margin(c(10, 5, 5, 5), "mm"),
            strip.background = element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
            strip.text = element_markdown(face = "bold"),
        ))
}
scale_fill_Publication <- function(...) {
    library(scales)
    discrete_scale(
        "fill", "Publication",
        manual_pal(values = c("#386cb0", "#fdb462", "#7fc97f", "#ef3b2c", "#662506", "#a6cee3", "#fb9a99", "#984ea3", "#ffff33")), ...
    )
}

scale_colour_Publication <- function(...) {
    library(scales)
    discrete_scale(
        "colour", "Publication",
        manual_pal(values = c("#386cb0", "#fdb462", "#7fc97f", "#ef3b2c", "#662506", "#a6cee3", "#fb9a99", "#984ea3", "#ffff33")), ...
    )
}
