# ----------------------- #
# --- SP GGPLOT Theme --- #
# ----------------------- #

theme_sp <- function(
    title_family = "Inter",
    text_family = "Inter",
    base_size = 13, text_color = "#252525",
    bg_color = "#F7F7F7", line_color = "#252525",
    plot_margin = margin(20,20,20,20),
    plots_pane = FALSE,
    md = FALSE
  ) {
  
  if (plots_pane == FALSE & md == FALSE) {
    ggplot2::theme_minimal() +
      ggplot2::theme(
        text = element_text(family = text_family,
                            size = base_size,
                            color = text_color),
        title = element_text(family = title_family,
                             color = text_color),
        line = element_line(color = line_color),
        
        plot.title = element_text(face = "bold",
                                  size = base_size * 2,
                                  lineheight = 1.2),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = base_size * 1.2,
                                     lineheight = 1.2),
        plot.margin = plot_margin,
        plot.background = element_rect(fill = bg_color,
                                       color = bg_color),
        
        axis.text = element_text(size = base_size * 1.2),
        axis.title = element_text(size = base_size * 1.6,
                                  hjust = .8),
        axis.line = element_line(color = line_color),
        
        legend.title = element_text(size = base_size * 1.3),
        legend.text = element_text(size = base_size * 1.1)
      )
  } else if (plots_pane == FALSE & md == TRUE) {
    ggplot2::theme_minimal() +
      ggplot2::theme(
        text = element_text(family = text_family,
                            size = base_size,
                            color = text_color),
        title = ggtext::element_markdown(family = title_family,
                                         color = text_color),
        line = element_line(color = line_color),
        
        plot.title = ggtext::element_markdown(face = "bold",
                                              size = base_size * 2,
                                              lineheight = 1.2),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_markdown(size = base_size * 1.2,
                                                 lineheight = 1.2),
        plot.margin = plot_margin,
        plot.background = element_rect(fill = bg_color,
                                       color = bg_color),
        
        axis.text = element_text(size = base_size * 1.2),
        axis.title = ggtext::element_markdown(size = base_size * 1.6,
                                              hjust = .8),
        axis.line = element_line(color = line_color),
        
        legend.title = ggtext::element_markdown(size = base_size * 1.3),
        legend.text = element_text(size = base_size * 1.1)
      )
  } else if (plots_pane == TRUE & md == TRUE) {
    # ---------------- #
    # --- DEV MODE --- #
    # ---------------- #
    #' *More changes to come*
    ggplot2::theme_minimal(base_size = base_size) +
      ggplot2::theme(
        text = element_text(family = text_family,
                            color = text_color),
        title = element_text(family = title_family),
        line = element_line(color = line_color),
        
        plot.title = ggtext::element_markdown(face = "bold",
                                              lineheight = 1.2),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_markdown(lineheight = 1.2),
        plot.margin = plot_margin,
        plot.background = element_rect(fill = bg_color,
                                       color = bg_color),
        # --- Axis Styling
        axis.title.x = ggtext::element_markdown(),
        axis.title.y = ggtext::element_markdown(),
        axis.line = element_line(color = line_color),
        
        # --- Gridlines
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()
      )
  } else {
    ggplot2::theme_minimal(base_size = base_size) +
      ggplot2::theme(
        text = element_text(family = text_family,
                            color = text_color),
        title = element_text(family = title_family),
        line = element_line(color = line_color),
        
        plot.title = element_text(face = "bold",
                                  lineheight = 1.2),
        plot.title.position = "plot",
        plot.subtitle = element_text(lineheight = 1.2),
        plot.margin = plot_margin,
        plot.background = element_rect(fill = bg_color,
                                       color = bg_color),
        
        axis.title = element_text(hjust = .8),
        axis.line = element_line(color = line_color)
      )
  }
}
