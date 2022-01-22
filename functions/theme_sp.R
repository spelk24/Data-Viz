# ----------------------- #
# --- SP GGPLOT Theme --- #
# ----------------------- #

theme_sp <- function(
    title_family = "Arial",
    text_family = "Arial",
    base_size = 12, text_color = "#252525",
    bg_color = "#F7F7F7", line_color = "#252525",
    plot_margin = margin(20,20,20,20),
    plots_pane = TRUE,
    md = TRUE
  ) {
  if (plots_pane == FALSE & md == FALSE) {
    ## Print format & regular text
    ggplot2::theme_minimal() +
    ggplot2::theme(
      text = element_text(family = text_family, size = base_size, color = text_color),
      title = element_text(family = title_family, color = text_color),
      line = element_line(color = line_color),
      plot.title = element_text(face = "bold", size = base_size * 2, lineheight = 1.2),
      plot.title.position = "plot",
      plot.subtitle = element_text(size = base_size * 1.2, lineheight = 1.2),
      plot.margin = plot_margin,
      plot.background = element_rect(fill = bg_color, color = bg_color),
      axis.text = element_text(size = base_size * 1.2),
      axis.title = element_text(size = base_size * 1.6),
      axis.line = element_line(color = line_color),
      legend.title = element_text(size = base_size * 1.3),
      legend.text = element_text(size = base_size * 1.1)
    )
  } else if (plots_pane == FALSE & md == TRUE) {
    ## Print format & markdown text
    ggplot2::theme_minimal() +
    ggplot2::theme(
      text = element_text(family = text_family, size = base_size, color = text_color),
      title = ggtext::element_markdown(family = title_family, color = text_color),
      line = element_line(color = line_color),
      plot.title = ggtext::element_markdown(face = "bold", size = base_size * 1.7, lineheight = 1.2),
      plot.title.position = "plot",
      plot.subtitle = ggtext::element_markdown(size = base_size * 1.3, lineheight = 1.2),
      plot.margin = plot_margin,
      plot.background = element_rect(fill = bg_color, color = bg_color),
      axis.text = element_text(size = base_size),
      axis.title = ggtext::element_markdown(size = base_size * 1.1, lineheight = 1.2),
      axis.line = element_line(color = line_color),
      legend.title = ggtext::element_markdown(size = base_size * 1.2),
      legend.text = element_text(size = base_size * 1.1)
    )
  } else if (plots_pane == TRUE & md == TRUE) {
    ## RStudio Pane format & markdown text
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      text = element_text(family = text_family, color = text_color),
      title = element_text(family = title_family),
      line = element_line(color = line_color),
      plot.title = ggtext::element_markdown(face = "bold", lineheight = 1.2),
      plot.title.position = "plot",
      plot.subtitle = ggtext::element_markdown(lineheight = 1, size = base_size),
      plot.margin = plot_margin,
      plot.background = element_rect(fill = bg_color, color = bg_color),
      axis.line = element_line(color = line_color),
    )
  } else {
    ## RStudio Pane format & regular text
    ggplot2::theme_minimal(base_size = base_size) +
      ggplot2::theme(
        text = element_text(family = text_family, color = text_color),
        title = element_text(family = title_family),
        line = element_line(color = line_color),
        plot.title = element_text(face = "bold", lineheight = 1.2),
        plot.title.position = "plot",
        plot.subtitle = element_text(lineheight = 1.2),
        plot.margin = plot_margin,
        plot.background = element_rect(fill = bg_color, color = bg_color),
        axis.line = element_line(color = line_color)
      )
  }
}
