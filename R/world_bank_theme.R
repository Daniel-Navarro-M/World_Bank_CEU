# =============================================================================
# World Bank theme and palette for plots
# Source this file from 3_eda.R, 5_correlations.R, 6_maps.R, etc.
# =============================================================================

wb_colors <- list(
  blue_dark = "#002244",
  blue_medium = "#0071BC",
  blue_light = "#009FDA",
  orange = "#F05023",
  yellow = "#FDB714",
  green = "#00AB51",
  red = "#EB1C2D",
  purple = "#872B90",
  gray_dark = "#414042",
  gray_medium = "#808285",
  gray_light = "#BCBEC0",
  gray_lighter = "#E6E7E8",
  balkans = "#F05023",
  europe = "#0071BC"
)

# theme_wb: full World Bank theme for general plots
theme_wb <- function(base_size = 12, base_family = "sans") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        color = wb_colors$blue_dark, size = ggplot2::rel(1.3), face = "bold",
        hjust = 0, margin = ggplot2::margin(b = 8)
      ),
      plot.subtitle = ggplot2::element_text(
        color = wb_colors$gray_dark, size = ggplot2::rel(1.0),
        hjust = 0, margin = ggplot2::margin(b = 12)
      ),
      plot.caption = ggplot2::element_text(
        color = wb_colors$gray_medium, size = ggplot2::rel(0.75),
        hjust = 0, lineheight = 1.3, margin = ggplot2::margin(t = 12)
      ),
      axis.title = ggplot2::element_text(color = wb_colors$gray_dark, size = ggplot2::rel(0.95), face = "bold"),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
      axis.text = ggplot2::element_text(color = wb_colors$gray_dark, size = ggplot2::rel(0.9)),
      axis.line = ggplot2::element_line(color = wb_colors$gray_light, linewidth = 0.5),
      axis.ticks = ggplot2::element_line(color = wb_colors$gray_light, linewidth = 0.3),
      panel.grid.major.y = ggplot2::element_line(color = wb_colors$gray_lighter, linewidth = 0.3),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(color = wb_colors$gray_dark, size = ggplot2::rel(0.9), face = "bold"),
      legend.text = ggplot2::element_text(color = wb_colors$gray_dark, size = ggplot2::rel(0.85)),
      legend.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.key = ggplot2::element_rect(fill = "white", color = NA),
      strip.text = ggplot2::element_text(
        color = wb_colors$blue_dark, size = ggplot2::rel(1.0), face = "bold",
        margin = ggplot2::margin(b = 5, t = 5)
      ),
      strip.background = ggplot2::element_rect(fill = wb_colors$gray_lighter, color = NA),
      plot.margin = ggplot2::margin(20, 20, 20, 20)
    )
}

# Scatter theme: minimal, consistent across correlation/validation plots
scatter_theme <- ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    plot.title       = ggplot2::element_text(face = "bold", size = 14),
    plot.subtitle    = ggplot2::element_text(size = 11, color = "gray30"),
    plot.caption     = ggplot2::element_text(hjust = 0, size = 9, color = "gray40", lineheight = 1.2),
    axis.title.x     = ggplot2::element_text(size = 12, face = "bold"),
    axis.title.y     = ggplot2::element_text(size = 12, face = "bold"),
    panel.background = ggplot2::element_rect(fill = "white", color = NA),
    plot.background  = ggplot2::element_rect(fill = "white", color = NA),
    panel.grid.major = ggplot2::element_line(color = "gray90"),
    panel.grid.minor = ggplot2::element_blank()
  )
