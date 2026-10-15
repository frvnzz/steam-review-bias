# Einheitlicher ggplot2-Stil für alle Grafiken der Seite.
# Farben passen zu styles/steam.scss; Positiv/Negativ nutzen die
# farbenblind-taugliche Okabe-Ito-Palette (Himmelblau vs. Orange).

library(ggplot2)

srb_col <- c(
  bg     = "#131a24",
  panel  = "#1b2838",
  grid   = "#2a3d55",
  text   = "#dce6ef",
  muted  = "#93a7bb",
  accent = "#66c0f4",
  pos    = "#56b4e9",
  neg    = "#e69f00",
  grey   = "#5b6f86"
)

# Okabe-Ito für mehrere Kategorien (ohne Schwarz – dunkler Hintergrund)
srb_qual <- c("#56b4e9", "#e69f00", "#009e73", "#f0e442", "#cc79a7", "#d55e00", "#0072b2")

theme_srb <- function(base_size = 13) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      plot.background   = element_rect(fill = srb_col[["bg"]], colour = NA),
      panel.background  = element_rect(fill = srb_col[["bg"]], colour = NA),
      panel.grid.major  = element_line(colour = srb_col[["grid"]], linewidth = 0.35),
      panel.grid.minor  = element_blank(),
      text              = element_text(colour = srb_col[["text"]]),
      axis.text         = element_text(colour = srb_col[["muted"]], size = rel(0.85)),
      axis.title        = element_text(colour = srb_col[["muted"]], size = rel(0.9)),
      plot.title        = element_text(face = "bold", size = rel(1.3), hjust = 0,
                                       margin = margin(b = 4)),
      plot.subtitle     = element_text(colour = srb_col[["muted"]], hjust = 0,
                                       margin = margin(b = 12)),
      plot.caption      = element_text(colour = srb_col[["muted"]], size = rel(0.7),
                                       hjust = 1, margin = margin(t = 10)),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      legend.position   = "top",
      legend.justification = "left",
      legend.text       = element_text(colour = srb_col[["text"]]),
      legend.title      = element_blank(),
      strip.text        = element_text(colour = srb_col[["text"]], face = "bold",
                                       margin = margin(b = 6)),
      plot.margin       = margin(16, 20, 12, 16)
    )
}

scale_fill_vote <- function(...) {
  scale_fill_manual(values = c(positiv = srb_col[["pos"]], negativ = srb_col[["neg"]]), ...)
}
scale_colour_vote <- function(...) {
  scale_colour_manual(values = c(positiv = srb_col[["pos"]], negativ = srb_col[["neg"]]), ...)
}

# Deutsche Zahlenformate
fmt_num <- function(x, digits = 0) {
  formatC(round(x, digits), format = "f", digits = digits, big.mark = ".", decimal.mark = ",")
}
fmt_pct <- function(x, digits = 0) paste0(fmt_num(100 * x, digits), " %")

theme_set(theme_srb())
update_geom_defaults("text", list(colour = srb_col[["text"]]))
