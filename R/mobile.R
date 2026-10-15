# Handy-Fassungen der Grafiken.
# Gleiche Daten und Geometrien wie am Desktop, aber für ~390 px Breite:
# kleinere Grundschrift, umbrochene Titel, Facetten untereinander,
# kleinere Punkte/Beschriftungen und weniger Datums-Ticks.
#
# In index.qmd steht jede Grafik zweimal: in einem Block .d-none.d-md-block
# (ab 768 px sichtbar) und in einem Block .d-md-none (darunter sichtbar).

library(ggplot2)

theme_srb_mobile <- function() {
  theme_srb(base_size = 11) +
    theme(
      plot.title    = element_text(face = "bold", size = rel(1.25), hjust = 0,
                                   lineheight = 1.05, margin = margin(b = 4)),
      plot.subtitle = element_text(colour = srb_col[["muted"]], size = rel(0.9), hjust = 0,
                                   lineheight = 1.1, margin = margin(b = 10)),
      plot.caption  = element_text(colour = srb_col[["muted"]], size = rel(0.75), hjust = 1,
                                   lineheight = 1.1, margin = margin(t = 8)),
      axis.title    = element_text(colour = srb_col[["muted"]], size = rel(0.9), lineheight = 1.05),
      legend.text   = element_text(colour = srb_col[["text"]], size = rel(0.85)),
      plot.margin   = margin(10, 18, 8, 6)
    )
}

# Zeilenumbruch für Beschriftungen (Breite in Zeichen, passend zu 4,2 Zoll Grafikbreite).
# Untertitel wie „A · B · C“ brechen bevorzugt an den Trennpunkten um; der Punkt
# entfällt dort, statt am Zeilenanfang oder -ende zu hängen.
wrap_label <- function(x, width) {
  if (is.null(x) || !is.character(x)) return(x)
  lines <- character()
  cur <- ""
  for (part in strsplit(x, " · ", fixed = TRUE)[[1]]) {
    cand <- if (nzchar(cur)) paste0(cur, " · ", part) else part
    if (!nzchar(cur) || nchar(cand) <= width) {
      cur <- cand
    } else {
      lines <- c(lines, cur)
      cur <- part
    }
  }
  lines <- c(lines, cur)
  paste(unlist(lapply(lines, strwrap, width = width)), collapse = "\n")
}

#' @param p           fertige ggplot-Grafik (Desktop-Fassung)
#' @param facet       optional neue Facettierung, z. B. facet_wrap(~game, ncol = 1)
#' @param size_scale  Faktor für fest eingestellte Punkt-/Textgrößen der Layer
#' @param date_breaks optional gröbere Datums-Ticks für Zeitreihen
#' @param drop        Geom-Klassen, die auf dem Handy wegfallen (z. B. "GeomText")
print_mobile <- function(p, facet = NULL, size_scale = 0.75, date_breaks = NULL, drop = NULL) {
  if (length(drop)) {
    p$layers <- Filter(\(l) !inherits(l$geom, drop), p$layers)
  }
  for (i in seq_along(p$layers)) {
    size <- p$layers[[i]]$aes_params$size
    if (!is.null(size)) p$layers[[i]]$aes_params$size <- size * size_scale
  }
  if (!is.null(facet)) p <- p + facet
  if (!is.null(date_breaks)) {
    p <- p + scale_x_date(date_labels = "%d.%m.%y", date_breaks = date_breaks)
  }
  lab <- get_labs(p)
  p <- p + labs(
    title    = wrap_label(lab$title, 34),
    subtitle = wrap_label(lab$subtitle, 48),
    caption  = wrap_label(lab$caption, 56),
    x        = wrap_label(lab$x, 44),
    y        = wrap_label(lab$y, 36)
  )
  # Grafiken mit vollständigem eigenem Theme (z. B. theme_void) behalten es
  old <- theme_set(theme_srb_mobile())
  on.exit(theme_set(old))
  print(p)
  invisible(NULL)
}
