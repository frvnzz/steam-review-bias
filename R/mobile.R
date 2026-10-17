# Handy-Fassungen der Grafiken.
# Gleiche Daten wie am Desktop, aber für ~360 px Breite. Am Handy stehen die
# Grafiken nicht sticky, sondern in voller Breite zwischen den Absätzen
# (js/linear-layout.html). Deshalb:
#   • Titel, Untertitel und Quellenhinweis als echter HTML-Text statt im Bild
#     (skaliert mit der Schrift, bricht sauber um, spart Bildhöhe)
#   • größere Grundschrift, Facetten untereinander, weniger Datums-Ticks
#   • eigene Layouts, wo drei Spiele nebeneinander zu eng wären
#
# In index.qmd steht jede Grafik zweimal: in einem Block .d-none.d-md-block
# (ab 768 px sichtbar) und in einem Block .d-md-none (darunter sichtbar).
# Die Handy-Chunks brauchen `output: asis`, damit der HTML-Text durchkommt.
#
# Braucht vorher: R/config.R, R/theme.R, R/analysis.R.

library(ggplot2)

theme_srb_mobile <- function() {
  theme_srb(base_size = 14, text_size = 14) +
    theme(
      axis.title  = element_text(colour = srb_col[["muted"]], size = rel(0.85), lineheight = 1.05),
      axis.text   = element_text(colour = srb_col[["muted"]], size = rel(0.8)),
      legend.text = element_text(colour = srb_col[["text"]], size = rel(0.85)),
      strip.text  = element_text(colour = srb_col[["text"]], face = "bold", size = rel(0.9),
                                 hjust = 0, margin = margin(t = 4, b = 4)),
      plot.margin = margin(4, 20, 4, 2)   # rechts Platz für das letzte Datum
    )
}

# Zeilenumbruch für Achsentitel (Breite in Zeichen, passend zu 4,2 Zoll Grafikbreite).
# Texte wie „A · B · C“ brechen bevorzugt an den Trennpunkten um; der Punkt
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

html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

#' @param p           fertige ggplot-Grafik (Desktop-Fassung oder eigene Handy-Fassung)
#' @param facet       optional neue Facettierung, z. B. facet_wrap(~game, ncol = 1)
print_mobile <- function(p, facet = NULL) {
  if (!is.null(facet)) p <- p + facet

  # Titel, Untertitel und Quellenhinweis als HTML um das Bild herum
  lab <- get_labs(p)
  head <- c(
    if (!is.null(lab$title))    sprintf('<p class="fig-title">%s</p>', html_escape(lab$title)),
    if (!is.null(lab$subtitle)) sprintf('<p class="fig-sub">%s</p>', html_escape(lab$subtitle))
  )
  if (length(head)) cat('<div class="fig-head">', head, "</div>\n\n", sep = "")

  p <- p + labs(
    title = NULL, subtitle = NULL, caption = NULL,
    x = wrap_label(lab$x, 38),
    y = wrap_label(lab$y, 30)
  )
  # Viele Gruppen auf einer diskreten x-Achse (z. B. neun Spielzeit-Gruppen) passen
  # am Handy nicht nebeneinander: Beschriftung auf zwei Zeilen versetzen statt drehen
  sx <- layer_scales(p)$x
  if (!is.null(sx) && sx$is_discrete() && length(sx$get_limits()) > 6) {
    p <- p + guides(x = guide_axis(n.dodge = 2))
  }
  # Grafiken mit vollständigem eigenem Theme (z. B. theme_void) behalten es
  old <- theme_set(theme_srb_mobile())
  on.exit(theme_set(old))
  print(p)

  if (!is.null(lab$caption)) {
    cat('\n\n<p class="fig-caption">', gsub("\n", "<br>", html_escape(lab$caption), fixed = TRUE),
        "</p>\n", sep = "")
  }
  invisible(NULL)
}

# ---- Eigene Handy-Layouts --------------------------------------------------
# Desktop: drei Spiele nebeneinander, Gruppen als Zeilen. Am Handy würde das die
# Beschriftungen dreimal wiederholen und die Punkte in ein schmales Drittel
# drängen. Stattdessen: ein Feld pro Gruppe (Überschrift über voller Breite),
# darin die drei Spiele als Zeilen.

# lab: Beschriftung der Desktop-Fassung (factors_labs() bzw. languages_labs())
dot_rows_mobile <- function(d, facet_var, lab, x_breaks = waiver()) {
  d$game <- factor(d$game, levels = rev(levels(d$game)))   # Baldur's Gate 3 oben
  ggplot(d, aes(share, game)) +
    geom_linerange(aes(xmin = lo, xmax = hi), colour = srb_col[["muted"]], linewidth = 0.8) +
    geom_point(colour = srb_col[["pos"]], size = 2.6) +
    n_label(size = 3.2) +
    facet_wrap(vars({{ facet_var }}), ncol = 1) +
    scale_y_discrete(drop = FALSE) +
    # rechts Platz für „n = …“ über Punkten nahe 100 %
    scale_x_continuous(labels = \(x) fmt_pct(x), breaks = x_breaks,
                       expand = expansion(mult = c(0.05, 0.2))) +
    labs(x = lab$x, y = NULL, title = lab$title, subtitle = lab$subtitle, caption = lab$caption) +
    theme(panel.grid.major.y = element_blank(), panel.spacing.y = unit(0.6, "lines"))
}

plot_factors_mobile <- function(reviews, min_n = min_group_n) {
  d <- factor_table(reviews) |> filter(n >= min_n)
  d$group <- factor(d$group, levels = rev(levels(d$group)))   # Reihenfolge wie im Text
  dot_rows_mobile(d, group, factors_labs(min_n), x_breaks = seq(0, 1, 0.2))
}

plot_languages_mobile <- function(reviews, top = 6) {
  d <- language_table(reviews, top) |>
    mutate(lang = coalesce(language_names[language], language))
  order <- d |> group_by(lang) |> summarise(m = mean(share)) |> arrange(desc(m)) |> pull(lang)
  d$lang <- factor(d$lang, levels = order)
  dot_rows_mobile(d, lang, languages_labs(top))
}
