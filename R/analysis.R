# Analysen und Grafiken für die Kapitel 1 und 3–6.
# Liest nur die bereinigten Daten aus data/ (keine IDs, kein Freitext).
# Braucht vorher: R/config.R (games, Schwellenwerte), R/theme.R (srb_col, fmt_*).

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

# ---- Daten ------------------------------------------------------------------

# Spaltentypen fest vorgegeben: Ändert sich das Format der Daten, fällt das hier auf
# statt als still falsch geratener Typ (readr warnt bei Abweichungen)
load_data <- function() {
  reviews <- read_csv("data/reviews_sample.csv",
                      col_types = cols(game = "c", language = "c", created = "D",
                                       appid = "i", author_num_reviews = "i",
                                       playtime_at_review_min = "i", playtime_forever_min = "i",
                                       .default = "l")) |>
    mutate(
      vote = factor(if_else(voted_up, "positiv", "negativ"), levels = c("positiv", "negativ")),
      playtime_h = playtime_at_review_min / 60,
      game = factor(game, levels = games$name)
    )
  meta <- read_csv("data/meta.csv", col_types = cols(.default = "c"))
  list(
    reviews   = reviews,
    # share_pos: nur Steam-Käufe, wie in Steams Wertung; share_pos_all: alle Kaufarten,
    # vergleichbar mit Stichprobe und Tageswerten
    summaries = read_csv("data/summaries.csv",
                         col_types = cols(review_score_desc = "c", game = "c", .default = "i")) |>
      mutate(share_pos = total_positive / total_reviews,
             share_pos_all = total_positive_all / total_reviews_all),
    daily     = read_csv("data/daily_counts.csv",
                         col_types = cols(game = "c", date = "D", filter_offtopic = "l",
                                          .default = "i")) |>
      mutate(share_pos = n_positive / n_total),
    meta      = setNames(meta$value, meta$key)
  )
}

# ---- Statistik-Helfer -------------------------------------------------------

# 95-%-Wilson-Intervall für einen Anteil (stabil auch bei kleinen n / Anteilen nahe 0 oder 1)
wilson <- function(x, n, conf = 0.95) {
  z <- qnorm(1 - (1 - conf) / 2)
  p <- x / n
  centre <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  tibble(share = p, lo = centre - half, hi = centre + half)
}

share_by <- function(data, ...) {
  data |>
    group_by(...) |>
    summarise(n = n(), n_pos = sum(voted_up), .groups = "drop") |>
    mutate(wilson(n_pos, n))
}

playtime_bins <- c(0, 1, 2, 5, 10, 20, 50, 100, 200, Inf)
playtime_labels <- c("< 1 h", "1–2 h", "2–5 h", "5–10 h", "10–20 h", "20–50 h",
                     "50–100 h", "100–200 h", "> 200 h")

reviewer_bins <- c(0, 1, 5, 20, 100, Inf)
reviewer_labels <- c("1 Review", "2–5", "6–20", "21–100", "> 100")

# ---- Kapitel 3: Spielzeit ---------------------------------------------------

# Anteil positiv je Spielzeit-Gruppe (Reviews ohne Spielzeit fallen weg)
playtime_table <- function(reviews) {
  reviews |>
    filter(!is.na(playtime_h)) |>
    mutate(bin = cut(playtime_h, playtime_bins, playtime_labels, right = FALSE)) |>
    share_by(game, bin)
}

playtime_stats <- function(reviews) {
  reviews |>
    group_by(game) |>
    summarise(
      n = n(),
      median_h = median(playtime_h, na.rm = TRUE),
      mean_h = mean(playtime_h, na.rm = TRUE),
      share_under_2h = mean(playtime_h < 2, na.rm = TRUE),
      .groups = "drop"
    )
}

plot_playtime_hist <- function(reviews, which_game) {
  d <- filter(reviews, game == which_game, playtime_h > 0)
  st <- tibble(stat = c("Median", "Mittelwert"),
               value = c(median(d$playtime_h), mean(d$playtime_h)))
  ggplot(d, aes(playtime_h)) +
    # Grau statt Blau: Blau steht auf der Seite nur für „positiv“
    geom_histogram(bins = 45, fill = srb_col[["grey"]], colour = srb_col[["bg"]],
                   linewidth = 0.3) +
    geom_vline(data = st, aes(xintercept = value), colour = srb_col[["text"]],
               linewidth = 0.6, linetype = c("solid", "22")) +
    geom_label(data = st, aes(x = value, y = Inf, label = paste0(stat, "\n", fmt_num(value), " h")),
               vjust = 1.1, hjust = c(1.05, -0.05), size = 3.8, lineheight = 0.9,
               fill = srb_col[["bg"]], colour = srb_col[["text"]], linewidth = 0) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.22))) +
    scale_x_log10(labels = \(x) paste(ifelse(x < 1, fmt_num(x, 1), fmt_num(x)), "h"),
                  breaks = c(0.1, 1, 10, 100, 1000)) +
    labs(x = "Spielzeit beim Schreiben der Review (logarithmische Achse)",
         y = "Anzahl Reviews",
         title = paste0(which_game, ": Spielzeit bis zur Review"),
         subtitle = "Jeder Balken umfasst einen gleich breiten Bereich auf der logarithmischen Achse")
}

plot_share_by_playtime <- function(reviews) {
  d <- playtime_table(reviews) |> filter(n >= min_group_n)
  ggplot(d, aes(bin, share, group = game)) +
    geom_linerange(aes(ymin = lo, ymax = hi), colour = srb_col[["muted"]], linewidth = 0.6) +
    geom_line(colour = srb_col[["pos"]], linewidth = 0.8) +
    geom_point(colour = srb_col[["pos"]], size = 2.4) +
    facet_wrap(~game, ncol = 1) +
    scale_y_continuous(labels = \(x) fmt_pct(x), limits = c(NA, 1)) +
    # Achse ohne „ h“: die Einheit steht im Achsentitel, so passen die Gruppen ungedreht
    scale_x_discrete(labels = \(x) sub(" h$", "", x)) +
    labs(x = "Spielzeit beim Schreiben der Review, in Stunden", y = "Anteil positiv",
         title = "Reviews nach weniger als 2 Stunden sind seltener positiv",
         subtitle = "Anteil positiver Reviews je Spielzeit-Gruppe mit 95-%-Unsicherheitsintervall (Striche)",
         caption = paste0("y-Achse beginnt nicht bei 0 %. Gruppen mit weniger als ", min_group_n,
                          " Reviews ausgeblendet.")) +
    # mehr Luft zwischen den Feldern: der linksbündige Spielname stößt sonst an die Achse darüber
    theme(panel.grid.major.x = element_blank(), panel.spacing.y = unit(1.2, "lines"))
}

# ---- Kapitel 4: Wer sind die Reviewer? -------------------------------------

reviewer_table <- function(reviews) {
  reviews |>
    mutate(bin = cut(author_num_reviews, reviewer_bins, reviewer_labels)) |>
    share_by(game, bin) |>
    group_by(game) |>
    mutate(prop = n / sum(n)) |>
    ungroup()
}

plot_reviewer_mix <- function(reviews) {
  d <- reviewer_table(reviews)
  ggplot(d, aes(prop, bin)) +
    geom_col(fill = srb_col[["grey"]], width = 0.7) +   # Grau: Menge, keine Wertung
    geom_text(aes(label = fmt_pct(prop)), hjust = -0.15, size = 3.6) +
    facet_wrap(~game, nrow = 1) +
    scale_x_continuous(labels = \(x) fmt_pct(x), expand = expansion(c(0, 0.25))) +
    labs(x = "Anteil der Reviews", y = "Reviews, die die Person\ninsgesamt geschrieben hat",
         title = "Nur wenige Reviews stammen von Viel-Reviewern",
         subtitle = "Verteilung nach Anzahl aller Reviews der Autor:innen (heutiger Stand)") +
    theme(panel.grid.major.y = element_blank(), panel.spacing = unit(1.5, "lines"))
}

plot_reviewer_share <- function(reviews) {
  d <- reviewer_table(reviews) |> filter(n >= min_group_n)
  ggplot(d, aes(bin, share, group = game)) +
    geom_linerange(aes(ymin = lo, ymax = hi), colour = srb_col[["muted"]], linewidth = 0.6) +
    geom_line(colour = srb_col[["pos"]], linewidth = 0.8) +
    geom_point(colour = srb_col[["pos"]], size = 2.4) +
    facet_wrap(~game, ncol = 1) +   # gemeinsame y-Achse: Unterschiede nicht optisch aufblähen
    scale_y_continuous(labels = \(x) fmt_pct(x), limits = c(NA, 1)) +
    labs(x = "Reviews, die die Person insgesamt geschrieben hat", y = "Anteil positiv",
         title = "Viel-Reviewer bewerten seltener positiv",
         subtitle = "Anteil positiver Reviews mit 95-%-Unsicherheitsintervall (Striche)",
         caption = paste0("y-Achse beginnt nicht bei 0 %. Gruppen mit weniger als ", min_group_n,
                          " Reviews ausgeblendet.")) +
    # senkrechte Linien weg: die Gruppen liest man an den Punkten ab, nicht an Gitterlinien;
    # mehr Luft zwischen den Feldern, damit der Spielname nicht an „70 %“ darüber stößt
    theme(panel.grid.major.x = element_blank(), panel.spacing.y = unit(1.2, "lines"))
}

# ---- Kapitel 5: Review-Bombing ---------------------------------------------

plot_daily_volume <- function(daily, which_game, shade = NULL) {
  d <- filter(daily, game == which_game, filter_offtopic)
  ggplot(d, aes(date, n_total)) +
    shade_layer(shade) +
    geom_col(fill = srb_col[["grey"]], width = 1) +   # Grau: Menge, keine Wertung
    scale_y_continuous(labels = \(x) fmt_num(x)) +
    scale_x_date(date_labels = "%d.%m.%Y", date_breaks = "2 weeks") +
    labs(x = NULL, y = "Neue Reviews pro Tag",
         title = paste0(which_game, ": Reviews pro Tag"),
         subtitle = "Nach Tag der Erstellung (UTC)") +
    theme(panel.grid.major.x = element_blank())
}

plot_daily_share <- function(daily, which_game, shade = NULL) {
  d <- filter(daily, game == which_game, filter_offtopic, n_total > 0) |>
    mutate(wilson(n_positive, n_total))
  ggplot(d, aes(date, share)) +
    shade_layer(shade) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = srb_col[["pos"]], alpha = 0.25) +
    geom_line(colour = srb_col[["pos"]], linewidth = 0.8) +
    scale_y_continuous(labels = \(x) fmt_pct(x), limits = c(0, 1)) +
    scale_x_date(date_labels = "%d.%m.%Y", date_breaks = "2 weeks") +
    labs(x = NULL, y = "Anteil positiv (heutiger Stand)",
         title = paste0(which_game, ": Anteil positiver Reviews pro Tag"),
         subtitle = "Nach Tag der Erstellung (UTC), positiv oder negativ nach heutigem Stand") +
    theme(panel.grid.major.x = element_blank())
}

# short_legend: kürzere Legende für die Handy-Fassung
plot_offtopic <- function(daily, which_game, short_legend = FALSE) {
  lab_all <- "Ohne Filter: alle Reviews"
  lab_std <- if (short_legend) "Standard: Off-Topic ausgeblendet"
             else "Standardansicht: Off-Topic-Phasen ausgeblendet"
  d <- daily |>
    filter(game == which_game) |>
    mutate(n_negative = n_total - n_positive,
           variant = if_else(filter_offtopic, lab_std, lab_all))
  ggplot(d, aes(date, n_negative, colour = variant)) +
    geom_line(linewidth = 0.8) +
    scale_colour_manual(values = c(srb_col[["neg"]], srb_col[["muted"]]),
                        breaks = c(lab_all, lab_std)) +
    scale_y_continuous(labels = \(x) fmt_num(x)) +
    scale_x_date(date_labels = "%d.%m.%Y", date_breaks = "2 weeks") +
    labs(x = NULL, y = "Negative Reviews pro Tag",
         title = paste0(which_game, ": Steams Filter blendet die Review-Welle aus"),
         subtitle = "Negative Reviews pro Tag, mit und ohne Steams Off-Topic-Filter") +
    theme(legend.direction = "vertical", panel.grid.major.x = element_blank())
}

# ---- Kapitel 6: Weitere Selektionsfaktoren ---------------------------------

factor_table <- function(reviews) {
  groups <- list(
    "Auf Steam gekauft"          = reviews$steam_purchase,
    "Nicht auf Steam gekauft"    = !reviews$steam_purchase,
    "Kostenlos erhalten"         = reviews$received_for_free,   # Häkchen der Autor:innen, kein Steam-Geschenk
    "Später zurückgegeben"       = reviews$refunded,
    "Meist auf dem Steam Deck"   = reviews$primarily_steam_deck
  )
  bind_rows(lapply(names(groups), function(g) {
    reviews[groups[[g]], ] |> share_by(game) |> mutate(group = g)
  })) |>
    mutate(group = factor(group, levels = rev(names(groups))))
}

# Beschriftung von plot_factors(), auch für die Handy-Fassung (R/mobile.R)
factors_labs <- function(min_n) {
  labs(x = "Anteil positiv", y = NULL,
       title = "Reviews zurückgegebener Spiele sind seltener positiv",
       subtitle = "Anteil positiver Reviews mit 95-%-Unsicherheitsintervall",
       caption = paste0("Gruppen mit weniger als ", min_n, " Reviews ausgeblendet"))
}

plot_factors <- function(reviews, min_n = min_group_n) {
  d <- factor_table(reviews)
  ggplot(filter(d, n >= min_n), aes(share, group)) +
    geom_linerange(aes(xmin = lo, xmax = hi), colour = srb_col[["muted"]], linewidth = 0.7) +
    geom_point(colour = srb_col[["pos"]], size = 2.8) +
    geom_text(aes(label = paste0("n = ", fmt_num(n))), nudge_y = 0.35, size = 3,
              colour = srb_col[["muted"]]) +
    facet_wrap(~game, nrow = 1) +
    scale_x_continuous(labels = \(x) fmt_pct(x), breaks = seq(0, 1, 0.2),
                       expand = expansion(mult = c(0.08, 0.2))) +
    factors_labs(min_n) +
    theme(panel.grid.major.y = element_blank(), panel.spacing = unit(1.5, "lines"))
}

language_table <- function(reviews, top = 6) {
  langs <- reviews |> count(language, sort = TRUE) |> slice_head(n = top) |> pull(language)
  reviews |>
    filter(language %in% langs) |>
    share_by(game, language)
}

language_names <- c(
  english = "Englisch", schinese = "vereinfachtes Chinesisch", russian = "Russisch",
  brazilian = "brasilianisches Portugiesisch", spanish = "Spanisch", french = "Französisch",
  koreana = "Koreanisch", turkish = "Türkisch", german = "Deutsch", polish = "Polnisch",
  japanese = "Japanisch", latam = "lateinamerikanisches Spanisch", tchinese = "traditionelles Chinesisch",
  ukrainian = "Ukrainisch", italian = "Italienisch"
)

# Beschriftung von plot_languages(), auch für die Handy-Fassung (R/mobile.R)
languages_labs <- function(top) {
  labs(x = "Anteil positiv", y = NULL,
       title = "Der Anteil positiver Reviews unterscheidet sich je nach Sprache",
       subtitle = paste0("Die ", top, " häufigsten Sprachen über alle drei Spiele, mit 95-%-Unsicherheitsintervall"))
}

plot_languages <- function(reviews, top = 6) {
  d <- language_table(reviews, top) |>
    mutate(lang = coalesce(language_names[language], language))
  ggplot(d, aes(share, reorder(lang, share))) +
    geom_linerange(aes(xmin = lo, xmax = hi), colour = srb_col[["muted"]], linewidth = 0.7) +
    geom_point(colour = srb_col[["pos"]], size = 2.8) +
    facet_wrap(~game, nrow = 1) +
    scale_x_continuous(labels = \(x) fmt_pct(x)) +
    languages_labs(top) +
    theme(panel.grid.major.y = element_blank(), panel.spacing = unit(1.5, "lines"))
}

# ---- Kennzahlen für den Fließtext ------------------------------------------

# Review-Bombing-Spitze: Tage, an denen mehr als `factor`-mal so viele Reviews
# geschrieben wurden wie im Median der ersten `baseline_days` Tage des Fensters.
# Liefert auch den Vergleichszeitraum, damit der Text ihn nicht nachrechnen muss.
spike_stats <- function(daily, which_game, factor = spike_factor,
                        baseline_days = spike_baseline_days) {
  d <- filter(daily, game == which_game, filter_offtopic)
  base <- filter(d, date < min(date) + baseline_days)
  baseline <- median(base$n_total)
  spike <- filter(d, n_total > factor * baseline)
  if (nrow(spike) == 0) stop("Keine Review-Spitze für ", which_game, " gefunden.", call. = FALSE)
  list(
    baseline      = baseline,
    base_from     = min(base$date),
    base_to       = max(base$date),
    days          = spike$date,
    n_days        = nrow(spike),
    n_reviews     = sum(spike$n_total),
    n_positive    = sum(spike$n_positive),
    max_day       = spike$date[which.max(spike$n_total)],
    max_n         = max(spike$n_total),
    share_pos     = sum(spike$n_positive) / sum(spike$n_total),
    share_pos_base = sum(base$n_positive) / sum(base$n_total)
  )
}

# Zeitraum, den Steams Off-Topic-Filter komplett ausblendet (Standard liefert 0 Reviews)
offtopic_stats <- function(daily, which_game) {
  w <- daily |>
    filter(game == which_game) |>
    mutate(n_negative = n_total - n_positive) |>
    select(date, filter_offtopic, n_total, n_negative) |>
    pivot_wider(names_from = filter_offtopic, values_from = c(n_total, n_negative))
  hidden <- filter(w, n_total_TRUE == 0, n_total_FALSE > 0)
  if (nrow(hidden) == 0) stop("Keine ausgeblendeten Tage für ", which_game, ".", call. = FALSE)
  list(
    from        = min(hidden$date),
    to          = max(hidden$date),
    n_hidden    = sum(hidden$n_total_FALSE),
    share_neg_hidden = sum(hidden$n_negative_FALSE) / sum(hidden$n_total_FALSE)
  )
}

fmt_date <- function(d) format(as.Date(d), "%d.%m.%Y")
fmt_hours <- function(h) paste0(fmt_num(h), "\u00a0Stunden")

# Grau hinterlegte Tage (z. B. die Review-Bombing-Spitze): ein Rechteck je
# zusammenhängender Folge von Tagen. Als erste Ebene einfügen, dann liegt es unter
# den Daten. Ohne Tage: NULL (fügt nichts hinzu).
shade_layer <- function(days) {
  if (length(days) == 0) return(NULL)
  days <- sort(days)
  runs <- split(days, cumsum(c(1, diff(days) != 1)))
  lapply(unname(runs), function(d) {
    annotate("rect", xmin = min(d) - 0.5, xmax = max(d) + 0.5,
             ymin = -Inf, ymax = Inf, fill = srb_col[["text"]], alpha = 0.08)
  })
}
