# Kapitel 2: Spielzeug-Simulation mit bekannter „Wahrheit“.
# Kein echter Steam-Datensatz – hier legen WIR fest, wie zufrieden alle sind,
# und sehen dann, was eine Review-Statistik daraus macht.
#
# Statt zufälliger Ziehungen verwenden wir Erwartungswerte (exakte Anteile),
# damit Grafik und Formel genau übereinstimmen.

library(dplyr)
library(ggplot2)

sim_params <- list(
  n          = 400,    # Spieler:innen in der Punktewolke (20 × 20)
  p_happy    = 0.70,   # wahrer Anteil Zufriedener
  rate_low   = 0.10,   # Review-Wahrscheinlichkeit der „ruhigeren“ Gruppe
  rate_high  = 0.30    # Review-Wahrscheinlichkeit der „lauteren“ Gruppe (3×)
)

# Anteil positiver Reviews, wenn Zufriedene mit r_happy und Unzufriedene mit
# r_unhappy eine Review schreiben (Satz von Bayes):
#   P(positiv | Review) = p·r_happy / (p·r_happy + (1−p)·r_unhappy)
observed_share <- function(p, r_happy, r_unhappy) {
  p * r_happy / (p * r_happy + (1 - p) * r_unhappy)
}

make_population <- function(prm = sim_params, seed = 42) {
  side <- sqrt(prm$n)
  n_happy <- round(prm$n * prm$p_happy)
  set.seed(seed)
  pop <- tibble(
    x = rep(seq_len(side), times = side),
    y = rep(seq_len(side), each = side),
    happy = sample(rep(c(TRUE, FALSE), c(n_happy, prm$n - n_happy)))
  )
  # Szenario A: Unzufriedene schreiben 3× so oft → genau die erwartete Anzahl markieren
  n_rev_happy   <- round(n_happy * prm$rate_low)
  n_rev_unhappy <- round((prm$n - n_happy) * prm$rate_high)
  pop |>
    group_by(happy) |>
    mutate(reviews = row_number(runif(n())) <= if_else(happy, n_rev_happy, n_rev_unhappy)) |>
    ungroup() |>
    mutate(vote = factor(if_else(happy, "positiv", "negativ"), levels = c("positiv", "negativ")))
}

sim_summary <- function(prm = sim_params) {
  pop <- make_population(prm)
  rev <- filter(pop, reviews)
  list(
    pop          = pop,
    n            = nrow(pop),
    n_happy      = sum(pop$happy),
    n_reviewers  = nrow(rev),
    n_rev_happy  = sum(rev$happy),
    share_true   = mean(pop$happy),
    share_A      = mean(rev$happy),   # Unzufriedene schreiben 3× so oft
    share_B      = observed_share(prm$p_happy, prm$rate_high, prm$rate_low),  # umgekehrt
    ratio        = prm$rate_high / prm$rate_low,
    prm          = prm
  )
}

plot_population <- function(s, highlight_reviewers = FALSE) {
  pop <- s$pop
  p <- ggplot(pop, aes(x, y, fill = vote))
  if (highlight_reviewers) {
    p <- p +
      geom_point(data = filter(pop, !reviews), shape = 21, size = 3.2, stroke = 0,
                 alpha = 0.18) +
      geom_point(data = filter(pop, reviews), shape = 21, size = 4.6, stroke = 1.1,
                 colour = srb_col[["bg"]])
  } else {
    p <- p + geom_point(shape = 21, size = 4.2, stroke = 0.6, colour = srb_col[["bg"]])
  }
  p +
    scale_fill_vote(labels = c(positiv = "zufrieden", negativ = "unzufrieden")) +
    coord_equal(clip = "off") +
    theme_void(base_size = 13) +
    theme(
      plot.background = element_rect(fill = "transparent", colour = NA),
      legend.position = "top",
      legend.text = element_text(colour = srb_col[["text"]], size = 12),
      legend.title = element_blank(),
      plot.margin = margin(10, 10, 10, 10)
    ) +
    guides(fill = guide_legend(override.aes = list(size = 5, alpha = 1)))
}

# stacked: Handy-Fassung – Beschriftung über jedem Balken statt links daneben,
# damit die Balken die volle Breite bekommen
plot_comparison <- function(s, stacked = FALSE) {
  d <- tibble(
    what = c(
      "Alle Spieler:innen\n(die Wahrheit)",
      sprintf("Reviews, wenn Unzufriedene\n%s× so oft schreiben", fmt_num(s$ratio)),
      sprintf("Reviews, wenn Zufriedene\n%s× so oft schreiben", fmt_num(s$ratio))
    ),
    share = c(s$share_true, s$share_A, s$share_B),
    truth = c(TRUE, FALSE, FALSE)
  ) |>
    mutate(what = factor(what, levels = rev(what)))

  p <- ggplot(d, aes(share, what)) +
    # Linie hinter den Balken: sichtbar zwischen ihnen, kreuzt aber keine Beschriftung
    geom_vline(xintercept = s$share_true, colour = srb_col[["text"]],
               linetype = "22", linewidth = 0.5) +
    geom_col(aes(fill = truth), width = 0.62) +
    # Beschriftung innen am Balkenende: kollidiert so nie mit der gestrichelten Linie
    geom_text(aes(label = fmt_pct(share)), hjust = 1.15, size = 5.2, fontface = "bold",
              colour = "#ffffff") +
    scale_fill_manual(values = c(`TRUE` = srb_col[["grey"]], `FALSE` = srb_col[["pos"]]),
                      guide = "none") +
    scale_x_continuous(labels = \(x) fmt_pct(x), limits = c(0, 1),
                       breaks = seq(0, 1, 0.25), expand = expansion(mult = c(0, 0.04))) +
    labs(x = "Anteil zufrieden bzw. positiv", y = NULL,
         title = "Dieselben Spieler:innen, drei verschiedene Zahlen",
         subtitle = "Die gestrichelte Linie ist der wahre Anteil Zufriedener") +
    theme(panel.grid.major.y = element_blank(),
          axis.text.y = element_text(colour = srb_col[["text"]], size = rel(1 / 0.85), hjust = 0))   # = Grundschrift (axis.text ist 0,85)
  if (stacked) {
    p <- p +
      facet_wrap(~ factor(what, levels = rev(levels(d$what))), ncol = 1, scales = "free_y") +
      theme(axis.text.y = element_blank(),
            plot.margin = margin(4, 20, 4, 14),   # links Platz für „0 %“ (Balken starten am Rand)
            strip.text = element_text(colour = srb_col[["text"]], hjust = 0, size = rel(0.95),
                                      lineheight = 1.1, margin = margin(t = 6, b = 4)))
  }
  p
}
