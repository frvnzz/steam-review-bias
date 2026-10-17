# Gemeinsame Einstellungen für Abruf, Bereinigung und Seite – die einzige Stelle,
# an der Spiele, Zeitfenster und Schwellenwerte festgelegt sind.
# Verknüpft wird überall über die appid, nicht über den Spielnamen.

# Spiele der Stichprobe (Kapitel 1, 3, 4, 6). Reihenfolge = Reihenfolge in Grafiken.
# category: Beschreibung in der Spieletabelle im Anhang
games <- data.frame(
  appid    = c(1086940L, 1145360L, 553850L),
  slug     = c("bg3", "hades", "helldivers2"),
  name     = c("Baldur's Gate 3", "Hades", "Helldivers 2"),
  category = c("Blockbuster", "Indie-Spiel", "Review-Bombing, zählt in der Gesamtwertung")
)

# Zeitfenster für die täglichen Zählungen (Kapitel 5), jeweils mit und ohne
# Off-Topic-Filter. Borderlands 2 dient nur als Beispiel für ein von Valve
# als „off-topic“ markiertes Review-Bombing.
bombing_windows <- data.frame(
  appid    = c(553850L, 49520L),
  slug     = c("helldivers2", "borderlands2"),
  name     = c("Helldivers 2", "Borderlands 2"),
  start    = as.Date(c("2024-04-01", "2019-03-01")),
  end      = as.Date(c("2024-06-30", "2019-05-31")),
  category = c(NA, "Review-Bombing, als off-topic ausgeblendet (nur Kapitel 5)")
)

game_name <- function(slug) {
  all <- rbind(games[c("slug", "name")], bombing_windows[c("slug", "name")])
  unique(all$name[all$slug == slug])
}

# Gruppen mit weniger Reviews blendet jede Grafik aus (Anteile wären zu unsicher)
min_group_n <- 30

# Review-Bombing-Spitze (Kapitel 5): Tage mit mehr als spike_factor-mal so vielen
# Reviews wie im Median der ersten spike_baseline_days Tage des Zeitfensters
spike_factor <- 10
spike_baseline_days <- 21
