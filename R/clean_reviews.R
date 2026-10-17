# Bereinigung: data-raw/ (gitignored, mit steamid + Texten) → data/ (committbar)
#
# Entfernt werden: steamid, recommendationid, Review-Text, Antwort der Entwickler:innen
# und exakte Zeitstempel (auf den Tag gerundet). Übrig bleiben nur Merkmale,
# die für die Analyse gebraucht werden – jede weitere Spalte macht es leichter,
# eine Zeile der öffentlichen Review (und damit dem Konto) zuzuordnen.
#
# Aufruf: Rscript R/clean_reviews.R

library(dplyr)
library(purrr)
library(readr)

source("R/fetch_reviews.R")   # für `games` und `bombing_windows`

as_date_utc <- function(ts) as.Date(as.POSIXct(ts, origin = "1970-01-01", tz = "UTC"))

# Fehlt ein Ja/Nein-Feld, bleibt es NA statt still zu FALSE zu werden
# (ein fehlendes voted_up wäre sonst eine „negative“ Review); main() prüft das.
flag <- function(x) if (is.null(x)) NA else as.logical(x)

flatten_review <- function(r) {
  a <- r$author
  tibble(
    recommendationid        = r$recommendationid,   # nur zum Deduplizieren, wird entfernt
    language                = r$language,
    created                 = as_date_utc(r$timestamp_created),
    voted_up                = flag(r$voted_up),
    steam_purchase          = flag(r$steam_purchase),
    received_for_free       = flag(r$received_for_free),
    early_access            = flag(r$written_during_early_access),
    primarily_steam_deck    = flag(r$primarily_steam_deck),
    refunded                = flag(r$refunded),
    author_num_reviews      = a$num_reviews %||% NA_integer_,
    playtime_at_review_min  = a$playtime_at_review %||% NA_integer_,
    playtime_forever_min    = a$playtime_forever %||% NA_integer_
  )
}

clean_game <- function(appid) {
  path <- file.path("data-raw", sprintf("reviews_%d.rds", appid))
  pages <- readRDS(path)
  fetched <- range(as.POSIXct(map_chr(pages, "fetched_at"), format = "%Y-%m-%dT%H:%M:%S%z"))

  reviews <- pages |>
    map(\(p) p$response$reviews) |>
    list_flatten() |>
    map(flatten_review) |>
    list_rbind() |>
    distinct(recommendationid, .keep_all = TRUE) |>
    select(-recommendationid) |>
    mutate(appid = appid, .before = 1)

  attr(reviews, "fetched") <- fetched
  reviews
}

main <- function() {
  if (!file.exists("_quarto.yml")) stop("Bitte im Projektordner ausführen.", call. = FALSE)
  dir.create("data", showWarnings = FALSE)

  reviews <- map(games$appid, clean_game)
  fetched <- do.call(c, map(reviews, attr, "fetched"))
  reviews <- list_rbind(reviews) |>
    left_join(select(games, appid, game = name), by = "appid") |>
    relocate(game, .after = appid)
  flags <- c("voted_up", "steam_purchase", "received_for_free", "early_access",
             "primarily_steam_deck", "refunded")
  missing <- colSums(is.na(reviews[flags]))
  if (any(missing > 0)) {
    stop("Fehlende Ja/Nein-Werte in den Rohdaten: ",
         paste(names(missing)[missing > 0], collapse = ", "), call. = FALSE)
  }
  write_csv(reviews, "data/reviews_sample.csv")

  summaries <- readRDS(file.path("data-raw", "summaries.rds")) |>
    select(-any_of("fetched_at")) |>   # ältere Abrufe: Zeitstempel mit lokaler Zeitzone
    left_join(select(games, appid, game = name, role), by = "appid")
  write_csv(summaries, "data/summaries.csv")

  daily <- readRDS(file.path("data-raw", "daily_counts.rds")) |>
    left_join(select(bombing_windows, appid, game = name), by = "appid") |>
    as_tibble() |>
    relocate(appid, game)
  write_csv(daily, "data/daily_counts.csv")

  # Abrufzeitpunkt für die Quellenangaben auf der Seite. (Die Stichprobengröße
  # zählt die Seite selbst aus reviews_sample.csv: SRB_MAX_REVIEWS gilt beim Abruf,
  # nicht unbedingt beim Bereinigen, und ein Spiel kann weniger Reviews haben.)
  meta <- tibble(
    key = c("fetched_from", "fetched_to"),
    value = c(format(min(fetched), "%Y-%m-%d"), format(max(fetched), "%Y-%m-%d"))
  )
  write_csv(meta, "data/meta.csv")

  message(sprintf("data/reviews_sample.csv: %d Zeilen", nrow(reviews)))
  message(sprintf("data/daily_counts.csv:   %d Zeilen", nrow(daily)))
}

if (sys.nframe() == 0L) main()
