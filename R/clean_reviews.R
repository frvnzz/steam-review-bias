# Bereinigung: data-raw/ (gitignored, mit steamid + Texten) → data/ (committbar)
#
# Entfernt werden: steamid, recommendationid, Review-Text, Antwort der Entwickler:innen
# und exakte Zeitstempel (auf den Tag gerundet). Übrig bleiben nur Merkmale,
# die für die Analyse gebraucht werden.
#
# Aufruf: Rscript R/clean_reviews.R

library(dplyr)
library(purrr)
library(readr)

source("R/fetch_reviews.R")   # für `games` und `bombing_windows`

as_date_utc <- function(ts) as.Date(as.POSIXct(ts, origin = "1970-01-01", tz = "UTC"))

flatten_review <- function(r) {
  a <- r$author
  tibble(
    recommendationid        = r$recommendationid,   # nur zum Deduplizieren, wird entfernt
    language                = r$language,
    created                 = as_date_utc(r$timestamp_created),
    updated                 = as_date_utc(r$timestamp_updated),
    voted_up                = isTRUE(r$voted_up),
    votes_up                = r$votes_up %||% NA_integer_,
    votes_funny             = r$votes_funny %||% NA_integer_,
    comment_count           = r$comment_count %||% NA_integer_,
    steam_purchase          = isTRUE(r$steam_purchase),
    received_for_free       = isTRUE(r$received_for_free),
    early_access            = isTRUE(r$written_during_early_access),
    primarily_steam_deck    = isTRUE(r$primarily_steam_deck),
    refunded                = isTRUE(r$refunded),
    has_dev_response        = !is.null(r$developer_response),
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
  dir.create("data", showWarnings = FALSE)

  reviews <- map(games$appid, clean_game)
  fetched <- do.call(c, map(reviews, attr, "fetched"))
  reviews <- list_rbind(reviews) |>
    left_join(select(games, appid, game = name), by = "appid") |>
    relocate(game, .after = appid)
  write_csv(reviews, "data/reviews_sample.csv")

  summaries <- readRDS(file.path("data-raw", "summaries.rds")) |>
    left_join(select(games, appid, game = name, role), by = "appid")
  write_csv(summaries, "data/summaries.csv")

  daily <- readRDS(file.path("data-raw", "daily_counts.rds")) |>
    left_join(select(bombing_windows, appid, game = name), by = "appid") |>
    as_tibble() |>
    relocate(appid, game)
  write_csv(daily, "data/daily_counts.csv")

  # Abrufzeitpunkt für die Quellenangaben auf der Seite
  meta <- tibble(
    key = c("fetched_from", "fetched_to", "max_reviews"),
    value = c(format(min(fetched), "%Y-%m-%d"), format(max(fetched), "%Y-%m-%d"),
              as.character(max_reviews))
  )
  write_csv(meta, "data/meta.csv")

  message(sprintf("data/reviews_sample.csv: %d Zeilen", nrow(reviews)))
  message(sprintf("data/daily_counts.csv:   %d Zeilen", nrow(daily)))
}

if (sys.nframe() == 0L) main()
