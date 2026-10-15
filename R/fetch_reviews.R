# Abruf der Steam-User-Reviews über den offiziellen Endpunkt
#   GET https://api.steampowered.com/IUserReviewsService/GetAppReviews/v1/
#   Doku: https://partner.steamgames.com/doc/webapi/IUserReviewsService
# (Nachfolger des veralteten store.steampowered.com/appreviews/<appid>?json=1)
#
# Alle Rohantworten werden unter data-raw/cache/ gespeichert (gitignored, enthalten
# steamid + Review-Texte). Ein erneuter Lauf liest aus dem Cache und schickt keine
# Requests – zum Aktualisieren den Cache-Ordner löschen.
#
# Aufruf:
#   Rscript R/fetch_reviews.R            # alles laut Konfiguration unten
#   Rscript R/fetch_reviews.R --test     # Mini-Abruf (2 Seiten à 20 Reviews, 3 Tage)

library(httr2)
library(jsonlite)

# ---- Konfiguration ----------------------------------------------------------

games <- data.frame(
  appid = c(1086940L, 1145360L, 553850L),
  slug  = c("bg3", "hades", "helldivers2"),
  name  = c("Baldur's Gate 3", "Hades", "Helldivers 2"),
  role  = c("Blockbuster", "Indie", "Review-Bombing")
)

# Obergrenze pro Spiel: die neuesten N Reviews (filter = Recent).
# Überschreibbar per Umgebungsvariable, z. B. SRB_MAX_REVIEWS=2000.
max_reviews <- as.integer(Sys.getenv("SRB_MAX_REVIEWS", "10000"))

# Zeitfenster für die täglichen Zählungen (Kapitel 5), jeweils mit und ohne
# Off-Topic-Filter. Borderlands 2 dient nur als Beispiel für ein von Valve
# als „off-topic“ markiertes Review-Bombing.
bombing_windows <- data.frame(
  appid = c(553850L, 49520L),
  slug  = c("helldivers2", "borderlands2"),
  name  = c("Helldivers 2", "Borderlands 2"),
  start = as.Date(c("2024-04-01", "2019-03-01")),
  end   = as.Date(c("2024-06-30", "2019-05-31"))
)

endpoint   <- "https://api.steampowered.com/IUserReviewsService/GetAppReviews/v1/"
user_agent <- "steam-review-bias/0.1 (Statistik-Gruppenprojekt; https://github.com/frvnzz/steam-review-bias)"
pause_s    <- 1.5   # Pause zwischen zwei echten Requests
cache_dir  <- file.path("data-raw", "cache")

# ---- Ein einzelner (gecachter) Request --------------------------------------

# Enum-Werte laut Doku
FILTER_RECENT      <- 1L
REVIEW_TYPE_ALL    <- 0L
REVIEW_TYPE_POS    <- 1L
PURCHASE_TYPE_ALL  <- 1L

get_reviews_page <- function(params) {
  input_json <- as.character(toJSON(params, auto_unbox = TRUE))
  key <- as.character(openssl::md5(input_json))
  path <- file.path(cache_dir, paste0(key, ".json"))

  if (file.exists(path)) {
    return(fromJSON(path, simplifyVector = FALSE))
  }

  Sys.sleep(pause_s)
  resp <- request(endpoint) |>
    req_url_query(input_json = input_json) |>
    req_user_agent(user_agent) |>
    req_timeout(60) |>
    req_retry(
      max_tries = 6,
      is_transient = \(r) resp_status(r) %in% c(429, 500, 502, 503, 504),
      backoff = \(i) min(120, 5 * 2^(i - 1))   # 5, 10, 20, 40, 80 s
    ) |>
    req_perform()

  body <- resp_body_json(resp)
  out <- list(
    request    = params,
    fetched_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    response   = body$response
  )
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(as.character(toJSON(out, auto_unbox = TRUE, null = "null", digits = NA)), path,
             useBytes = TRUE)
  out
}

# ---- Neueste N Reviews eines Spiels (Cursor-Paging) -------------------------

fetch_recent_reviews <- function(appid, max_n = max_reviews, per_page = 100L,
                                 filter_offtopic = TRUE) {
  cursor <- "*"
  seen_cursors <- character()
  pages <- list()
  n <- 0L

  while (n < max_n) {
    page <- get_reviews_page(list(
      appid = appid,
      filter = FILTER_RECENT,
      languages = list("all"),
      review_type = REVIEW_TYPE_ALL,
      purchase_type = PURCHASE_TYPE_ALL,
      num_per_page = min(per_page, max_n - n),
      filter_offtopic_activity = filter_offtopic,
      cursor = cursor
    ))
    reviews <- page$response$reviews
    if (length(reviews) == 0) break

    pages[[length(pages) + 1]] <- page
    n <- n + length(reviews)
    seen_cursors <- c(seen_cursors, cursor)
    cursor <- page$response$cursor
    if (is.null(cursor) || cursor %in% seen_cursors) break

    if (length(pages) %% 10 == 0) message(sprintf("  app %d: %d Reviews", appid, n))
  }
  message(sprintf("  app %d: fertig, %d Reviews auf %d Seiten", appid, n, length(pages)))
  invisible(pages)
}

# ---- Tägliche Zählungen über total_matching ---------------------------------

day_start_utc <- function(d) as.integer(as.POSIXct(paste(d, "00:00:00"), tz = "UTC"))

count_reviews <- function(appid, from, to, review_type, filter_offtopic) {
  page <- get_reviews_page(list(
    appid = appid,
    filter = FILTER_RECENT,
    languages = list("all"),
    review_type = review_type,
    purchase_type = PURCHASE_TYPE_ALL,
    num_per_page = 1L,
    filter_offtopic_activity = filter_offtopic,
    date_range_start = from,
    date_range_end = to
  ))
  page$response$total_matching %||% 0L
}

fetch_daily_counts <- function(appid, start, end) {
  days <- seq(start, end, by = "day")
  grid <- expand.grid(date = days, filter_offtopic = c(TRUE, FALSE),
                      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  grid$n_total <- NA_integer_
  grid$n_positive <- NA_integer_

  for (i in seq_len(nrow(grid))) {
    from <- day_start_utc(grid$date[i])
    to   <- day_start_utc(grid$date[i] + 1) - 1L
    grid$n_total[i]    <- count_reviews(appid, from, to, REVIEW_TYPE_ALL, grid$filter_offtopic[i])
    grid$n_positive[i] <- count_reviews(appid, from, to, REVIEW_TYPE_POS, grid$filter_offtopic[i])
    if (i %% 20 == 0) message(sprintf("  app %d: %d/%d Tage", appid, i, nrow(grid)))
  }
  grid$appid <- appid
  grid
}

# ---- Gesamtzahlen (query_summary, nur auf der ersten Seite) ------------------

fetch_summary <- function(appid) {
  page <- get_reviews_page(list(
    appid = appid,
    filter = FILTER_RECENT,
    languages = list("all"),
    review_type = REVIEW_TYPE_ALL,
    purchase_type = PURCHASE_TYPE_ALL,
    num_per_page = 1L,
    cursor = "*"
  ))
  s <- page$response$query_summary
  data.frame(appid = appid, fetched_at = page$fetched_at,
             review_score_desc = s$review_score_desc,
             total_positive = s$total_positive, total_negative = s$total_negative,
             total_reviews = s$total_reviews)
}

# ---- Hauptprogramm ----------------------------------------------------------

main <- function(test = FALSE) {
  dir.create("data-raw", showWarnings = FALSE)

  if (test) {
    message("Testmodus: kleiner Abruf")
    fetch_recent_reviews(games$appid[1], max_n = 40L, per_page = 20L)
    print(fetch_summary(games$appid[1]))
    print(fetch_daily_counts(553850L, as.Date("2024-05-03"), as.Date("2024-05-05")))
    return(invisible())
  }

  message("1/3 Gesamtzahlen")
  summaries <- do.call(rbind, lapply(games$appid, fetch_summary))
  saveRDS(summaries, file.path("data-raw", "summaries.rds"))

  message("2/3 Neueste ", max_reviews, " Reviews pro Spiel")
  for (id in games$appid) {
    pages <- fetch_recent_reviews(id)
    saveRDS(pages, file.path("data-raw", sprintf("reviews_%d.rds", id)))
  }

  message("3/3 Tägliche Zählungen in den Review-Bombing-Fenstern")
  daily <- do.call(rbind, lapply(seq_len(nrow(bombing_windows)), function(i) {
    w <- bombing_windows[i, ]
    fetch_daily_counts(w$appid, w$start, w$end)
  }))
  saveRDS(daily, file.path("data-raw", "daily_counts.rds"))

  message("Fertig. Weiter mit: Rscript R/clean_reviews.R")
}

if (sys.nframe() == 0L) {
  main(test = "--test" %in% commandArgs(trailingOnly = TRUE))
}
