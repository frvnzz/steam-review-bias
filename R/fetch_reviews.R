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
#   Rscript R/fetch_reviews.R            # alles laut R/config.R und Konfiguration unten
#   Rscript R/fetch_reviews.R --test     # Mini-Abruf (2 Seiten à 20 Reviews, 3 Tage)

library(httr2)
library(jsonlite)

source("R/config.R")   # games, bombing_windows

# ---- Konfiguration ----------------------------------------------------------

# Obergrenze pro Spiel: die neuesten N Reviews (filter = Recent).
# Überschreibbar per Umgebungsvariable, z. B. SRB_MAX_REVIEWS=2000.
max_reviews <- suppressWarnings(as.integer(Sys.getenv("SRB_MAX_REVIEWS", "10000")))
if (is.na(max_reviews) || max_reviews < 1) {
  stop("SRB_MAX_REVIEWS muss eine positive ganze Zahl sein.", call. = FALSE)
}

endpoint   <- "https://api.steampowered.com/IUserReviewsService/GetAppReviews/v1/"
user_agent <- "steam-review-bias/0.1 (Statistik-Gruppenprojekt; https://github.com/frvnzz/steam-review-bias)"
pause_s    <- 1.5   # Pause zwischen zwei echten Requests
cache_dir  <- file.path("data-raw", "cache")

# ---- Ein einzelner (gecachter) Request --------------------------------------

# Enum-Werte laut Doku
FILTER_RECENT      <- 1L
REVIEW_TYPE_ALL    <- 0L
REVIEW_TYPE_POS    <- 1L
PURCHASE_TYPE_STEAM <- 0L   # nur Käufe über Steam: Grundlage von Steams angezeigter Wertung
PURCHASE_TYPE_ALL  <- 1L    # alle Kaufarten (auch aktivierte Keys)

# keep_reviews = FALSE: Für Zählungen braucht es nur die Summen. Die Schnittstelle
# liefert trotzdem mindestens eine Review mit (inkl. Text und steamid); die wird
# vor dem Speichern entfernt.
get_reviews_page <- function(params, keep_reviews = TRUE) {
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
      retry_on_failure = TRUE,   # auch Timeouts und Verbindungsabbrüche
      backoff = \(i) min(120, 5 * 2^(i - 1))   # 5, 10, 20, 40, 80 s
    ) |>
    req_perform()

  body <- resp_body_json(resp)
  # Leere oder fehlerhafte Antworten nicht cachen: Sonst würde jeder spätere Lauf
  # sie wiederverwenden, und aus einem Fehler würde still eine 0
  if (!is.list(body$response) || length(body$response) == 0) {
    stop("Leere Antwort von Steam für ", input_json, call. = FALSE)
  }
  if (!keep_reviews) body$response$reviews <- NULL
  out <- list(
    request    = params,
    fetched_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    response   = body$response
  )
  # erst in eine Temp-Datei, dann umbenennen: Ein Abbruch mitten im Schreiben
  # hinterlässt so keine halbe JSON-Datei, an der jeder weitere Lauf scheitert
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(tmpdir = cache_dir, fileext = ".tmp")
  writeLines(as.character(toJSON(out, auto_unbox = TRUE, null = "null", digits = NA)), tmp,
             useBytes = TRUE)
  file.rename(tmp, path)
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

# Alle und positive Reviews eines Zeitraums aus einer einzigen Anfrage:
# total_matching zählt alle Treffer, query_summary$total_positive die positiven
count_reviews <- function(appid, from, to, filter_offtopic) {
  page <- get_reviews_page(list(
    appid = appid,
    filter = FILTER_RECENT,
    languages = list("all"),
    review_type = REVIEW_TYPE_ALL,
    purchase_type = PURCHASE_TYPE_ALL,
    num_per_page = 1L,
    filter_offtopic_activity = filter_offtopic,
    date_range_start = from,
    date_range_end = to
  ), keep_reviews = FALSE)
  n <- c(total = page$response$total_matching,
         positive = page$response$query_summary$total_positive)
  if (length(n) != 2 || !is.numeric(n)) {
    stop("Antwort ohne total_matching/total_positive für app ", appid, call. = FALSE)
  }
  n
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
    n <- count_reviews(appid, from, to, grid$filter_offtopic[i])
    grid$n_total[i]    <- n[["total"]]
    grid$n_positive[i] <- n[["positive"]]
    if (i %% 20 == 0) message(sprintf("  app %d: %d/%d Tage", appid, i, nrow(grid)))
  }
  grid$appid <- appid
  grid
}

# ---- Gesamtzahlen (query_summary, nur auf der ersten Seite) ------------------

query_summary <- function(appid, purchase_type) {
  page <- get_reviews_page(list(
    appid = appid,
    filter = FILTER_RECENT,
    languages = list("all"),
    review_type = REVIEW_TYPE_ALL,
    purchase_type = purchase_type,
    num_per_page = 1L,
    cursor = "*"
  ), keep_reviews = FALSE)
  qs <- page$response$query_summary
  if (!is.numeric(qs$total_reviews)) {
    stop("Antwort ohne query_summary für app ", appid, call. = FALSE)
  }
  qs
}

# Steams Wertung im Shop zählt laut Steamworks-Doku nur Reviews von Steam-Käufen
# (keine Key-Aktivierungen). Stichprobe und Tageswerte enthalten dagegen alle
# Kaufarten – für Vergleiche damit gibt es die Gesamtzahlen zusätzlich als *_all.
fetch_summary <- function(appid) {
  st <- query_summary(appid, PURCHASE_TYPE_STEAM)
  al <- query_summary(appid, PURCHASE_TYPE_ALL)
  data.frame(appid = appid,
             review_score_desc = st$review_score_desc,
             total_positive = st$total_positive, total_negative = st$total_negative,
             total_reviews = st$total_reviews,
             total_positive_all = al$total_positive, total_negative_all = al$total_negative,
             total_reviews_all = al$total_reviews)
}

# ---- Hauptprogramm ----------------------------------------------------------

main <- function(test = FALSE) {
  if (!file.exists("_quarto.yml")) stop("Bitte im Projektordner ausführen.", call. = FALSE)
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
  args <- commandArgs(trailingOnly = TRUE)
  # Tippfehler wie --tset sollen nicht den vollständigen Abruf starten
  unknown <- setdiff(args, "--test")
  if (length(unknown)) stop("Unbekannte Option: ", paste(unknown, collapse = " "), call. = FALSE)
  main(test = "--test" %in% args)
}
