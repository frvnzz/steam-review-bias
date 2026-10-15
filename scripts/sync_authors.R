# Hält die Namen in CITATION.cff (Quelle der Wahrheit) und _quarto.yml synchron.
#
#   Rscript scripts/sync_authors.R          # nur prüfen (Exit-Code 1 bei Abweichung)
#   Rscript scripts/sync_authors.R --fix    # author-Block in _quarto.yml neu schreiben
#
# index.qmd ruft check_authors() ebenfalls beim Rendern auf.

cff_authors <- function(path = "CITATION.cff") {
  cff <- yaml::read_yaml(path)
  vapply(cff$authors, function(a) {
    if (!is.null(a$name)) a$name else paste(a$`given-names`, a$`family-names`)
  }, character(1))
}

quarto_authors <- function(path = "_quarto.yml") {
  q <- yaml::read_yaml(path)
  vapply(q$author, function(a) if (is.list(a)) a$name else a, character(1))
}

check_authors <- function(cff = "CITATION.cff", quarto = "_quarto.yml") {
  a <- cff_authors(cff)
  b <- quarto_authors(quarto)
  if (!identical(a, b)) {
    stop(
      "Autor:innen in CITATION.cff und _quarto.yml stimmen nicht überein.\n",
      "  CITATION.cff: ", paste(a, collapse = ", "), "\n",
      "  _quarto.yml:  ", paste(b, collapse = ", "), "\n",
      "Beheben mit: Rscript scripts/sync_authors.R --fix",
      call. = FALSE
    )
  }
  invisible(a)
}

fix_authors <- function(cff = "CITATION.cff", quarto = "_quarto.yml") {
  lines <- readLines(quarto, encoding = "UTF-8")
  start <- grep("^# >>> authors", lines)
  end <- grep("^# <<< authors", lines)
  stopifnot(length(start) == 1, length(end) == 1, start < end)
  block <- c("author:", sprintf('  - name: "%s"', cff_authors(cff)))
  writeLines(c(lines[seq_len(start)], block, lines[end:length(lines)]), quarto, useBytes = TRUE)
  message("author-Block in ", quarto, " aktualisiert.")
}

if (sys.nframe() == 0L) {
  if ("--fix" %in% commandArgs(trailingOnly = TRUE)) fix_authors()
  check_authors()
  message("OK: Namen in CITATION.cff und _quarto.yml sind konsistent.")
}
