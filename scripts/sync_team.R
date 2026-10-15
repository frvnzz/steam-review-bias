# Überträgt die Gruppenmitglieder aus _team.yml (einzige Quelle) nach CITATION.cff.
#
#   Rscript scripts/sync_team.R          # CITATION.cff aktualisieren
#   Rscript scripts/sync_team.R --check  # nur prüfen (Exit-Code 1 bei Abweichung)
#
# Titelblock und Footer der Seite lesen _team.yml direkt – dafür ist
# kein Sync nötig. Beim Rendern prüft der Shortcode „team“ zusätzlich, ob
# CITATION.cff noch passt.

team_to_cff <- function(author) {
  n <- author$name
  person <- if (is.list(n)) {
    list(`given-names` = n$given, `family-names` = n$family)
  } else {
    list(name = n)
  }
  if (!is.null(author$email)) person$email <- author$email
  if (!is.null(author$orcid)) {
    person$orcid <- if (grepl("^https://", author$orcid)) author$orcid
                    else paste0("https://orcid.org/", author$orcid)
  }
  aff <- author$affiliations %||% author$affiliation
  if (!is.null(aff)) {
    first <- if (is.list(aff)) aff[[1]] else aff
    person$affiliation <- if (is.list(first)) first$name else first
  }
  person
}

main <- function(check_only = FALSE) {
  team <- yaml::read_yaml("_team.yml")$author
  if (length(team) == 0) stop("_team.yml enthält keine Einträge unter 'author:'.", call. = FALSE)
  authors <- lapply(team, team_to_cff)

  cff <- yaml::read_yaml("CITATION.cff")
  if (identical(cff$authors, authors)) {
    message("OK: CITATION.cff ist aktuell (", length(authors), " Personen).")
  } else if (check_only) {
    message("CITATION.cff passt nicht zu _team.yml – ausführen: Rscript scripts/sync_team.R")
    quit(status = 1)
  } else {
    cff$authors <- authors
    yaml::write_yaml(cff, "CITATION.cff", indent.mapping.sequence = TRUE)
    message("CITATION.cff aktualisiert (", length(authors), " Personen).")
  }

  placeholders <- grepl("[", vapply(team, \(a) paste(unlist(a$name), collapse = " "), ""), fixed = TRUE)
  if (any(placeholders)) {
    message("Hinweis: ", sum(placeholders), " Eintrag/Einträge sind noch Platzhalter.")
  }
}

if (sys.nframe() == 0L) main(check_only = "--check" %in% commandArgs(trailingOnly = TRUE))
