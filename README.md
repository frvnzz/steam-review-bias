# Wer schreibt Reviews – und warum verzerrt das?

Scrollytelling-Webseite über **Selektionsbias in Steam-User-Reviews**: Die Zahl „X % positiv“ beschreibt nicht alle Spieler:innen, sondern nur die, die sich entschieden haben, eine Review zu schreiben.

- **Seite:** <https://frvnzz.github.io/steam-review-bias/>
- **Team:** siehe [`CITATION.cff`](CITATION.cff) (wird auch im Titelblock und im Team-Abschnitt der Seite angezeigt)
- Gruppenassignment Statistik · Sprache: Deutsch

## Aufbau

| Pfad | Inhalt |
|---|---|
| `index.qmd` | die Scrollytelling-Seite (Quarto + [Closeread](https://closeread.dev)) |
| `_quarto.yml` | Projekt- und Format-Konfiguration, `author:`-Block |
| `styles/steam.scss` | eigenes Theme (dunkles Blau, Akzentblau; System-Schriften) |
| `R/fetch_reviews.R` | Abruf über die Steam-API → `data-raw/` (Cache, gitignored) |
| `R/clean_reviews.R` | Bereinigung → `data/` (ohne IDs, ohne Freitext) |
| `R/theme.R` | ggplot2-Theme + Zahlenformate |
| `scripts/sync_authors.R` | prüft/synchronisiert Namen zwischen `CITATION.cff` und `_quarto.yml` |
| `data/` | bereinigte, committbare Daten |
| `_freeze/` | eingefrorene R-Ergebnisse – CI braucht dadurch kein R |
| `_extensions/` | Closeread-Extension |
| `renv.lock` | exakte R-Paketversionen |

## Setup (lokal)

Voraussetzungen: **R ≥ 4.4** und **Quarto ≥ 1.5**. Wir arbeiten in VS Code mit der R- und der Quarto-Extension; RStudio ist nicht nötig.

```sh
git clone https://github.com/frvnzz/steam-review-bias.git
cd steam-review-bias
Rscript -e 'install.packages("renv"); renv::restore()'   # installiert die Pakete aus renv.lock
quarto preview                                           # lokale Vorschau mit Live-Reload
```

> Quarto ohne Admin-Rechte installieren (macOS): das `quarto-<version>-macos.tar.gz` von den [Quarto-Releases](https://github.com/quarto-dev/quarto-cli/releases) nach `~/.local/opt/` entpacken und `~/.local/opt/quarto-<version>/bin/quarto` nach `~/.local/bin/` verlinken.

## Daten neu abrufen

Die Daten stammen ausschließlich vom offiziellen Steam-Endpunkt
[`IUserReviewsService/GetAppReviews`](https://partner.steamgames.com/doc/webapi/IUserReviewsService) (kein API-Key nötig). Der früher dokumentierte Endpunkt `store.steampowered.com/appreviews/<appid>?json=1` ist laut Steamworks-Doku veraltet.

```sh
Rscript R/fetch_reviews.R --test   # Mini-Abruf zum Ausprobieren
Rscript R/fetch_reviews.R          # vollständiger Abruf (dauert, siehe unten)
Rscript R/clean_reviews.R          # data-raw/ → data/
quarto render                      # Seite neu rendern, aktualisiert _freeze/
```

- **Stichprobe:** pro Spiel die neuesten `SRB_MAX_REVIEWS` Reviews (Standard 10 000), z. B. `SRB_MAX_REVIEWS=2000 Rscript R/fetch_reviews.R`.
- **Höflich:** 1,5 s Pause zwischen Requests, Retry mit exponentiellem Backoff bei 429/5xx, eigener User-Agent.
- **Cache:** jede Antwort liegt unter `data-raw/cache/<md5 der Parameter>.json`. Wiederholte Läufe senden keine Requests. Für frische Daten den Ordner löschen.
- **Review-Bombing:** Steam blendet als „off-topic“ markierte Reviews standardmäßig aus. Mit `filter_offtopic_activity: false` werden sie mitgeliefert. Für die Zeitfenster in Kapitel 5 zählen wir beide Varianten.

### Datenschutz

Das Repo ist öffentlich. Rohdaten mit `steamid` und Review-Texten liegen nur in `data-raw/` (gitignored). In `data/` landen nur Spalten ohne Identifier und ohne Freitext, Zeitstempel sind auf den Tag gerundet. Secrets (z. B. ein optionaler `STEAM_KEY`) gehören in `.Renviron`, das ebenfalls gitignored ist.

## Namen der Gruppenmitglieder

`CITATION.cff` ist die Quelle der Wahrheit. Nach Änderungen:

```sh
Rscript scripts/sync_authors.R --fix   # schreibt den author-Block in _quarto.yml neu
```

Beim Rendern prüft `index.qmd` die Konsistenz und bricht bei Abweichungen ab.

## Deployment (GitHub Pages)

Einmalig: im Repo unter **Settings → Pages → Build and deployment → Source** die Option **GitHub Actions** wählen.

Danach baut [`.github/workflows/publish.yml`](.github/workflows/publish.yml) bei jedem Push auf `main` die Seite mit `quarto-dev/quarto-actions` und veröffentlicht sie. Weil `execute: freeze: auto` gesetzt ist, läuft in der CI kein R. **Wichtig:** Vor dem Push lokal `quarto render` ausführen und `_freeze/` mit committen.
