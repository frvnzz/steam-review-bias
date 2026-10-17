# Wer schreibt Reviews – und warum verzerrt das?

Scrollytelling-Webseite über **Selektionsbias in Steam-User-Reviews**: Die Zahl „X % positiv“ beschreibt nicht alle Spieler:innen, sondern nur die, die sich entschieden haben, eine Review zu schreiben.

- **Seite:** <https://frvnzz.github.io/steam-review-bias/>
- **Team:** siehe [`_team.yml`](_team.yml) – die einzige Stelle, an der Namen eingetragen werden
- Gruppenassignment Statistik · Sprache: Deutsch

## Aufbau

| Pfad | Inhalt |
|---|---|
| `index.qmd` | die Scrollytelling-Seite (Quarto + [Closeread](https://closeread.dev)) |
| `_quarto.yml` | Projekt- und Format-Konfiguration |
| `_team.yml` | Gruppenmitglieder (einzige Quelle für alle Namen) |
| `styles/steam.scss` | eigenes Theme (dunkles Blau, Akzentblau; System-Schriften) |
| `R/fetch_reviews.R` | Abruf über die Steam-API → `data-raw/` (Cache, gitignored) |
| `R/clean_reviews.R` | Bereinigung → `data/` (ohne IDs, ohne Freitext) |
| `R/theme.R` | ggplot2-Theme + Zahlenformate |
| `scripts/sync_team.R` | überträgt die Namen aus `_team.yml` nach `CITATION.cff` |
| `shortcodes/team.lua` | Namen im Footer + Konsistenz-Check gegen `CITATION.cff` |
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
quarto render index.qmd            # R neu ausführen, aktualisiert _freeze/
```

- **Stichprobe:** pro Spiel die neuesten `SRB_MAX_REVIEWS` Reviews (Standard 10 000), z. B. `SRB_MAX_REVIEWS=2000 Rscript R/fetch_reviews.R`.
- **Höflich:** 1,5 s Pause zwischen Requests, Retry mit exponentiellem Backoff bei 429/5xx, eigener User-Agent.
- **Cache:** jede Antwort liegt unter `data-raw/cache/<md5 der Parameter>.json`. Wiederholte Läufe senden keine Requests. Für frische Daten den Ordner löschen.
- **Review-Bombing:** Steam blendet als „off-topic“ markierte Reviews standardmäßig aus. Mit `filter_offtopic_activity: false` werden sie mitgeliefert. Für die Zeitfenster in Kapitel 5 zählen wir beide Varianten.

### Datenschutz

Das Repo ist öffentlich. Rohdaten mit `steamid` und Review-Texten liegen nur in `data-raw/` (gitignored). In `data/` landen nur Spalten ohne Identifier und ohne Freitext, Zeitstempel sind auf den Tag gerundet. Secrets (z. B. ein optionaler `STEAM_KEY`) gehören in `.Renviron`, das ebenfalls gitignored ist.

## Namen der Gruppenmitglieder

Namen werden **nur in [`_team.yml`](_team.yml)** eingetragen. Neue Person: einen Block kopieren und ausfüllen. Optional sind `email`, `orcid` und `affiliations` möglich, die Reihenfolge der Blöcke ist die Anzeige-Reihenfolge.

```sh
Rscript scripts/sync_team.R   # überträgt die Namen nach CITATION.cff
quarto render                 # Seite neu bauen
git add _team.yml CITATION.cff _freeze && git commit -m "Team aktualisiert" && git push
```

| Wo die Namen erscheinen | Wie sie dort hinkommen |
|---|---|
| Titelblock der Seite | Quarto lädt `_team.yml` über `metadata-files` in `_quarto.yml` |
| Footer der Seite | Shortcode `{{< team >}}` (`shortcodes/team.lua`) |
| `CITATION.cff` | `scripts/sync_team.R` |

Passen `_team.yml` und `CITATION.cff` nicht zusammen, bricht `quarto render` mit einer Fehlermeldung ab, lokal **und** in der GitHub Action. So geht nie eine Seite mit falschen Namen online.

## Deployment (GitHub Pages)

Einmalig: im Repo unter **Settings → Pages → Build and deployment → Source** die Option **GitHub Actions** wählen.

Danach baut [`.github/workflows/publish.yml`](.github/workflows/publish.yml) bei jedem Push auf `main` die Seite mit `quarto-dev/quarto-actions` und veröffentlicht sie. Weil `execute: freeze: auto` gesetzt ist, läuft in der CI kein R: Sie baut die Seite aus `_freeze/`.

**Wichtig:** Nach jeder Änderung an `index.qmd`, `R/` oder `data/` lokal `quarto render index.qmd` ausführen und `_freeze/` mit committen. Ein bloßes `quarto render` reicht dafür nicht: `freeze: auto` erkennt nur Änderungen an `index.qmd` selbst und würde sonst die alten Ergebnisse wiederverwenden. Das gilt auch für Grafik-Optionen in `_quarto.yml` (z. B. `fig-dpi`).

Die Action prüft das vor dem Bauen (auch bei Pull Requests): Passt `_freeze/` nicht zu `index.qmd` oder zum Fingerabdruck von `R/*.R` und `data/*.csv` (Chunk `freeze-fingerprint`), bricht sie mit einem Hinweis ab, statt veraltete Zahlen zu veröffentlichen.
