-- Alle Bilder mit loading="lazy": Jede Grafik steht zweimal in der Seite (Desktop-
-- und Handy-Fassung, eine davon per display: none versteckt). Ohne lazy lädt der
-- Browser trotzdem beide – rund 9 MB beim ersten Aufruf, egal auf welchem Gerät.
-- Mit lazy lädt er nur die sichtbare Fassung, und auch die erst kurz bevor sie
-- ins Bild scrollt.

function Image(el)
  el.attributes.loading = "lazy"
  el.attributes.decoding = "async"
  return el
end
