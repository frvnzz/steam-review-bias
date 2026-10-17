-- Shortcode {{< team >}}: setzt die Gruppenmitglieder aus _team.yml als Fließtext
-- ein („A, B und C“, für den Footer). Quarto lädt die Datei als Metadaten. Dabei wird
-- geprüft, ob CITATION.cff dieselben Namen in derselben Reihenfolge enthält.
-- Läuft ohne R, also auch in der CI. Verglichen werden nur die Namen; E-Mail, ORCID
-- und Affiliation prüft `Rscript scripts/sync_team.R --check`.

local stringify = pandoc.utils.stringify

-- Vorname und Nachname, soweit vorhanden (kein Leerzeichen, wenn ein Teil fehlt)
local function join_name(given, family)
  local parts = {}
  if given then table.insert(parts, stringify(given)) end
  if family then table.insert(parts, stringify(family)) end
  return table.concat(parts, " ")
end

local function person_name(author)
  local n = author.name
  if type(n) == "table" and (n.given or n.family or n.literal) then
    if n.literal then return stringify(n.literal) end
    return join_name(n.given, n.family)
  end
  return stringify(n or author)
end

-- Abbruch statt error(): error() würde Quarto nur loggen und trotzdem rendern.
-- So schlägt auch die CI fehl, und nichts Inkonsistentes wird veröffentlicht.
local function fail(msg)
  io.stderr:write("\nFEHLER: " .. msg .. "\n\n")
  os.exit(1)
end

local function cff_names(path)
  local f = io.open(path, "r")
  if not f then fail(path .. " fehlt. Wiederherstellen mit: git checkout -- CITATION.cff") end
  local content = f:read("a")
  f:close()
  -- CITATION.cff ist YAML: als Metadatenblock eines leeren Dokuments von Pandoc parsen lassen
  local meta = pandoc.read("---\n" .. content .. "\n---\n", "markdown").meta
  local names = {}
  for _, a in ipairs(meta.authors or {}) do
    if a.name then
      table.insert(names, stringify(a.name))
    else
      table.insert(names, join_name(a["given-names"], a["family-names"]))
    end
  end
  return names
end

return {
  team = function(args, kwargs, meta)
    local team = {}
    for _, a in ipairs(meta.author or {}) do
      table.insert(team, person_name(a))
    end

    -- Pfad relativ zum Projektordner, nicht zum Arbeitsverzeichnis des Aufrufs
    local dir = quarto.project and quarto.project.directory
    local cff = cff_names(dir and pandoc.path.join({ dir, "CITATION.cff" }) or "CITATION.cff")
    if table.concat(cff, "\n") ~= table.concat(team, "\n") then
      fail("CITATION.cff passt nicht zu _team.yml.\n" ..
           "  _team.yml:    " .. table.concat(team, ", ") .. "\n" ..
           "  CITATION.cff: " .. table.concat(cff, ", ") .. "\n" ..
           "Beheben mit: Rscript scripts/sync_team.R")
    end

    -- jeder Name in einem .nowrap-Span: nie über zwei Zeilen getrennt (auch nicht am Bindestrich)
    local out = {}
    for i, name in ipairs(team) do
      if i > 1 then
        table.insert(out, pandoc.Str(i == #team and " und " or ", "))
      end
      table.insert(out, pandoc.Span({ pandoc.Str(name) }, pandoc.Attr("", { "nowrap" })))
    end
    return pandoc.Inlines(out)
  end
}
