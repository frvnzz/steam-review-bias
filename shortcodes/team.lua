-- Shortcode {{< team >}}: setzt die Gruppenmitglieder aus _team.yml als Fließtext
-- ein („A, B und C“, für den Footer). Quarto lädt die Datei als Metadaten. Dabei wird
-- geprüft, ob CITATION.cff dieselben Namen in derselben Reihenfolge enthält.
-- Läuft ohne R, also auch in der CI.

local stringify = pandoc.utils.stringify

local function person_name(author)
  local n = author.name
  if type(n) == "table" and (n.given or n.family or n.literal) then
    if n.literal then return stringify(n.literal) end
    local parts = {}
    if n.given then table.insert(parts, stringify(n.given)) end
    if n.family then table.insert(parts, stringify(n.family)) end
    return table.concat(parts, " ")
  end
  return stringify(n or author)
end

local function cff_names(path)
  local f = io.open(path, "r")
  if not f then return nil end
  local content = f:read("a")
  f:close()
  -- CITATION.cff ist YAML: als Metadatenblock eines leeren Dokuments von Pandoc parsen lassen
  local meta = pandoc.read("---\n" .. content .. "\n---\n", "markdown").meta
  local names = {}
  for _, a in ipairs(meta.authors or {}) do
    if a.name then
      table.insert(names, stringify(a.name))
    else
      table.insert(names, stringify(a["given-names"] or "") .. " " .. stringify(a["family-names"] or ""))
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

    local cff = cff_names("CITATION.cff")
    if cff and table.concat(cff, "\n") ~= table.concat(team, "\n") then
      -- error() würde Quarto nur loggen und trotzdem rendern → hart abbrechen,
      -- damit auch die CI fehlschlägt und nichts Inkonsistentes veröffentlicht.
      io.stderr:write("\nFEHLER: CITATION.cff passt nicht zu _team.yml.\n" ..
                      "  _team.yml:    " .. table.concat(team, ", ") .. "\n" ..
                      "  CITATION.cff: " .. table.concat(cff, ", ") .. "\n" ..
                      "Beheben mit: Rscript scripts/sync_team.R\n\n")
      os.exit(1)
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
