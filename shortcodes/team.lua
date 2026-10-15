-- Shortcode {{< team >}}: listet die Gruppenmitglieder aus _team.yml auf
-- (Quarto lädt die Datei als Metadaten) und prüft dabei, ob CITATION.cff
-- dieselben Namen in derselben Reihenfolge enthält. Läuft ohne R, also auch in der CI.

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

    local items = {}
    for _, name in ipairs(team) do
      table.insert(items, { pandoc.Plain({ pandoc.Str(name) }) })
    end
    return pandoc.Div({ pandoc.BulletList(items) }, pandoc.Attr("", { "team-list" }))
  end
}
