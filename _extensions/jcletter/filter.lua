local function extract_reviewers(meta, key)
    local bullet_list = {}

    if meta.reviewers and meta.reviewers[key] then
        for _, reviewer in ipairs(meta.reviewers[key]) do
            local title = pandoc.utils.stringify(reviewer.title)
            local name = pandoc.utils.stringify(reviewer.name)
            local affili = pandoc.utils.stringify(reviewer.affiliation)
            local email = pandoc.utils.stringify(reviewer.email)
            local items = pandoc.Plain({pandoc.Str(title .. " " .. name .. ", " .. affili .. ": " .. email)})
            table.insert(bullet_list, items)
        end
    end
    return pandoc.BulletList(bullet_list)
end

return {
  {
    Meta = function(meta)
      recommended = extract_reviewers(meta, "recommend")
      excluded = extract_reviewers(meta, "exclude")
      return meta
    end
  },
  {
    Div = function(el)
      if el.identifier == "recommend" then
        table.insert(el.content, recommended)
        return el
      end
    end
  },
  {
    Div = function(el)
      if el.identifier == "exclude" then
        table.insert(el.content, excluded)
        return el
      end
    end
  }
}