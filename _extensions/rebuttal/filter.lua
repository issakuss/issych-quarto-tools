function Div(el)
    if el.classes:includes('comment') then
        table.insert(el.content, 1, pandoc.RawBlock('latex', '\\begin{reviewerbox}'))
        table.insert(el.content, pandoc.RawBlock('latex', '\\end{reviewerbox}'))
        return el
    end


  if el.classes:includes('change') then
    local div_children = {}
    for _, child in ipairs(el.content) do
      if child.t == "Div" then
        table.insert(div_children, child)
      end
    end

    local latex = [[\begin{longtable}{p{0.18\textwidth} p{0.36\textwidth} p{0.36\textwidth}}
                    \toprule
                    \textbf{Location} & \textbf{Before} & \textbf{After} \\
                    \midrule
                    \endhead
                    ]]

    for i = 1, #div_children, 3 do
      local loc    = pandoc.utils.stringify(div_children[i] or "")
      local before = pandoc.write(pandoc.Pandoc(div_children[i+1] or {}), 'latex')
      local after  = pandoc.write(pandoc.Pandoc(div_children[i+2] or {}), 'latex')

      before = before:gsub("^%s*(.-)%s*$", "%1")
      after  = after:gsub("^%s*(.-)%s*$", "%1")

      latex = latex .. loc .. " & " .. before .. " & " .. after .. " \\\\\n"
      latex = latex .. "\\midrule\n"
    end

    latex = latex .. "\\end{longtable}"

    return pandoc.RawBlock('latex', latex)
  end
end