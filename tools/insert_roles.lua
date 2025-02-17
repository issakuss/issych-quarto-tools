-- Add to _extensions/quarto-journals/{journal name}/_extension.yml
local roles_output = pandoc.Inlines({}) -- 役割情報を格納するInlines

-- authors から name の initials と role を取得する関数
local function extract_roles(meta)
  local all_inlines = {} -- すべての著者情報を格納するテーブル

  if meta.authors then
    for _, author in ipairs(meta.authors) do
      local one_author_roles = {}

      -- 役割（roles）を取得
      if author.roles then
        for _, role in ipairs(author.roles) do
          if role["vocab-term"] then
            table.insert(one_author_roles, pandoc.utils.stringify(role["vocab-term"]))
          else
            table.insert(one_author_roles, pandoc.utils.stringify(role))
          end
        end

        -- 名前のinitialsを取得
        local family = pandoc.utils.stringify(author.name.family)
        local given_full = pandoc.utils.stringify(author.name.given)
        local given, middle = given_full:match("([^ ]+) (.+)")
        given = given or given_full
        middle = middle or ""
        local initials = given:sub(1, 1) .. middle:sub(1, 1) .. family:sub(1, 1) .. ": "

        -- initials を太字にする
        local bold_initials = pandoc.Strong(pandoc.Str(initials))

        -- 役割の文字列を結合
        local roles_text = table.concat(one_author_roles, ", ")

        -- initials（太字）と役割を結合し、Inlinesとして保存
        table.insert(all_inlines, bold_initials)           -- 太字のinitials
        table.insert(all_inlines, pandoc.Str(roles_text))  -- 役割のテキスト
        table.insert(all_inlines, pandoc.LineBreak())      -- 改行を挿入
        -- table.insert(all_inlines, pandoc.Str("  "))        -- 区切りのスペース
      end
    end
  end

  -- all_inlines を Inlines オブジェクトとして返す
  return pandoc.Inlines(all_inlines)
end

-- Meta関数: authors情報を取得
return {
  {
    Meta = function(meta)
      roles_output = extract_roles(meta)
      return meta
    end
  },
  {
    Div = function(el)
      if el.identifier == "roles" then
        -- roles_output（Inlinesオブジェクト）を挿入
        table.insert(el.content, pandoc.Para(roles_output))
        return el
      end
    end
  }
}