if FORMAT:match 'html' then
  local function require_non_breaking_space(txt, spc)
    return spc and spc.t == 'Space'
      and txt and txt.t == 'Str'
      and (txt.c:len() == 1 or txt.c == "(tj.")
  end

  function replace_spaces (s)
    local content = s.content

    for i = #content-1, 1, -1 do
      if require_non_breaking_space(content[i], content[i+1]) then
        content[i+1] = pandoc.RawInline("html", "&nbsp;")
      end
    end

    s.content = content
    return s
  end

  function Inlines (content)

    for i = #content-1, 1, -1 do
      if require_non_breaking_space(content[i], content[i+1]) then
        content[i+1] = pandoc.RawInline("html", "A&nbsp;")
      end
    end
    return(content)
  end

end

