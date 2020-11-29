local function non_breaking_space()
  if FORMAT:match 'html' then
    return(pandoc.RawInline("html", "&nbsp;"))
  elseif FORMAT:match 'latex' then
    return(pandoc.RawInline("latex", "~"))
  else
    error("Unsupported format for avoid_letters_at_end_of_line.lua")
  end
end

-- Other strings to force a nbsp after
-- Should be all lowercase
local additional_strings = {
  ["(tj."] = true,
  ["tj."] = true,
  ["tzv."] = true
}

local function require_non_breaking_space(txt, spc)
  return spc and spc.t == 'Space'
    and txt and txt.t == 'Str'
    and (txt.c:len() == 1 or additional_strings[pandoc.text.lower(txt.c)])
end


function replace_spaces (content)
  for i = #content-1, 1, -1 do
    if require_non_breaking_space(content[i], content[i+1]) then
      content[i+1] = non_breaking_space()
    end
  end

  return content
end

function replace_spaces_content (s)
  s.content = replace_spaces(s.content)
  return s
end

function replace_spaces_caption(s)
  s.caption = replace_spaces(s.caption)
  return s
end

-- In theory, we should be able to filter all inline text elemnts with:
-- return {{ Inlines = replace_spaces }}
-- But for some reason, I couldn't make it work, so explicitly lising elements
-- whose contents should be transformed.

return {{ Para = replace_spaces_content,
  Header = replace_spaces_content,
  LineBlock = replace_spaces_content,
  Plain = replace_spaces_content,
  Emph = replace_spaces_content,
  Caption = replace_spaces_content,
  Link = replace_spaces_content,
  Quoted = replace_spaces_content,
  SmallCaps = replace_spaces_content,
  Span = replace_spaces_content,
  Strikeout = replace_spaces_content,
  Strong = replace_spaces_content,
  Underline = replace_spaces_content,
  Image = replace_spaces_caption
}}
