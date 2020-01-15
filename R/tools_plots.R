dark_blue_color <- "#0040ae"
darkest_fill <- "#668cce"
midd_fill <- "#b2c5e6"


set_theme_revizers <- function() {
  theme_set(cowplot::theme_cowplot())
  theme_update(text = element_text(family = "Roboto", color = "white"),
               plot.background = element_rect(fill = dark_blue_color),
               line = element_line(color = "white"),
               rect = element_rect(color = "white"),
               axis.text = element_text(color = "white", face = "bold"),
               axis.title = element_text(color = "white"),
               axis.line = element_line(color = "white"), axis.ticks = element_line(color = "white"),
               strip.background = element_rect(color = "white", fill = darkest_fill))
  sysfonts::font_add_google("Roboto")
  sysfonts::font_add(family = "Skaut Bold", paste0(Sys.getenv("APPDATA"),"/../Local/Microsoft/Windows/Fonts/skaut-bold-webfont.otf"))
  showtext_auto()

}
