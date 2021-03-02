# gitbook_output <- (isTRUE(getOption('knitr.in.progress')) &&
#                      knitr::opts_knit$get('rmarkdown.pandoc.to') )
document_output <- isTRUE(getOption('knitr.in.progress'))
gitbook_output <- document_output && (knitr::opts_knit$get('rmarkdown.pandoc.to') == "html")

if(document_output) {
  table_format <- knitr::kable
} else {
  table_format <- identity
}

#V palcich
if(!document_output || gitbook_output) {
  plot_size_multiplier <- 0.66
} else {
  plot_size_multiplier <- 1
}

default_plot_width <- 12 * plot_size_multiplier
default_plot_height <- (default_plot_width / 16) * 9

knitr::opts_chunk$set(fig.width = default_plot_width, fig.height = default_plot_height)
