document_output <- (isTRUE(getOption('knitr.in.progress')) &&
                      ("word_document" %in% rmarkdown::all_output_formats(knitr::current_input()) ||
                         "html_document" %in% rmarkdown::all_output_formats(knitr::current_input()) ||
                         any(grepl("book", rmarkdown::all_output_formats(knitr::current_input()))) ||
                         is.null(rmarkdown::all_output_formats(knitr::current_input())) || # GitBook has this, probably not great option :-)
                         "pdf_document" %in% rmarkdown::all_output_formats(knitr::current_input())) )

word_output <- (isTRUE(getOption('knitr.in.progress')) &&
                  "word_document" %in% rmarkdown::all_output_formats(knitr::current_input()))

if(document_output) {
  table_format <- knitr::kable
} else {
  table_format <- identity
}

#V palcich
if(document_output) {
  plot_size_multiplier <- 1
} else {
  plot_size_multiplier <- 1
}

default_plot_width <- 12 * plot_size_multiplier
default_plot_height <- (default_plot_width / 16) * 9

knitr::opts_chunk$set(fig.width = default_plot_width, fig.height = default_plot_height)
