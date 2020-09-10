document_output <- (isTRUE(getOption('knitr.in.progress')) &&
                            ("word_document" %in% rmarkdown::all_output_formats(knitr::current_input()) ||
                               "html_document" %in% rmarkdown::all_output_formats(knitr::current_input()) ||
                               "pdf_document" %in% rmarkdown::all_output_formats(knitr::current_input())) )


if(document_output) {
  table_format <- knitr::kable
} else {
  table_format <- identity
}
