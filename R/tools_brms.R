std_normalize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

replace_na <- function(x) {
  if_else(is.na(x), "Neuvedeno", as.character(x)) %>% factor()
}

normalized_translator <- function(x) {
  function(val) {
    (val - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  }
}
