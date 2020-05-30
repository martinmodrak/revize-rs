brm_with_cache <- function(cache_file = NULL, ...) {
  fit <- NULL
  stancode <- brms::make_stancode(...)
  standata <- brms::make_standata(...)
  if(!is.null(cache_file) && file.exists(cache_file)) {
    cache_contents <- readRDS(cache_file)
    if(!is.list(cache_contents) || is.null(cache_contents$stancode) ||
       is.null(cache_contents$standata) || is.null(cache_contents$fit)) {
      message("Invalid cache content")
    } else {
      if(identical(stancode, cache_contents$stancode) &&
         identical(standata, cache_contents$standata)) {
        fit <- cache_contents$fit
      } else {
        message("Cache file out of date, refitting")
      }
    }
  }

  if(is.null(fit)) {
    fit <- brms::brm(...)
    if(!is.null(cache_file) && brms:::contains_samples(fit)) {
      saveRDS(loo::nlist(stancode, standata, fit), file = cache_file)
    }
  }

  fit
}


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
