plot_mds <- function(base_mds, mapping = NULL, color_aes = NULL, shape_aes = NULL) {
  color_aes <- enquo(color_aes)
  shape_aes <- enquo(shape_aes)


  to_data <- function(x) {
    x %>% as.data.frame() %>% set_names("MDS1","MDS2") %>% rownames_to_column("observation")
  }
  data_to_plot <- base_mds$points %>% to_data() %>% mutate(sample = "Orig")

  if(!is.null(mapping)) {
    data_to_plot <- data_to_plot %>% cbind(mapping)
  }

  data_to_plot %>%
    ggplot(aes(MDS1, MDS2, shape = !!shape_aes, color = !!color_aes)) +
    geom_point()
}
