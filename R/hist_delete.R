hist_delete <- function(data, ID) {
  data %>% filter(!text_ID %in% ID)}
