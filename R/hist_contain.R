hist_contain <- function(data, ...) {
  words <- c(...)
  make_filter <- function(entry) {
    if (grepl("\\+", entry)) {
      sub_words <- strsplit(entry, "\\+")[[1]]
      return(function(txt) all(str_detect(txt, fixed(sub_words, ignore_case = TRUE))))
    } else {
      return(function(txt) str_detect(txt, fixed(entry, ignore_case = TRUE)))}}
  filters <- lapply(words, make_filter)
  result <- data %>%
    filter(sapply(text, function(txt) any(sapply(filters, function(word_list)
      word_list(txt)))))
  return(result)}
