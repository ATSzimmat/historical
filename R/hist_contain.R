#' Filter the source dataframe by the occurrence of different words
#'
#' @param "words" (The words for which the source dataframe should be filtered)
#' @returns the filtered source dataframe
#' @export
#'
#' @examples
#' # Before using hist_cotain, you should have created a dataframe containing the sources you want to examine. To do this, you must have used hist_sources, where output_text must not be set to TRUE.
#' Xenophon_Corinthian <- hist_sources(author="Xenophon",context="Corinthian War", output_text = FALSE)
#'
#' # Filter a source dataframe for sources that contain one specific word, e.g., "king"
#' Xenophon_Corinthian %>% hist_contain("king")
#' # Filter a source dataframe for sources that contain several specific words together, e.g. "king", "battle" and "Thebans"
#' Xenophon_Corinthian %>% hist_contain("king+battle+Thebans")
#' # Filter a source dataframe for sources where either a specific word or another specific word occurs, e.g., "king" or "Thebans"
#' Xenophon_Corinthian %>% hist_contain("king","Thebans")
#' # Filter a source dataframe for sources where e.g. either a specific word or two specific word thogether occur, e.g., "king+battle" or "Thebans"
#' Xenophon_Corinthian %>% hist_contain("king+battle","Thebans")
#'
#' After using hist_cotain you will receive your filtered source dataframe, which you can further edit or convert to a source paper using hist_text
#' Xenophon_filtered <- Xenophon_Corinthian %>% hist_contain("king+battle","Thebans")
#' Xenophon_filtered %>% hist_text()
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
