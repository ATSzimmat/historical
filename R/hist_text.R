#' Output the source-paper
#'
#' @param filtered_sources Automatisch von hist_sources generierter Platzhaltername für das source dataframe
#'
#' @returns The rendered text of the sources of the respective source dataframe (so the source-paper)
#' @export
#'
#' @examples
#' # Example source dataframe created with hist_sources
#' Xenophon_Corinthian <- hist_sources(author="Xenophon",context="Corinthian War", output_text = FALSE)
#' # Outout the source-paper
#' Xenophon_Corinthian %>% hist_text()
#'
hist_text <- function(filtered_sources) {
  output <- filtered_sources %>%
    mutate(formatted = glue("{text_ID} – {citation}\n{trimws(text)}")) %>%
    pull(formatted)
  cat(paste(output, collapse = "\n\n"))}
