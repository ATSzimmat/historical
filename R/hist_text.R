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
  formatted <- glue::glue(
    "{filtered_sources$text_ID} – {filtered_sources$citation}\n{trimws(filtered_sources$text)}"
  )
  cat(paste(formatted, collapse = "\n\n"))
}
