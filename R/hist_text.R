hist_text <- function(filtered_sources) {
  output <- filtered_sources %>%
    mutate(formatted = glue("{text_ID} – {citation}\n{trimws(text)}")) %>%
    pull(formatted)
  cat(paste(output, collapse = "\n\n"))}
