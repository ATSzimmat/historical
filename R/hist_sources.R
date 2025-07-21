#' Create source dataframe nach Wunsch
#'
#' @param author Der Autor nach die Quellen gefiltert werden sollen
#' @param context Der Kontext nach dem die Quellen gefiltert werden sollen
#' @param event_type Der Art des Ereignisses nach dem die Quellen gefiltert werden sollen
#' @param event Das Ereignis nach dem die Quellen gefiltert werden sollen
#' @param work Das Werk nach dem die Quellen gefiltert weden sollen
#' @param citation Die Zitation nach der die Quellen gefiltert werden sollen
#' @param output_text Möglichkeit sich den gerenderten Text der Quellen ausgeben zu lassen
#'
#' @returns die gewünschten Quellenstellen als DataFrame
#' @export
#'
#' @examples
#' # Alle Quellenstellen von Xenophon über den Korinthischen Krieg als DataFrame
#' Xenophon_Corinthian <- hist_sources(author="Xenophon",context="Corinthian War", output_text=FALSE)
#' # Alle Quellenstellen von Xenophon über den Korinthischen Krieg als Text
#' Xenophon_Corinthian <- hist_sources(author="Xenophon",context="Corinthian War", output_text=TRUE)
hist_sources <- function(
    author = NULL, context = NULL, event_type = NULL, event = NULL, work = NULL,
    citation = NULL, output_text = FALSE, source_data = the_sources)
{
  filtered_sources <- source_data

  if (!is.null(author)) {
    filtered_sources <- dplyr::filter(filtered_sources, author == author)
  }
  if (!is.null(context)) {
    filtered_sources <- dplyr::filter(filtered_sources, context == context)
  }
  if (!is.null(event_type)) {
    filtered_sources <- dplyr::filter(filtered_sources, event_type == event_type)
  }
  if (!is.null(event)) {
    filtered_sources <- dplyr::filter(filtered_sources, event == event)
  }
  if (!is.null(work)) {
    filtered_sources <- dplyr::filter(filtered_sources, work == work)
  }
  if (!is.null(citation)) {
    filtered_sources <- dplyr::filter(filtered_sources, citation == citation)
  }

  if (output_text) {
    output <- dplyr::mutate(
      filtered_sources,
      formatted = glue::glue("{text_ID} – {citation}\n{trimws(text)}")
    ) |> dplyr::pull(formatted)
    cat(paste(output, collapse = "\n\n"))
  } else {
    return(filtered_sources)
  }
}
