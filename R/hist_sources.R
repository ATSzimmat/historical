#' Erzeuge ein gefiltertes Quellen-DataFrame
#'
#' Diese Funktion filtert das interne Datenset \code{the_sources} nach den angegebenen Parametern.
#' Falls \code{output_text = TRUE}, wird der Ergebnistext formatiert als Ausgabe in der Konsole dargestellt.
#'
#' @param author Der Autor, nach dem die Quellen gefiltert werden sollen (z.B. "Xenophon")
#' @param context Der historische Kontext (z.B. "Corinthian War")
#' @param event_type Der Typ des Ereignisses (z.B. "battle")
#' @param event Das spezifische Ereignis (z.B. "Leuctra")
#' @param work Das Werk (z.B. "Hellenika")
#' @param citation Die Zitation (z.B. "1.1.1")
#' @param output_text Wenn \code{TRUE}, wird der Ergebnistext formatiert in der Konsole ausgegeben
#'
#' @return Ein gefiltertes DataFrame mit Quellenstellen (sofern \code{output_text = FALSE})
#' @export
#'
#' @examples
#' # Alle Quellenstellen von Xenophon über den Korinthischen Krieg als DataFrame
#' Xenophon_Corinthian <- hist_sources(author = "Xenophon", context = "Corinthian War")
#'
#' # Als formatierter Text
#' hist_sources(author = "Xenophon", context = "Corinthian War", output_text = TRUE)
hist_sources <- function(
    author = NULL,
    context = NULL,
    event_type = NULL,
    event = NULL,
    work = NULL,
    citation = NULL,
    output_text = FALSE
) {
  data <- the_sources

  if (!is.null(author)) {
    data <- data[data$author == author, ]
  }
  if (!is.null(context)) {
    data <- data[data$context == context, ]
  }
  if (!is.null(event_type)) {
    data <- data[data$event_type == event_type, ]
  }
  if (!is.null(event)) {
    data <- data[data$event == event, ]
  }
  if (!is.null(work)) {
    data <- data[data$work == work, ]
  }
  if (!is.null(citation)) {
    data <- data[data$citation == citation, ]
  }

  if (output_text) {
    output <- paste0(data$text_ID, " – ", data$citation, "\n", trimws(data$text))
    cat(paste(output, collapse = "\n\n"))
  } else {
    return(data)
  }
}
