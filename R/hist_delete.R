#' Delete unintended source excerpts
#'
#' @param data Your current source dataframe
#' @param ID Die text_ID der Quellenstellen, die man entfernen möchte
#'
#' @returns The source dataframe ohne die Quellenstellen, die du löschen möchtest
#' @export
#'
#' @examples
#' #Entferne die Quellenstellen mit den IDs 5 und 14
#' Xenophon_Corinthian <- Xenophon_Corinthian %>% hist_delete(ID = c(5, 14))
hist_delete <- function(data, ID) {
  data %>% filter(!text_ID %in% ID)}
