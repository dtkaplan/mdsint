#' Assign names to slots in a string
#' 
#' Slots are identified by the usual `{{name}}` convention. One use for this 
#' is constructing formulas from input text strings in a manipulate or shiny app
#' 
#' @param template_str a character string with named slots, e.g. `"{{person}} is {{age}} years old."`
#' @param ... values for the slots, e.g. `person = Bill`, `age = 12`
#' 
#' @examples
#' fill_template("{{person}} is {{age}} years old.", person = "Bill", age = 12 )
#' as.formula(fill_template("{{y}} ~ {{x}} - 1", x = "height", y = "age"))
#' @export
fill_template <- function(template_str, ...) {
  text_form <- template_str
  dots <- list(...)
  for (k in 1:length(dots)) {
    pat <- paste0("\\{\\{", names(dots)[k], "\\}\\}")
    text_form <- gsub(pat, as.character(dots[[k]]), text_form)
  }
  
  text_form
}
