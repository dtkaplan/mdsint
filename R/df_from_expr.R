#' Construct from an expression a data frame holding what's needed to evaluate the expression
#' 
#' Expressions, including model formulas, are often created to refer to variables in a particular
#' data frame. This function figures out which variables are being referred to and makes a data frame
#' containing just those variables.
#' 
#' @details When the expression involves a transformation of a variable, e.g. log(price), when `base = TRUE` the variable itself
#' (e.g. `price`) will be in the data frame rather than the transformation.
#' 
#' @param E An expression intended to be evaluated with respect to a data frame. The expression could be produced by `quote()`, but more
#' often it will be produced by one of the lang-related functions in the rlang package or be extracted from one side or the other of a formula.
#' @param data The data frame that `E` is with respect to.
#' @param base A flag saying whether the variable itself (default: `TRUE``) or the transformation of the variable (`FALSE`) should appear
#' in the data frame.
#'
#' @examples
#' head(mtcars) %>% df_from_expr(quote(mpg))
#' head(mtcars) %>% df_from_expr(quote(log(mpg)))
#' head(mtcars) %>% df_from_expr(quote(log(mpg)), base = FALSE)
#' head(mtcars) %>% df_from_expr(quote(log(mpg) + 7))

#' @export
df_from_expr <- function(data = NULL, E, base = TRUE) {
  if(is_lang(E) && lang_name(E) %in% c("+", "&", "*", ":")) {
    if (length(E) == 2) df_from_expr(E[[2]], data = data) 
    else dplyr::bind_cols(df_from_expr(E[[2]], data = data), 
                     df_from_expr(E[[3]], data = data))
  } else {
    vars <- all.vars(E)
    if ( !all(vars %in% names(data))) {
      bogus <- vars[ ! vars %in% names(data)]
      bogus <- paste0(paste0('"', bogus, '"'), collapse = ", ")
      stop(ifelse(length(bogus)>1, "variables", variable), bogus, " not in the data frame")
    }
    if (base) data[vars]
    else {
      # Evaluate each component
      v <- eval_tidy(E, data = data)
      nm <- if (is_lang(E)) expr_label(E) else expr_text(E)
      setNames(data_frame(v), nm)
    }
  }
}
