#' Pull out a data frame containing the variables referenced in a formula
#'
#' Essentially, a form of dplyr::select() which uses a formula to imply the variables to be selected.
#' 
#' @param data A data frame containing the wanted variables (and generally some others as well).
#' @param formula The formula that references the variables in the data frame
#' 
#' @examples
#' head(mtcars) %>% df_from_formula(mpg ~ hp)
#' head(mtcars) %>% df_from_formula(log(mpg) ~ sqrt(hp) + cyl)
#' 
#' @export
df_from_formula <- function(data = NULL, formula) {
  formula_parts <- mosaicCore::parse.formula(formula)
  left <- df_from_expr(formula_parts$lhs, data = data)
  right <- df_from_expr(formula_parts$rhs, data = data)
  condition <- df_from_expr(formula_parts$condition, data = data)
  
  All <- dplyr::bind_cols(left, right, condition)
  set_attrs(All, left_vars = names(left), right_vars = names(right), cond_vars = names(condition))
}