#' Show residuals from groupwise means
#'
#' Makes a specific format of plot that shows groupwise means and the residuals from those means. 
#' 
#' @param data A data frame containing the data to be used
#' @param formula A formula `y ~ gp`` specifying the grouping variable (`gp`) and the response variable (`y`) Formulas with a second grouping variable (e.g. `y ~ gp1 + gp2`) are also accepted.
#' @param alpha Numeric 0 to 1. Level of opaqueness.
#' @param seed Optional integer seed for the random numbers used in jittering. Use if you want reproducibility.
#' @export
draw_groupwise_mod <- function(data = NULL, formula, alpha = 0.2, seed = NULL, ...) {
  mod <- lm(formula, data = data)
  Dat <- df_from_formula(formula, data = data)
  response_var <- (Dat %@% "left_vars")[1]
  explanatory_var <- (Dat %@% "right_vars")[1]
  if (is.na(explanatory_var)) {
    # there was a formula like y ~ 1
    explanatory_var <- "all_in_same_group"
    Dat$all_in_same_group <- ""
  }
  if (! is.numeric(Dat[[response_var]])) stop("Response variable must be numeric.")
  # if (is.numeric(Dat[[explanatory_var]])) stop("Explanatory variable must be categorical.")
  if ( ! is.factor(Dat[[explanatory_var]])) Dat[[explanatory_var]] <- as.factor(Dat[[explanatory_var]])
  if (!is.null(seed) && is_integer(seed)) set.seed(seed)
  for_jitter <- runif(nrow(Dat), max = 0.3, min = -0.3)
  Dat[["horiz_position"]] <- as.numeric(Dat[[explanatory_var]]) + for_jitter
  # draw the model line extending a bit beyond the jitter.
  Dat[["horiz_position_2"]] <- as.numeric(Dat[[explanatory_var]]) + 1.2 * 0.3 * sign(for_jitter)
  
  Dat$model_output <- mosaicModel::mod_eval(mod, data = data, append = FALSE)$model_output
  Dat$residuals <- Dat[[response_var]] - Dat$model_output
  P <- gf_blank(as.formula(fill_template("{{y}} ~ {{x}}", x = explanatory_var, y = response_var)),
                data = Dat, show.legend = FALSE)
  # If there's a color 
  dots <- list(...)
  if (length(Dat %@% "right_vars") > 1) {
    color_var <- (Dat %@% "right_vars")[2]
    Dat[["color_variable"]] <- paste(Dat[[color_var]], Dat[[explanatory_var]])
    for_color <- as.formula(fill_template("~ {{x}}", x = color_var))
    show.legend = TRUE
  }
  else {
    for_color <- as.formula(fill_template("~ {{x}}", x = explanatory_var))
    Dat[["color_variable"]] <- as.factor(Dat[[explanatory_var]])
    show.legend = FALSE
  } 
  
  
  # Can we break up the model by the 2nd variable. Use just the formula.
  P2 <- 
    P %>%
    gf_point(as.formula(fill_template("{{y}} ~ {{x}}", x = "horiz_position", y = response_var)),
             data = Dat, color = for_color, alpha = alpha, show.legend = show.legend) %>%
    gf_line(as.formula(fill_template("{{y}} ~ {{x}}", x = "horiz_position_2", y = "model_output")),
            data = Dat, color = for_color, group = ~ color_variable, alpha = 1, show.legend = show.legend)
  
  P2 <- P2 %>%
    gf_segment(as.formula(fill_template("{{y1}} + {{y2}} ~ {{x}} + {{x}}", 
                                        y1 = response_var, y2 = "model_output", x = "horiz_position")),
               data = Dat, color = for_color, alpha = alpha, show.legend = FALSE)
  
  list(plot = P2, matrix = Dat, model = mod) # return the data frame with all the information. For brushing add ons
}
