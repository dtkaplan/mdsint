#' Display a violin plot of density together with a jitter plot
#' 
#' This function tries to behave well with respect to formulas that gf_violin() doesn't handle well, e.g.
#' `~ mpg` or `mpg ~ 1`. It's a light shell on `gf_violin()` and `gf_jitter()`.
#' 
#' @param data The data frame containing what's to be plotted.
#' @param formula A formula specifying which variable to show the density of and what are the conditioning variables
#' @param alpha The opaqueness of the violin plot
#' @param show.dots A number between 0 and 1 giving the fraction of individual cases (randomly selected) to be shown in the jitter plot. Default: 1, that is, all of them.
#' @param alpha.jit The opaqueness of the dots in the jitter plot.
#' @param max.levels A safeguard against unintentially constructing a graph with an unreasonably high number of groups. If the x-axis variable is numeric and integer valued, treat each integer value as a separate level. 
#' But give an error if there are more than `max.levels` different values. (Default: 20) In ordinary use, the end user doesn't have to 
#' give this argument. 
#' 
#' @details To be consistent with functions like `gf_density()`, this function will accept formulas like `~ mpg` where the cases are not to
#' be broken down by another variable. A synonym for this is `mpg ~ 1`. When you want separate violin plots for different groups, say different
#' numbers of cylinders, `mpg ~ cyl`. The cylinders will become the main spatial variable on the x-axis. A second variable on the right-hand side
#' of the formula will be rendered as color, e.g. `mpg ~ cyl + carb`. As with other ggformula functions, you can specify variables to be used
#' for faceting by including them after the vertical bar, e.g. `mpg ~ cyl + carb | gear`. Note that the function will accept a quantitative 
#' variable for the grouping so long as it is integer valued (and there aren't more than 20 groups). The x-axis will then be numerical, so
#' you can plot something else (e.g. labels).
#' 
#' @examples
#' mtcars %>% violin_density(~ mpg)
#' mtcars %>% violin_density(mpg ~ 1) # same thing
#' mtcars %>% violin_density(mpg ~ cyl)
#' mtcars %>% violin_density(mpg ~ 1 | cyl)
#' Hill_runners %>% violin_density(time ~  year, fill = ~ sex)
#' Hill_runners %>% violin_density(time ~ year, fill = ~ sex, color = ~ sex)
#' @export

violin_density <- function(data = NULL, formula, alpha = 0.3, show.dots = 1, alpha.jit = alpha/2, max.levels = 20, ...) {
  components <- mosaicCore::parse.formula(formula)
  dummy_name <- "density"
  if (is.null(components$lhs)) { # no LHS, so use var on RHS as variable for which the density is to be calculated
    data[[dummy_name]] <- ""
    components$lhs <- components$rhs
    components$rhs <- as.name(dummy_name)
  } else if (expr_text(components$rhs) == "1") { # There's a constant on the rhs
    data[[dummy_name]] <- ""
    components$rhs <- as.name(dummy_name)
  }
  rhs_vals <- df_from_expr(components$rhs, data = data)
  if (is.numeric(rhs_vals[[1]])) {
    if (all(rhs_vals == round(rhs_vals)) && length(unique(rhs_vals)) <= max.levels) {
      data[[all.vars(components$rhs)[1] ]] <- as.factor(rhs_vals[[1]])
    }
  }
  
  # put the components back together
  if ( ! is.null(components$condition))
    RHS <- lang("|", components$rhs, components$condition)
  else
    RHS <- components$rhs
  
  new_f <- new_formula(components$lhs, RHS)
  
  P <- gf_violin(new_f, data = data, alpha = alpha, ...) 
  
  if (show.dots > 1) warning("show.dots should be in range 0 to 1. Trimming it to 1.")
  if (show.dots < 0) warning("show.dots should be in range 0 to 1. Setting it to zero, so no jitter component will be included.")
  
  if (show.dots > 0) {
    
    show.dots <- pmin(1, show.dots)
    dot_data <- sample_frac(data, size = show.dots) 
    dots <- list(...)
    position_fun <- 
      if (is_formula(dots$fill) || is_formula(dots$color) || is_formula(dots$group)) position_jitterdodge
      else position_jitter
    P <- 
      P %>%
      gf_jitter(new_f, data = dot_data, alpha = alpha.jit, ..., position = position_fun())
  }

  
  P
}