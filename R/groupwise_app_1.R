#' Shiny app to show resids from a groupwise model
#' 
#' @export
groupwise_app_1 <- function(data, response_var, explanatory_vars = c("year", "sex", "race name" = "race")) {
  require(shiny)
  require(shinydashboard)
  require(markdown)
  shinyApp(
    ui <- dashboardPage(
      dashboardHeader(title = "Analysis of Variance"),
      dashboardSidebar(
        selectInput("group_by", "Grouping variable", c("none" = "1", explanatory_vars)),
        selectInput("var_measure", "Measure of spread", 
                    c("standard deviation", "variance", "sum of squares", "mean square", "range")), 
        hr(),
        p(paste0("Response variable is <", response_var, ">")),
        p(textOutput("measure_choice", inline = TRUE), "of response:", textOutput("total_spread", inline = TRUE)),
        p("That's the \"whole\"."), 
        p("Breaking into components..."),
        p("   Model values ..."),
        p("\t      ", textOutput("model_spread", inline = "TRUE")),
        p("   Residuals ..."), 
        p("      ", textOutput("resid_spread", inline = TRUE)), 
        hr(),
        p("Sum of components ...", textOutput("sum_spreads", inline = "TRUE")),
        p(textOutput("sum_matches", inline = TRUE))
      ),
      dashboardBody(plotOutput("plot1"))
    ), 
    server = function(input, output, session) {
      fitted <- reactiveValues(
        plot = 1,
        matrix = 2, 
        model = lm(hp ~ mpg, data = mtcars) # just to have something in the right format
      )
      gmean <- reactive({mean(data[[response_var]], na.rm = TRUE)}) # grand mean
      total_spread <- reactive({
        x <- data[[response_var]]
        switch(input$var_measure,
               "standard deviation" = sd(x, na.rm = TRUE),
               "variance" = var(x, na.rm = TRUE),
               "sum of squares" = sum((x-gmean())^2),
               "mean square" = sum((x-gmean())^2) / nrow(data),
               "range" = diff(range(x, na.rm = TRUE)))
      })
      model_spread <- reactive({
        x <- fitted$matrix$model_output
        mod <- fitted$model
        switch(input$var_measure,
               "standard deviation" = sd(x, na.rm = TRUE),
               "variance" = var(x, na.rm = TRUE),
               "sum of squares" = sum((x-gmean())^2),
               "mean square" = sum((x-gmean())^2) / (length(coef(mod)) - 1),
               "range" = diff(range(x, na.rm = TRUE)))
      })
      resid_spread <- reactive({
        x <- fitted$matrix$residuals
        mod <- fitted$model
        switch(input$var_measure,
               "standard deviation" = sd(x, na.rm = TRUE),
               "variance" = var(x, na.rm = TRUE),
               "sum of squares" = sum(x^2),
               "mean square" = sum(x^2) / (nrow(data) - length(coef(mod))),
               "range" = diff(range(x, na.rm = TRUE))) 
      })
      output$measure_choice <- renderText(input$var_measure)
      output$sum_spreads <- renderText({model_spread() + resid_spread()})
      output$total_spread <- renderText({total_spread()})
      output$model_spread <- renderText({model_spread()})
      output$resid_spread <- renderText({resid_spread()})
      output$resid_sum <- renderText({
        round(sum(fitted[["matrix"]]$residuals, na.rm = TRUE), -1)
      })
      output$sum_matches <- renderText({
        if (input$group_by == "1") return("The model values have no variation.")
        if (input$var_measure %in% c("variance", "sum of squares")) "Components add to whole!"
        else "The whole is not the sum of the components."
      })
      
      output$resid_sq_sum <- renderText({
        format(round(sum(fitted[["matrix"]]$residuals^2, na.rm = TRUE), -3), big.mark = ",")
      })
      
      observe({
        form <- as.formula(fill_template("{{y}} ~ {{x}}", x = req(input$group_by), y = response_var))
        res <- draw_groupwise_mod(data, form, seed = 101)
        fitted$plot <- res$plot
        fitted$matrix <- res$matrix
      })
      output$plot1 <- renderPlot( 
        fitted$plot
      )
      output$resid_sum <- renderText({})
    }
  )
}
