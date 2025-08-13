#' Interactive diagnostic dashboard
#'
#' Launch a Shiny-based dashboard for running heteroscedasticity tests
#' and viewing diagnostic plots and remediation suggestions.
#'
#' @param model Fitted \code{lm} model.
#' @param data Data frame used to fit the model.
#' @return A \code{shiny.appobj} that can be run with \code{shiny::runApp()}.
#' @examples
#' \dontrun{
#' data(mtcars)
#' mod <- lm(mpg ~ wt + hp, data = mtcars)
#' launchDiagnosticDashboard(mod, mtcars)
#' }
#' @export
launchDiagnosticDashboard <- function(model, data) {
  if (!requireNamespace("shiny", quietly = TRUE) ||
    !requireNamespace("DT", quietly = TRUE)) {
    stop("Install 'shiny' and 'DT' packages for interactive dashboard")
  }

  ui <- shiny::fluidPage(
    shiny::titlePanel("Heteroscedasticity Diagnostics Dashboard"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::checkboxGroupInput(
          "tests", "Select Tests:",
          choices = .test_factory$get_available(),
          selected = c("white", "breusch_pagan")
        ),
        shiny::numericInput(
          "alpha", "Significance Level:",
          value = 0.05, min = 0.01, max = 0.10, step = 0.01
        ),
        shiny::actionButton("run_tests", "Run Tests", class = "btn-primary")
      ),
      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel("Test Results", DT::DTOutput("test_table")),
          shiny::tabPanel("Diagnostic Plots", shiny::plotOutput("plots")),
          shiny::tabPanel("Remediation", shiny::verbatimTextOutput("suggestions"))
        )
      )
    )
  )

  server <- function(input, output, session) {
    test_results <- shiny::eventReactive(input$run_tests, {
      runHeteroTests(model, data, tests = input$tests)
    })

    output$test_table <- DT::renderDT({
      req(test_results())
      results_df <- data.frame(
        Test = names(test_results()),
        Statistic = sapply(test_results(), function(x) round(x$statistic, 4)),
        P_Value = sapply(test_results(), function(x) round(x$p.value, 4)),
        Significant = sapply(test_results(), function(x) x$p.value < input$alpha)
      )
      DT::datatable(results_df, options = list(pageLength = 10))
    })

    output$plots <- shiny::renderPlot({
      plots <- plotDiagnosticSuiteEnhanced(model)
      gridExtra::grid.arrange(grobs = plots[1:4], ncol = 2)
    })

    output$suggestions <- shiny::renderText({
      req(test_results())
      suggestions <- suggestRemediation(test_results())
      paste(capture.output(str(suggestions)), collapse = "\n")
    })
  }

  shiny::shinyApp(ui = ui, server = server)
}
