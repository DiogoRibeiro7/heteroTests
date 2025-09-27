#' Interactive diagnostic dashboard
#'
#' Launch a production-ready Shiny dashboard for running heteroscedasticity
#' diagnostics, exploring interactive plots, and experimenting with
#' simulation studies.
#'
#' @param model Fitted \code{lm} model.
#' @param data Data frame used to fit the model.
#' @return A \code{shiny.appobj} that can be run with \code{shiny::runApp()}.
#' @details Requires optional packages \pkg{shiny}, \pkg{DT}, and \pkg{plotly}.
#'   Excel uploads additionally depend on \pkg{readxl} and exporting
#'   interactive graphics relies on \pkg{htmlwidgets}. The dashboard guides
#'   users through data preparation, model fitting, diagnostic testing,
#'   interactive Plotly visualisations, and a real-time simulation lab for
#'   exploring heteroscedastic data-generating processes. Download buttons are
#'   provided for both results tables and interactive plots.
#' @examples
#' \dontrun{
#' data(mtcars)
#' mod <- lm(mpg ~ wt + hp, data = mtcars)
#' launchDiagnosticDashboard(mod, mtcars)
#' }
#' @export
launchDiagnosticDashboard <- function(model, data) {
  required_pkgs <- c("shiny", "DT", "plotly")
  missing_pkgs <- required_pkgs[!vapply(
    required_pkgs,
    requireNamespace,
    FUN.VALUE = logical(1),
    quietly = TRUE
  )]
  if (length(missing_pkgs) > 0) {
    stop(
      sprintf(
        "The following packages are required for the dashboard: %s.",
        paste(missing_pkgs, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (!inherits(model, "lm")) {
    stop("`model` must be a fitted 'lm' object.", call. = FALSE)
  }
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame used to fit the model.", call. = FALSE)
  }
  if (nrow(data) == 0L) {
    stop("`data` must contain at least one observation.", call. = FALSE)
  }

  available_tests <- .test_factory$get_available()
  default_tests <- intersect(c("white", "breusch_pagan"), available_tests)
  if (length(default_tests) == 0) {
    default_tests <- head(available_tests, 3L)
  }

  formula_vars <- tryCatch(stats::all.vars(stats::formula(model)), error = function(e) NULL)
  default_response <- if (!is.null(formula_vars)) formula_vars[1] else names(data)[1]
  default_predictors <- if (!is.null(formula_vars)) formula_vars[-1] else names(data)[-1]
  default_predictors <- default_predictors[default_predictors %in% names(data)]

  initial_choices <- names(data)
  if (is.null(initial_choices) || !length(initial_choices)) {
    stop("`data` must have named columns to build the dashboard controls.", call. = FALSE)
  }

  plot_labels <- c(
    residuals_fitted = "Residuals vs Fitted",
    spread_level = "Spread-Level Plot",
    density = "Residual Density",
    qq = "Normal Q-Q",
    bubble_variance = "Variance Bubble Plot"
  )

  plot_tab_panels <- lapply(names(plot_labels), function(name) {
    shiny::tabPanel(
      title = plot_labels[[name]],
      plotly::plotlyOutput(paste0("plot_", name), height = "500px")
    )
  })

  make_results_df <- function(results, alpha) {
    if (is.null(results) || !length(results)) {
      return(NULL)
    }
    stats <- lapply(results, function(x) {
      statistic <- x$statistic
      if (length(statistic) > 1) {
        statistic <- statistic[[1]]
      }
      if (length(statistic) == 0) {
        statistic <- NA_real_
      }
      c(statistic = unname(statistic), p_value = x$p.value)
    })
    df <- data.frame(
      Test = names(results),
      Statistic = vapply(stats, function(x) x[["statistic"]], numeric(1)),
      P_Value = vapply(stats, function(x) x[["p_value"]], numeric(1)),
      stringsAsFactors = FALSE
    )
    df$Significant <- df$P_Value < alpha
    df
  }

  get_sigma_function <- function(pattern) {
    switch(pattern,
      "Linear" = sigma_linear,
      "Exponential" = sigma_exponential,
      "Group shift" = sigma_group,
      "Piecewise" = sigma_piecewise,
      "Sinusoidal" = sigma_sin,
      "Polynomial" = sigma_poly,
      "Multiplicative" = function(x) sigma_multiplicative(x, function(z) 0.5 + z, p = 1),
      "U-shaped" = sigma_u_shape,
      "Logistic" = sigma_logistic,
      "Inverse" = sigma_inverse,
      "Power" = sigma_power,
      "Threshold" = sigma_step,
      "Gaussian peak" = sigma_gaussian_peak,
      sigma_linear
    )
  }

  simulate_dataset <- function(n, beta0, beta1, pattern, strength, seed = NULL) {
    if (!is.null(seed) && !is.na(seed)) {
      set.seed(seed)
    }
    x <- stats::runif(n, min = 0, max = 10)
    sigma_fun <- get_sigma_function(pattern)
    base_sigma <- sigma_fun(x)
    sigma <- pmax(1e-06, strength * base_sigma)
    eps <- stats::rnorm(n)
    y <- beta0 + beta1 * x + sigma * eps
    data.frame(x = x, y = y, sigma = sigma)
  }

  ui <- shiny::fluidPage(
    shiny::titlePanel("Heteroscedasticity Diagnostics Dashboard"),
    shiny::tabsetPanel(
      id = "dashboard_tabs",
      shiny::tabPanel(
        title = "Guided Workflow",
        shiny::fluidRow(
          shiny::column(
            width = 8,
            shiny::wellPanel(
              shiny::h4("Getting started"),
              shiny::p(
                "Follow the numbered steps to load data, fit your model, and",
                "run a suite of heteroscedasticity diagnostics."
              ),
              shiny::tags$ol(
                shiny::tags$li("Load data or upload a new dataset."),
                shiny::tags$li("Specify the response and predictors then fit the model."),
                shiny::tags$li("Choose diagnostics to run and review the comparison tables."),
                shiny::tags$li("Explore interactive plots and iterate using the simulation lab."),
                shiny::tags$li("Export tables or plots to document your findings.")
              ),
              shiny::actionButton(
                "go_to_data", "Start with Step 1", class = "btn-primary"
              )
            )
          ),
          shiny::column(
            width = 4,
            shiny::wellPanel(
              shiny::h5("Current model summary"),
              shiny::verbatimTextOutput("guided_model_summary")
            )
          )
        )
      ),
      shiny::tabPanel(
        title = "Data & Model",
        shiny::fluidRow(
          shiny::column(
            width = 4,
            shiny::wellPanel(
              shiny::radioButtons(
                "data_source", "Data source", inline = TRUE,
                choices = c("Provided" = "provided", "Upload CSV/Excel" = "upload"),
                selected = "provided"
              ),
              shiny::conditionalPanel(
                condition = "input.data_source == 'upload'",
                shiny::fileInput(
                  "uploaded_file", "Upload file", accept = c(
                    ".csv", ".tsv", ".txt", ".xls", ".xlsx"
                  )
                ),
                shiny::helpText(
                  "CSV/TSV files are read with base R. Excel uploads require the",
                  "optional readxl package and use the first worksheet."
                )
              ),
              shiny::numericInput(
                "preview_rows", "Rows to preview", value = 10, min = 5, max = 100
              ),
              shiny::selectInput(
                "response_var", "Response variable",
                choices = initial_choices, selected = default_response
              ),
              shiny::selectizeInput(
                "predictor_vars", "Predictor variables",
                choices = initial_choices,
                selected = default_predictors,
                multiple = TRUE
              ),
              shiny::actionButton(
                "fit_model", "Fit model", class = "btn-success"
              ),
              shiny::helpText(
                "The selected variables define the formula",
                shiny::textOutput("model_formula_display", inline = TRUE)
              )
            )
          ),
          shiny::column(
            width = 8,
            shiny::tabsetPanel(
              shiny::tabPanel(
                "Data preview",
                DT::DTOutput("data_preview")
              ),
              shiny::tabPanel(
                "Model diagnostics",
                shiny::verbatimTextOutput("model_summary")
              )
            )
          )
        )
      ),
      shiny::tabPanel(
        title = "Diagnostics",
        shiny::fluidRow(
          shiny::column(
            width = 4,
            shiny::wellPanel(
              shiny::checkboxGroupInput(
                "tests", "Select diagnostics",
                choices = available_tests,
                selected = default_tests
              ),
              shiny::numericInput(
                "alpha", "Significance level",
                value = 0.05, min = 0.001, max = 0.20, step = 0.01
              ),
              shiny::actionButton(
                "run_tests", "Run diagnostics", class = "btn-primary"
              ),
              shiny::downloadButton(
                "download_results", "Download results (CSV)", class = "btn-secondary"
              )
            )
          ),
          shiny::column(
            width = 8,
            shiny::tabsetPanel(
              shiny::tabPanel(
                "Results table",
                DT::DTOutput("test_table")
              ),
              shiny::tabPanel(
                "Remediation guidance",
                shiny::verbatimTextOutput("suggestions")
              )
            )
          )
        )
      ),
      shiny::tabPanel(
        title = "Interactive plots",
        shiny::fluidRow(
          shiny::column(
            width = 4,
            shiny::wellPanel(
              shiny::selectInput(
                "plot_selection", "Choose diagnostic plot",
                choices = stats::setNames(names(plot_labels), plot_labels)
              ),
              shiny::downloadButton(
                "download_plot", "Download plot (HTML)", class = "btn-secondary"
              ),
              shiny::helpText(
                "Plots leverage plotly for hover details, zooming, and",
                "interactive legends."
              )
            )
          ),
          shiny::column(
            width = 8,
            do.call(shiny::tabsetPanel, c(list(id = "plot_tabs"), plot_tab_panels))
          )
        )
      ),
      shiny::tabPanel(
        title = "Simulation lab",
        shiny::fluidRow(
          shiny::column(
            width = 4,
            shiny::wellPanel(
              shiny::sliderInput(
                "sim_n", "Sample size", min = 100, max = 5000, value = 500, step = 50
              ),
              shiny::selectInput(
                "sim_pattern", "Variance pattern",
                choices = c(
                  "Linear", "Exponential", "Group shift", "Piecewise",
                  "Sinusoidal", "Polynomial", "Multiplicative", "U-shaped",
                  "Logistic", "Inverse", "Power", "Threshold", "Gaussian peak"
                ),
                selected = "Linear"
              ),
              shiny::sliderInput(
                "sim_strength", "Heteroscedastic strength",
                min = 0.1, max = 5, value = 1, step = 0.1
              ),
              shiny::numericInput("sim_beta0", "Beta0", value = 0),
              shiny::numericInput("sim_beta1", "Beta1", value = 1),
              shiny::numericInput("sim_seed", "Seed", value = 123),
              shiny::checkboxGroupInput(
                "sim_tests", "Diagnostics to evaluate",
                choices = available_tests,
                selected = default_tests
              ),
              shiny::helpText(
                "Results refresh automatically as parameters change."
              )
            )
          ),
          shiny::column(
            width = 8,
            plotly::plotlyOutput("simulation_plot", height = "400px"),
            DT::DTOutput("simulation_results")
          )
        )
      ),
      shiny::tabPanel(
        title = "Comparison & export",
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::wellPanel(
              shiny::p(
                "Compare diagnostics obtained from the observed data and",
                "the current simulation configuration."
              ),
              shiny::downloadButton(
                "download_comparison", "Download comparison (CSV)",
                class = "btn-secondary"
              )
            ),
            DT::DTOutput("comparison_table")
          )
        )
      )
    )
  )

  server <- function(input, output, session) {
    data_store <- shiny::reactiveVal(data)
    model_store <- shiny::reactiveVal(model)
    results_store <- shiny::reactiveVal(NULL)
    simulation_store <- shiny::reactiveVal(NULL)

    preview_rows <- function() {
      rows <- input$preview_rows
      if (is.null(rows) || is.na(rows)) {
        return(10L)
      }
      rows <- as.integer(rows)
      if (is.na(rows) || rows < 1L) {
        return(10L)
      }
      rows
    }

    output$model_formula_display <- shiny::renderText({
      predictors <- input$predictor_vars
      response <- input$response_var
      if (!length(predictors)) {
        return(paste0(response, " ~ 1"))
      }
      paste(response, "~", paste(predictors, collapse = " + "))
    })

    output$guided_model_summary <- shiny::renderPrint({
      shiny::req(model_store())
      stats::summary(model_store())$coefficients
    })

    shiny::observeEvent(input$go_to_data, {
      shiny::updateTabsetPanel(session, "dashboard_tabs", selected = "Data & Model")
    })

    shiny::observeEvent(input$uploaded_file, {
      shiny::req(input$uploaded_file)
      ext <- tools::file_ext(input$uploaded_file$name)
      data_attempt <- tryCatch({
        if (ext %in% c("csv", "txt")) {
          utils::read.csv(input$uploaded_file$datapath, stringsAsFactors = FALSE)
        } else if (ext == "tsv") {
          utils::read.delim(input$uploaded_file$datapath, stringsAsFactors = FALSE)
        } else if (ext %in% c("xls", "xlsx")) {
          if (!requireNamespace("readxl", quietly = TRUE)) {
            stop("The readxl package is required to import Excel files.")
          }
          readxl::read_excel(input$uploaded_file$datapath, .name_repair = "unique")
        } else {
          stop("Unsupported file type: ", ext)
        }
      }, error = function(e) {
        shiny::showNotification(
          paste("Failed to import data:", e$message), type = "error"
        )
        NULL
      })
      if (!is.null(data_attempt)) {
        if (!is.data.frame(data_attempt)) {
          shiny::showNotification(
            "Imported object is not a data frame.", type = "error"
          )
        } else {
          data_store(data_attempt)
          shiny::updateSelectInput(
            session, "response_var",
            choices = names(data_attempt), selected = names(data_attempt)[1]
          )
          shiny::updateSelectizeInput(
            session, "predictor_vars",
            choices = names(data_attempt), selected = names(data_attempt)[-1]
          )
          shiny::showNotification("Data uploaded successfully.", type = "message")
        }
      }
    })

    output$data_preview <- DT::renderDT({
      shiny::req(data_store())
      preview <- head(data_store(), preview_rows())
      dt <- DT::datatable(
        preview,
        options = list(
          pageLength = preview_rows(),
          lengthChange = FALSE,
          searching = FALSE
        )
      )
      dt
    })

    output$model_summary <- shiny::renderPrint({
      shiny::req(model_store())
      stats::summary(model_store())
    })

    shiny::observeEvent(input$fit_model, {
      shiny::req(data_store())
      predictors <- input$predictor_vars
      response <- input$response_var
      if (!length(response) || response == "") {
        shiny::showNotification("Select a response variable before fitting.", type = "error")
        return()
      }
      data_current <- data_store()
      missing_vars <- setdiff(c(response, predictors), names(data_current))
      if (length(missing_vars)) {
        shiny::showNotification(
          paste("The dataset is missing variables:", paste(missing_vars, collapse = ", ")), type = "error"
        )
        return()
      }
      formula_text <- if (!length(predictors)) {
        paste0(response, " ~ 1")
      } else {
        paste(response, "~", paste(predictors, collapse = " + "))
      }
      new_model <- tryCatch({
        stats::lm(stats::as.formula(formula_text), data = data_current)
      }, error = function(e) {
        shiny::showNotification(
          paste("Model fitting failed:", e$message), type = "error"
        )
        NULL
      })
      if (!is.null(new_model)) {
        model_store(new_model)
        shiny::showNotification("Model fitted successfully.", type = "message")
      }
    })

    shiny::observeEvent(input$run_tests, {
      shiny::req(model_store())
      tests <- input$tests
      if (!length(tests)) {
        shiny::showNotification("Select at least one diagnostic test.", type = "error")
        return()
      }
      test_output <- tryCatch({
        runHeteroTests(model_store(), data_store(), tests = tests)
      }, error = function(e) {
        shiny::showNotification(
          paste("Failed to run diagnostics:", e$message), type = "error"
        )
        NULL
      })
      if (!is.null(test_output)) {
        results_store(test_output)
      }
    })

    output$test_table <- DT::renderDT({
      shiny::req(results_store())
      df <- make_results_df(results_store(), input$alpha)
      DT::datatable(
        df,
        options = list(pageLength = min(10, nrow(df))),
        rownames = FALSE
      ) %>%
        DT::formatRound(columns = c("Statistic", "P_Value"), digits = 4)
    })

    output$suggestions <- shiny::renderPrint({
      shiny::req(results_store())
      suggestRemediation(results_store())
    })

    diagnostic_plots <- shiny::reactive({
      shiny::req(model_store())
      plotDiagnosticSuiteEnhanced(model_store())
    })

    lapply(names(plot_labels), function(name) {
      output[[paste0("plot_", name)]] <- plotly::renderPlotly({
        shiny::req(diagnostic_plots()[[name]])
        plotly::ggplotly(diagnostic_plots()[[name]], tooltip = c("x", "y", "colour"))
      })
    })

    download_plot_data <- shiny::reactive({
      shiny::req(diagnostic_plots())
      diagnostic_plots()[[input$plot_selection]]
    })

    output$download_plot <- shiny::downloadHandler(
      filename = function() {
        selected <- input$plot_selection
        label <- plot_labels[[selected]]
        slug <- gsub("[^a-zA-Z0-9]+", "-", tolower(label))
        paste0("hetero-diagnostic-", slug, ".html")
      },
      content = function(file) {
        shiny::req(download_plot_data())
        if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
          stop("The htmlwidgets package is required to export interactive plots.")
        }
        widget <- plotly::ggplotly(download_plot_data())
        htmlwidgets::saveWidget(widget, file)
      }
    )

    output$download_results <- shiny::downloadHandler(
      filename = function() {
        paste0("hetero-diagnostics-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv")
      },
      content = function(file) {
        shiny::req(results_store())
        utils::write.csv(make_results_df(results_store(), input$alpha), file, row.names = FALSE)
      }
    )

    simulation_data <- shiny::reactive({
      shiny::req(input$sim_n, input$sim_strength)
      simulate_dataset(
        n = input$sim_n,
        beta0 = input$sim_beta0,
        beta1 = input$sim_beta1,
        pattern = input$sim_pattern,
        strength = input$sim_strength,
        seed = input$sim_seed
      )
    })

    simulation_model <- shiny::reactive({
      shiny::req(simulation_data())
      stats::lm(y ~ x, data = simulation_data())
    })

    shiny::observe({
      shiny::req(simulation_model())
      tests <- input$sim_tests
      if (!length(tests)) {
        simulation_store(NULL)
        return()
      }
      results <- tryCatch({
        runHeteroTests(simulation_model(), simulation_data(), tests = tests)
      }, error = function(e) {
        shiny::showNotification(
          paste("Simulation diagnostics failed:", e$message), type = "error"
        )
        NULL
      })
      simulation_store(results)
    })

    output$simulation_plot <- plotly::renderPlotly({
      shiny::req(simulation_data())
      df <- simulation_data()
      plotly::plot_ly(
        df,
        x = ~x,
        y = ~y,
        text = ~sprintf("x: %.2f<br>y: %.2f<br>sigma: %.2f", x, y, sigma),
        type = "scatter",
        mode = "markers",
        color = ~sigma,
        colors = "Viridis",
        hoverinfo = "text",
        marker = list(colorbar = list(title = "Sigma"))
      ) %>%
        plotly::layout(
          title = "Simulated response with heteroscedastic variance",
          xaxis = list(title = "Predictor"),
          yaxis = list(title = "Response")
        )
    })

    output$simulation_results <- DT::renderDT({
      shiny::req(simulation_store())
      df <- make_results_df(simulation_store(), input$alpha)
      df$Dataset <- "Simulated"
      df <- df[, c("Dataset", "Test", "Statistic", "P_Value", "Significant")]
      DT::datatable(
        df,
        options = list(pageLength = min(10, nrow(df))),
        rownames = FALSE
      ) %>%
        DT::formatRound(columns = c("Statistic", "P_Value"), digits = 4)
    })

    comparison_data <- shiny::reactive({
      obs <- results_store()
      sim <- simulation_store()
      alpha <- input$alpha
      pieces <- list()
      if (!is.null(obs)) {
        df <- make_results_df(obs, alpha)
        df$Dataset <- "Observed"
        pieces[[length(pieces) + 1]] <- df
      }
      if (!is.null(sim)) {
        df <- make_results_df(sim, alpha)
        df$Dataset <- "Simulated"
        pieces[[length(pieces) + 1]] <- df
      }
      if (!length(pieces)) {
        return(NULL)
      }
      combined <- do.call(rbind, pieces)
      combined[, c("Dataset", "Test", "Statistic", "P_Value", "Significant")]
    })

    output$comparison_table <- DT::renderDT({
      shiny::req(comparison_data())
      DT::datatable(
        comparison_data(),
        options = list(pageLength = min(10, nrow(comparison_data()))),
        rownames = FALSE
      ) %>%
        DT::formatRound(columns = c("Statistic", "P_Value"), digits = 4)
    })

    output$download_comparison <- shiny::downloadHandler(
      filename = function() {
        paste0("hetero-comparison-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv")
      },
      content = function(file) {
        shiny::req(comparison_data())
        utils::write.csv(comparison_data(), file, row.names = FALSE)
      }
    )
  }

  shiny::shinyApp(ui = ui, server = server)
}
