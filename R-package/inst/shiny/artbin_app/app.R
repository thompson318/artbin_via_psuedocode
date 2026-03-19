# artbin Shiny Application
# Recreates the functionality of artbin.dlg (Stata dialog box)
# Requires: shiny, shinyWidgets, bslib

library(shiny)
library(shinyWidgets)
library(artbin)

# ---- UI ----
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),

  titlePanel("artbin — Sample Size and Power for Binary Trials"),
  p("ART (Analysis of Resources for Trials) — Binary Outcomes"),
  p("MRC Clinical Trials Unit at UCL"),
  hr(),

  sidebarLayout(
    sidebarPanel(
      width = 4,

      h4("Event Probabilities"),
      textInput("pr", "Anticipated probabilities pr() (space-separated)",
                value = "0.1 0.2",
                placeholder = "e.g. 0.1 0.2  or  0.1 0.2 0.3 0.4"),
      helpText("First value is control group. Additional values are treatment groups."),

      hr(),
      h4("Trial Type"),
      numericInput("margin", "Margin (0 = superiority)", value = 0, step = 0.01),
      radioButtons("outcome", "Outcome direction",
                   choices = c("Infer automatically" = "auto",
                               "Favourable (higher = better)" = "fav",
                               "Unfavourable (lower = better)" = "unfav"),
                   selected = "auto"),

      hr(),
      h4("Power & Sample Size"),
      radioButtons("calc_mode", "Calculate",
                   choices = c("Sample size (given power)" = "ss",
                               "Power (given n)" = "power"),
                   selected = "ss"),
      conditionalPanel(
        condition = "input.calc_mode == 'ss'",
        numericInput("power", "Desired power", value = 0.8, min = 0.01, max = 0.99, step = 0.05)
      ),
      conditionalPanel(
        condition = "input.calc_mode == 'power'",
        numericInput("n_given", "Total sample size n", value = 200, min = 2, step = 1)
      ),
      numericInput("alpha", "Significance level (alpha)", value = 0.05,
                   min = 0.001, max = 0.5, step = 0.005),
      checkboxInput("onesided", "One-sided test", value = FALSE),

      hr(),
      h4("Allocation & LTFU"),
      textInput("aratios", "Allocation ratios (space-separated, optional)",
                value = "",
                placeholder = "e.g.  1 2  for 1:2"),
      numericInput("ltfu", "Loss to follow-up proportion (0–<1, optional)",
                   value = 0, min = 0, max = 0.99, step = 0.01),

      hr(),
      h4("Statistical Test"),
      radioButtons("test_type", "Test",
                   choices = c("Score test (default)" = "score",
                               "Wald test" = "wald",
                               "Conditional test (Peto's OR)" = "condit",
                               "Linear trend test" = "trend"),
                   selected = "score"),
      conditionalPanel(
        condition = "input.test_type == 'trend'",
        textInput("doses", "Doses (optional, space-separated)", value = "",
                  placeholder = "e.g. 0 1 4")
      ),

      hr(),
      h4("Method Options"),
      selectInput("nvmethod", "Null variance method (2-arm only)",
                  choices = c("1 — Sample estimates" = "1",
                              "2 — Fixed marginal totals" = "2",
                              "3 — Constrained ML (default)" = "3"),
                  selected = "3"),
      checkboxInput("local", "Local alternatives", value = FALSE),
      checkboxInput("ccorrect", "Continuity correction", value = FALSE),
      checkboxInput("noround", "Do not round sample sizes", value = FALSE),

      hr(),
      actionButton("run", "Calculate", class = "btn-primary btn-lg",
                   style = "width: 100%;")
    ),

    mainPanel(
      width = 8,
      uiOutput("results_ui"),
      hr(),
      h5("R Code"),
      verbatimTextOutput("r_code")
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {

  result <- eventReactive(input$run, {
    # Parse pr
    pr_vals <- tryCatch(
      as.numeric(strsplit(trimws(input$pr), "\\s+")[[1]]),
      error = function(e) NULL
    )
    if (is.null(pr_vals) || any(is.na(pr_vals)))
      return(list(error = "Invalid pr() values. Please enter space-separated numbers."))

    # Parse aratios
    aratios_val <- NULL
    if (nzchar(trimws(input$aratios))) {
      aratios_val <- tryCatch(
        as.numeric(strsplit(trimws(input$aratios), "\\s+")[[1]]),
        error = function(e) NULL
      )
    }

    # Parse doses
    doses_val <- NULL
    if (nzchar(trimws(input$doses))) {
      doses_val <- tryCatch(
        as.numeric(strsplit(trimws(input$doses), "\\s+")[[1]]),
        error = function(e) NULL
      )
    }

    # ltfu: 0 means none
    ltfu_val <- if (input$ltfu > 0) input$ltfu else NULL
    margin_val <- if (input$margin == 0) NULL else input$margin
    n_val    <- if (input$calc_mode == "power") input$n_given else NULL
    power_val <- if (input$calc_mode == "ss") input$power else NULL

    favourable_val <- switch(input$outcome,
      auto = NULL, fav = TRUE, unfav = FALSE)

    wald_val   <- input$test_type == "wald"
    condit_val <- input$test_type == "condit"
    trend_val  <- input$test_type == "trend"
    nvm_val    <- as.integer(input$nvmethod)

    tryCatch(
      artbin(
        pr         = pr_vals,
        margin     = margin_val,
        alpha      = input$alpha,
        power      = power_val,
        n          = n_val,
        aratios    = aratios_val,
        ltfu       = ltfu_val,
        onesided   = input$onesided,
        favourable = favourable_val,
        condit     = condit_val,
        local      = input$local,
        trend      = trend_val,
        doses      = doses_val,
        nvmethod   = nvm_val,
        wald       = wald_val,
        ccorrect   = input$ccorrect,
        noround    = input$noround
      ),
      error = function(e) list(error = conditionMessage(e))
    )
  })

  output$results_ui <- renderUI({
    res <- result()
    if (is.null(res)) return(p("Press Calculate to run."))
    if (!is.null(res$error)) {
      return(div(class = "alert alert-danger", res$error))
    }

    npr <- sum(startsWith(names(res), "n") & nchar(names(res)) == 2)
    n_rows <- lapply(seq_len(npr), function(a) {
      tags$tr(
        tags$td(paste0("Group ", a)),
        tags$td(format(res[[paste0("n", a)]], big.mark = ",")),
        tags$td(format(round(res[[paste0("D", a)]], 2), big.mark = ","))
      )
    })

    tagList(
      h4("Results"),
      div(class = "alert alert-success",
          strong("Total sample size: "), format(res$n, big.mark = ","), " &nbsp;&nbsp; ",
          strong("Power: "), sprintf("%.4f", res$power), " &nbsp;&nbsp; ",
          strong("Total events: "), sprintf("%.2f", res$D)
      ),
      tags$table(
        class = "table table-bordered table-sm",
        tags$thead(tags$tr(
          tags$th("Group"), tags$th("Sample size"), tags$th("Events")
        )),
        tags$tbody(n_rows)
      )
    )
  })

  output$r_code <- renderText({
    req(input$run > 0)

    pr_str <- paste(trimws(input$pr))
    args <- c(
      sprintf("pr = c(%s)", gsub("\\s+", ", ", pr_str))
    )
    if (input$margin != 0) args <- c(args, sprintf("margin = %s", input$margin))
    if (input$alpha != 0.05) args <- c(args, sprintf("alpha = %s", input$alpha))
    if (input$calc_mode == "ss" && input$power != 0.8)
      args <- c(args, sprintf("power = %s", input$power))
    if (input$calc_mode == "power")
      args <- c(args, sprintf("n = %s", input$n_given))
    if (nzchar(trimws(input$aratios)))
      args <- c(args, sprintf("aratios = c(%s)", gsub("\\s+", ", ", trimws(input$aratios))))
    if (input$ltfu > 0) args <- c(args, sprintf("ltfu = %s", input$ltfu))
    if (input$onesided) args <- c(args, "onesided = TRUE")
    if (input$outcome == "fav") args <- c(args, "favourable = TRUE")
    if (input$outcome == "unfav") args <- c(args, "favourable = FALSE")
    if (input$test_type == "wald") args <- c(args, "wald = TRUE")
    if (input$test_type == "condit") args <- c(args, "condit = TRUE")
    if (input$test_type == "trend") args <- c(args, "trend = TRUE")
    if (nzchar(trimws(input$doses)))
      args <- c(args, sprintf("doses = c(%s)", gsub("\\s+", ", ", trimws(input$doses))))
    if (input$nvmethod != "3") args <- c(args, sprintf("nvmethod = %s", input$nvmethod))
    if (input$local) args <- c(args, "local = TRUE")
    if (input$ccorrect) args <- c(args, "ccorrect = TRUE")
    if (input$noround) args <- c(args, "noround = TRUE")

    paste0("artbin(\n  ", paste(args, collapse = ",\n  "), "\n)")
  })
}

shinyApp(ui = ui, server = server)
