library(shiny)
library(dagitty)
library(ggplot2)
library(tidyr)
library(knitr)
library(DT)
library(dplyr)
library(bslib)
library(shinyBS)
library(shinyjs)


ui <- fluidPage(
  useShinyjs(),
  titlePanel("A Simulation of Bias in Listwise Deletion with Missing Data"),
  theme = bs_theme(
    version = 5,                  
    preset = "simplex",    
    primary = "#6c757d",
    base_font = font_google("Inter")),
  tags$style(HTML("
    body {
      font-family: 'Helvetica Neue', sans-serif;
      background-color: #f9f9f9;
    }
    .shiny-input-container {
      margin-bottom: 15px;
    }
    h2 {
      color: #37474F;
    }
  ")),
  sidebarLayout(
    sidebarPanel(
      accordion(
        id = "sidebar-accordion", open = c("notation", "simulation"),
        accordion_panel("📓 Simulation Guide", value = "notation",
                        tags$ul(
                          tags$li("X: independent variable"),
                          tags$li("Y: dependent variable"),
                          tags$li("R_X: missingness indicator for X"),
                          tags$li("R_Y: missingness indicator for Y"),
                          tags$li("U_Y: confounder (third variable) between R_Y and Y")
                        ),
                        tags$ul(
                          tags$li("Number of iterations: 100"),
                          tags$li("Sample size: 50,000 per iteration")
                        ),
                        tags$p("Note: A bidirected arrow between X and Y was excluded to rule out the possibility of omitted variable bias, which is beyond the scope of this simulation.")
        ),
        accordion_panel("⚙️ Missing Data Proportion Setting", value = "simulation",
                        radioButtons("missingDirectionX", "Missing Mechanism Threshold of X:", 
                                     choices = c("Top" = "top", "Bottom" = "bottom"),
                                     selected = "top"),
                        radioButtons("missingDirectionY", "Missing Mechanism Threshold of Y:", 
                                     choices = c("Top" = "top", "Bottom" = "bottom"),
                                     selected = "top"),
                        sliderInput("missingRateX", "Proportion of Missing Data of X", 
                                    min = 0, max = 0.5, value = 0.2, step = 0.05),
                        sliderInput("missingRateY", "Proportion of Missing Data of Y", 
                                    min = 0, max = 0.5, value = 0.2, step = 0.05)
        ),
        
        accordion_panel("🔄 Path Coefficients", value = "paths",
                        sliderInput("focalpath", "Focal Path (X → Y)", min = -0.9, max = 0.9, value = 0.5),
                        sliderInput("path1", "Bidirected arrow (X ↔ R_X)", min = -0.9, max = 0.9, value = 0),
                        sliderInput("path3", "Bidirected arrow (Y ↔ R_Y)", min = -0.9, max = 0.9, value = 0),
                        sliderInput("path2", "Bidirected arrow (X ↔ R_Y)", min = -0.9, max = 0.9, value = 0),
                        sliderInput("path10", "Bidirected arrow (R_X ↔ R_Y)", min = -0.9, max = 0.9, value = 0),
                        sliderInput("path12", "Bidirected arrow (R_X ↔ Y)", min = -0.9, max = 0.9, value = 0),
                        sliderInput("path8", "Directed arrow (X → R_X)", min = -0.9, max = 0.9, value = 0),
                        sliderInput("path5", "Directed arrow (Y → R_Y)", min = -0.9, max = 0.9, value = 0),
                        sliderInput("path4", "Directed arrow (X → R_Y)", min = -0.9, max = 0.9, value = 0),
                        sliderInput("path9", "Directed arrow (R_X → R_Y)", min = -0.9, max = 0.9, value = 0),
                        sliderInput("path11", "Directed arrow (R_X → Y)", min = -0.9, max = 0.9, value = 0)
        ),
      actionButton("runSim", "Run Simulation")
    )),
    
    mainPanel(
      fluidRow(
        column(6,
               card(
                 card_header("DAGs"),
                 plotOutput("dagPlot")
                 )
               ),
        column(6,
               card(
                 card_header("D-Separation Criterion"),
                 checkboxInput("useInteraction", "Include X × U_Y interaction", value = FALSE),
                 uiOutput("interactionToggleNote"),
                 verbatimTextOutput("ciRxExtendedTable"), 
                 verbatimTextOutput("ciRyExtendedTable"),
                 verbatimTextOutput("finalBiasSummary"),
                 uiOutput("interactionWarning")
               )
        )
      ),
      fluidRow(
        column(6,
               card(
                 card_header("Intercept Plot"),
                 plotOutput("interceptPlot"),
                 full_screen = TRUE
               )
        ),
        column(6,
               card(
                 card_header("Slope Plot"),
                 plotOutput("slopePlot"),
                 full_screen = TRUE
               )
        )
      ),
        fluidRow(
        column(8,
               card(
                 card_header("Scatter Plot"),
                 plotOutput("Scatter"), 
                 full_screen = TRUE)
               ),
        column(4,
               card(
                 card_header("Regression Results"),
                 DT::dataTableOutput("regResults")
               )
        )
      )
    )
  )
)


server <- function(input, output) {
  ## Reactive DAG generation
  dagReactive <- reactive({
    edges <- list(
      "rx <-> x"       = input$path1 != 0,
      "ry <-> x"     = input$path2 != 0,
      "rx <-> ry"        = input$path10 != 0,
      "x -> y"         = input$focalpath != 0,
      "x -> ry"        = input$path4 != 0,
      "y -> ry"        = input$path5 != 0,
      "y <-> ry"      = input$path3 != 0,
      "x -> rx"        = input$path8 != 0,
      "rx -> ry"        = input$path9 != 0,
      "rx -> y"        = input$path11 != 0,
      "rx <-> y"        = input$path12 != 0
    )
    
    dag_lines <- c("dag {")
    for (edge in names(edges)) {
      if (edges[[edge]]) {
        dag_lines <- c(dag_lines, edge)
      }
    }
    dag_lines <- c(dag_lines, "}")
    g <- dagitty(paste(dag_lines, collapse = "\n"))
    
    coord_x <- c(x = 1, y = 3, rx = 1, ry = 3)
    coord_y <- c(x = 1, y = 1, rx = 3, ry = 3)
    dag_nodes <- names(g)
    coord_x <- coord_x[names(coord_x) %in% dag_nodes]
    coord_y <- coord_y[names(coord_y) %in% dag_nodes]
    coordinates(g) <- list(x = coord_x, y = coord_y)
    return(g)
  })
  
  output$dagPlot <- renderPlot({
    plot(dagReactive())
  })
  

  ## Conditional independence checks (updated in real time)
  biasInterp <- reactiveValues(rx = NULL, ry = NULL)
  
  output$ciRxExtendedTable <- renderPrint({
    g <- dagReactive()
    if (all(c("rx","ry") %in% names(g))) {
      cond1 <- dseparated(g, "y", "rx", c("x","ry"))
      cond2 <- !dseparated(g, "x", "rx", c("y","ry"))
      cond3 <- dseparated(g, "x", "rx", c("ry"))
      
      ci_rx_extended <- data.frame(
        Statement = c("Y ⫫ R_X | X, R_Y", "X ⫫̸ R_X | Y, R_Y", "X ⫫ R_X| R_Y"),
        Result = c(cond1, cond2, cond3)
      )
      
      # Bias interpretation
      interpretation_rx <- if (cond1) {
        "Intercept and Slope are unbiased."
      } else if (!cond1 && cond2 && cond3) {
        "Only Slope is unbiased."
      } else {
        "Both Intercept and Slope are biased."
      }
      
      biasInterp$rx <- c(cond1 = cond1, cond2 = cond2, cond3 = cond3)  # Save for summary
      
      cat("Conditions involving R_X:\n")
      print(kable(ci_rx_extended, format = "simple"))
      cat("\n→ Bias interpretation: ", interpretation_rx, "\n")
    }else if ("rx" %in% names(g)) {
      cond1 <- dseparated(g, "y", "rx", c("x"))
      cond2 <- !dseparated(g, "x", "rx", c("y"))
      cond3 <- dseparated(g, "x", "rx", c())
      
      ci_rx_extended <- data.frame(
        Statement = c("Y ⫫ R_X | X, R_Y", "X ⫫̸ R_X | Y, R_Y", "X ⫫ R_X| R_Y"),
        Result = c(cond1, cond2, cond3)
      )
      
      # Bias interpretation
      interpretation_rx <- if (cond1) {
        "Intercept and Slope are unbiased."
      } else if (!cond1 && cond2 && cond3) {
        "Only Slope is unbiased."
      } else {
        "Both Intercept and Slope are biased."
      }
      
      biasInterp$rx <- c(cond1 = cond1, cond2 = cond2, cond3 = cond3)  # Save for summary
      
      cat("Conditions involving R_X:\n")
      print(kable(ci_rx_extended, format = "simple"))
      cat("\n→ Bias interpretation: ", interpretation_rx, "\n")
    } else {
      cat("Node 'rx' does not exist in the DAG.\n")
    }
  })
  
  output$ciRyExtendedTable <- renderPrint({
    g <- dagReactive()
    if (all(c("rx","ry") %in% names(g))) {
      cond1_ry <- dseparated(g, "y", "ry", c("x","rx"))
      cond2_ry <- !dseparated(g, "x", "ry", c("y","rx"))
      cond3_ry <- dseparated(g, "x", "ry", c("rx"))
      
      ci_ry_extended <- data.frame(
        Statement = c("Y ⫫ R_Y | X, R_X", "X ⫫̸ R_Y | Y,R_X", "X ⫫ R_Y|R_X"),
        Result = c(cond1_ry, cond2_ry, cond3_ry)
      )
      
      # Bias interpretation
      interpretation_ry <- if (cond1_ry) {
        "Intercept and Slope are unbiased."
      } else if (!cond1_ry && cond2_ry && cond3_ry) {
        "Only Slope is unbiased."
      } else {
        "Both Intercept and Slope are biased."
      }
      
      biasInterp$ry <- c(cond1 = cond1_ry, cond2 = cond2_ry, cond3 = cond3_ry)
      # Save for summary
      
      cat("Conditions involving R_Y:\n")
      print(kable(ci_ry_extended, format = "simple"))
      cat("\n→ Bias interpretation: ", interpretation_ry, "\n")
    } else if ("ry" %in% names(g)){
      cond1_ry <- dseparated(g, "y", "ry", c("x"))
      cond2_ry <- !dseparated(g, "x", "ry", c("y"))
      cond3_ry <- dseparated(g, "x", "ry", c())
      
      ci_ry_extended <- data.frame(
        Statement = c("Y ⫫ R_Y | X, R_X", "X ⫫̸ R_Y | Y,R_X", "X ⫫ R_Y|R_X"),
        Result = c(cond1_ry, cond2_ry, cond3_ry)
      )
      
      # Bias interpretation
      interpretation_ry <- if (cond1_ry) {
        "Intercept and Slope are unbiased."
      } else if (!cond1_ry && cond2_ry && cond3_ry) {
        "Only Slope is unbiased."
      } else {
        "Both Intercept and Slope are biased."
      }
      
      biasInterp$ry <- c(cond1 = cond1_ry, cond2 = cond2_ry, cond3 = cond3_ry)
      # Save for summary
      
      cat("Conditions involving R_Y:\n")
      print(kable(ci_ry_extended, format = "simple"))
      cat("\n→ Bias interpretation: ", interpretation_ry, "\n")
    }else {
      cat("Node 'ry' does not exist in the DAG.\n")
    }
  })
  observeEvent(dagReactive(), {
    g <- dagReactive()
    
    if (all(c("rx","ry") %in% names(g))) {
      biasInterp$rx <- c(
        cond1 = dseparated(g, "y", "rx", c("x","ry")),
        cond2 = !dseparated(g, "x", "rx", c("y","ry")),
        cond3 = dseparated(g, "x", "rx", c("ry"))
      )
    } else if ("rx" %in% names(g)) {
      biasInterp$rx <- c(
        cond1 = dseparated(g, "y", "rx", c("x")),
        cond2 = !dseparated(g, "x", "rx", c("y")),
        cond3 = dseparated(g, "x", "rx", c())
      )
    } else {
      biasInterp$rx <- NULL
    }
    
    if (all(c("rx","ry") %in% names(g))) {
      biasInterp$ry <- c(
        cond1 = dseparated(g, "y", "ry", c("x","rx")),
        cond2 = !dseparated(g, "x", "ry", c("y","rx")),
        cond3 = dseparated(g, "x", "ry", c("rx"))
      )
    } else if ("ry" %in% names(g)) {
      biasInterp$ry <- c(
        cond1 = dseparated(g, "y", "ry", c("x")),
        cond2 = !dseparated(g, "x", "ry", c("y")),
        cond3 = dseparated(g, "x", "ry", c())
      )
    }else {
      biasInterp$ry <- NULL
    }
  })
  observe({
    if (input$path3 == 0 || input$focalpath == 0) {
      shinyjs::disable("useInteraction")
    } else {
      shinyjs::enable("useInteraction")
    }
  })
  output$interactionToggleNote <- renderUI({
    if (input$path3 == 0 || input$focalpath == 0) {
      tags$p("⚠️ Enable interaction only when both Y ↔ R_Y, and focal X → Y are non-zero.",
             style = "color: #cc0000; font-size: 0.9em;")
    } else {
      NULL
    }
  })
  
  output$interactionWarning <- renderUI({
    if (isTRUE(input$useInteraction)) {
      tags$p("⚠️ Interaction is included, so d-separation criteria do not hold.",
             style = "color: #cc0000; font-size: 0.9em; margin-top: 10px;")
    } else {
      NULL
    }
  })
  
  output$finalBiasSummary <- renderPrint({
    cat("=== FINAL BIAS SUMMARY ===\n")
   
    rx <- biasInterp$rx
    ry <- biasInterp$ry
    
    if (is.null(rx) && is.null(ry)) {
      cat("→ No missing data.\n")
      return(invisible())
    }
    
    if (!is.null(rx) && is.null(ry)) {
      if (rx[["cond1"]]) {
        cat("→ Intercept and Slope are unbiased.\n")
      } else if (!rx[["cond1"]] && rx[["cond2"]] && rx[["cond3"]]) {
        cat("→ Only Slope is unbiased.\n")
      } else {
        cat("→ Both Intercept and Slope are biased.\n")
      }
      return(invisible())
    }
    
    if (is.null(rx) && !is.null(ry)) {
      if (ry[["cond1"]]) {
        cat("→ Intercept and Slope are unbiased.\n")
      } else if (!ry[["cond1"]] && ry[["cond2"]] && ry[["cond3"]]) {
        cat("→ Only Slope is unbiased.\n")
      } else {
        cat("→ Both Intercept and Slope are biased.\n")
      }
      return(invisible())
    }
    if (rx[["cond1"]] && ry[["cond1"]]) {
      cat("→ Both intercept and slope are unbiased.\n")
    } else if (!rx[["cond1"]] && rx[["cond2"]] && rx[["cond3"]] &&
               !ry[["cond1"]] && ry[["cond2"]] && ry[["cond3"]]) {
      cat("→ Intercept is biased. Slope is unbiased.\n")
    } else if ((rx[["cond1"]] && !ry[["cond1"]] && ry[["cond2"]] && ry[["cond3"]]) ||
               (ry[["cond1"]] && !rx[["cond1"]] && rx[["cond2"]] && rx[["cond3"]])) {
      cat("→ Intercept is biased. Slope is unbiased.\n")
    } else {
      cat("→ Both intercept and slope are biased.\n")
    }
  })
  

  
  
  ## Run simulation when button is clicked
 
  observeEvent(input$runSim, {
    n <- 50000
    iter <- 100
    Result <- matrix(NA, nrow = iter, ncol = 4)
    
    for (i in 1:iter) {
      u_x <- rnorm(n)
      u_y <- rnorm(n)
      u_xry <- rnorm(n)
      u_rxy <- rnorm(n)
      u_rr <- rnorm(n)
      
      x <- input$path1 * 10 * u_x + 0.1* u_xry + rnorm(n, 0, 1)

      if (input$path8 != 0 || input$path1 != 0| input$path12 != 0 | input$path10 !=0) {
        rx <- input$path8 * x + 0.1 * u_x  + 0.1*u_rxy +0.1*u_rr
      } else {rx<-rnorm(n)}
      
      if (input$useInteraction == "TRUE" && input$path3 != 0 && input$focalpath != 0) {
        y <- input$focalpath * x + input$path3 * 10 * u_y  + input$path12*10*u_rxy+ input$path11*rx + 0.5 * x * u_y + rnorm(n, 0, 1)
      } else {
        y <- input$focalpath * x + input$path3 * 10 * u_y + input$path11*rx + input$path12*10*u_rxy+ rnorm(n, 0, 1)
      }
      
      
      if (input$path4 != 0 || input$path5 != 0 || input$path2 != 0 || input$path3 !=0 | input$path9 !=0 | input$path10 !=0)
        {ry <- input$path4 * x + input$path5 * y + input$path2 * 10 * u_xry + 0.1 * u_y + input$path9*rx + input$path10*10*u_rr}
      else {ry <- rnorm(n)}
      
      if (!is.null(ry)) {
        threshold_ry <- quantile(ry, probs = ifelse(input$missingDirectionY == "top", 1 - input$missingRateY, input$missingRateY), na.rm = TRUE)
        yobs <- if (input$missingDirectionY == "top") {
          ifelse(ry > threshold_ry, NA, y)
        } else {
          ifelse(ry < threshold_ry, NA, y)
        }
      } else {
        yobs <- y
      }
      
      if (!is.null(rx)) {
        threshold_rx <- quantile(rx, probs = ifelse(input$missingDirectionX == "top", 1 - input$missingRateX, input$missingRateX), na.rm = TRUE)
        xobs <- if (input$missingDirectionX == "top") {
          ifelse(rx > threshold_rx, NA, x)
        } else {
          ifelse(rx < threshold_rx, NA, x)
        }
      } else {
        xobs <- x
      }
      
      d1 <- data.frame(xobs, yobs)
      d1 <- d1[complete.cases(d1), ]
      d2 <- data.frame(x, y)
      
      Result[i, 1] <- tryCatch(coef(lm(yobs ~ xobs, data = d1))[1], error = function(e) NA)
      Result[i, 2] <- coef(lm(y ~ x))[1]
      Result[i, 3] <- tryCatch(coef(lm(yobs ~ xobs, data = d1))[2], error = function(e) NA)
      Result[i, 4] <- coef(lm(y ~ x))[2]
    }
    
    ## Format for plots
    Result_intercept <- pivot_longer(as.data.frame(Result[, 1:2]), everything(),
                                     names_to = "model", values_to = "value")|>
      mutate(model = recode(model, V1 = "List-wise", V2 = "Complete"))
    colnames(Result_intercept) <- c("model", "value")
    
    Result_slope <- pivot_longer(as.data.frame(Result[, 3:4]), everything(),
                                 names_to = "model", values_to = "value")|>
      mutate(model = recode(model, V1 = "List-wise", V2 = "Complete"))
    colnames(Result_slope) <- c("model", "value")
    
    ## t-tests
    ttest1 <- t.test(Result[, 1], mu = 0)
    ttest2 <- t.test(Result[, 3], mu = input$focalpath)
    ttest3 <- t.test(Result[, 2], mu = 0)
    ttest4 <- t.test(Result[, 4], mu = input$focalpath)
    
    test_results <- data.frame(
      Parameter = c("Intercept", "Slope"),
      Listwise = round(c(ttest1$estimate, ttest2$estimate), 3),
      Complete = round(c(ttest3$estimate, ttest4$estimate), 3)
    )
    
    output$regResults <- DT::renderDataTable({
      test_results <- data.frame(
        Parameter = c("Intercept", "Slope"),
        Listwise = round(c(ttest1$estimate, ttest2$estimate), 3),
        Complete = round(c(ttest3$estimate, ttest4$estimate), 3)
      )
      
      DT::datatable(
        test_results,
        rownames = FALSE,
        options = list(
          dom = 't',  # table only, no search box or pagination
          ordering = FALSE
        )
      )
    })
    
    
    output$interceptPlot <- renderPlot({
      ggplot(Result_intercept, aes(x = value,  fill = model), color = "black") +
        geom_density(alpha = 0.3) +
        geom_vline(xintercept = 0, linetype = "dashed") +
        theme_minimal() +
        labs(title = "Intercept", x = "Value", y = "Density")
    })
    
    output$slopePlot <- renderPlot({
      ggplot(Result_slope, aes(x = value, fill = model), color = "black") +
        geom_density(alpha = 0.3) +
        geom_vline(xintercept = input$focalpath, linetype = "dashed") +
        theme_minimal() +
        labs(title = "Slope", x = "Value", y = "Density")
    })
    output$Scatter <- renderPlot({
      ggplot() +
        geom_point(data = d1, aes(x=xobs, y=yobs),color="pink", fill = "pink") +
        geom_point(data = d2, aes(x=x, y=y),color="skyblue", fill = "skyblue",alpha = .5) +
        geom_smooth(data = d1, aes(x=xobs, y=yobs), method = "lm",color="red")+
        geom_smooth(data = d2, aes(x=x, y=y), method = "lm", color = "blue") +
        theme_bw() +
        labs(title = "Scatter", x = "X", y = "Y", 
             caption = "Blue shows the complete data; red shows the listwise-deleted data")
    })
  })
}

shinyApp(ui = ui, server = server)
