devtools::install_github('wwylab/DeMixSC')

library(DeMixSC)
library(plotly)
library(dplyr)
library(shinyjs)

server <- function(input, output, session) {
  values <- reactiveValues(
    data_uploaded = FALSE,
    custom_analysis_complete = FALSE,
    retina_analysis_complete = FALSE,
    hgsc_analysis_complete = FALSE,
    custom_results = NULL,
    retina_results = NULL,
    hgsc_results = NULL,
    counts = NULL,
    annotations = NULL
  )
  
  # File upload handlers
  observeEvent(input$countsFile, {
    req(input$countsFile)
    values$counts <- read.csv(input$countsFile$datapath, row.names=1)
    values$data_uploaded <- !is.null(values$counts) && !is.null(values$annotations)
  })
  
  observeEvent(input$annotFile, {
    req(input$annotFile)
    values$annotations <- read.csv(input$annotFile$datapath)
    values$data_uploaded <- !is.null(values$counts) && !is.null(values$annotations)
  })
  
  # Analysis handlers
  observeEvent(input$runAnalysis, {
    req(values$counts)
    withProgress(message = 'Running DeMixSC analysis...', value = 0, {
      try({
        values$custom_results <- DeMixSC(
          option = "user.defined",
          benchmark.mode = FALSE,
          mat.target = values$counts,
          min.expression = input$minExpression,
          scale.factor = input$scaleFactor
        )
        values$custom_analysis_complete <- TRUE
      })
    })
  })
  
  observeEvent(input$runRetina, {
    withProgress(message = 'Loading Retina data...', value = 0, {
      if(input$retinaDataset == "AMD Cohort") {
        amd_cohort_raw_counts = get("retina_amd_cohort_raw_counts", envir = asNamespace("DeMixSC"))
        values$retina_results <- DeMixSC(
          option = "retina",
          benchmark.mode = FALSE,
          mat.target = amd_cohort_raw_counts,
          min.expression = 3,
          scale.factor = 1e5
        )
        values$retina_analysis_complete <- TRUE
      }
    })
  })
  
  observeEvent(input$runHGSC, {
    withProgress(message = 'Loading HGSC data...', value = 0, {
      if(input$hgscDataset == "Lee Cohort") {
        lee_cohort_raw_counts = get("hgsc_lee_cohort_raw_counts", envir = asNamespace("DeMixSC"))
        values$hgsc_results <- DeMixSC(
          option = "hgsc",
          benchmark.mode = FALSE,
          mat.target = lee_cohort_raw_counts,
          min.expression = 5,
          scale.factor = 1e6
        )
        values$hgsc_analysis_complete <- TRUE
      }
    })
  })
  
  # Plot renderers
  output$customPlot1 <- renderPlotly({
    req(values$custom_results)
    plot_data <- values$custom_results$cell.type.proportions
    plot_ly(type = "bar", x = rownames(plot_data), y = as.numeric(plot_data[,1])) %>%
      layout(title = "Cell Type Proportions",
             xaxis = list(title = "Cell Types"),
             yaxis = list(title = "Proportion"))
  })
  
  # Table renderers
  output$customTable <- renderDT({
    req(values$custom_results)
    datatable(values$custom_results$cell.type.proportions, options = list(scrollX = TRUE))
  })
  
  output$retinaTable <- renderDT({
    req(values$retina_results)
    datatable(values$retina_results$cell.type.proportions, options = list(scrollX = TRUE))
  })
  
  output$hgscTable <- renderDT({
    req(values$hgsc_results)
    datatable(values$hgsc_results$cell.type.proportions, options = list(scrollX = TRUE))
  })
  
  # Download handlers
  output$downloadCustomResults <- downloadHandler(
    filename = function() paste("custom-demixsc-results-", Sys.Date(), ".csv", sep=""),
    content = function(file) write.csv(values$custom_results$cell.type.proportions, file)
  )
  
  output$downloadRetinaResults <- downloadHandler(
    filename = function() paste("retina-demixsc-results-", Sys.Date(), ".csv", sep=""),
    content = function(file) write.csv(values$retina_results$cell.type.proportions, file)
  )
  
  output$downloadHGSCResults <- downloadHandler(
    filename = function() paste("hgsc-demixsc-results-", Sys.Date(), ".csv", sep=""),
    content = function(file) write.csv(values$hgsc_results$cell.type.proportions, file)
  )
}
