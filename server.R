library(DeMixSC)
library(plotly)
library(dplyr)
library(shinyjs)
library(DT)
library(shinyWidgets)
library(parallel)

check_and_install <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    message(paste("Installing package:", package_name))
    if (package_name == "DeMixSC") {
      if (!requireNamespace("devtools", quietly = TRUE)) {
        install.packages("devtools")
      }
      devtools::install_github('wwylab/DeMixSC')
    } else {
      install.packages(package_name)
    }
  }
}

required_packages <- c("shiny", "plotly", "dplyr", "shinyjs", "DT", "shinyWidgets", "parallel")
for (pkg in required_packages) {
  check_and_install(pkg)
}

tryCatch({
  check_and_install("DeMixSC")
}, error = function(e) {
  message("Could not install DeMixSC. Please install manually using devtools::install_github('wwylab/DeMixSC')")
})

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
    annotations = NULL,
    reference = NULL,
    mat_a = NULL,
    mat_b = NULL,
    target_bulk = NULL,
    error_message = NULL,
    is_processing = FALSE
  )
  
  # For user-defined analysis, additional file uploaders
  observeEvent(input$referenceFile, {
    req(input$referenceFile)
    tryCatch({
      values$reference <- read.csv(input$referenceFile$datapath, row.names=1)
      showNotification("Reference file loaded successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading reference file:", e$message), type = "error")
      values$reference <- NULL
    })
  })
  
  observeEvent(input$matAFile, {
    req(input$matAFile)
    tryCatch({
      values$mat_a <- read.csv(input$matAFile$datapath, row.names=1)
      showNotification("Benchmark bulk file loaded successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading benchmark bulk file:", e$message), type = "error")
      values$mat_a <- NULL
    })
  })
  
  observeEvent(input$matBFile, {
    req(input$matBFile)
    tryCatch({
      values$mat_b <- read.csv(input$matBFile$datapath, row.names=1)
      showNotification("Benchmark pseudobulk file loaded successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading benchmark pseudobulk file:", e$message), type = "error")
      values$mat_b <- NULL
    })
  })
  
  observeEvent(input$runRetina, {
    values$is_processing <- TRUE
    values$error_message <- NULL
    
    shinyjs::disable("runRetina")
    
    withProgress(message = 'Loading and analyzing Retina data...', value = 0, {
      tryCatch({
        if (input$retinaDataset == "AMD Cohort") {
          amd_cohort_raw_counts = get("retina_amd_cohort_raw_counts", envir = asNamespace("DeMixSC"))
          
          values$retina_results <- DeMixSC(
            option = "retina",
            benchmark.mode = FALSE,
            mat.target = amd_cohort_raw_counts,
            min.expression = 3,
            scale.factor = 1e5,
            nthread = min(detectCores() - 1, 1)
          )
          values$retina_analysis_complete <- TRUE
          showNotification("Retina analysis completed successfully!", type = "message")
        } else if (input$retinaDataset == "Benchmark") {
          benchmark_bulk = get("retina_benchmark_bulk_batch1", envir = asNamespace("DeMixSC"))
          benchmark_pseudobulk = get("retina_benchmark_pseudobulk_batch1", envir = asNamespace("DeMixSC"))
          benchmark_ref = get("retina_benchmark_reference_batch1", envir = asNamespace("DeMixSC"))
          
          values$retina_results <- DeMixSC(
            option = "user.defined",
            benchmark.mode = TRUE,
            reference = benchmark_ref$reference,
            mat.a = benchmark_bulk,
            mat.b = benchmark_pseudobulk,
            min.expression = 3,
            scale.factor = 1e5,
            nthread = min(detectCores() - 1, 1)
          )
          values$retina_analysis_complete <- TRUE
          showNotification("Retina benchmark analysis completed successfully!", type = "message")
        }
      }, error = function(e) {
        values$error_message <- paste("Error in Retina analysis:", e$message)
        showNotification(values$error_message, type = "error")
      }, finally = {
        values$is_processing <- FALSE
        shinyjs::enable("runRetina")
      })
    })
  })
  
  observeEvent(input$runHGSC, {
    values$is_processing <- TRUE
    values$error_message <- NULL
    
    shinyjs::disable("runHGSC")
    
    withProgress(message = 'Loading and analyzing HGSC data...', value = 0, {
      tryCatch({
        if (input$hgscDataset == "Lee Cohort") {
          lee_cohort_raw_counts = get("hgsc_lee_cohort_raw_counts", envir = asNamespace("DeMixSC"))
          
          values$hgsc_results <- DeMixSC(
            option = "hgsc",
            benchmark.mode = FALSE,
            mat.target = lee_cohort_raw_counts,
            min.expression = 5,
            scale.factor = 1e6,
            nthread = min(detectCores() - 1, 1)
          )
          values$hgsc_analysis_complete <- TRUE
          showNotification("HGSC analysis completed successfully!", type = "message")
        } else if (input$hgscDataset == "Benchmark") {
          benchmark_bulk = get("hgsc_benchmark_bulk", envir = asNamespace("DeMixSC"))
          benchmark_pseudobulk = get("hgsc_benchmark_pseudobulk", envir = asNamespace("DeMixSC"))
          benchmark_ref = get("hgsc_benchmark_reference", envir = asNamespace("DeMixSC"))
          
          values$hgsc_results <- DeMixSC(
            option = "user.defined",
            benchmark.mode = TRUE,
            reference = benchmark_ref$reference,
            mat.a = benchmark_bulk,
            mat.b = benchmark_pseudobulk,
            min.expression = 5,
            scale.factor = 1e6,
            nthread = min(detectCores() - 1, 1)
          )
          values$hgsc_analysis_complete <- TRUE
          showNotification("HGSC benchmark analysis completed successfully!", type = "message")
        }
      }, error = function(e) {
        values$error_message <- paste("Error in HGSC analysis:", e$message)
        showNotification(values$error_message, type = "error")
      }, finally = {
        values$is_processing <- FALSE
        shinyjs::enable("runHGSC")
      })
    })
  })
  
  output$retinaPlot <- renderPlotly({
    req(values$retina_results)
    
    if (input$retinaDataset == "AMD Cohort") {
      plot_data <- values$retina_results$cell.type.proportions
      
      if (!is.null(plot_data) && ncol(plot_data) > 0 && nrow(plot_data) > 0) {
        if (ncol(plot_data) > 10) {
          plot_data <- plot_data[, 1:10]
        }
        
        plot_df <- data.frame(
          CellType = rownames(plot_data),
          stringsAsFactors = FALSE
        )
        
        for (i in 1:ncol(plot_data)) {
          plot_df[[colnames(plot_data)[i]]] <- plot_data[, i]
        }
        
        plot_long <- tidyr::gather(plot_df, "Sample", "Proportion", -CellType)
        
        p <- plot_ly(plot_long, x = ~CellType, y = ~Proportion, color = ~Sample, type = "bar") %>%
          layout(title = "Retina Cell Type Proportions",
                 xaxis = list(title = "Cell Types", tickangle = 45),
                 yaxis = list(title = "Proportion"),
                 barmode = "group")
        
        return(p)
      }
    } else {
      bulk_prop <- values$retina_results$bulk.prop
      pseudo_prop <- values$retina_results$pseudobulk.prop
      
      plot_df <- data.frame(
        CellType = rownames(bulk_prop),
        stringsAsFactors = FALSE
      )
      
      for (i in 1:ncol(bulk_prop)) {
        plot_df[[paste0("Bulk_", colnames(bulk_prop)[i])]] <- bulk_prop[, i]
      }
      
      for (i in 1:ncol(pseudo_prop)) {
        plot_df[[paste0("PseudoBulk_", colnames(pseudo_prop)[i])]] <- pseudo_prop[, i]
      }
      
      plot_long <- tidyr::gather(plot_df, "Sample", "Proportion", -CellType)
      
      p <- plot_ly(plot_long, x = ~CellType, y = ~Proportion, color = ~Sample, type = "bar") %>%
        layout(title = "Retina Benchmark Comparison",
               xaxis = list(title = "Cell Types", tickangle = 45),
               yaxis = list(title = "Proportion"),
               barmode = "group")
      
      return(p)
    }
    
    plot_ly() %>% 
      layout(title = "No valid data available for plotting",
             xaxis = list(title = ""),
             yaxis = list(title = ""))
  })
  
  output$hgscPlot <- renderPlotly({
    req(values$hgsc_results)
    
    if (input$hgscDataset == "Lee Cohort") {
      plot_data <- values$hgsc_results$cell.type.proportions
      
      if (!is.null(plot_data) && ncol(plot_data) > 0 && nrow(plot_data) > 0) {
        if (ncol(plot_data) > 10) {
          plot_data <- plot_data[, 1:10]
        }
        
        plot_df <- data.frame(
          CellType = rownames(plot_data),
          stringsAsFactors = FALSE
        )
        
        for (i in 1:ncol(plot_data)) {
          plot_df[[colnames(plot_data)[i]]] <- plot_data[, i]
        }
        
        plot_long <- tidyr::gather(plot_df, "Sample", "Proportion", -CellType)
        
        p <- plot_ly(plot_long, x = ~CellType, y = ~Proportion, color = ~Sample, type = "bar") %>%
          layout(title = "HGSC Cell Type Proportions",
                 xaxis = list(title = "Cell Types", tickangle = 45),
                 yaxis = list(title = "Proportion"),
                 barmode = "group")
        
        return(p)
      }
    } else {
      bulk_prop <- values$hgsc_results$bulk.prop
      pseudo_prop <- values$hgsc_results$pseudobulk.prop
      
      plot_df <- data.frame(
        CellType = rownames(bulk_prop),
        stringsAsFactors = FALSE
      )
      
      for (i in 1:ncol(bulk_prop)) {
        plot_df[[paste0("Bulk_", colnames(bulk_prop)[i])]] <- bulk_prop[, i]
      }
      
      for (i in 1:ncol(pseudo_prop)) {
        plot_df[[paste0("PseudoBulk_", colnames(pseudo_prop)[i])]] <- pseudo_prop[, i]
      }
      
      plot_long <- tidyr::gather(plot_df, "Sample", "Proportion", -CellType)
      
      p <- plot_ly(plot_long, x = ~CellType, y = ~Proportion, color = ~Sample, type = "bar") %>%
        layout(title = "HGSC Benchmark Comparison",
               xaxis = list(title = "Cell Types", tickangle = 45),
               yaxis = list(title = "Proportion"),
               barmode = "group")
      
      return(p)
    }
    
    plot_ly() %>% 
      layout(title = "No valid data available for plotting",
             xaxis = list(title = ""),
             yaxis = list(title = ""))
  })
  
  output$retinaTable <- renderDT({
    req(values$retina_results)
    
    if (input$retinaDataset == "AMD Cohort") {
      dt <- datatable(
        round(t(values$retina_results$cell.type.proportions), 4),
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          lengthMenu = c(5, 10, 15, 20)
        ),
        rownames = TRUE,
        caption = "Retina Cell Type Proportions (rounded to 4 decimal places)"
      ) %>%
        formatStyle(
          columns = colnames(t(values$retina_results$cell.type.proportions)),
          background = styleColorBar(c(0, 1), 'lightblue'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
      return(dt)
    } else {
      bulk_prop <- round(t(values$retina_results$bulk.prop), 4)
      pseudo_prop <- round(t(values$retina_results$pseudobulk.prop), 4)
      
      bulk_prop$Source <- "Bulk"
      pseudo_prop$Source <- "PseudoBulk"
      
      combined <- rbind(bulk_prop, pseudo_prop)
      
      dt <- datatable(
        combined,
        options = list(
          scrollX = TRUE,
          pageLength = 15,
          lengthMenu = c(5, 10, 15, 20)
        ),
        rownames = TRUE,
        caption = "Retina Benchmark Results (rounded to 4 decimal places)"
      ) %>%
        formatStyle(
          columns = setdiff(colnames(combined), "Source"),
          background = styleColorBar(c(0, 1), 'lightblue'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
      return(dt)
    }
  })
  
  output$hgscTable <- renderDT({
    req(values$hgsc_results)
    
    if (input$hgscDataset == "Lee Cohort") {
      dt <- datatable(
        round(t(values$hgsc_results$cell.type.proportions), 4),
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          lengthMenu = c(5, 10, 15, 20)
        ),
        rownames = TRUE,
        caption = "HGSC Cell Type Proportions (rounded to 4 decimal places)"
      ) %>%
        formatStyle(
          columns = colnames(t(values$hgsc_results$cell.type.proportions)),
          background = styleColorBar(c(0, 1), 'lightblue'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
      return(dt)
    } else {
      bulk_prop <- round(t(values$hgsc_results$bulk.prop), 4)
      pseudo_prop <- round(t(values$hgsc_results$pseudobulk.prop), 4)
      
      bulk_prop$Source <- "Bulk"
      pseudo_prop$Source <- "PseudoBulk"
      
      combined <- rbind(bulk_prop, pseudo_prop)
      
      dt <- datatable(
        combined,
        options = list(
          scrollX = TRUE,
          pageLength = 15,
          lengthMenu = c(5, 10, 15, 20)
        ),
        rownames = TRUE,
        caption = "HGSC Benchmark Results (rounded to 4 decimal places)"
      ) %>%
        formatStyle(
          columns = setdiff(colnames(combined), "Source"),
          background = styleColorBar(c(0, 1), 'lightblue'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
      return(dt)
    }
  })
  
  output$downloadRetinaResults <- downloadHandler(
    filename = function() {
      if (input$retinaDataset == "AMD Cohort") {
        paste0("retina-amd-demixsc-results-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv")
      } else {
        paste0("retina-benchmark-demixsc-results-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv")
      }
    },
    content = function(file) {
      req(values$retina_results)
      if (input$retinaDataset == "AMD Cohort") {
        write.csv(values$retina_results$cell.type.proportions, file)
      } else {
        results_list <- list(
          bulk = values$retina_results$bulk.prop,
          pseudobulk = values$retina_results$pseudobulk.prop
        )
        saveRDS(results_list, file)
      }
    }
  )
  
  output$downloadHGSCResults <- downloadHandler(
    filename = function() {
      if (input$hgscDataset == "Lee Cohort") {
        paste0("hgsc-lee-demixsc-results-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv")
      } else {
        paste0("hgsc-benchmark-demixsc-results-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv")
      }
    },
    content = function(file) {
      req(values$hgsc_results)
      if (input$hgscDataset == "Lee Cohort") {
        write.csv(values$hgsc_results$cell.type.proportions, file)
      } else {
        results_list <- list(
          bulk = values$hgsc_results$bulk.prop,
          pseudobulk = values$hgsc_results$pseudobulk.prop
        )
        saveRDS(results_list, file)
      }
    }
  )
  
  # Display error messages
  output$errorDisplay <- renderUI({
    if (!is.null(values$error_message)) {
      div(
        style = "color: red; background-color: #ffeeee; padding: 10px; border-radius: 5px; margin-top: 10px;",
        icon("exclamation-triangle"),
        p(values$error_message)
      )
    }
  })
  
  # Show processing indicator
  output$processingIndicator <- renderUI({
    if (values$is_processing) {
      div(
        style = "text-align: center; margin-top: 20px;",
        spin_flower(),
        p("Processing... This may take a few minutes.")
      )
    }
  })
}
