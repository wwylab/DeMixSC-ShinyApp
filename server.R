library(DeMixSC)
library(plotly)
library(dplyr)
library(shinyjs)
library(DT)
library(shinyWidgets)
library(parallel)

options(shiny.maxRequestSize = 66*1024^2)  

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

server <- function(input, output, session) {
  values <- reactiveValues(
    data_uploaded = FALSE,
    custom_analysis_complete = FALSE,
    retina_analysis_complete = FALSE,
    hgsc_analysis_complete = FALSE,
    retina_predefined_analysis_complete = FALSE,
    hgsc_predefined_analysis_complete = FALSE,
    custom_results = NULL,
    retina_results = NULL,
    hgsc_results = NULL,
    retina_predefined_results = NULL,
    hgsc_predefined_results = NULL,
    counts = NULL,
    annotations = NULL,
    reference = NULL,
    mat_a = NULL,
    mat_b = NULL,
    target_bulk = NULL,
    retina_target = NULL,
    hgsc_target = NULL,
    error_message = NULL,
    is_processing = FALSE
  )
  
  # For user-defined analysis
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
  
  # Upload handlers for predefined analysis
  observeEvent(input$retinaTargetFile, {
    req(input$retinaTargetFile)
    tryCatch({
      values$retina_target <- read.csv(input$retinaTargetFile$datapath, row.names=1)
      showNotification("Retina target bulk file loaded successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading retina target bulk file:", e$message), type = "error")
      values$retina_target <- NULL
    })
  })
  
  observeEvent(input$hgscTargetFile, {
    req(input$hgscTargetFile)
    tryCatch({
      values$hgsc_target <- read.csv(input$hgscTargetFile$datapath, row.names=1)
      showNotification("HGSC target bulk file loaded successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading HGSC target bulk file:", e$message), type = "error")
      values$hgsc_target <- NULL
    })
  })
  
  # Run example retina analysis
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
  
  # Run example HGSC analysis
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
  
  # Run pre-defined analysis for Retina with user upload
  observeEvent(input$runRetinaPredefined, {
    req(values$retina_target)
    
    values$is_processing <- TRUE
    values$error_message <- NULL
    
    shinyjs::disable("runRetinaPredefined")
    
    withProgress(message = 'Analyzing your Retina data...', value = 0, {
      tryCatch({
        reference = get("retina_consensus_reference", envir = asNamespace("DeMixSC"))
        benchmark_bulk = get("retina_benchmark_bulk_batch1", envir = asNamespace("DeMixSC"))
        benchmark_pseudobulk = get("retina_benchmark_pseudobulk_batch1", envir = asNamespace("DeMixSC"))
        
        values$retina_predefined_results <- DeMixSC(
          option = "user.defined",
          benchmark.mode = FALSE,
          reference = reference,
          mat.a = benchmark_bulk,
          mat.b = benchmark_pseudobulk,
          mat.target = values$retina_target,
          min.expression = input$retinaMinExpression,
          scale.factor = input$retinaScaleFactor,
          top.ranked.genes = input$retinaTopGenes,
          nthread = min(detectCores() - 1, 1)
        )
        values$retina_predefined_analysis_complete <- TRUE
        showNotification("Retina analysis of your data completed successfully!", type = "message")
      }, error = function(e) {
        values$error_message <- paste("Error in Retina analysis:", e$message)
        showNotification(values$error_message, type = "error")
      }, finally = {
        values$is_processing <- FALSE
        shinyjs::enable("runRetinaPredefined")
      })
    })
  })
  
  # Run pre-defined analysis for HGSC with user upload
  observeEvent(input$runHGSCPredefined, {
    req(values$hgsc_target)
    
    values$is_processing <- TRUE
    values$error_message <- NULL
    
    shinyjs::disable("runHGSCPredefined")
    
    withProgress(message = 'Analyzing your HGSC data...', value = 0, {
      tryCatch({
        reference = get("hgsc_consensus_reference", envir = asNamespace("DeMixSC"))
        benchmark_bulk = get("hgsc_benchmark_bulk", envir = asNamespace("DeMixSC"))
        benchmark_pseudobulk = get("hgsc_benchmark_pseudobulk", envir = asNamespace("DeMixSC"))
        
        values$hgsc_predefined_results <- DeMixSC(
          option = "user.defined",
          benchmark.mode = FALSE,
          reference = reference,
          mat.a = benchmark_bulk,
          mat.b = benchmark_pseudobulk,
          mat.target = values$hgsc_target,
          min.expression = input$hgscMinExpression,
          scale.factor = input$hgscScaleFactor,
          top.ranked.genes = input$hgscTopGenes,
          nthread = min(detectCores() - 1, 1)
        )
        values$hgsc_predefined_analysis_complete <- TRUE
        showNotification("HGSC analysis of your data completed successfully!", type = "message")
      }, error = function(e) {
        values$error_message <- paste("Error in HGSC analysis:", e$message)
        showNotification(values$error_message, type = "error")
      }, finally = {
        values$is_processing <- FALSE
        shinyjs::enable("runHGSCPredefined")
      })
    })
  })
  
  # Function to generate a proper color palette 
  generateColorPalette <- function(n) {
    base_colors <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", 
                     "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3",
                     "#4D4D4D", "#D53E4F", "#1F78B4", "#33A02C")
    
    if (n <= length(base_colors)) {
      return(base_colors[1:n])
    } else {
      return(colorRampPalette(base_colors)(n))
    }
  }
  
  # Example Retina plots and tables
  output$retinaPlot <- renderPlotly({
    req(values$retina_results)
    
    if (input$retinaPlotType == "boxplot") {
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
          
          n_samples <- length(unique(plot_long$Sample))
          color_palette <- generateColorPalette(n_samples)
          
          p <- plot_ly(plot_long, x = ~CellType, y = ~Proportion, color = ~Sample, 
                       type = "bar",
                       colors = color_palette) %>%
            layout(title = "Retina Cell Type Proportions",
                   xaxis = list(title = "Cell Types", tickangle = 45),
                   yaxis = list(title = "Proportion"),
                   barmode = "group")
          
          return(p)
        }
      } else {
        if (!is.null(values$retina_results$bulk.prop) && !is.null(values$retina_results$pseudobulk.prop)) {
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
          
          n_samples <- length(unique(plot_long$Sample))
          color_palette <- generateColorPalette(n_samples)
          
          p <- plot_ly(plot_long, x = ~CellType, y = ~Proportion, color = ~Sample, 
                       type = "bar", 
                       colors = color_palette) %>%
            layout(title = "Retina Benchmark Comparison",
                   xaxis = list(title = "Cell Types", tickangle = 45),
                   yaxis = list(title = "Proportion"),
                   barmode = "group")
          
          return(p)
        }
      }
    } else {
      req(input$retinaSampleID)
      
      if (input$retinaDataset == "AMD Cohort") {
        plot_data <- values$retina_results$cell.type.proportions[, input$retinaSampleID, drop = FALSE]
        
        if (!is.null(plot_data) && ncol(plot_data) > 0 && nrow(plot_data) > 0) {
          plot_df <- data.frame(
            CellType = rownames(plot_data),
            Proportion = plot_data[,1],
            stringsAsFactors = FALSE
          )
          
          plot_df <- plot_df[order(plot_df$Proportion, decreasing = TRUE),]
          
          p <- plot_ly(plot_df, x = ~CellType, y = ~Proportion, type = "bar",
                       marker = list(color = "#3498db")) %>%
            layout(title = paste0("Cell Type Proportions for Sample: ", input$retinaSampleID),
                   xaxis = list(title = "Cell Types", tickangle = 45, categoryorder = "array", categoryarray = plot_df$CellType),
                   yaxis = list(title = "Proportion"))
          
          return(p)
        }
      } else {
        if (!is.null(values$retina_results$bulk.prop)) {
          plot_data <- values$retina_results$bulk.prop[, input$retinaSampleID, drop = FALSE]
          
          plot_df <- data.frame(
            CellType = rownames(plot_data),
            Proportion = plot_data[,1],
            stringsAsFactors = FALSE
          )
          
          plot_df <- plot_df[order(plot_df$Proportion, decreasing = TRUE),]
          
          p <- plot_ly(plot_df, x = ~CellType, y = ~Proportion, type = "bar",
                       marker = list(color = "#3498db")) %>%
            layout(title = paste0("Bulk Proportion for Sample: ", input$retinaSampleID),
                   xaxis = list(title = "Cell Types", tickangle = 45, categoryorder = "array", categoryarray = plot_df$CellType),
                   yaxis = list(title = "Proportion"))
          
          return(p)
        }
      }
    }
    
    plot_ly() %>% 
      layout(title = "No valid data available for plotting",
             xaxis = list(title = ""),
             yaxis = list(title = ""))
  })
  
  output$hgscPlot <- renderPlotly({
    req(values$hgsc_results)
    
    if (input$hgscPlotType == "boxplot") {
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
          
          n_samples <- length(unique(plot_long$Sample))
          color_palette <- generateColorPalette(n_samples)
          
          p <- plot_ly(plot_long, x = ~CellType, y = ~Proportion, color = ~Sample, 
                       type = "bar", 
                       colors = color_palette) %>%
            layout(title = "HGSC Cell Type Proportions",
                   xaxis = list(title = "Cell Types", tickangle = 45),
                   yaxis = list(title = "Proportion"),
                   barmode = "group")
          
          return(p)
        }
      } else {
        if (!is.null(values$hgsc_results$bulk.prop) && !is.null(values$hgsc_results$pseudobulk.prop)) {
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
          
          n_samples <- length(unique(plot_long$Sample))
          color_palette <- generateColorPalette(n_samples)
          
          p <- plot_ly(plot_long, x = ~CellType, y = ~Proportion, color = ~Sample, 
                       type = "bar", 
                       colors = color_palette) %>%
            layout(title = "HGSC Benchmark Comparison",
                   xaxis = list(title = "Cell Types", tickangle = 45),
                   yaxis = list(title = "Proportion"),
                   barmode = "group")
          
          return(p)
        }
      }
    } else {
      req(input$hgscSampleID)
      
      if (input$hgscDataset == "Lee Cohort") {
        plot_data <- values$hgsc_results$cell.type.proportions[, input$hgscSampleID, drop = FALSE]
        
        if (!is.null(plot_data) && ncol(plot_data) > 0 && nrow(plot_data) > 0) {
          plot_df <- data.frame(
            CellType = rownames(plot_data),
            Proportion = plot_data[,1],
            stringsAsFactors = FALSE
          )
          
          plot_df <- plot_df[order(plot_df$Proportion, decreasing = TRUE),]
          
          p <- plot_ly(plot_df, x = ~CellType, y = ~Proportion, type = "bar",
                       marker = list(color = "#FC8D62")) %>%
            layout(title = paste0("Cell Type Proportions for Sample: ", input$hgscSampleID),
                   xaxis = list(title = "Cell Types", tickangle = 45, categoryorder = "array", categoryarray = plot_df$CellType),
                   yaxis = list(title = "Proportion"))
          
          return(p)
        }
      } else {
        if (!is.null(values$hgsc_results$bulk.prop)) {
          plot_data <- values$hgsc_results$bulk.prop[, input$hgscSampleID, drop = FALSE]
          
          plot_df <- data.frame(
            CellType = rownames(plot_data),
            Proportion = plot_data[,1],
            stringsAsFactors = FALSE
          )
          
          plot_df <- plot_df[order(plot_df$Proportion, decreasing = TRUE),]
          
          p <- plot_ly(plot_df, x = ~CellType, y = ~Proportion, type = "bar",
                       marker = list(color = "#FC8D62")) %>%
            layout(title = paste0("Bulk Proportion for Sample: ", input$hgscSampleID),
                   xaxis = list(title = "Cell Types", tickangle = 45, categoryorder = "array", categoryarray = plot_df$CellType),
                   yaxis = list(title = "Proportion"))
          
          return(p)
        }
      }
    }
    
    plot_ly() %>% 
      add_trace(type = "scatter", mode = "markers", x = c(), y = c()) %>%
      layout(title = "No valid data available for plotting",
             xaxis = list(title = ""),
             yaxis = list(title = ""))
  })
  
  # Predefined analysis plots
  output$retinaPredefinedPlot <- renderPlotly({
    req(values$retina_predefined_results)
    
    if (input$retinaPredefinedPlotType == "boxplot") {
      plot_data <- values$retina_predefined_results$cell.type.proportions
      
      if (!is.null(plot_data) && ncol(plot_data) > 0 && nrow(plot_data) > 0) {
        if (ncol(plot_data) > 20) {
          sampled_cols <- sample(1:ncol(plot_data), 20)
          plot_data <- plot_data[, sampled_cols]
        }
        
        plot_df <- data.frame(
          CellType = rownames(plot_data),
          stringsAsFactors = FALSE
        )
        
        for (i in 1:ncol(plot_data)) {
          plot_df[[colnames(plot_data)[i]]] <- plot_data[, i]
        }
        
        plot_long <- tidyr::gather(plot_df, "Sample", "Proportion", -CellType)
        
        n_samples <- length(unique(plot_long$Sample))
        color_palette <- generateColorPalette(n_samples)
        
        p <- plot_ly(plot_long, x = ~CellType, y = ~Proportion, color = ~Sample, type = "bar",
                     colors = color_palette) %>%
          layout(title = "Retina Cell Type Proportions in Your Data",
                 xaxis = list(title = "Cell Types", tickangle = 45),
                 yaxis = list(title = "Proportion"),
                 barmode = "group")
        
        return(p)
      }
    } else {
      req(input$retinaPredefinedSampleID)
      
      plot_data <- values$retina_predefined_results$cell.type.proportions[, input$retinaPredefinedSampleID, drop = FALSE]
      
      if (!is.null(plot_data) && ncol(plot_data) > 0 && nrow(plot_data) > 0) {
        plot_df <- data.frame(
          CellType = rownames(plot_data),
          Proportion = plot_data[,1],
          stringsAsFactors = FALSE
        )
        
        plot_df <- plot_df[order(plot_df$Proportion, decreasing = TRUE),]
        
        p <- plot_ly(plot_df, x = ~CellType, y = ~Proportion, type = "bar",
                     marker = list(color = "#66C2A5")) %>%
          layout(title = paste0("Cell Type Proportions for Sample: ", input$retinaPredefinedSampleID),
                 xaxis = list(title = "Cell Types", tickangle = 45, categoryorder = "array", categoryarray = plot_df$CellType),
                 yaxis = list(title = "Proportion"))
        
        return(p)
      }
    }
    
    plot_ly() %>% 
      layout(title = "No valid data available for plotting",
             xaxis = list(title = ""),
             yaxis = list(title = ""))
  })
  
  output$hgscPredefinedPlot <- renderPlotly({
    req(values$hgsc_predefined_results)
    
    if (input$hgscPredefinedPlotType == "boxplot") {
      plot_data <- values$hgsc_predefined_results$cell.type.proportions
      
      if (!is.null(plot_data) && ncol(plot_data) > 0 && nrow(plot_data) > 0) {
        if (ncol(plot_data) > 20) {
          set.seed(123) 
          sampled_cols <- sample(1:ncol(plot_data), 20)
          plot_data <- plot_data[, sampled_cols]
        }
        
        plot_df <- data.frame(
          CellType = rownames(plot_data),
          stringsAsFactors = FALSE
        )
        
        for (i in 1:ncol(plot_data)) {
          plot_df[[colnames(plot_data)[i]]] <- plot_data[, i]
        }
        
        plot_long <- tidyr::gather(plot_df, "Sample", "Proportion", -CellType)
        
        n_samples <- length(unique(plot_long$Sample))
        color_palette <- generateColorPalette(n_samples)
        
        p <- plot_ly(plot_long, x = ~CellType, y = ~Proportion, color = ~Sample, type = "bar",
                     colors = color_palette) %>%
          layout(title = "HGSC Cell Type Proportions in Your Data",
                 xaxis = list(title = "Cell Types", tickangle = 45),
                 yaxis = list(title = "Proportion"),
                 barmode = "group")
        
        return(p)
      }
    } else {
      req(input$hgscPredefinedSampleID)
      
      plot_data <- values$hgsc_predefined_results$cell.type.proportions[, input$hgscPredefinedSampleID, drop = FALSE]
      
      if (!is.null(plot_data) && ncol(plot_data) > 0 && nrow(plot_data) > 0) {
        plot_df <- data.frame(
          CellType = rownames(plot_data),
          Proportion = plot_data[,1],
          stringsAsFactors = FALSE
        )
        
        plot_df <- plot_df[order(plot_df$Proportion, decreasing = TRUE),]
        
        p <- plot_ly(plot_df, x = ~CellType, y = ~Proportion, type = "bar",
                     marker = list(color = "#FC8D62")) %>%
          layout(title = paste0("Cell Type Proportions for Sample: ", input$hgscPredefinedSampleID),
                 xaxis = list(title = "Cell Types", tickangle = 45, categoryorder = "array", categoryarray = plot_df$CellType),
                 yaxis = list(title = "Proportion"))
        
        return(p)
      }
    }
    
    plot_ly() %>% 
      layout(title = "No valid data available for plotting",
             xaxis = list(title = ""),
             yaxis = list(title = ""))
  })
  
  # Example data tables
  output$retinaTable <- renderDT({
    req(values$retina_results)
    
    if (input$retinaDataset == "AMD Cohort") {
      prop_table <- round(t(values$retina_results$cell.type.proportions), 4)
      
      dt <- datatable(
        prop_table,
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          lengthMenu = c(5, 10, 15, 20)
        ),
        rownames = TRUE,
        caption = "Retina Cell Type Proportions"
      )
      
      if (ncol(prop_table) > 0) {
        dt <- dt %>%
          formatStyle(
            columns = colnames(prop_table),
            background = styleColorBar(c(0, 1), 'lightblue'),
            backgroundSize = '100% 90%',
            backgroundRepeat = 'no-repeat',
            backgroundPosition = 'center'
          )
      }
      
      return(dt)
    } else {
      if (!is.null(values$retina_results$bulk.prop) && !is.null(values$retina_results$pseudobulk.prop)) {
        bulk_data <- as.data.frame(round(t(values$retina_results$bulk.prop), 4))
        pseudo_data <- as.data.frame(round(t(values$retina_results$pseudobulk.prop), 4))
        
        bulk_data$Source <- "Bulk"
        pseudo_data$Source <- "PseudoBulk"
        
        combined <- rbind(bulk_data, pseudo_data)
        
        dt <- datatable(
          combined,
          options = list(
            scrollX = TRUE,
            pageLength = 15,
            lengthMenu = c(5, 10, 15, 20)
          ),
          rownames = TRUE,
          caption = "Retina Benchmark Results"
        )
        
        numeric_cols <- setdiff(colnames(combined), "Source")
        
        if (length(numeric_cols) > 0) {
          dt <- dt %>%
            formatStyle(
              columns = numeric_cols,
              background = styleColorBar(c(0, 1), 'lightblue'),
              backgroundSize = '100% 90%',
              backgroundRepeat = 'no-repeat',
              backgroundPosition = 'center'
            )
        }
        
        return(dt)
      }
    }
    
    return(datatable(data.frame(Message = "No valid data available for display"), 
                     caption = "Data Error", options = list(dom = 't')))
  })
  
  output$hgscTable <- renderDT({
    req(values$hgsc_results)
    
    if (input$hgscDataset == "Lee Cohort") {
      prop_table <- round(t(values$hgsc_results$cell.type.proportions), 4)
      
      dt <- datatable(
        prop_table,
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          lengthMenu = c(5, 10, 15, 20)
        ),
        rownames = TRUE,
        caption = "HGSC Cell Type Proportions"
      )
      
      if (ncol(prop_table) > 0) {
        dt <- dt %>%
          formatStyle(
            columns = colnames(prop_table),
            background = styleColorBar(c(0, 1), 'lightblue'),
            backgroundSize = '100% 90%',
            backgroundRepeat = 'no-repeat',
            backgroundPosition = 'center'
          )
      }
      
      return(dt)
    } else {
      if (!is.null(values$hgsc_results$bulk.prop) && !is.null(values$hgsc_results$pseudobulk.prop)) {
        bulk_data <- as.data.frame(round(t(values$hgsc_results$bulk.prop), 4))
        pseudo_data <- as.data.frame(round(t(values$hgsc_results$pseudobulk.prop), 4))
        
        bulk_data$Source <- "Bulk"
        pseudo_data$Source <- "PseudoBulk"
        
        combined <- rbind(bulk_data, pseudo_data)
        
        dt <- datatable(
          combined,
          options = list(
            scrollX = TRUE,
            pageLength = 15,
            lengthMenu = c(5, 10, 15, 20)
          ),
          rownames = TRUE,
          caption = "HGSC Benchmark Results"
        )
        
        numeric_cols <- setdiff(colnames(combined), "Source")
        
        if (length(numeric_cols) > 0) {
          dt <- dt %>%
            formatStyle(
              columns = numeric_cols,
              background = styleColorBar(c(0, 1), 'lightblue'),
              backgroundSize = '100% 90%',
              backgroundRepeat = 'no-repeat',
              backgroundPosition = 'center'
            )
        }
        
        return(dt)
      }
    }
    
    return(datatable(data.frame(Message = "No valid data available for display"), 
                     caption = "Data Error", options = list(dom = 't')))
  })
  
  # Predefined analysis tables
  output$retinaPredefinedTable <- renderDT({
    req(values$retina_predefined_results)
    
    dt <- datatable(
      round(t(values$retina_predefined_results$cell.type.proportions), 4),
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        lengthMenu = c(5, 10, 15, 20)
      ),
      rownames = TRUE,
      caption = "Retina Cell Type Proportions in Your Data"
    ) %>%
      formatStyle(
        columns = colnames(t(values$retina_predefined_results$cell.type.proportions)),
        background = styleColorBar(c(0, 1), 'lightblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
    return(dt)
  })
  
  output$hgscPredefinedTable <- renderDT({
    req(values$hgsc_predefined_results)
    
    dt <- datatable(
      round(t(values$hgsc_predefined_results$cell.type.proportions), 4),
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        lengthMenu = c(5, 10, 15, 20)
      ),
      rownames = TRUE,
      caption = "HGSC Cell Type Proportions in Your Data"
    ) %>%
      formatStyle(
        columns = colnames(t(values$hgsc_predefined_results$cell.type.proportions)),
        background = styleColorBar(c(0, 1), 'lightblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
    return(dt)
  })
  
  # Download handlers
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
  
  # Download handlers for predefined analysis results
  output$downloadRetinaPredefinedResults <- downloadHandler(
    filename = function() {
      paste0("retina-custom-demixsc-results-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv")
    },
    content = function(file) {
      req(values$retina_predefined_results)
      write.csv(values$retina_predefined_results$cell.type.proportions, file)
    }
  )
  
  output$downloadHGSCPredefinedResults <- downloadHandler(
    filename = function() {
      paste0("hgsc-custom-demixsc-results-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv")
    },
    content = function(file) {
      req(values$hgsc_predefined_results)
      write.csv(values$hgsc_predefined_results$cell.type.proportions, file)
    }
  )
  
  # Error messages
  output$errorDisplay <- renderUI({
    if (!is.null(values$error_message)) {
      div(
        style = "color: red; background-color: #ffeeee; padding: 10px; border-radius: 5px; margin-top: 10px;",
        icon("exclamation-triangle"),
        p(values$error_message)
      )
    }
  })
  
  # Error displays for predefined analysis
  output$errorDisplayRetinaPredefined <- renderUI({
    if (!is.null(values$error_message)) {
      div(
        style = "color: red; background-color: #ffeeee; padding: 10px; border-radius: 5px; margin-top: 10px;",
        icon("exclamation-triangle"),
        p(values$error_message)
      )
    }
  })
  
  output$errorDisplayHGSCPredefined <- renderUI({
    if (!is.null(values$error_message)) {
      div(
        style = "color: red; background-color: #ffeeee; padding: 10px; border-radius: 5px; margin-top: 10px;",
        icon("exclamation-triangle"),
        p(values$error_message)
      )
    }
  })
  
  # Processing indicator
  output$processingIndicator <- renderUI({
    if (values$is_processing) {
      div(
        style = "text-align: center; margin-top: 20px;",
        spin_flower(),
        p("Processing... This may take a few minutes.")
      )
    }
  })
  
  # Processing indicators for predefined analysis
  output$processingIndicatorRetinaPredefined <- renderUI({
    if (values$is_processing) {
      div(
        style = "text-align: center; margin-top: 20px;",
        spin_flower(),
        p("Processing your retina data... This may take a few minutes.")
      )
    }
  })
  
  output$processingIndicatorHGSCPredefined <- renderUI({
    if (values$is_processing) {
      div(
        style = "text-align: center; margin-top: 20px;",
        spin_flower(),
        p("Processing your HGSC data... This may take a few minutes.")
      )
    }
  })
  
  # Handle target bulk file upload for user-defined analysis
  observeEvent(input$targetBulkFile, {
    req(input$targetBulkFile)
    tryCatch({
      values$target_bulk <- read.csv(input$targetBulkFile$datapath, row.names=1)
      showNotification("Target bulk file loaded successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading target bulk file:", e$message), type = "error")
      values$target_bulk <- NULL
    })
  })
  
  # User-defined analysis
  observeEvent(input$runAdvanced, {
    req(values$reference, values$mat_a, values$mat_b)
    
    values$is_processing <- TRUE
    values$error_message <- NULL
    
    shinyjs::disable("runAdvanced")
    
    withProgress(message = 'Running user-defined analysis...', value = 0, {
      tryCatch({
        benchmark_mode <- is.null(values$target_bulk)
        
        reference_to_use <- NULL
        if (is.list(values$reference) && "reference" %in% names(values$reference)) {
          reference_to_use <- as.matrix(values$reference$reference)
        } else if (is.data.frame(values$reference) || is.matrix(values$reference)) {
          reference_to_use <- as.matrix(values$reference)
        } else {
          stop(paste("Cannot process reference of class:", class(values$reference)[1]))
        }
        
        mat_a_to_use <- as.matrix(values$mat_a)
        mat_b_to_use <- as.matrix(values$mat_b)
        
        target_bulk_to_use <- NULL
        if (!benchmark_mode) {
          target_bulk_to_use <- as.matrix(values$target_bulk)
        }
        
        if (benchmark_mode) {
          values$custom_results <- DeMixSC(
            option = "user.defined",
            benchmark.mode = TRUE,
            reference = reference_to_use,
            mat.a = mat_a_to_use,
            mat.b = mat_b_to_use,
            min.expression = input$customMinExpression,
            scale.factor = input$customScaleFactor,
            top.ranked.genes = input$customTopGenes,
            nthread = min(detectCores() - 1, 1)
          )
        } else {
          values$custom_results <- DeMixSC(
            option = "user.defined",
            benchmark.mode = FALSE,
            reference = reference_to_use,
            mat.a = mat_a_to_use,
            mat.b = mat_b_to_use,
            mat.target = target_bulk_to_use,
            min.expression = input$customMinExpression,
            scale.factor = input$customScaleFactor,
            top.ranked.genes = input$customTopGenes,
            nthread = min(detectCores() - 1, 1)
          )
        }
        
        values$custom_analysis_complete <- TRUE
        showNotification("User-defined analysis completed successfully!", type = "message")
      }, error = function(e) {
        values$error_message <- paste("Error in user-defined analysis:", e$message)
        showNotification(values$error_message, type = "error", duration = NULL)
      }, finally = {
        values$is_processing <- FALSE
        shinyjs::enable("runAdvanced")
      })
    })
  })
  
  # Create outputs for visualizing results
  output$customPlot <- renderPlotly({
    req(values$custom_results)
    
    if (input$customPlotType == "boxplot") {
      if (!is.null(values$custom_results$cell.type.proportions)) {
        plot_data <- values$custom_results$cell.type.proportions
        
        if (ncol(plot_data) > 20) {
          set.seed(123)
          sampled_cols <- sample(1:ncol(plot_data), 20)
          plot_data <- plot_data[, sampled_cols]
        }
        
        plot_df <- data.frame(
          CellType = rownames(plot_data),
          stringsAsFactors = FALSE
        )
        
        for (i in 1:ncol(plot_data)) {
          plot_df[[colnames(plot_data)[i]]] <- plot_data[, i]
        }
        
        plot_long <- tidyr::gather(plot_df, "Sample", "Proportion", -CellType)
        
        n_samples <- length(unique(plot_long$Sample))
        color_palette <- generateColorPalette(n_samples)
        
        p <- plot_ly(plot_long, x = ~CellType, y = ~Proportion, color = ~Sample, type = "bar",
                     colors = color_palette) %>%
          layout(title = "Cell Type Proportions in Target Data",
                 xaxis = list(title = "Cell Types", tickangle = 45),
                 yaxis = list(title = "Proportion"),
                 barmode = "group")
        
        return(p)
      } else if (!is.null(values$custom_results$bulk.prop) && !is.null(values$custom_results$pseudobulk.prop)) {
        bulk_prop <- values$custom_results$bulk.prop
        pseudo_prop <- values$custom_results$pseudobulk.prop
        
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
        
        n_samples <- length(unique(plot_long$Sample))
        color_palette <- generateColorPalette(n_samples)
        
        p <- plot_ly(plot_long, x = ~CellType, y = ~Proportion, color = ~Sample, type = "bar",
                     colors = color_palette) %>%
          layout(title = "Benchmark Results Comparison",
                 xaxis = list(title = "Cell Types", tickangle = 45),
                 yaxis = list(title = "Proportion"),
                 barmode = "group")
        
        return(p)
      }
    } else {
      req(input$customSampleID)
      
      if (!is.null(values$custom_results$cell.type.proportions)) {
        plot_data <- values$custom_results$cell.type.proportions[, input$customSampleID, drop = FALSE]
        
        plot_df <- data.frame(
          CellType = rownames(plot_data),
          Proportion = plot_data[,1],
          stringsAsFactors = FALSE
        )
        
        plot_df <- plot_df[order(plot_df$Proportion, decreasing = TRUE),]
        
        p <- plot_ly(plot_df, x = ~CellType, y = ~Proportion, type = "bar",
                     marker = list(color = "#8DA0CB")) %>%
          layout(title = paste0("Cell Type Proportions for Sample: ", input$customSampleID),
                 xaxis = list(title = "Cell Types", tickangle = 45, categoryorder = "array", categoryarray = plot_df$CellType),
                 yaxis = list(title = "Proportion"))
        
        return(p)
      } else if (!is.null(values$custom_results$bulk.prop)) {
        plot_data <- values$custom_results$bulk.prop[, input$customSampleID, drop = FALSE]
        
        plot_df <- data.frame(
          CellType = rownames(plot_data),
          Proportion = plot_data[,1],
          stringsAsFactors = FALSE
        )
        
        plot_df <- plot_df[order(plot_df$Proportion, decreasing = TRUE),]
        
        p <- plot_ly(plot_df, x = ~CellType, y = ~Proportion, type = "bar",
                     marker = list(color = "#8DA0CB")) %>%
          layout(title = paste0("Bulk Proportion for Sample: ", input$customSampleID),
                 xaxis = list(title = "Cell Types", tickangle = 45, categoryorder = "array", categoryarray = plot_df$CellType),
                 yaxis = list(title = "Proportion"))
        
        return(p)
      }
    }
    
    plot_ly() %>% 
      layout(title = "No valid data available for plotting",
             xaxis = list(title = ""),
             yaxis = list(title = ""))
  })
  
  output$customTable <- renderDT({
    req(values$custom_results)
    
    if (!is.null(values$custom_results$cell.type.proportions)) {
      dt <- datatable(
        round(t(values$custom_results$cell.type.proportions), 4),
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          lengthMenu = c(5, 10, 15, 20)
        ),
        rownames = TRUE,
        caption = "Cell Type Proportions in Target Data"
      ) %>%
        formatStyle(
          columns = colnames(t(values$custom_results$cell.type.proportions)),
          background = styleColorBar(c(0, 1), 'lightblue'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
      return(dt)
    } else if (!is.null(values$custom_results$bulk.prop) && !is.null(values$custom_results$pseudobulk.prop)) {
      # Benchmark mode table
      bulk_data <- as.data.frame(round(t(values$custom_results$bulk.prop), 4))
      pseudo_data <- as.data.frame(round(t(values$custom_results$pseudobulk.prop), 4))
      
      bulk_data$Source <- "Bulk"
      pseudo_data$Source <- "PseudoBulk"
      
      combined <- rbind(bulk_data, pseudo_data)
      
      dt <- datatable(
        combined,
        options = list(
          scrollX = TRUE,
          pageLength = 15,
          lengthMenu = c(5, 10, 15, 20)
        ),
        rownames = TRUE,
        caption = "Benchmark Results"
      )
      
      numeric_cols <- setdiff(colnames(combined), "Source")
      
      if (length(numeric_cols) > 0) {
        dt <- dt %>%
          formatStyle(
            columns = numeric_cols,
            background = styleColorBar(c(0, 1), 'lightblue'),
            backgroundSize = '100% 90%',
            backgroundRepeat = 'no-repeat',
            backgroundPosition = 'center'
          )
      }
      
      return(dt)
    }
    
    return(datatable(data.frame(Message = "No valid data available for display"), 
                     caption = "Data Error", options = list(dom = 't')))
  })
  
  # Add download handler
  output$downloadAdvancedResults <- downloadHandler(
    filename = function() {
      paste0("user-defined-demixsc-results-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv")
    },
    content = function(file) {
      req(values$custom_results)
      if (!is.null(values$custom_results$cell.type.proportions)) {
        write.csv(values$custom_results$cell.type.proportions, file)
      } else {
        results_list <- list(
          bulk = values$custom_results$bulk.prop,
          pseudobulk = values$custom_results$pseudobulk.prop
        )
        saveRDS(results_list, file)
      }
    }
  )
  
  output$processingIndicatorCustom <- renderUI({
    if (values$is_processing) {
      div(
        style = "text-align: center; margin-top: 20px;",
        spin_flower(),
        p("Processing your data... This may take a few minutes.")
      )
    }
  })
  
  output$errorDisplayCustom <- renderUI({
    if (!is.null(values$error_message)) {
      div(
        style = "color: red; background-color: #ffeeee; padding: 10px; border-radius: 5px; margin-top: 10px;",
        icon("exclamation-triangle"),
        p(values$error_message)
      )
    }
  })
  
  # Update sample choices for retina analysis
  observe({
    req(values$retina_results)
    
    if (input$retinaDataset == "AMD Cohort") {
      sample_ids <- colnames(values$retina_results$cell.type.proportions)
    } else {
      if (!is.null(values$retina_results$bulk.prop)) {
        sample_ids <- colnames(values$retina_results$bulk.prop)
      } else {
        sample_ids <- character(0)
      }
    }
    
    updateSelectInput(session, "retinaSampleID", choices = sample_ids)
  })
  
  # Update sample choices for HGSC analysis
  observe({
    req(values$hgsc_results)
    
    if (input$hgscDataset == "Lee Cohort") {
      sample_ids <- colnames(values$hgsc_results$cell.type.proportions)
    } else {
      if (!is.null(values$hgsc_results$bulk.prop)) {
        sample_ids <- colnames(values$hgsc_results$bulk.prop)
      } else {
        sample_ids <- character(0)
      }
    }
    
    updateSelectInput(session, "hgscSampleID", choices = sample_ids)
  })
  
  # Update sample choices for retina predefined analysis
  observe({
    req(values$retina_predefined_results)
    sample_ids <- colnames(values$retina_predefined_results$cell.type.proportions)
    updateSelectInput(session, "retinaPredefinedSampleID", choices = sample_ids)
  })
  
  # Update sample choices for HGSC predefined analysis
  observe({
    req(values$hgsc_predefined_results)
    sample_ids <- colnames(values$hgsc_predefined_results$cell.type.proportions)
    updateSelectInput(session, "hgscPredefinedSampleID", choices = sample_ids)
  })
  
  # Update sample choices for custom analysis
  observe({
    req(values$custom_results)
    
    if (!is.null(values$custom_results$cell.type.proportions)) {
      sample_ids <- colnames(values$custom_results$cell.type.proportions)
    } else if (!is.null(values$custom_results$bulk.prop)) {
      sample_ids <- colnames(values$custom_results$bulk.prop)
    } else {
      sample_ids <- character(0)
    }
    
    updateSelectInput(session, "customSampleID", choices = sample_ids)
  })
}
