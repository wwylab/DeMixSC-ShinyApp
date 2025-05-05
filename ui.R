library(shiny)
library(plotly)
library(shinyjs)
library(DT)
library(shinyWidgets)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Arial', sans-serif;
      }
      .title-panel {
        background-color: #f8f9fa;
        padding: 15px;
        margin-bottom: 20px;
        border-radius: 5px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .param-box {
        background-color: #f8f9fa;
        padding: 15px;
        border-radius: 5px;
        margin-bottom: 15px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.1);
      }
      .result-box {
        background-color: white;
        padding: 15px;
        border-radius: 5px;
        margin-top: 15px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.1);
      }
      .btn-primary {
        background-color: #3498db;
      }
      #progress {
        position: fixed;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        width: 50%;
        z-index: 9999;
      }
      .info-text {
        font-size: 0.9em;
        color: #666;
        margin-top: 5px;
      }
      h4 {
        color: #2c3e50;
      }
    "))
  ),
  
  div(class = "title-panel",
      titlePanel("DeMixSC-on-Web: Cell Type Deconvolution Framework"),
      p("A deconvolution framework using single-cell sequencing plus benchmark datasets for accurate analysis of cell type ratios")
  ),
  
  navbarPage(
    title = div(
    ),
    id = "navBar",
    
    # About tab
    tabPanel("About",
             fluidRow(
               column(12,
                      div(class = "result-box",
                          h3("Description"),
                          p("Bulk deconvolution with single-cell/nucleus RNA-seq data is critical for understanding heterogeneity in complex biological samples, yet the technological 
                            discrepancy across sequencing platforms limits deconvolution accuracy. To address this, we utilize an experimental design to match inter-platform biological 
                            signals, hence revealing the technological discrepancy, and then develop a deconvolution framework called DeMixSC using this well-matched, i.e., benchmark, 
                            data."),
                          p("Built upon a novel weighted nonnegative least-squares framework, DeMixSC identifies and adjusts genes with high technological discrepancy and aligns 
                            the benchmark data with large patient cohorts of matched-tissue-type for large-scale deconvolution. Our results using two benchmark datasets of healthy retinas 
                            and ovarian cancer tissues suggest much-improved deconvolution accuracy."),
                          
                          h3("How it Works"),
                          tags$ul(
                            tags$li(strong("Input: "), "The algorithm requires a count matrix and cell type annotations. For benchmark-based deconvolution, it also utilizes a paired benchmark of bulk and pseudobulk RNA-seq data."),
                            tags$li(strong("Processing: "), "DeMixSC identifies and weights genes based on technological discrepancy between single-cell and bulk RNA-seq data."),
                            tags$li(strong("Output: "), "Cell type proportions for your samples, visualized with interactive charts and downloadable as CSV files.")
                          ),
                          
                          h3("Available Analysis Modules"),
                          tags$ul(
                            tags$li(strong("Pre-defined Analysis"), " - Uses built-in reference datasets for retina or HGSC (High-Grade Serous Ovarian Carcinoma)."),
                            tags$li(strong("Custom Analysis"), " - Upload your own data for a customized analysis."),
                            tags$li(strong("Example Data"), " - Try out the app with included example datasets.")
                          ),
                          
                          h4("Contact Information"),
                          p(strong("For issues with the app, please contact:")),
                          p("Aaron Wu - ", a(href="mailto:aw80@rice.edu", "aw80@rice.edu")),
                          p("Quang Tran - ", a(href="mailto:qmtran@mdanderson.org", "qmtran@mdanderson.org")),
                          
                          h4("Citation"),
                          p("Guo, S., Liu, X., Cheng, X., Jiang, Y., Ji, S., Liang, Q., … & Wang, W. (2023). The DeMixSC deconvolution framework uses single-cell sequencing plus a small benchmark dataset for improved analysis of cell-type ratios in complex tissue samples."),
                          a(href="https://genome.cshlp.org/content/early/2024/11/22/gr.278822.123.abstract",
                            "https://genome.cshlp.org/content/early/2024/11/22/gr.278822.123.abstract")
                      )
               )
             )
    ),
    
    # Quick Start tab
    tabPanel("Quick Start",
             fluidRow(
               column(12, 
                      div(class = "result-box",
                          h3("Quick Start Guide"),
                          
                          h4("Step 1: Choose your analysis type"),
                          p("Select the type of analysis you want to perform:"),
                          tags$ul(
                            tags$li(strong("Pre-loaded Reference Data:"), " Use the 'Package Data' tab to run analysis with built-in retina or HGSC datasets"),
                            tags$li(strong("Custom Analysis:"), " Use the 'Custom Analysis' tab to upload and analyze your own data")
                          ),
                          
                          h4("Step 2: Load your data or use example data"),
                          p("If using custom analysis:"),
                          tags$ul(
                            tags$li("Upload your count matrix (genes × samples) in CSV format with gene names as the first column"),
                            tags$li("Upload your cell type annotations file"),
                            tags$li("Or click 'Load Example Data' to try with a pre-loaded dataset")
                          ),
                          
                          h4("Step 3: Set parameters and run analysis"),
                          p("Adjust parameters such as minimum expression threshold and scale factor, then click 'Run Analysis'"),
                          
                          h4("Step 4: View and download results"),
                          p("After analysis completes:"),
                          tags$ul(
                            tags$li("Interactive visualizations will show cell type proportions"),
                            tags$li("Detailed tables provide the numerical results"),
                            tags$li("Use the download button to save results as CSV files")
                          ),
                          
                          hr(),
                          
                          h4("Supported File Formats"),
                          tags$ul(
                            tags$li(strong("Count Matrix:"), " CSV with genes in rows, samples in columns, gene names as first column"),
                            tags$li(strong("Annotations:"), " CSV with sample identifiers and corresponding metadata")
                          ),
                          
                          h4("Example Usage Scenario"),
                          p("A researcher has bulk RNA-seq data from retina samples and wants to estimate the proportions of different cell types:"),
                          tags$ol(
                            tags$li("Go to 'Package Data' tab and select 'Retina' and 'AMD Cohort'"),
                            tags$li("Click 'Run Analysis' and wait for processing to complete"),
                            tags$li("View the cell type proportion results and download for further analysis")
                          ),
                          br()
                      )
               )
             )
    ),
    
    # Example tab
    tabPanel("Example",
             tabsetPanel(
               tabPanel("Retina",
                        sidebarLayout(
                          sidebarPanel(
                            width = 3,
                            div(class = "param-box",
                                h4("Retina Analysis"),
                                
                                selectInput("retinaDataset", "Select Dataset:", 
                                            choices = c("AMD Cohort" = "AMD Cohort", 
                                                        "Benchmark" = "Benchmark")),
                                
                                div(class = "info-text", 
                                    HTML("<b>AMD Cohort:</b> 453 age-related macular degeneration samples<br>
                                         <b>Benchmark:</b> Paired bulk and pseudobulk data")),
                                
                                hr(),
                                
                                actionButton("runRetina", "Run Analysis", 
                                             class = "btn-primary btn-block"),
                                
                                br(),
                                downloadButton("downloadRetinaResults", 
                                               "Download Results", 
                                               class = "btn-success btn-block")
                            )
                          ),
                          mainPanel(
                            width = 9,
                            div(class = "result-box",
                                # Processing indicator and error messages
                                uiOutput("processingIndicator"),
                                uiOutput("errorDisplay"),
                                
                                # Results visualization
                                h3("Retina Analysis Results"),
                                p("Cell type proportions in retinal tissue:"),
                                
                                tabsetPanel(
                                  tabPanel("Chart",
                                           plotlyOutput("retinaPlot", height = "500px")
                                  ),
                                  tabPanel("Table",
                                           DTOutput("retinaTable")
                                  ),
                                  tabPanel("Documentation",
                                           h4("About Retina Dataset"),
                                           p("The retina dataset includes:"),
                                           tags$ul(
                                             tags$li(strong("AMD Cohort: "), "453 bulk RNA-seq samples from the Ratnapriya et al. AMD retina cohort"),
                                             tags$li(strong("Benchmark: "), "Paired bulk and pseudobulk samples for accurate deconvolution")
                                           ),
                                           
                                           h4("Cell Types"),
                                           p("Major cell types in the analysis include:"),
                                           tags$ul(
                                             tags$li("AC: Amacrine cells"),
                                             tags$li("BC: Bipolar cells"),
                                             tags$li("Cone: Cone photoreceptors"),
                                             tags$li("HC: Horizontal cells"),
                                             tags$li("MG: Müller glia"),
                                             tags$li("RGC: Retinal ganglion cells"),
                                             tags$li("Rod: Rod photoreceptors"),
                                             tags$li("Astrocyte: Astrocytes"),
                                             tags$li("Microglia: Microglial cells"),
                                             tags$li("RPE: Retinal pigment epithelium")
                                           ),
                                           
                                           h4("Visualization Guide"),
                                           p("The chart visualizes proportions of each cell type across samples:"),
                                           tags$ul(
                                             tags$li("X-axis: Cell types"),
                                             tags$li("Y-axis: Proportion (0-1)"),
                                             tags$li("For AMD cohort: Samples are colored by MGS level (severity)")
                                           )
                                  )
                                )
                            )
                          )
                        )
               ),
               tabPanel("HGSC",
                        sidebarLayout(
                          sidebarPanel(
                            width = 3,
                            div(class = "param-box",
                                h4("HGSC Analysis"),
                                
                                selectInput("hgscDataset", "Select Dataset:", 
                                            choices = c("Lee Cohort" = "Lee Cohort", 
                                                        "Benchmark" = "Benchmark")),
                                
                                div(class = "info-text", 
                                    HTML("<b>Lee Cohort:</b> High-grade serous ovarian carcinoma cohort<br>
                                         <b>Benchmark:</b> Paired bulk and pseudobulk data")),
                                
                                hr(),
                                
                                actionButton("runHGSC", "Run Analysis", 
                                             class = "btn-primary btn-block"),
                                
                                br(),
                                downloadButton("downloadHGSCResults", 
                                               "Download Results", 
                                               class = "btn-success btn-block")
                            )
                          ),
                          mainPanel(
                            width = 9,
                            div(class = "result-box",
                                # Processing indicator and error messages
                                uiOutput("processingIndicator"),
                                uiOutput("errorDisplay"),
                                
                                # Results visualization
                                h3("HGSC Analysis Results"),
                                p("Cell type proportions in high-grade serous ovarian carcinoma:"),
                                
                                tabsetPanel(
                                  tabPanel("Chart",
                                           plotlyOutput("hgscPlot", height = "500px")
                                  ),
                                  tabPanel("Table",
                                           DTOutput("hgscTable")
                                  ),
                                  tabPanel("Documentation",
                                           h4("About HGSC Dataset"),
                                           p("The HGSC dataset includes:"),
                                           tags$ul(
                                             tags$li(strong("Lee Cohort: "), "RNA-seq samples from high-grade serous ovarian carcinoma patients"),
                                             tags$li(strong("Benchmark: "), "Paired bulk and pseudobulk samples for accurate deconvolution")
                                           ),
                                           
                                           h4("Cell Types"),
                                           p("Major cell types in the analysis include:"),
                                           tags$ul(
                                             tags$li("Epithelial cells"),
                                             tags$li("Endothelial cells"),
                                             tags$li("Fibroblasts"),
                                             tags$li("B cells"),
                                             tags$li("T cells"),
                                             tags$li("NK cells"),
                                             tags$li("Macrophages"),
                                             tags$li("And other immune cells")
                                           ),
                                           
                                           h4("Visualization Guide"),
                                           p("The chart visualizes proportions of each cell type across samples:"),
                                           tags$ul(
                                             tags$li("X-axis: Cell types"),
                                             tags$li("Y-axis: Proportion (0-1)"),
                                             tags$li("For Lee cohort: Samples are colored by treatment response (R0, ER, PR)")
                                           )
                                  )
                                )
                            )
                          )
                        )
               )
             )
    ),
    
    # User-defined Analysis
    tabPanel("User-defined Analysis",
             fluidRow(
               column(3,
                      div(class = "param-box",
                          h4("User-Defined Analysis with Benchmark"),
                          p("For advanced users who want to provide their own benchmark data."),
                          
                          h4("Required Files"),
                          fileInput("referenceFile", "Reference Matrix (CSV):", 
                                    accept = c(".csv", ".txt")),
                          div(class = "info-text", "Single-cell based matrix with genes as rows and cell types as columns"),
                          
                          fileInput("matAFile", "Benchmark Bulk (CSV):", 
                                    accept = c(".csv", ".txt")),
                          div(class = "info-text", "Benchmark bulk RNA-seq data"),
                          
                          fileInput("matBFile", "Benchmark Pseudo-bulk (CSV):", 
                                    accept = c(".csv", ".txt")),
                          div(class = "info-text", "Benchmark pseudo-bulk RNA-seq data"),
                          
                          fileInput("targetBulkFile", "Target Bulk (CSV, optional):", 
                                    accept = c(".csv", ".txt")),
                          div(class = "info-text", "Target bulk samples to deconvolve (optional)"),
                          
                          hr(),
                          
                          actionButton("runAdvanced", "Run Advanced Analysis", 
                                       class = "btn-primary btn-block"),
                          
                          br(),
                          downloadButton("downloadAdvancedResults", 
                                         "Download Results",
                                         class = "btn-success btn-block")
                      )
               ),
               column(9,
                      div(class = "result-box",
                          h3("Advanced Analysis Documentation"),
                          
                          h4("User-Defined Analysis Workflow"),
                          p("This advanced module allows you to run DeMixSC with your own reference and benchmark data:"),
                          
                          tags$ol(
                            tags$li(strong("Benchmark Mode:"), " If you only provide reference, benchmark bulk, and benchmark pseudo-bulk files (no target bulk), DeMixSC will run in benchmark mode to evaluate deconvolution performance."),
                            tags$li(strong("Real Data Mode:"), " If you also provide a target bulk file, DeMixSC will deconvolve your target samples using your benchmark data as reference.")
                          ),
                          
                          h4("File Format Requirements"),
                          tags$ul(
                            tags$li(strong("Reference Matrix:"), " CSV with genes in rows and cell types in columns"),
                            tags$li(strong("Benchmark Bulk:"), " CSV with genes in rows and samples in columns"),
                            tags$li(strong("Benchmark Pseudo-bulk:"), " CSV with genes in rows and samples in columns"),
                            tags$li(strong("Target Bulk:"), " CSV with genes in rows and samples in columns")
                          ),
                          
                          h4("Tips for Successful Analysis"),
                          tags$ul(
                            tags$li("Ensure gene names are consistent across all files"),
                            tags$li("Pre-filter low-quality genes and samples for better results"),
                            tags$li("Start with default parameters and adjust based on results"),
                            tags$li("For optimal performance, aim for a well-matched benchmark dataset")
                          ),
                          
                          h4("Example Workflow"),
                          p("To run a benchmark evaluation:"),
                          tags$ol(
                            tags$li("Upload your reference matrix with cell type-specific profiles"),
                            tags$li("Upload paired benchmark data (bulk and pseudo-bulk)"),
                            tags$li("Run analysis in benchmark mode"),
                            tags$li("Evaluate accuracy by comparing with ground truth proportions")
                          )
                      )
               )
             )
    ),
    
    # Citations tab
    tabPanel("Citations",
             div(class = "result-box",
                 h3("Citations"),
                 
                 h4("Published manuscript:"),
                 p("Shuai Guo, Xiaoqian Liu, Xuesen Cheng, Yujie Jiang, Shuangxi Ji, Qingnan Liang, Andrew Koval, Yumei Li, 
                   Leah A. Owen, Ivana K. Kim, Ana Aparicio, Sanghoon Lee, Anil K. Sood, Scott Kopetz, John Paul Shen, 
                   John N. Weinstein, Margaret M. DeAngelis, Rui Chen, Wenyi Wang"),
                 p("The DeMixSC deconvolution framework uses single-cell sequencing plus a small benchmark dataset for improved analysis of cell-type ratios in complex tissue samples."),
                 a(href="https://genome.cshlp.org/content/early/2024/11/22/gr.278822.123.abstract",
                   "https://genome.cshlp.org/content/early/2024/11/22/gr.278822.123.abstract", target="_blank"),
                 
                 hr(),
                 
                 h4("Related Publications:"),
                 
                 h5("Retina Data"),
                 p("Liang, Q., Dharmat, R., Owen, L., Shakoor, A., Li, Y., Kim, S., … & Chen, R. (2019). Single-nuclei RNA-seq on human retinal tissue provides improved transcriptome profiling."),
                 a(href="https://www.nature.com/articles/s41467-019-12917-9",
                   "Nature communications, 10(1), 5743", target="_blank"),
                 
                 p("Ratnapriya, R., Sosina, O. A., Starostik, M. R., Kwicklis, M., Kapphahn, R. J., Fritsche, L. G., … & Swaroop, A. (2019). Retinal transcriptome and eQTL analyses identify genes associated with age-related macular degeneration."),
                 a(href="https://www.nature.com/articles/s41588-019-0351-9",
                   "Nature genetics, 51(4), 606-610", target="_blank"),
                 
                 h5("HGSC Data"),
                 p("Hippen, A. A., Omran, D. K., Weber, L. M., Jung, E., Drapkin, R., Doherty, J. A., … & Greene, C. S. (2023). Performance of computational algorithms to deconvolve heterogeneous bulk ovarian tumor tissue depends on experimental factors."),
                 a(href="https://genomebiology.biomedcentral.com/articles/10.1186/s13059-023-03077-7",
                   "Genome biology, 24(1), 239", target="_blank"),
                 
                 p("Lee, S., Zhao, L., Rojas, C., Bateman, N. W., Yao, H., Lara, O. D., … & Sood, A. K. (2020). Molecular analysis of clinically defined subsets of high-grade serous ovarian cancer."),
                 a(href="https://www.cell.com/cell-reports/fulltext/S2211-1247(20)30392-2",
                   "Cell reports, 31(2)", target="_blank")
             )
    )
  ),
  
  div(
    style = "text-align: center; margin-top: 30px; padding: 20px; background-color: #f8f9fa; border-top: 1px solid #ddd;",
    p("Copyright © 2024 Wang Lab at MD Anderson. All rights reserved."),
    p("DeMixSC version 1.0.0")
  )
)
