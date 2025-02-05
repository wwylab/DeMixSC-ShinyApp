library(shiny)
library(plotly)
library(shinyjs)
library(DT)
library(shinyWidgets)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  
  # CSS styling
  tags$head(
    tags$style(HTML("
      #progress {
        position: fixed;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        width: 50%;
        z-index: 9999;
      }
    "))
  ),
  
  titlePanel("DeMixSC: A deconvolution framework that uses single-cell sequencing plus a small benchmark dataset for accurate analysis of cell type ratios in complex tissue samples"),
  div(p("Copyright © 2024 Wang Lab at MD Anderson. All rights reserved.")),
  
  navbarPage("",
             # About tab
             tabPanel("About",
                      tags$div(
                        tags$h4(tags$b("Description")),
                        p("Bulk deconvolution with single-cell/nucleus RNA-seq data is critical for understanding heterogeneity in complex biological samples, yet the technological 
                          discrepancy across sequencing platforms limits deconvolution accuracy. To address this, we utilize an experimental design to match inter-platform biological 
                          signals, hence revealing the technological discrepancy, and then develop a deconvolution framework called DeMixSC using this well-matched, i.e., benchmark, 
                          data. Built upon a novel weighted nonnegative least-squares framework, DeMixSC identifies and adjusts genes with high technological discrepancy and aligns 
                          the benchmark data with large patient cohorts of matched-tissue-type for large-scale deconvolution. Our results using two benchmark datasets of healthy retinas 
                          and ovarian cancer tissues suggest much-improved deconvolution accuracy. Leveraging tissue-specific benchmark datasets, we applied DeMixSC to a large cohort 
                          of 453 age-related macular degeneration patients and a cohort of 30 ovarian cancer patients with various responses to neoadjuvant chemotherapy. Only DeMixSC 
                          successfully unveiled biologically meaningful differences across patient groups, demonstrating its broad applicability in diverse real-world clinical 
                          scenarios. Our findings reveal the impact of technological discrepancy on deconvolution performance and underscore the importance of a well-matched dataset 
                          to resolve this challenge. The developed DeMixSC framework is generally applicable for accurately deconvolving large cohorts of disease tissues, including 
                          cancers, when a well-matched benchmark dataset is available."),
                        tags$br(),
                        p(strong("For issues with the app, please contact:")),
                        p("Aaron Wu - ", a(href="mailto:aw80@rice.edu", "aw80@rice.edu")),
                        p("Quang Tran - ", a(href="mailto:qmtran@mdanderson.org", "qmtran@mdanderson.org"))
                      )
             ),
             
             # Tutorial tab  
             tabPanel("Tutorial",
                      tags$div(
                        tags$iframe(
                          src = "https://wwylab.github.io/DeMixSC/",
                          width = "100%",
                          height = "800px",
                          frameborder = "0",
                          style = "border: none;"
                        )
                      )
             ),
             
             # Custom Analysis tab
             tabPanel("Custom Analysis",
                      sidebarLayout(
                        sidebarPanel(
                          tags$div(
                            p("Required input files:"),
                            p("1. Count Matrix (genes x samples)"),
                            p("2. Cell Type Annotations"),
                            
                            fileInput("countsFile", "Upload Count Matrix:", accept = c(".csv", ".txt")),
                            fileInput("annotFile", "Upload Annotations:", accept = c(".csv", ".txt")),
                            numericInput("minExpression", "Minimum Expression:", value = 3),
                            numericInput("scaleFactor", "Scale Factor:", value = 1e5),
                            actionButton("runAnalysis", "Run Analysis", class = "btn-primary"),
                            br(),
                            downloadButton("downloadCustomResults", "Download Results")
                          )
                        ),
                        mainPanel(
                          plotlyOutput("customPlot1"),
                          DTOutput("customTable")
                        )
                      )
             ),
             
             # Package Data tab
             tabPanel("Package Data",
                      tags$div(
                        tabsetPanel(
                          tabPanel("Retina",
                                   tags$div(
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput("retinaDataset", "Select Dataset:", 
                                                     choices = c("AMD Cohort", "Benchmark")),
                                         actionButton("runRetina", "Run Analysis", 
                                                      class = "btn-primary"),
                                         br(), br(),
                                         downloadButton("downloadRetinaResults", 
                                                        "Download Results")
                                       ),
                                       mainPanel(
                                         plotlyOutput("retinaPlot"),
                                         DTOutput("retinaTable")
                                       )
                                     )
                                   )
                          ),
                          tabPanel("HGSC",
                                   tags$div(
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput("hgscDataset", "Select Dataset:", 
                                                     choices = c("Lee Cohort", "Benchmark")),
                                         actionButton("runHGSC", "Run Analysis", 
                                                      class = "btn-primary"),
                                         br(), br(),
                                         downloadButton("downloadHGSCResults", 
                                                        "Download Results")
                                       ),
                                       mainPanel(
                                         plotlyOutput("hgscPlot"),
                                         DTOutput("hgscTable")
                                       )
                                     )
                                   )
                          )
                        )
                      )
             ),
             
             # Citations tab
             tabPanel("Citations",
                      tags$div(
                        h4(tags$b("Published manuscript:")),
                        p("Shuai Guo, Xiaoqian Liu, Xuesen Cheng, Yujie Jiang, Shuangxi Ji, Qingnan Liang, Andrew Koval, Yumei Li, 
                          Leah A. Owen, Ivana K. Kim, Ana Aparicio, Sanghoon Lee, Anil K. Sood, Scott Kopetz, John Paul Shen, 
                          John N. Weinstein, Margaret M. DeAngelis, Rui Chen, Wenyi Wang"),
                        a(href="https://genome.cshlp.org/content/early/2024/11/22/gr.278822.123.abstract",
                          "https://genome.cshlp.org/content/early/2024/11/22/gr.278822.123.abstract")
                      )
             )
  )
)
