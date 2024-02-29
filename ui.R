# List required packages
required_packages <- c("shiny", "shinydashboard", "shinythemes", "shinycssloaders")

# Check if packages are not installed, then install them
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# Load required libraries
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinycssloaders)

ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    theme = shinytheme("flatly"),
    tags$head(
      tags$style(HTML("
        body { background-color: #f4f4f4; font-family: 'Noto Sans', sans-serif; }
        .center-text { text-align: center; }
        .custom-font { font-family: 'Helvetica'; color: #2C3E50; }
        .example-title { font-size: 28px; font-weight: bold; color: #333; }
        .btn-primary { background-color: #0056b3; color: white; border-radius: 5px; padding: 10px 15px; }
        .input-section, .output-section { border: 1px solid #ccc; padding: 20px; margin-bottom: 20px; background-color: white; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
        .divider { border-top: 1px solid #eee; margin: 20px 0; }
        .left-align { text-align: left; }
        .right-align { text-align: right; }
        .footer { background-color: #f4f4f4; color: #333; text-align: left; padding: 10px; }
        .description-box { padding: 20px; background-color: #fff; box-shadow: 0 2px 4px rgba(0,0,0,0.1); margin-bottom: 20px; }
        .description-box h3 { color: #0056b3; font-size: 20px; }
        .description-box p { font-size: 16px; color: #333; }
        /* Center-align cell data and column names */
        #sens_spec table th, 
        #sens_spec table td:nth-child(n+2) {
          text-align: center !important;
        }
      "))
    ),
    fluidRow(
      box(
        title = NULL, status = "primary", solidHeader = FALSE, collapsible = FALSE,
        width = 12,
        h3(strong('Comparative Analysis of SNP Calling Methods in the Absence of Gold Standard'), class = 'center-text custom-font')
      )
    ),
    fluidRow(
      box(title = "Data Upload & Settings", status = "primary", solidHeader = TRUE, width = 12,
          box(title = "Custom Analytics", status = 'info', style = "min-height: 250px;",
              solidHeader = FALSE, width = 6,
              fluidRow(
                column(9,
                       fileInput("file1", "Choose CSV File",
                                 accept = c(".csv"),
                                 buttonLabel = "Browse...",
                                 placeholder = "File size < 5MB. Include only POS of variations.")
                ),
                column(1,
                       style = "padding-top: 25px;",
                       align = "center",
                       downloadButton("downloadPreprocessor", "Preprocessor")
                ),
                column(9,
                       numericInput("baseCount", "Reference Base Count", value = 1),
                       helpText("Note: Enter the total number bases in your specified segment.")
                ),
                column(1,
                       style = "padding-top: 83px;",
                       align = "center",
                       actionButton("submit", "Run Analysis", icon = icon("play"))
                )
              )
          ),
          box(title = "Sample Explorer", status = 'warning', style = "min-height: 250px;",
              solidHeader = FALSE, width = 6,
              selectInput("exampleSelect", "Choose Example",
                          choices = c("Example1", "Example2", "Example3")),
              uiOutput("exampleUI")
          )
      )
    ),
    fluidRow(
      box(
        title = "Analysis Results & Visualization", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 12,
        conditionalPanel(
          condition = "input.submit > 0 || input.runAnalysis1 > 0 || input.runAnalysis2 > 0 || input.runAnalysis3 > 0",
          div(class = 'description-box',
              h3('Description of Analysis'),
              p('In this interface, we provide sensitivity and specificity estimated by the latent class model under the assumptions of conditional independence, constant random effects, and varying random effects, as well as the BIC (Bayesian Information Criterion) values for each assumption. We recommend that you report the results of your data based on the assumption that corresponds to the minimum correlation residuals. If all correlation residuals are large, it indicates that your data does not meet the three assumptions provided on this webpage. You can refer to our article for new ideas.')
          ),
          div(class = 'output-section',
              tabsetPanel(
                tabPanel("Venn Diagram",
                         br(),
                         fluidRow(
                           column(width = 8, offset = 2, align = "center",
                                  withSpinner(plotOutput("vennPlot"), type = 8)
                           )
                         ),
                         downloadButton("downloadVennClass", "Download CSV"),
                         downloadButton("downloadVennPlot", "Download PDF")
                ),
                tabPanel("Sensitivity & False Positive Rate",
                         br(),
                         fluidRow(
                           column(width = 12, align = "center",
                                  withSpinner(tableOutput("sens_spec"), type = 8)
                           ),
                           br(),
                           downloadButton("downloadSensSpec", "Download CSV")
                         )
                ),
                tabPanel("Residual Analysis",
                         br(),
                         fluidRow(
                           column(width = 6, offset = 3, align = "center",
                                  withSpinner(plotOutput("residualPlot_LCA"), type = 8)
                           )
                         ),
                         downloadButton("downloadResidualPlot", "Download PNG"),
                         downloadButton("downloadResidualPlotPDF", "Download PDF")
                ),
                tabPanel("SNP Count Estimation",
                         br(),
                         fluidRow(
                           column(width = 10, offset = 1, align = "center",
                                  withSpinner(tableOutput("numbersnp"), type = 8)
                           )
                         ),
                         downloadButton("downloadExpectedNumberSNP", "Download CSV")
                )
              )
          )
        )
      )
    ),
    tags$footer(
      style = "background-color: #f4f4f4; color: #333; text-align: left; padding: 10px;",
      HTML(
        "<div style='font-size: 24px; line-height: 1.5;'>
      <h5>Contact: 
        <a href='https://github.com/LiSH7450' target='_blank' style='color: #0056b3;'>Qianqian Song</a>
        <br>Email: 
        <a href='mailto:songqq0201@stu.pku.edu.cn' style='color: #0056b3;'>songqq0201@stu.pku.edu.cn</a>
      </h5>
      <strong>
        <h5>
          <a href='https://github.com/LiSH7450' target='_blank' style='color: #0056b3;'> 
            Department of Biostatistics, School of Public Health, Peking University
          </a>
        </h5>
      </strong>
    </div>"
      )
    )
    
  )
)