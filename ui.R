# ui.R
library(shiny)
library(bslib)
library(refineR)
library(readxl)
library(moments)
library(shinyjs)
library(shinyWidgets)
library(shinyFiles)
library(ggplot2)
library(bsicons)

# Defines the main UI using a navigation bar with three tabs
ui <- navbarPage(
  title = tagList(
    div(bs_icon("clipboard-pulse"), "Analysis tool", 
        style = "font-size: 1.4rem; font-weight: 500; line-height: 1; color: #555;"),
    div("VeritasRI", 
        style = "font-size: 1.8em; color: #000000ff; line-height: 1.1;")
  ),
  id = "tabs",
  theme = bs_theme(version = 5, base_font = font_google("Inter"), heading_font = font_google("Rethink Sans"), font_scale = 1.1, bootswatch = "default"),

  # First tab for the main RefineR analysis
  tabPanel(
    title = "Main Analysis",
    useShinyjs(),
    # JavaScript to manage tab disabling during analysis
    tags$head(
      includeCSS("www/styles.css"),
      tags$script(HTML("
        var analysisRunning = false;
        Shiny.addCustomMessageHandler('analysisStatus', function(status) {
          analysisRunning = status;
          if (status) {
            $('a[data-toggle=\"tab\"]').each(function() {
              if (!$(this).parent().hasClass('active')) {
                $(this).addClass('disabled-tab-link');
              }
            });
          } else {
            $('a.disabled-tab-link').removeClass('disabled-tab-link');
          }
        });
        $(document).on('click', 'a.disabled-tab-link', function(event) {
          event.preventDefault();
          Shiny.setInputValue('tab_switch_blocked', new Date().getTime());
          return false;
        });

        // JavaScript to handle download button state
        $(document).on('click', '.download-btn', function() {
          var button = $(this);
          if (button.hasClass('disabled-download')) {
            return;
          }
          // Store the original HTML content (icon + text)
          var original_html = button.html();
          // Change the button to the 'Downloading...' state
          button.addClass('disabled-download').html('<i class=\"bi bi-hourglass-split\"></i> Downloading...');
          // After a delay, restore the original HTML content
          setTimeout(function() {
            button.removeClass('disabled-download').html(original_html);
          }, 3000); // Re-enable after 3 seconds
        });
      "))
    ),
    sidebarLayout(
      sidebarPanel(
        style = "padding-right: 15px;",
        
        div(class = "card", style = "border: 1px solid #ccc; border-radius: 8px;",
          div(class = "card-header", style = "background-color: #f7f7f7; padding: 10px; border-bottom: 1px solid #ccc; border-top-left-radius: 8px; border-top-right-radius: 8px;",
            h5(
              tooltip(
                trigger = list("Main Analysis Inputs", bs_icon("info-circle")),
                "This section contains the core inputs for filtering data and running the main RefineR analysis, including gender, age range, and model selection."
              ),
              style = "margin-top: 0; margin-bottom: 0;"
            )
          ),
          div(class = "card-body", style = "padding: 15px;",
            selectInput(inputId = "gender_choice", label = "Select Gender:", choices = c("Male" = "M", "Female" = "F", "Both" = "Both"), selected = "Both"),
            sliderInput(inputId = "age_range", label = "Age Range:", min = 0, max = 100, value = c(0, 100), step = 1)
          )
        ),
        
        br(),
        fileInput(inputId = "data_file", label = "Upload Data (Excel File)", accept = c(".xlsx")),
        hr(),
        selectInput(inputId = "col_value", label = "Select Column for Values:", choices = c("None" = ""), selected = ""),
        selectInput(inputId = "col_age", label = "Select Column for Age:", choices = c("None" = ""), selected = ""),
        selectInput(inputId = "col_gender", label = "Select Column for Gender:", choices = c("None" = ""), selected = ""),
        hr(),
        sliderInput(
          inputId = "nbootstrap_speed",
          label = tags$span(
            tooltip(
              trigger = list(tags$span(bs_icon("info-circle"))),
              "This slider controls the number of bootstrap iterations, a statistical method used to verify the stability and accuracy of the calculated reference interval. Higher values lead to more precise confidence intervals at the cost of longer computation time. For final results, a higher value (e.g., 200) is recommended."
            ),
            "Select Computation Speed:"
          ),
          min = 1, max = 200, value = 50, step = 1
        ),
        radioButtons(inputId = "model_choice",
                     label = tags$span(
                       tooltip(
                         trigger = list(tags$span(bs_icon("info-circle"))),
                         "BoxCox: For positive-valued data with light to moderate skewness. modBoxCox: For data with high skewness or values close to zero. Auto-select: Automatically chooses the optimal transformation based on data skewness."
                       ),
                       "Select Transformation Model:"
                     ),
                     choices = c("BoxCox" = "BoxCox", "modBoxCox" = "modBoxCox", "Auto-select" = "AutoSelect"),
                     selected = "AutoSelect", inline = TRUE),
        
        div(class = "main-buttons",
            actionButton("analyze_btn", "Run Analysis", class = "btn-primary"),
            actionButton("reset_btn", "Reset File", class = "btn-secondary"),
            downloadButton("download_report", "Download Report", class = "download-btn")
        ),
        uiOutput("main_message"),
        hr(),
        tags$details(
          tags$summary(style = "cursor: pointer; font-weight: bold;", "Advanced Settings"),
          div(style = "padding-top: 10px;",
            numericInput("ref_low", label = tags$span(tooltip(tags$span(bs_icon("info-circle")), "Enter known reference limits here. They will be added to the plot as dashed lines for direct comparison against the calculated interval."), "Reference Lower Limit:"), value = NA),
            numericInput("ref_high", label = tags$span(tooltip(tags$span(bs_icon("info-circle")), "Enter known reference limits here. They will be added to the plot as dashed lines for direct comparison against the calculated interval."), "Reference Upper Limit:"), value = NA),
            textInput(inputId = "unit_input", label = "Unit of Measurement", value = NA, placeholder = "ex. g/L"),
            hr()
          )
        )
      ),
      mainPanel(
        plotOutput("result_plot"),
        verbatimTextOutput("result_text")
      )
    )
  ),

  # Second tab for Gaussian Mixture Model (GMM) subpopulation detection
  tabPanel(
    title = "Subpopulation Detection (GMM)",
    useShinyjs(),
    sidebarLayout(
      sidebarPanel(
        div(class = "card", style = "border: 1px solid #ccc; border-radius: 8px;",
          div(class = "card-header", style = "background-color: #f7f7f7; padding: 10px; border-bottom: 1px solid #ccc; border-top-left-radius: 8px; border-top-right-radius: 8px;",
            h5(
              tooltip(
                trigger = list("GMM Analysis", bs_icon("info-circle")),
                "Gaussian Mixture Models (GMM) detect hidden subpopulations. The mclust package selects the best model and components using BIC. Data is preprocessed with Yeo-Johnson transformation (if skewed) and standardization for values and age."
              ),
              style = "margin-top: 0; margin-bottom: 0;"
            )
          ),
          div(class = "card-body", style = "padding: 15px;",
            fileInput(inputId = "gmm_file_upload", label = "Upload Data (Excel File)", accept = c(".xlsx")),
            hr(),
            selectInput(inputId = "gmm_value_col", label = "Select Column for Values:", choices = c("None" = ""), selected = ""),
            selectInput(inputId = "gmm_age_col", label = "Select Column for Age:", choices = c("None" = ""), selected = ""),
            selectInput(
              inputId = "gmm_gender_col",
              label = tags$span(
                tooltip(
                  trigger = list(tags$span(bs_icon("info-circle")), "Select Column for Gender:"),
                  "Optional. If not selected, analysis will be run on combined data."
                )
              ),
              choices = c("None" = ""),
              selected = ""
            ),
            hr(),
            uiOutput("gmm_gender_choice_ui"),
            radioButtons(
              inputId = "gmm_model_selection_choice",
              label = "Select BIC Model Option:",
              choices = c("Auto-select", "Manual Selection"),
              selected = "Auto-select"
            ),
            uiOutput("gmm_manual_model_ui"),
            div(class = "gmm-buttons",
                actionButton("run_gmm_analysis_btn", "Run Analysis", class = "btn-primary"),
                actionButton("reset_gmm_analysis_btn", "Reset File", class = "btn-secondary"),
                downloadButton("download_gmm_report", "Download GMM Report", class = "download-btn")
            ),
            uiOutput("app_message")
          )
        )
      ),
      mainPanel(
        uiOutput("gmm_results_ui")
      )
    )
  ),

  # Third tab for Parallel RefineR Analysis
  tabPanel(
    title = "Parallel Analysis",
    useShinyjs(),
    sidebarLayout(
      sidebarPanel(
        style = "padding-right: 15px;",
        div(class = "card", style = "border: 1px solid #ccc; border-radius: 8px;",
          div(class = "card-header", style = "background-color: #f7f7f7; padding: 10px; border-bottom: 1px solid #ccc; border-top-left-radius: 8px; border-top-right-radius: 8px;",
            h5(
              tooltip(
                trigger = list("Parallel Analysis", bs_icon("info-circle")),
                "This tool runs multiple RefineR analyses for different subpopulations simultaneously using parallel processing, significantly speeding up computation time."
              ),
              style = "margin-top: 0; margin-bottom: 0;"
            )
          ),
          div(class = "card-body", style = "padding: 15px;",
            textAreaInput(
              inputId = "male_age_ranges",
              label = tags$span(tooltip(tags$span(bs_icon("info-circle")), "Enter age ranges for the male subpopulation. Use commas to separate multiple ranges."), "Male Age Ranges:"),
              rows = 1,
              placeholder = "e.g., 0-10, 10-20"
            ),
            textAreaInput(
              inputId = "female_age_ranges",
              label = tags$span(tooltip(tags$span(bs_icon("info-circle")), "Enter age ranges for the female subpopulation. Use commas to separate multiple ranges."), "Female Age Ranges:"),
              rows = 1,
              placeholder = "e.g., 0-10, 10-20"
            ),
            textAreaInput(
              inputId = "combined_age_ranges",
              label = tags$span(tooltip(tags$span(bs_icon("info-circle")), "Enter age ranges for the both subpopulations. Use commas to separate multiple ranges."), "All Genders Age Ranges:"),
              rows = 1,
              placeholder = "e.g., 0-10, 10-20"
            )
          )
        ),
        
        br(),
        fileInput(inputId = "parallel_file", label = "Upload Data (Excel File)", accept = c(".xlsx")),
        hr(),
        selectInput(inputId = "parallel_col_value", label = "Select Column for Values:", choices = c("None" = ""), selected = ""),
        selectInput(inputId = "parallel_col_age", label = "Select Column for Age:", choices = c("None" = ""), selected = ""),
        selectInput(inputId = "parallel_col_gender", label = "Select Column for Gender:", choices = c("None" = ""), selected = ""),
        hr(),
        sliderInput(
          inputId = "parallel_nbootstrap_speed",
          label = tags$span(tooltip(trigger = list(tags$span(bs_icon("info-circle"))), "This slider controls the number of bootstrap iterations, a statistical method used to verify the stability and accuracy of the calculated reference interval. Higher values lead to more precise confidence intervals at the cost of longer computation time. For final results, a higher value (e.g., 200) is recommended."), "Select Computation Speed:"),
          min = 1, max = 200, value = 50, step = 1
        ),
        radioButtons(inputId = "parallel_model_choice",
                     label = tags$span(tooltip(trigger = list(tags$span(bs_icon("info-circle"))), "BoxCox: For positive-valued data with light to moderate skewness. modBoxCox: For data with high skewness or values close to zero. Auto-select: Automatically chooses the optimal transformation based on data skewness."), "Select Transformation Model:"),
                     choices = c("BoxCox" = "BoxCox", "modBoxCox" = "modBoxCox", "Auto-select" = "AutoSelect"),
                     selected = "AutoSelect", inline = TRUE),
        
        div(class = "parallel-buttons",
            actionButton("run_parallel_btn", "Run Parallel Analysis", class = "btn-primary"),
            actionButton("reset_parallel_btn", "Reset File", class = "btn-secondary"),
            downloadButton("download_parallel_report", "Download Parallel Report", class = "download-btn")
        ),
        uiOutput("parallel_message"),
        hr(),
        tags$details(
          tags$summary(style = "cursor: pointer; font-weight: bold;", "Advanced Settings"),
          div(style = "padding-top: 10px;",
            numericInput("cores", label = tags$span(tooltip(tags$span(bs_icon("info-circle")), "Specify the number of CPU cores to use for the analysis. A higher number can significantly speed up the computation, but it's recommended not to exceed the number of physical cores in your computer to avoid performance issues."), "Number of Cores:"), value = 1, min = 1),
            textInput(inputId = "parallel_unit_input", label = "Unit of Measurement", value = NA, placeholder = "ex. g/L"),
            hr()
          )
        )
      ),
      mainPanel(
        tabsetPanel(
          type = "pills", id = "my-nav",
          tabPanel("Individual Results",
                   div(style = "margin-top: 15px;"),
                   uiOutput("parallel_results_ui")
          ),
          tabPanel("Combined Summary",
                   div(style = "margin-top: 15px;"),
                   div(class = "gender-filter-container",
                       tags$span("Select Genders to Display:"),
                       checkboxGroupInput(inputId = "parallel_gender_filter", label = NULL, choices = c("Male", "Female", "Combined"), selected = c("Male", "Female", "Combined"), inline = TRUE)
                   ),
                   plotOutput("combined_dumbbell_plot"),
                   div(class = "spacing-div"),
                   plotOutput("combined_ri_plot"),
                   div(class = "spacing-div"),
                   plotOutput("combined_density_plot"),
                   div(class = "spacing-div"),
                   plotOutput("single_density_plot"),
                   div(class = "spacing-div"),
                   card(
                    plotOutput("combined_box_plot"),
                    card_footer(
                      "Plot Description:",
                      tooltip(bs_icon("info-circle"), "The square in each box plot represents the middle 50% of the data, also known as the interquartile range (IQR). The line inside the box is the median, which is the midpoint of the HGB data. The whiskers extending from the box show the normal range of the data that is not considered an outlier. The red dots are outliers, which are values significantly different from the rest of their subpopulation and fall outside of the whiskers.")
                    )
                   ),
                   div(class = "spacing-div"),
                   verbatimTextOutput("combined_summary")
          )
        )
      )
    )
  ),
  tabPanel(
    title = "About",
    icon = icon("info-circle"),
    fluidPage(
      div(
        class = "about-container",
        # Introduction
        div(
          class = "about-header",
          h2("About VeritasRI: Understanding the Methodology")
        ),
        div(
          class = "about-content",
          p("VeritasRI is a specialized tool designed to streamline the complex process of estimating clinical reference intervals. This section provides a transparent look into the statistical methods and R packages that power each of the application's core features, ensuring you have a clear understanding of how your results are generated."),
          p("Below, you'll find a breakdown of the backend methodologies for the Main Analysis, Subpopulation Detection (GMM), and Parallel Analysis tabs.")
        ),
        
        # Main Analysis Section
        div(
          class = "about-header",
          h3(bs_icon("clipboard-pulse"), "Main Analysis: Core Reference Interval Estimation")
        ),
        div(
          class = "about-content",
          p(strong("Core Engine:"), " The primary analysis is driven by the ", code("refineR"), " R package. This is a robust and widely accepted method for estimating reference intervals directly from mixed populations, meaning you don't need to manually exclude 'pathological' data points before analysis."),
          p(strong("Data Transformation:"), " To ensure statistical validity, especially with non-normally distributed data, the app employs two key transformation techniques:"),
          tags$ul(
            tags$li(strong("Box-Cox Transformation:"), " A power transformation often used to achieve normality and stabilize variance, suitable for strictly positive, right-skewed data."),
            tags$li(strong("Modified Box-Cox (modBoxCox):"), " An adaptation of the Box-Cox transformation that can handle data including zero or negative values, making it more flexible for diverse datasets."),
            tags$li(strong("Auto-select:"), " The app can automatically choose the most appropriate transformation. It calculates the skewness of your data (using the ", code("moments"), " package) and, if the absolute skewness is greater than 0.5, defaults to the more robust ", code("modBoxCox"), " transformation; otherwise, ", code("Box-Cox"), " is used.")
          )
        ),

        # GMM Section
        div(
          class = "about-header",
          h3(bs_icon("columns-gap"), "Subpopulation Detection (GMM): Uncovering Hidden Groups")
        ),
        div(
          class = "about-content",
          p(strong("Core Engine:"), " This feature uses Gaussian Mixture Models (GMM) from the powerful ", code("mclust"), " package to identify hidden subpopulations within your data. This is particularly useful when your dataset might contain distinct groups (e.g., healthy vs. disease onset) that aren't obvious from single-variable analysis, based on two variables (e.g., age and a biomarker value)."),
          p(strong("Model Selection:"), " The app automatically determines the optimal number of clusters and the best model structure by evaluating the ", strong("Bayesian Information Criterion (BIC)"), ". The model with the highest BIC score is selected, as it represents the best balance between model fit and complexity, favoring simpler models that still explain the data well."),
          p(strong("Data Preprocessing:"), " Before clustering, the data undergoes a two-step standardization process to ensure that both variables contribute equally to the analysis and prevent one variable from dominating the clustering outcome:"),
          tags$ul(
            tags$li(strong("Yeo-Johnson Transformation:"), " Employed (via the ", code("car"), " package) for any variable with high skewness to make its distribution more symmetric, suitable for data that includes zero or negative values."),
            tags$li(strong("Z-score Transformation:"), " Both variables are then scaled to have a mean of 0 and a standard deviation of 1. This ensures that the clustering algorithm considers the relative spread of data rather than absolute magnitudes.")
          )
        ),
        
        # Parallel Analysis Section
        div(
          class = "about-header",
          h3(bs_icon("speedometer2"), "Parallel Analysis: High-Speed Batch Processing")
        ),
        div(
          class = "about-content",
          p(strong("Core Engine:"), " The efficiency of this tab comes from the ", code("future"), " and ", code("future.apply"), " R packages. These packages enable parallel computing, which allows the app to run multiple independent ", code("refineR"), " analyses—one for each of your defined subpopulations—simultaneously."),
          p(strong("How it Works:"), " When you click 'Run Parallel Analysis,' the app intelligently distributes the analysis jobs across multiple CPU cores on your machine. This means that if you define several age-gender groups, their respective reference interval calculations can happen at the same time, significantly reducing the total computation time compared to running them sequentially. The number of cores used can be configured in the Advanced Settings.")
        )
      ) # End about-container
    ) # End fluidPage
  ),
  footer = tags$footer(
    HTML('© 2025 <a href="https://github.com/yakubinaweed/refineR-reference-interval" target="_blank">Naweed Yakubi</a> • All rights reserved.'),
    style = "
      position: bottom;
      bottom: 0;
      width: 100%;
      text-align: center;
      padding: 10px 0;
      color: #777;
      font-size: 0.8em;"
  )
)