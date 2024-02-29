# List required packages
required_packages <- c("readxl", "ggVennDiagram", "randomLCA", "ggplot2", "ggthemes", "dplyr", "ggvenn")

# Check if packages are not installed, then install them
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# Load required libraries
library(readxl)
library(ggVennDiagram)
library(randomLCA)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(ggvenn)

# Function to expand the dataset rows based on the given frequency.
expand_data <- function(data, freq) {
  expanded_data <- data[rep(1:nrow(data), freq), 1:(ncol(data))]
  return(expanded_data)
}

# Function to read data from an csv file and preprocess it by removing NA values.
read_and_preprocess <- function(input_file) {
  data_frame <- read.csv(input_file, na.strings = "")
  
  if (!is.data.frame(data_frame)) {
    stop("The file was not read correctly as a data frame.")
  }
  
  return(data_frame)
}


# Function to process the data frame. It creates unique positions, 
# generates a binary flag data frame, and calculates frequency of combinations.
process_data <- function(data_frame, base_number) {
  unique_positions <- unique(unlist(data_frame))
  result_df <- data.frame(POS = unique_positions)
  
  for (column_index in seq_along(data_frame)) {
    column_data_pre <- data_frame[[column_index]]
    column_data <- na.omit(column_data_pre)
    binary_flag_df <- data.frame(POS = column_data, Flag = 1)
    result_df <- merge(result_df, binary_flag_df, by = "POS", all = TRUE)
    column_name <- colnames(data_frame)[column_index]
    result_df[column_name] <- ifelse(is.na(result_df$Flag), 0, 1)
    result_df <- result_df[-which(names(result_df) == "Flag")]
  }
  
  result_df$combination <- apply(result_df[, -1], 1, paste, collapse = "")
  combination_freq <- table(result_df$combination)
  zero_pattern <- paste(rep("0", ncol(data_frame)), collapse = "")
  combination_freq[zero_pattern] <- base_number - sum(combination_freq) + 1 
  final_data <- data.frame(Pattern = names(combination_freq), Frequency = as.numeric(combination_freq))
  
  return(list(final_data = final_data, detailed_results = result_df))
}


# Function to perform Latent Class Analysis (LCA) on the result from process_data function
perform_LCA_analysis <- function(result) {
  # Use the final_data output from the process_data function
  data_for_lca <- result$final_data
  
  # Convert Pattern strings into a binary data matrix
  binary_patterns <- do.call(rbind, lapply(strsplit(data_for_lca$Pattern, ""), as.integer))
  relevant_colnames <- setdiff(colnames(result$detailed_results), c("POS", "combination"))
  colnames(binary_patterns) <- relevant_colnames
  
  # Convert binary patterns into a data frame
  data_frame_patterns <- as.data.frame(binary_patterns)
  
  # Set model parameters
  nclass_val <- 2
  quadpoints_val <- 189
  
  # Fit LCA models and handle errors
  tryCatch({
    set.seed(123)
    LCA <- randomLCA(data_frame_patterns, freq = data_for_lca$Frequency, nclass = nclass_val, quadpoints = quadpoints_val)
    LCR <- randomLCA(data_frame_patterns, freq = data_for_lca$Frequency, nclass = nclass_val, random = TRUE, constload = TRUE, probit = TRUE, quadpoints = quadpoints_val)
    LCMR <- randomLCA(data_frame_patterns, freq = data_for_lca$Frequency, nclass = nclass_val, random = TRUE, constload = FALSE, probit = TRUE, quadpoints = quadpoints_val)
  }, error = function(e) {
    stop("Error in model fitting: ", e$message)
  })
  
  # Expand data according to the fitted LCA models
  data_fitted_LCA <- expand_data(data_frame_patterns, LCA$fitted)
  data_fitted_LCR <- expand_data(data_frame_patterns, LCR$fitted)
  data_fitted_LCMR <- expand_data(data_frame_patterns, LCMR$fitted)
  data_ori <- expand_data(data_frame_patterns, result$final_data$Frequency)
  
  # Calculate correlations and residuals
  cor_LCA <- cor(data_fitted_LCA)
  cor_LCR <- cor(data_fitted_LCR)
  cor_LCMR <- cor(data_fitted_LCMR)
  cor_ori <- cor(data_ori)
  residuals_LCA <- cor_ori - cor_LCA 
  residuals_LCR <- cor_ori - cor_LCR 
  residuals_LCMR <- cor_ori - cor_LCMR
  
  # Determine order based on outcome probabilities
  order <- ifelse(LCA$outcomep[2] > LCA$outcomep[1], 1, 2)
  orderF <- ifelse(LCR$outcomep[2] > LCR$outcomep[1], 1, 2)
  orderR <- ifelse(LCMR$outcomep[2] > LCMR$outcomep[1], 1, 2)
  
  # Function to calculate the correct order based on outcome probabilities
  get_order <- function(outcomep) {
    ifelse(outcomep[2] > outcomep[1], 1, 2)
  }
  
  # Function to calculate sensitivity and false positive rate
  calculate_metrics <- function(outcomep) {
    order <- get_order(outcomep)
    sens <- sprintf("%3.20f", outcomep[3 - order, ]) 
    fpr <- sprintf("%3.20f", outcomep[order, ])      
    c(sens, fpr)
  }
  
  # Calculate Metrics for Each Model
  sens_spec <- lapply(list(LCA$outcomep, LCR$outcomep, LCMR$outcomep), calculate_metrics)
  
  
  # Calculate Bayesian Information Criterion (BIC)
  BIC_values <- sapply(list(LCA, LCR, LCMR), BIC)
  
  # Combine results into matrices
  output_matrix <- do.call(cbind, sens_spec)
  outcomeprob <- rbind(output_matrix, BIC_values)
  rownames(outcomeprob) <- c(paste0("Sens", names(LCA$patterns)),
                             paste0("FPR", names(LCA$patterns)), "BIC")
  
  # Combine numerical results
  number <- cbind(LCA$patterns, LCA$observed, LCA$fitted, LCR$fitted, LCMR$fitted)
  colnames(number) <- c(names(LCA$patterns), "Observed freq", "Expected_CI", "Expected_constant_random", "Expected_varying_random")
  
  # Combine class probability results
  classprob <- cbind(LCA$patterns,
                     LCA$classprob,
                     LCR$classprob[, c(3 - orderF, orderF)],
                     LCMR$classprob[, c(3 - orderR, orderR)])
  colnames(classprob) <- c(names(LCA$patterns),
                           "positive_LCMC", "negative_LCMC",
                           "positive_LCR", "negative_LCR",
                           "positive_LCMR", "negative_LCMR")
  
  # Return all results
  return(list(sens_spec = sens_spec, outcomeprob = outcomeprob, classprob = classprob, number = number, residuals_LCA = residuals_LCA, residuals_LCR = residuals_LCR, residuals_LCMR = residuals_LCMR))
}


# Function to extract non-diagonal residuals from a matrix
extract_residuals <- function(residual_matrix) {
  # Remove diagonal elements
  diag(residual_matrix) <- NA
  
  # Initialize a dataframe to store results
  result_df <- data.frame(Name = character(), Value = numeric())
  
  # Iterate through the matrix and extract non-diagonal elements
  for (i in 1:(nrow(residual_matrix) - 1)) {
    for (j in (i + 1):ncol(residual_matrix)) {
      if (!is.na(residual_matrix[i, j])) {
        # Construct a name using row and column names
        name <- paste(rownames(residual_matrix)[i], colnames(residual_matrix)[j], sep = "_")
        
        # Add results to the dataframe
        result_df <- rbind(result_df, data.frame(Name = name, Value = residual_matrix[i, j]))
      }
    }
  }
  
  return(result_df)
}

server <- function(input, output, session) {
  analysisCompleted <- reactiveVal(FALSE)
  
  analysisResults <- reactiveValues(
    df = NULL,
    sets = NULL,
    data_df = NULL,
    all_data = NULL,
    vennPlot = NULL,
    residualsPlot = NULL,
    result = NULL,
    numbersnp = NULL,
    vennclass = NULL
  )
  
  generateVennPlot <- function(sets) {
    ggvenn(
      sets,
      fill_color = c("#99A98F", "#CC3721", "#776AD8", "#0E71CA"),
      fill_alpha = 0.5,
      stroke_linetype = "solid",
      stroke_color = "grey50",
      stroke_size = 0.5,
      set_name_size = 6,
      text_size = 5
    )
  }
  
  generateResidualPlot <- function(all_data) {
    ggplot(all_data, aes(x = Name, y = Value, color = Methods, shape = Methods, group = Methods)) +
      geom_point(size = 2) +
      geom_line(linewidth = 0.7) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5) +
      geom_hline(yintercept = 0.03, linetype = "dashed", color = "gray", linewidth = 0.5) +
      geom_hline(yintercept = -0.03, linetype = "dashed", color = "gray", linewidth = 0.5) +
      theme_minimal() +
      theme(
        axis.title.x = element_text(size = 10, face = "bold"),
        axis.title.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8),
        legend.position = "right",
        plot.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      labs(title = "Correlation Residuals", x = "Pairs", y = "Residual Value") +
      scale_color_manual(values = c("#12A1AC", "#D173A7", "#7A9D44")) +
      scale_shape_manual(values = c(16, 17, 18)) +
      coord_cartesian(ylim = c(-0.06, 0.06))
  }
  
  performAnalysis <- function(filePath, baseCount) {
    
    data_frame <- read_and_preprocess(filePath)
    
    list_sets <- lapply(names(data_frame), function(col_name) na.omit(data_frame[[col_name]]))
    names(list_sets) <- names(data_frame)
    
    analysisResults$df <- data_frame
    analysisResults$sets <- list_sets
    analysisResults$vennPlot <- generateVennPlot(list_sets)
    result <- process_data(data_frame, base_num = baseCount)
    analysisResults$vennclass <- result$detailed_results
    result_LCA <- perform_LCA_analysis(result)
    
    analysisResults$numbersnp <- result_LCA[["number"]]
    residuals_LCA <- result_LCA$residuals_LCA
    residuals_LCR <- result_LCA$residuals_LCR
    residuals_LCMR <- result_LCA$residuals_LCMR
    
    data_df <- as.data.frame(result_LCA$outcomeprob)
    colnames(data_df) <- c("LCMC", "LCR", "LCMR")
    
    # Formatting data
    data_df[grepl("^Sens", rownames(data_df)), ] <- lapply(
      data_df[grepl("^Sens", rownames(data_df)), ],
      function(x) format(as.numeric(x), scientific = FALSE, nsmall = 0)
    )
    
    data_df[grepl("^FPR", rownames(data_df)), ] <- lapply(
      data_df[grepl("^FPR", rownames(data_df)), ],
      function(x) sprintf("%.3e", as.numeric(x))
    )
    
    data_df[grepl("^BIC", rownames(data_df)), ] <- lapply(
      data_df[grepl("^BIC", rownames(data_df)), ],
      function(x) sprintf("%.3f", as.numeric(x))
    )
    
    analysisResults$data_df <- data_df
    
    extracted_data_LCA <- extract_residuals(residuals_LCA)
    extracted_data_LCR <- extract_residuals(residuals_LCR)
    extracted_data_LCMR <- extract_residuals(residuals_LCMR)
    
    all_data <- rbind(
      extracted_data_LCA %>% mutate(Methods = "LCMC"),
      extracted_data_LCR %>% mutate(Methods = "LCR"),
      extracted_data_LCMR %>% mutate(Methods = "LCMR")
    )
    
    analysisResults$all_data <- all_data
    analysisResults$residualsPlot <- generateResidualPlot(all_data)
    
    output$vennPlot <- renderPlot({ analysisResults$vennPlot })
    output$residualPlot_LCA <- renderPlot({ analysisResults$residualsPlot })
    output$sens_spec <- renderTable({ analysisResults$data_df }, rownames = TRUE, colnames = TRUE)
    output$numbersnp <- renderTable({ analysisResults$numbersnp }, colnames = TRUE)
    analysisCompleted(TRUE)
  }
  
  # Download handlers
  output$downloadVennPlot <- downloadHandler(
    filename = function() {
      paste("venn_diagram", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file)
      print(analysisResults$vennPlot)
      dev.off()
    }
  )
  
  output$downloadResidualPlot <- downloadHandler(
    filename = function() {
      paste("residual_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file)
      print(analysisResults$residualsPlot)
      dev.off()
    }
  )
  
  output$downloadResidualPlotPDF <- downloadHandler(
    filename = function() {
      paste("residual_plot", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file)
      print(analysisResults$residualsPlot)
      dev.off()
    }
  )
  
  output$downloadSensSpec <- downloadHandler(
    filename = function() {
      paste("sensitivity_specificity", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(analysisResults$data_df, file, row.names = FALSE)
    }
  )
  
  output$downloadExpectedNumberSNP <- downloadHandler(
    filename = function() {
      paste("Number_of_SNP", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(analysisResults$numbersnp, file, row.names = FALSE)
    }
  )
  
  output$downloadVennClass <- downloadHandler(
    filename = function() {
      paste("VennClass", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(analysisResults$vennclass, file, row.names = FALSE)
    }
  )
  
  output$downloadPreprocessor <- downloadHandler(
    filename = function() {
      "Preprocessor.R"
    },
    content = function(file) {
      file.copy("PreprocessorV1.R", file)
    }
  )
  
  # Dynamic UI for example selection
  output$exampleUI <- renderUI({
    if (input$exampleSelect == "Example1") {
      fluidRow(
        column(12,
               h4("Example1", class = "example-title"),
               h5("Refcount :4924610", class = "example-ref"),
               h5("The result of multi chromosomes variation of one breast cancer patient. All three LCM models have maximum likelihood estimates on this data.", class = "example-desc")
        ),
        column(6, class = "left-align",
               downloadButton("downloadExample1", "Download Example1", class = "example-button")
        ),
        column(6, class = "right-align",
               actionButton("runAnalysis1", "Run Example1", icon = icon("play"), class = "example-button")
        )
      )
    } else if (input$exampleSelect == "Example2") {
      fluidRow(
        column(12,
               h4("Example2", class = "example-title"),
               h5("Refcount : 50818468"),
               h5("The PrecisionFDA result on chromosome 22 with average coverage 8X. All three LCM models have maximum likelihood estimates on this data.")
        ),
        column(6, class = "left-align",
               downloadButton("downloadExample2", "Download Example2", class = "example-button")
        ),
        column(6, class = "right-align",
               actionButton("runAnalysis2", "Run Example2", icon = icon("play"), class = "example-button")
        )
      )
    } else if (input$exampleSelect == "Example3") {
      fluidRow(
        column(12,
               h4("Example3", class = "example-title"),
               h5("Refcount :118", class = "example-ref"),
               h5("Uterine carcinoma data in the randomLCA package, which is from seven pathologists. All three LCM models have maximum likelihood estimates on this data.", class = "example-desc")
        ),
        column(6, class = "left-align",
               downloadButton("downloadExample3", "Download Example3", class = "example-button")
        ),
        column(6, class = "right-align",
               actionButton("runAnalysis3", "Run Example3", icon = icon("play"), class = "example-button")
        )
      )
    }
  })
  
  output$downloadExample1 <- downloadHandler(
    filename = function() {
      "example1.csv"
    },
    content = function(file) {
      file.copy("./data/example1.csv", file)
    }
  )
  
  output$downloadExample2 <- downloadHandler(
    filename = function() {
      "example2.csv"
    },
    content = function(file) {
      file.copy("./data/example2.csv", file)
    }
  )
  
  output$downloadExample3 <- downloadHandler(
    filename = function() {
      "example3.csv"
    },
    content = function(file) {
      file.copy("./data/example3.csv", file)
    }
  )
  

  presetBaseCount1 <- 4924610
  presetFilePath1 <- "./data/example1.csv"
  presetBaseCount2 <- 50818468
  presetFilePath2 <- "./data/example2.csv"
  presetBaseCount3 <- 118
  presetFilePath3 <- "./data/example3.csv"
  
  observeEvent(input$runAnalysis1, {
    updateNumericInput(session, "baseCount", value = presetBaseCount1)
    performAnalysis(presetFilePath1, presetBaseCount1)
  })
  
  observeEvent(input$runAnalysis2, {
    updateNumericInput(session, "baseCount", value = presetBaseCount2)
    performAnalysis(presetFilePath2, presetBaseCount2)
  })
  
  observeEvent(input$runAnalysis3, {
    updateNumericInput(session, "baseCount", value = presetBaseCount3)
    performAnalysis(presetFilePath3, presetBaseCount3)
  })
  
  observeEvent(input$submit, {
    req(input$file1)
    performAnalysis(input$file1$datapath, input$baseCount)
  })
}