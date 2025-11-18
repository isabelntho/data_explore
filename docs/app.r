# ============================================================================
# SHINY APP FOR EXPLORING EC-ES RELATIONSHIPS
# ============================================================================

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)
library(terra)
library(sf)
library(leaflet)
library(viridis)
library(corrplot)
library(ggpubr)  # For stat_cor in scatter plots

# Source required files
#source("./Set_up.R")
#source("./spatial_data_processing_functions.R")
#tryCatch(source("./Process_LU.R"), error = function(e) message("Process_LU.R not found, skipping"))
#source("./sample_data.R")

# ============================================================================
# DATA CONFIGURATION
# ============================================================================

# EC variables by category and ecosystem type
ec_categories <- list(
  "Abiotic state" = list(
    all = c("smd" = "Soil Moisture Deficit", "sbd" = "Soil Bulk Density", "soc" = "Soil Organic Carbon"),
    forest = c("smd" = "Soil Moisture Deficit", "sbd" = "Soil Bulk Density", "soc" = "Soil Organic Carbon"),
    agricultural = c("smd" = "Soil Moisture Deficit", "sbd" = "Soil Bulk Density", "soc" = "Soil Organic Carbon"),
    grassland = c("smd" = "Soil Moisture Deficit", "sbd" = "Soil Bulk Density", "soc" = "Soil Organic Carbon")
  ),
  "Biotic state" = list(
    forest = c("tsd" = "Tree Species Diversity","can" = "Canopy Height", "lai" = "Leaf Area Index"),
    agricultural = c("uzl" = "UZL Species", "cdi" = "Crop Diversity Index", "swf_h" = "Small Woody Features (Hedgerows)", "swf_t" = "Small Woody Features (Trees)", "ndvi" = "NDVI"),
    grassland = c("uzl" = "UZL Species", "swf_h" = "Small Woody Features (Hedgerows)", "swf_t" = "Small Woody Features (Trees)", "ndvi" = "NDVI")
  ),
  "Landscape characteristics" = list(
    all = c("snh" = "Semi-natural Habitat Density", "frag" = "Forest Fragmentation"),
    forest = c("frag" = "Forest Fragmentation"),
    agricultural = c("snh" = "Semi-natural Habitat Density"),
    grassland = c("snh" = "Semi-natural Habitat Density")
  )
)

# ES variables by category
es_categories <- list(
  "InVEST Models" = c(
    "POL" = "Pollination", 
    "HAB" = "Habitat Quality", 
    "REC" = "Recreation", 
    "CAR" = "Carbon Storage",
    "FF" = "Flood Prevention"
  ),
  "Indicator Approach" = c(
    "POL_ind" = "Pollination (Indicator)", 
    "HAB_ind" = "Habitat (Indicator)", 
    "REC_ind" = "Recreation (Indicator)",
    "CAR_ind" = "Carbon (Indicator)"
  )
)

# Ecosystem type mapping
ecosystem_types <- c(
  "Forest" = "forest",
  "Agricultural Land" = "agricultural", 
  "Grasslands & Pastures" = "grassland"
)
kb_dir <- "Y:/CH_Kanton_Bern/03_Workspaces/03_Habitat_condition/"
data_dir <- "Y:/EU_BioES_SELINA/WP3/4. Spatially_Explicit_EC/Data/"
# ============================================================================
# DATA LOADING FUNCTIONS
# ============================================================================

# Load EC data
load_ec_data <- function() {
  # Check if kb_dir exists, otherwise use local paths
  base_dir <- if(exists("kb_dir") && dir.exists(kb_dir)) {
    kb_dir
  } else {
    "./"  # Use current directory as fallback
  }
  
  ec_vars <- list(
    smd = "./ec_data_aligned/smd.tif",
    sbd ="./ec_data_aligned/Restoration_potential/Data/sbd.tif",
    soc = "./ec_data_aligned/Restoration_potential/Data/soc.tif",
    uzl = "./ec_data_aligned/uzl.tif",
    swf_h = "./ec_data_aligned/swf_h.tif",
    swf_t = "./ec_data_aligned/swf_t.tif",
    ndvi = "./ec_data_aligned/ndvi.tif",
    cdi = "./ec_data_aligned/.tif",
    snh = "./ec_data_aligned/snh.tif",
    md = "./ec_data_aligned/md.tif",
    tsd = "./ec_data_aligned/tsd.tif",
    can = "./ec_data_aligned/can.tif",
    lai = "./ec_data_aligned/lai.tif",
    frag = "./ec_data_aligned/frag.tif"
  )
  
  # Load existing rasters with better error reporting
  ec_rasters <- lapply(names(ec_vars), function(var_name) {
    file_path <- ec_vars[[var_name]]
    if(file.exists(file_path)) {
      tryCatch({
        raster_data <- rast(file_path)
        message(paste("Successfully loaded:", var_name))
        return(raster_data)
      }, error = function(e) {
        warning(paste("Error loading", var_name, ":", e$message))
        return(NULL)
      })
    } else {
      warning(paste("File not found:", file_path))
      return(NULL)
    }
  })
  names(ec_rasters) <- names(ec_vars)
  
  # Remove NULL entries and return
  loaded_data <- ec_rasters[!sapply(ec_rasters, is.null)]
  message(paste("Loaded", length(loaded_data), "EC datasets out of", length(ec_vars), "total"))
  
  # If no data loaded, create sample data for testing
  if(length(loaded_data) == 0) {
    message("No EC data found. Creating sample data for testing...")
    loaded_data <- create_sample_ec_data()
  }
  
  return(loaded_data)
}

# Load ES data
load_es_data <- function() {
  # Check if kb_dir exists, otherwise use local paths
  base_dir <- if(exists("kb_dir") && dir.exists(kb_dir)) {
    kb_dir
  } else {
    "./"  # Use current directory as fallback
  }
  
  update_dir <- paste0(base_dir, "Restoration_potential/Outputs/Update_october/31-10-25/")
  
  # InVEST data
  invest_files <- list(
    CAR = "Y:/CH_Kanton_Bern/03_Workspaces/05_Web_platform/raster_data/car-2020-rcp45-ref_central-ref-bau-canton.tif",
    HAB = "Y:/CH_Kanton_Bern/03_Workspaces/05_Web_platform/raster_data/hab-2020-rcp45-ref_central-ref-bau-canton.tif",
    REC = "Y:/CH_Kanton_Bern/03_Workspaces/05_Web_platform/raster_data/rec-2020-rcp45-ref_central-ref-bau-canton.tif",
    POL = "Y:/CH_Kanton_Bern/03_Workspaces/05_Web_platform/raster_data/pol-2020-rcp45-ref_central-ref-bau-canton.tif",
    FF = "Y:/CH_Kanton_Bern/03_Workspaces/05_Web_platform/raster_data/ff-2020-rcp45-ref_central-ref-bau-canton.tif"
  )
  
  # Indicator data (placeholder - adjust paths as needed)
  indicator_files <- list(
    POL_ind = "Y:/CH_Kanton_Bern/03_Workspaces/02_SDM/sdm_summarisation/raster_data/sdm-2025-rcp45-npa-all-bau-canton.tif",
    HAB_ind = "Y:/CH_Kanton_Bern/03_Workspaces/02_SDM/sdm_summarisation/raster_data/sdm-2025-rcp45-npa-all-bau-canton.tif",
    REC_ind = "Y:/CH_Kanton_Bern/03_Workspaces/02_SDM/sdm_summarisation/raster_data/sdm-2025-rcp45-npa-all-bau-canton.tif",
    CAR_ind = paste0(data_dir, "AGB/N50E000_ESACCI-BIOMASS-L4-AGB-MERGED-100m-2018-fv6.0.tif")
  )
  
  all_files <- c(invest_files, indicator_files)
  
  # Load existing rasters with better error reporting
  es_rasters <- lapply(names(all_files), function(var_name) {
    file_path <- all_files[[var_name]]
    if(file.exists(file_path)) {
      tryCatch({
        raster_data <- rast(file_path)
        message(paste("Successfully loaded ES:", var_name))
        return(raster_data)
      }, error = function(e) {
        warning(paste("Error loading ES", var_name, ":", e$message))
        return(NULL)
      })
    } else {
      warning(paste("ES file not found:", file_path))
      return(NULL)
    }
  })
  names(es_rasters) <- names(all_files)
  
  # Remove NULL entries and return
  loaded_data <- es_rasters[!sapply(es_rasters, is.null)]
  message(paste("Loaded", length(loaded_data), "ES datasets out of", length(all_files), "total"))
  
  # If no data loaded, create sample data for testing
  if(length(loaded_data) == 0) {
    message("No ES data found. Creating sample data for testing...")
    loaded_data <- create_sample_es_data()
  }
  
  return(loaded_data)
}

# ============================================================================
# UI DEFINITION
# ============================================================================

ui <- dashboardPage(
  dashboardHeader(title = "EC-ES Data Explorer"),
  
  dashboardSidebar(
    sidebarMenu(
      # Ecosystem Type Selection
      menuItem("Ecosystem Selection", startExpanded = TRUE,
        radioButtons("ecosystem_type", "Select Ecosystem Type:",
                    choices = ecosystem_types,
                    selected = "forest")
      ),
      
      # EC Variables Selection
      menuItem("EC Variables", startExpanded = TRUE,
        uiOutput("ec_variables_ui")
      ),
      
      # ES Variables Selection  
      menuItem("ES Variables", startExpanded = TRUE,
        radioButtons("es_category", "ES Category:",
                    choices = c("InVEST Models" = "invest", "Indicator Approach" = "indicator"),
                    selected = "invest"),
        
        conditionalPanel("input.es_category == 'invest'",
          selectInput("es_invest", "Select InVEST ES:",
                     choices = es_categories[["InVEST Models"]],
                     selected = es_categories[["InVEST Models"]][1])  # Select first value (full name)
        ),
        
        conditionalPanel("input.es_category == 'indicator'",
          selectInput("es_indicator", "Select Indicator ES:",
                     choices = es_categories[["Indicator Approach"]],
                     selected = es_categories[["Indicator Approach"]][1])  # Select first value (full name)
        )
      ),
      
      # Analysis Options
      menuItem("Analysis Options", startExpanded = TRUE,
        checkboxInput("show_correlation", "Show Correlation Analysis", FALSE),
        numericInput("n_intervals", "Number of intervals for box plots:", 
                    value = 5, min = 3, max = 10, step = 1)
      )
    )
  ),
  
  dashboardBody(
    fluidRow(
      # EC Raster with Histogram
      column(6,
        box(title = "Ecosystem Condition", status = "primary", solidHeader = TRUE, width = 12,
          fluidRow(
            column(8, plotOutput("ec_raster", height = "400px")),
            column(4, plotOutput("ec_histogram", height = "400px"))
          )
        ),
        box(title = "EC Statistics", status = "info", solidHeader = TRUE, width = 12,
          verbatimTextOutput("ec_stats")
        )
      ),
      
      # ES Raster with Histogram  
      column(6,
        box(title = "Ecosystem Service", status = "success", solidHeader = TRUE, width = 12,
          fluidRow(
            column(8, plotOutput("es_raster", height = "400px")),
            column(4, plotOutput("es_histogram", height = "400px"))
          )
        ),
        box(title = "ES Statistics", status = "info", solidHeader = TRUE, width = 12,
          verbatimTextOutput("es_stats")
        )
      )
    ),
    
    fluidRow(
      # Relationship Analysis
      column(12,
        box(title = "EC-ES Relationship Analysis", status = "warning", solidHeader = TRUE, width = 12,
          tabsetPanel(
            tabPanel("Scatter Plot", 
              plotOutput("scatter_plot", height = "400px")
            ),
            tabPanel("Box Plots",
              plotOutput("box_plots", height = "400px")
            ),
            tabPanel("Correlation Matrix",
              conditionalPanel("input.show_correlation",
                plotOutput("correlation_plot", height = "400px")
              )
            )
          )
        )
      )
    ),
    tags$script(HTML("
  $(document).ready(function() {
    // Expand all sidebar menu items
    $('.sidebar-menu li.treeview').addClass('menu-open');
    $('.sidebar-menu li.treeview > a').css('display', 'block');
    $('.sidebar-menu li.treeview > .treeview-menu').css('display', 'block');
  });
"))
  )
)

# ============================================================================
# SERVER DEFINITION
# ============================================================================

server <- function(input, output, session) {
  
  # Load data once at startup
  ec_data <- load_ec_data()
  es_data <- load_es_data()
  
  # Load LU raster
  LU <- rast("Y:/CH_Kanton_Bern/03_Workspaces/05_Web_platform/raster_data/LULC-2020-rcp45-ref_central-ref-bau-canton.tif")
  common_extent <- ext(2556200, 2677700, 1130600, 1243700)
  LU <- crop(LU, common_extent)
# Mapping of ecosystem types to LU codes
  lu_classes <- list(
        forest = c(12, 13),
        agricultural = c(15),
        grassland = c(16, 17)
  )

# Function to mask any raster by ecosystem type
  mask_by_ecosystem <- function(r, ecosystem) {
  allowed <- lu_classes[[ecosystem]]

  # Create a mask raster with NA outside allowed classes and 1 inside
  lu_mask <- LU
  vals <- values(lu_mask)

  vals[!(vals %in% allowed)] <- NA
  vals[vals %in% allowed] <- 1

  values(lu_mask) <- vals

  mask(r, lu_mask)
 }

  # Create reactive values for data status
  data_status <- reactiveValues(
    ec_loaded = length(ec_data) > 0,
    es_loaded = length(es_data) > 0,
    message = ""
  )
  
  # Update data status messages
  observe({
    if(length(ec_data) == 0 && length(es_data) == 0) {
      data_status$message <- "No data files found. Please check file paths in Set_up.R"
    } else if(length(ec_data) == 0) {
      data_status$message <- "No EC data files found. ES data loaded successfully."
    } else if(length(es_data) == 0) {
      data_status$message <- "No ES data files found. EC data loaded successfully."
    } else {
      data_status$message <- paste("Data loaded:", length(ec_data), "EC variables (", paste(names(ec_data), collapse=", "), "),", length(es_data), "ES variables (", paste(names(es_data), collapse=", "), ")")
    }
    message("Data status: ", data_status$message)
  })
  
  # Dynamic UI for EC variables based on ecosystem type
  output$ec_variables_ui <- renderUI({
    ecosystem <- input$ecosystem_type
    
    # Generate EC variable checkboxes by category
    ec_ui_elements <- lapply(names(ec_categories), function(category) {
      vars <- ec_categories[[category]][[ecosystem]]
     print(vars)
      if(length(vars) > 0) {
        tagList(
          h5(category, style = "font-weight: bold; margin-top: 15px;"),
          checkboxGroupInput(
            inputId = paste0("ec_", gsub("[^A-Za-z0-9]", "_", category)),
            label = NULL,
            choices  = setNames(as.vector(vars), as.vector(vars)),
            selected = NULL  # Auto-select first soil property (use value, not name)
          )
        )
      } else {
        NULL
      }
    })
    
    # Remove NULL elements
    ec_ui_elements <- ec_ui_elements[!sapply(ec_ui_elements, is.null)]
    
    # Return UI elements
    if(length(ec_ui_elements) > 0) {
      do.call(tagList, ec_ui_elements)
    } else {
      p("No EC variables available for selected ecosystem type.")
    }

  })
  
  # Reactive function to get selected EC variables
selected_ec_vars <- reactive({
  ecosystem <- input$ecosystem_type
  all_vars <- c()
  
  for(category in names(ec_categories)) {
    input_id <- paste0("ec_", gsub("[^A-Za-z0-9]", "_", category))
    
    # These are the descriptive names (labels)
    selected_labels <- input[[input_id]]
    
    if(!is.null(selected_labels) && length(selected_labels) > 0) {
      # Mapping for this category and ecosystem: code = name, label = value
      category_vars <- ec_categories[[category]][[ecosystem]]
      
      # Find codes (names) that correspond to the selected labels (values)
      var_codes <- names(category_vars)[category_vars %in% selected_labels]
      all_vars <- c(all_vars, var_codes)
    }
  }
  
  all_vars
})
  
  # Reactive function to get selected ES variable
  selected_es_var <- reactive({
    selected_label <- if(input$es_category == "invest") {
      input$es_invest
    } else {
      input$es_indicator
    }
    
    # Convert label back to variable code
    if(!is.null(selected_label)) {
      if(input$es_category == "invest") {
        # Find the key that matches the selected label
        es_vars <- es_categories[["InVEST Models"]]
        var_code <- names(es_vars)[es_vars == selected_label]
        if(length(var_code) > 0) var_code[1] else NULL
      } else {
        # Find the key that matches the selected label  
        es_vars <- es_categories[["Indicator Approach"]]
        var_code <- names(es_vars)[es_vars == selected_label]
        if(length(var_code) > 0) var_code[1] else NULL
      }
    } else {
      NULL
    }
  })
  
  # Create EC raster (single variable or aggregated index)
  ec_raster_data <- reactive({
    selected_vars <- selected_ec_vars()
    
    # Return NULL if no variables selected (don't use req())
    if(length(selected_vars) == 0) {
      return(NULL)
    }
    
    if(length(selected_vars) == 1) {
      # Single variable
      if(selected_vars %in% names(ec_data)) {
        #ec_data[[selected_vars]]
        mask_by_ecosystem(ec_data[[selected_vars]], input$ecosystem_type)
      } else {
        message("EC variable '", selected_vars, "' not found in loaded data")
        NULL
      }
    } else if(length(selected_vars) > 1) {
      # Multiple variables - create aggregated index
      available_vars <- intersect(selected_vars, names(ec_data))
      
      if(length(available_vars) > 0) {
        tryCatch({
          # Stack rasters and calculate mean
          stack_list <- ec_data[available_vars]
          combined_stack <- rast(stack_list)
          
          # Calculate mean (simple aggregation)
          #mean(combined_stack, na.rm = TRUE)
          mask_by_ecosystem(mean(combined_stack, na.rm = TRUE), input$ecosystem_type)
        }, error = function(e) {
          message("Error creating EC aggregate: ", e$message)
          NULL
        })
      } else {
        message("No selected EC variables found in loaded data")
        NULL
      }
    } else {
      NULL
    }
  })
  
  # Create ES raster
  es_raster_data <- reactive({
    es_var <- selected_es_var()
    
    # Debug: show what was selected
    selected_label <- if(input$es_category == "invest") {
      input$es_invest
    } else {
      input$es_indicator
    }
    message("ES selected label: '", selected_label, "', converted to code: '", es_var, "'")
    message("Available ES choices (codes): ", paste(names(es_categories[["InVEST Models"]]), collapse=", "))
    message("Available ES choices (labels): ", paste(es_categories[["InVEST Models"]], collapse=", "))
    
    # Return NULL if no variable selected
    if(is.null(es_var) || length(es_var) == 0) {
      return(NULL)
    }
    
    if(es_var %in% names(es_data)) {
      message("Loading ES variable: ", es_var)
      es_data[[es_var]]
    } else {
      message("ES variable '", es_var, "' not found in loaded data")
      message("Available ES variables: ", paste(names(es_data), collapse=", "))
      NULL
    }
  })
  
  # EC Raster Plot
  output$ec_raster <- renderPlot({
    if(!data_status$ec_loaded) {
      # Show message plot when no data
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No EC data available\nCheck file paths", 
                size = 6, hjust = 0.5, vjust = 0.5) +
        theme_void() +
        xlim(0, 1) + ylim(0, 1)
    } else if(is.null(ec_raster_data())) {
      # Show message when no variables selected
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Please select EC variables\nfrom the sidebar", 
                size = 6, hjust = 0.5, vjust = 0.5) +
        theme_void() +
        xlim(0, 1) + ylim(0, 1)
    } else {
      # Show actual data
      tryCatch({
        # Convert to data frame for ggplot (optimized)
        raster_data <- ec_raster_data()
        
        # Sample data if too large for faster rendering
        if(ncell(raster_data) > 10000) {
          # Aggregate raster for faster display
          raster_data <- aggregate(raster_data, fact = 2, fun = "mean")
        }
        
        df <- as.data.frame(raster_data, xy = TRUE)
        names(df)[3] <- "value"
        df <- df[!is.na(df$value), ]
        
        if(nrow(df) == 0) {
          ggplot() + 
            annotate("text", x = 0.5, y = 0.5, label = "No valid data points", 
                    size = 6, hjust = 0.5, vjust = 0.5) +
            theme_void() +
            xlim(0, 1) + ylim(0, 1)
        } else {
          ggplot(df, aes(x = x, y = y, fill = value)) +
            geom_raster(interpolate = FALSE) +  # Disable interpolation for speed
            scale_fill_viridis_c(name = "Value", guide = "none") +  # Remove legend
            coord_equal() +
            theme_void() +
            theme(plot.margin = margin(5, 5, 5, 5)) +
            labs(title = ifelse(length(selected_ec_vars()) == 1, 
                               paste("EC:", selected_ec_vars()[1]),
                               "EC Aggregated Index"))
        }
      }, error = function(e) {
        ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = paste("Error plotting EC data:\n", e$message), 
                  size = 5, hjust = 0.5, vjust = 0.5) +
          theme_void() +
          xlim(0, 1) + ylim(0, 1)
      })
    }
  })
  
  # ES Raster Plot
  output$es_raster <- renderPlot({
    if(!data_status$es_loaded) {
      # Show message plot when no data
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No ES data available\nCheck file paths", 
                size = 6, hjust = 0.5, vjust = 0.5) +
        theme_void() +
        xlim(0, 1) + ylim(0, 1)
    } else if(is.null(es_raster_data())) {
      # Show message when no valid ES selected
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Selected ES data\nnot available", 
                size = 6, hjust = 0.5, vjust = 0.5) +
        theme_void() +
        xlim(0, 1) + ylim(0, 1)
    } else {
      # Show actual data
      tryCatch({
        # Convert to data frame for ggplot (optimized)
        raster_data <- es_raster_data()
        
        # Sample data if too large for faster rendering
        if(ncell(raster_data) > 10000) {
          # Aggregate raster for faster display
          raster_data <- aggregate(raster_data, fact = 2, fun = "mean")
        }
        
        df <- as.data.frame(raster_data, xy = TRUE)
        names(df)[3] <- "value"
        df <- df[!is.na(df$value), ]
        
        if(nrow(df) == 0) {
          ggplot() + 
            annotate("text", x = 0.5, y = 0.5, label = "No valid data points", 
                    size = 6, hjust = 0.5, vjust = 0.5) +
            theme_void() +
            xlim(0, 1) + ylim(0, 1)
        } else {
          ggplot(df, aes(x = x, y = y, fill = value)) +
            geom_raster(interpolate = FALSE) +  # Disable interpolation for speed
            scale_fill_viridis_c(name = "Value", option = "plasma", guide = "none") +  # Remove legend
            coord_equal() +
            theme_void() +
            theme(plot.margin = margin(5, 5, 5, 5)) +
            labs(title = paste("ES:", selected_es_var()))
        }
      }, error = function(e) {
        ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = paste("Error plotting ES data:\n", e$message), 
                  size = 5, hjust = 0.5, vjust = 0.5) +
          theme_void() +
          xlim(0, 1) + ylim(0, 1)
      })
    }
  })
  
  # EC Histogram (rotated to serve as color legend)
  output$ec_histogram <- renderPlot({
    if(!data_status$ec_loaded || is.null(ec_raster_data())) {
      ggplot() + theme_void()
    } else {
      tryCatch({
        values <- values(ec_raster_data())
        values <- values[!is.na(values)]
        
        df <- data.frame(value = values)
        
        ggplot(df, aes(y = value)) +
          geom_histogram(aes(fill = after_stat(y)), bins = 25, alpha = 0.8, color = "white", linewidth = 0.1) +
          scale_fill_viridis_c(name = "EC Value") +
          theme_minimal() +
          theme(axis.title.x = element_text(size = 9),
                axis.title.y = element_text(size = 9),
                legend.position = "right",
                legend.key.width = unit(0.25, "cm"),
                plot.margin = margin(2, 2, 2, 2)) +
          labs(y = "EC Value", x = "Frequency")
      }, error = function(e) {
        ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "No data", size = 4) +
          theme_void()
      })
    }
  })
  
  # ES Histogram (rotated to serve as color legend)
  output$es_histogram <- renderPlot({
    if(!data_status$es_loaded || is.null(es_raster_data())) {
      ggplot() + theme_void()
    } else {
      tryCatch({
        values <- values(es_raster_data())
        values <- values[!is.na(values)]
        
        df <- data.frame(value = values)
        
        ggplot(df, aes(y = value)) +
          geom_histogram(aes(fill = after_stat(y)), bins = 50, alpha = 0.8, color = "white", linewidth = 0.1) +
          scale_fill_viridis_c(name = "ES Value", option = "plasma") +
          theme_minimal() +
          theme(axis.title.x = element_text(size = 10),
                axis.title.y = element_text(size = 10),
                legend.position = "right",
                legend.key.width = unit(0.3, "cm"),
                plot.margin = margin(5, 5, 5, 5)) +
          labs(y = "ES Value", x = "Frequency")
      }, error = function(e) {
        ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "No data", size = 4) +
          theme_void()
      })
    }
  })
  
  # EC Statistics
  output$ec_stats <- renderText({
    req(ec_raster_data())
    
    values <- values(ec_raster_data())
    values <- values[!is.na(values)]
    
    paste0(
      "Mean: ", round(mean(values, na.rm = TRUE), 4), "\n",
      "SD: ", round(sd(values, na.rm = TRUE), 4), "\n",
      "Min: ", round(min(values, na.rm = TRUE), 4), "\n",
      "Max: ", round(max(values, na.rm = TRUE), 4), "\n",
      "N: ", length(values)
    )
  })
  
  # ES Statistics
  output$es_stats <- renderText({
    req(es_raster_data())
    
    values <- values(es_raster_data())
    values <- values[!is.na(values)]
    
    paste0(
      "Mean: ", round(mean(values, na.rm = TRUE), 4), "\n",
      "SD: ", round(sd(values, na.rm = TRUE), 4), "\n",
      "Min: ", round(min(values, na.rm = TRUE), 4), "\n",
      "Max: ", round(max(values, na.rm = TRUE), 4), "\n",
      "N: ", length(values)
    )
  })
  
  # Combined data for relationship analysis
  combined_data <- reactive({
    req(ec_raster_data(), es_raster_data())
    
    tryCatch({
      ec_vals <- values(ec_raster_data())
      es_vals <- values(es_raster_data())
      
      # Ensure same length
      min_length <- min(length(ec_vals), length(es_vals))
      ec_vals <- ec_vals[1:min_length]
      es_vals <- es_vals[1:min_length]
      
      # Remove NAs more efficiently
      valid_indices <- !is.na(ec_vals) & !is.na(es_vals)
      
      if(sum(valid_indices) == 0) {
        return(NULL)
      }
      
      result <- data.frame(
        EC = ec_vals[valid_indices],
        ES = es_vals[valid_indices]
      )
      
      message("Combined data created with ", nrow(result), " valid points")
      return(result)
    }, error = function(e) {
      message("Error creating combined data: ", e$message)
      return(NULL)
    })
  })
  
  # Scatter Plot
  output$scatter_plot <- renderPlot({
    if(!data_status$ec_loaded || !data_status$es_loaded) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Need both EC and ES data\nfor relationship analysis", 
                size = 6, hjust = 0.5, vjust = 0.5) +
        theme_void() +
        xlim(0, 1) + ylim(0, 1)
    } else {
      tryCatch({
        df <- combined_data()
        
        if(is.null(df) || nrow(df) == 0) {
          ggplot() + 
            annotate("text", x = 0.5, y = 0.5, label = "No overlapping data\nSelect variables to compare", 
                    size = 6, hjust = 0.5, vjust = 0.5) +
            theme_void() +
            xlim(0, 1) + ylim(0, 1)
        } else {
          # Calculate correlation on full dataset
          cor_val <- cor(df$EC, df$ES, use = "complete.obs")
          cor_text <- paste("r =", round(cor_val, 3))
          
          # Sample data for faster plotting if dataset is large
          if(nrow(df) > 10000) {
            sample_size <- 10000
            df_plot <- df[sample(nrow(df), sample_size), ]
            perc <- round((sample_size / nrow(df)) * 100, 1)
            title_text <- paste("EC-ES Relationship (showing", perc, "% of points)")
          } else {
            df_plot <- df
            title_text <- "EC-ES Relationship"
          }
          
          ggplot(df_plot, aes(x = EC, y = ES)) +
            geom_point(alpha = 0.4, size = 0.8) +  # Smaller, more transparent points
            geom_smooth(data = df, method = "lm", color = "red", se = TRUE) +  # Use full data for trend line
            theme_minimal() +
            labs(x = "Ecological Condition", y = "Ecosystem Service",
                 title = title_text) +
            annotate("text", x = Inf, y = Inf, label = cor_text, 
                    hjust = 1.1, vjust = 1.1, size = 4, color = "blue")
        }
      }, error = function(e) {
        ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = paste("Error creating scatter plot:\n", e$message), 
                  size = 5, hjust = 0.5, vjust = 0.5) +
          theme_void() +
          xlim(0, 1) + ylim(0, 1)
      })
    }
  })
  
  # Box Plots
  output$box_plots <- renderPlot({
    req(combined_data())
    
    df <- combined_data()
    
    if(nrow(df) > 0) {
      # Create intervals
      n_intervals <- input$n_intervals
      df$EC_interval <- cut(df$EC, breaks = n_intervals, labels = FALSE)
      df$EC_interval_label <- cut(df$EC, breaks = n_intervals, 
                                 labels = paste0("Q", 1:n_intervals))
      
      ggplot(df, aes(x = EC_interval_label, y = ES)) +
        geom_boxplot(fill = "lightblue", alpha = 0.7) +
        theme_minimal() +
        labs(x = "EC Intervals", y = "Ecosystem Service",
             title = "ES Distribution across EC Intervals") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  # Correlation Plot (if multiple EC variables selected)
  output$correlation_plot <- renderPlot({
    req(input$show_correlation, selected_ec_vars())
    
    if(length(selected_ec_vars()) > 1) {
      available_vars <- intersect(selected_ec_vars(), names(ec_data))
      
      if(length(available_vars) > 1) {
        # Extract values for correlation analysis
        cor_data <- lapply(available_vars, function(var) {
          values(ec_data[[var]])
        })
        names(cor_data) <- available_vars
        
        # Convert to data frame
        cor_df <- do.call(cbind, cor_data)
        
        # Calculate correlation matrix
        cor_matrix <- cor(cor_df, use = "complete.obs")
        
        # Plot correlation matrix
        corrplot(cor_matrix, method = "color", type = "upper", 
                order = "hclust", tl.cex = 0.8, tl.col = "black")
      }
    }
  })
}

# ============================================================================
# RUN APP
# ============================================================================

shinyApp(ui = ui, server = server)