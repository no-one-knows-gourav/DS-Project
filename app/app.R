# --- 1. SETUP: Install and load necessary packages ---
library(shiny)
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
# Note: tidyverse & reshape2 will be loaded by analysis.R

# ==============================================================================
# --- 2. PRE-PROCESSING: (RUNS ONCE AT STARTUP) ---
# ==============================================================================

# --- A. LOAD RAW DATA (FOR BOTH MAPS AND ANALYSIS) ---
# (This section is unchanged)
if (!exists("state_month_avg_nvi_heatwaves")) {
  message("--- Data 'state_month_avg_nvi_heatwaves' not found. Trying to read from file... ---")
  tryCatch({
    state_month_avg_nvi_heatwaves <- read.csv("state_month_avg_nvi_heatwaves.csv")
  }, error = function(e) {
    stop("Failed to read 'state_month_avg_nvi_heatwaves.csv'. Error: ", e$message)
  })
} else {
  message("--- Using 'state_month_avg_nvi_heatwaves' from Global Environment. ---")
}
if (!exists("state_month_avg_nvi_lightvul")) {
  message("--- Data 'state_month_avg_nvi_lightvul' not found. Trying to read from file... ---")
  tryCatch({
    state_month_avg_nvi_lightvul <- read.csv("state_month_avg_nvi_lightvul.csv")
  }, error = function(e) {
    stop("Failed to read 'state_month_avg_nvi_lightvul.csv'. Error: ", e$message)
  })
} else {
  message("--- Using 'state_month_avg_nvi_lightvul' from Global Environment. ---")
}
if (!exists("state_month_avg_nvi_coldwaves")) {
  message("--- Data 'state_month_avg_nvi_coldwaves' not found. Trying to read from file... ---")
  tryCatch({
    state_month_avg_nvi_coldwaves <- read.csv("state_month_avg_nvi_coldwaves.csv")
  }, error = function(e) {
    stop("Failed to read 'state_month_avg_nvi_coldwaves.csv'. Error: ", e$message)
  })
} else {
  message("--- Using 'state_month_avg_nvi_coldwaves' from Global Environment. ---")
}


# --- B. PROCESS DATA FOR INTERACTIVE MAPS ---
# (This section is unchanged)
india_states_sf <- ne_states(country = "India", returnclass = "sf")
india_states_processed <- india_states_sf %>%
  mutate(state_upper = toupper(name))

process_vulnerability_data <- function(df, map_sf) {
  state_data_processed <- df %>%
    mutate(state_upper = toupper(`properties.STATE`))
  
  left_join(map_sf, state_data_processed, by = "state_upper") %>%
    mutate(
      nvi_binned = cut(
        avg_NVI,
        breaks = c(0, 0.25, 0.50, 0.75, 1.0),
        labels = c("Low (0-0.25)", "Medium (0.25-0.50)", "High (0.50-0.75)", "Very High (0.75-1.0)"),
        include.lowest = TRUE,
        right = FALSE
      )
    )
}

map_and_data_heatwaves <- process_vulnerability_data(state_month_avg_nvi_heatwaves, india_states_processed)
map_and_data_lightning <- process_vulnerability_data(state_month_avg_nvi_lightvul, india_states_processed)
map_and_data_coldwaves <- process_vulnerability_data(state_month_avg_nvi_coldwaves, india_states_processed)

create_vulnerability_map <- function(data_sf, base_map_sf, month_name, main_title, color_palette) {
  month_has_data <- month_name %in% unique(data_sf$time_period)
  plot_subtitle <- if (month_has_data) {
    paste("Monthly Average for", month_name)
  } else {
    paste("No NVI Data Available for", month_name)
  }
  p <- ggplot() +
    geom_sf(data = base_map_sf, fill = "grey80", color = "black", size = 0.25)
  if (month_has_data) {
    filtered_data <- data_sf %>% filter(time_period == month_name)
    p <- p +
      geom_sf(data = filtered_data, aes(fill = nvi_binned), color = "black", size = 0.25) +
      scale_fill_brewer(
        palette = color_palette,
        name = "Avg. Vulnerability (NVI)",
        na.value = "grey80",
        drop = FALSE
      )
  }
  p +
    labs(
      title = main_title,
      subtitle = plot_subtitle,
      caption = "Data Source: Aggregated NVI Data"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "#333333"),
      plot.subtitle = element_text(size = 14, hjust = 0.5, color = "#333333"),
      plot.caption = element_text(size = 9, color = "#555555"),
      legend.position = "bottom",
      legend.title = element_text(size = 10, face = "bold", color = "#333333"),
      legend.text = element_text(size = 9, color = "#333333"),
      legend.key.width = unit(1.5, "cm"),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
      # --- ADD THESE LINES for transparent plot backgrounds ---
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA)
    )
}


# --- C. RUN FULL DATA ANALYSIS SCRIPT ---
message("--- Sourcing analysis.R to pre-calculate analysis data... ---")
source("analysis.R", local = TRUE)
message("--- Analysis script sourced successfully. ---")


# --- END OF PRE-PROCESSING ---
message("--- All data loaded and pre-processed. Starting Shiny App... ---")


# ==============================================================================
# --- 4. SHINY UI: Define the user interface ---
# ==============================================================================

ui <- navbarPage(
  title = "India Climate Vulnerability Dashboard",
  
  # --- ADD ALL THE CSS FOR THE NEW LOOK ---
  tags$head(
    tags$style(HTML("
      /* --- 1. Background Image --- */
      body {
        background-image: url('sunset.png');
        background-size: cover;
        background-position: center center;
        background-attachment: fixed;
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif;
        color: #333; /* Darker text for readability on light backgrounds */
      }

      /* --- 2. Glassmorphism Navbar --- */
      .navbar {
        background-color: rgba(255, 255, 255, 0.75) !important;
        backdrop-filter: blur(15px);
        -webkit-backdrop-filter: blur(15px);
        border-bottom: 1px solid rgba(255, 255, 255, 0.2);
        box-shadow: 0 4px 12px rgba(0,0,0,0.05);
      }
      .navbar-default .navbar-brand {
        color: #111 !important;
        font-weight: 600;
      }
      .navbar-default .navbar-nav > li > a {
        color: #333 !important;
        font-weight: 500;
      }
      .navbar-default .navbar-nav > .active > a {
        background-color: rgba(0, 74, 153, 0.1) !important;
        border-radius: 8px;
      }

      /* --- 3. Compact & Translucent Sidebar (Tab 1) --- */
      .sidebar {
        background-color: rgba(255, 255, 255, 0.75) !important;
        backdrop-filter: blur(15px);
        -webkit-backdrop-filter: blur(15px);
        border-radius: 16px;
        border: 1px solid rgba(255, 255, 255, 0.2);
        box-shadow: 0 4px 12px rgba(0,0,0,0.05);
        padding: 20px;
      }
      
      /* --- 4. Transparent Main Panels --- */
      .main {
        background: none !important;
        border: none !important;
      }
      
      /* --- 5. Bento Box Style for all content --- */
      .bento-box {
        background-color: rgba(255, 255, 255, 0.75);
        backdrop-filter: blur(15px);
        -webkit-backdrop-filter: blur(15px);
        border-radius: 16px;
        padding: 20px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.05);
        border: 1px solid rgba(255, 255, 255, 0.2);
        margin-bottom: 20px; /* Space between boxes */
        height: 100%; /* Make boxes in a row the same height */
      }
      
      /* --- 6. Analysis Tab Styling --- */
      .tab-content {
        background: none !important;
        padding-top: 20px;
      }
      .nav-tabs {
        background: none !important;
        border-bottom: 1px solid rgba(0,0,0,0.1);
      }
      .nav-tabs > li > a {
        background-color: rgba(255, 255, 255, 0.6);
        border: 1px solid rgba(0,0,0,0.05);
        color: #333 !important;
        margin-right: 5px;
        border-radius: 8px 8px 0 0 !important;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        background-color: rgba(255, 255, 255, 0.9) !important;
        border-bottom-color: transparent !important;
        border-color: rgba(0,0,0,0.1) !important;
      }
      
      /* --- 7. Titles & Text --- */
      h2, h3, h4 {
        font-weight: 600;
        color: #111;
      }
      /* Remove the old title panel style */
      .title-panel-container { display: none; }
    "))
  ),
  
  # --- TAB 1: INTERACTIVE MAPS ---
  tabPanel("Interactive Maps",
           sidebarLayout(
             # --- COMPACT SIDEBAR (width = 3 out of 12) ---
             sidebarPanel(
               width = 3,
               h4("Map Controls"),
               sliderInput("month_slider",  
                           label = "Select Month:",
                           min = 1,  
                           max = 12,  
                           value = 4,
                           step = 1,
                           animate = animationOptions(interval = 1500, loop = TRUE),
                           timeFormat = "%B")
             ),
             
             # --- Main panel (width = 9 out of 12) ---
             mainPanel(
               width = 9,
               # --- Wrap each plot in a bento-box div ---
               div(class = "bento-box",
                   plotOutput("map_plot_heatwave", height = "550px")
               ),
               div(class = "bento-box",
                   plotOutput("map_plot_lightning", height = "550px")
               ),
               div(class = "bento-box",
                   plotOutput("map_plot_coldwave", height = "550px")
               )
             )
           )
  ), # --- End of Tab 1 ---
  
  # --- TAB 2: DATA ANALYSIS ---
  tabPanel("Data Analysis",
           fluidPage(
             tabsetPanel(
               
               # --- RQ1 ---
               tabPanel("RQ1: Seasonal Variation",
                        fluidRow(
                          column(6, div(class = "bento-box", 
                                        h4("Heatwave (Boxplot)"),
                                        plotOutput("rq1_box_heat", height = "500px"))),
                          column(6, div(class = "bento-box", 
                                        h4("Heatwave (Bubble)"),
                                        plotOutput("rq1_bubble_heat", height = "500px")))
                        ),
                        fluidRow(
                          column(6, div(class = "bento-box", 
                                        h4("Coldwave (Boxplot)"),
                                        plotOutput("rq1_box_cold", height = "500px"))),
                          column(6, div(class = "bento-box", 
                                        h4("Coldwave (Bubble)"),
                                        plotOutput("rq1_bubble_cold", height = "500px")))
                        ),
                        fluidRow(
                          column(6, div(class = "bento-box", 
                                        h4("Lightning (Boxplot)"),
                                        plotOutput("rq1_box_light", height = "500px"))),
                          column(6, div(class = "bento-box", 
                                        h4("Lightning (Bubble)"),
                                        plotOutput("rq1_bubble_light", height = "500px")))
                        )
               ), # <-- Comma 1 (after RQ1 Panel)
               
               # --- RQ2 ---
               tabPanel("RQ2: Hotspot Ranking",
                        fluidRow(
                          column(12, div(class = "bento-box", 
                                         plotOutput("rq2_hotspot", height = "800px")))
                        )
               ), # <-- Comma 2 (after RQ2 Panel)
               
               # --- RQ3 ---
               tabPanel("RQ3: Hazard Correlation",
                        fluidRow(
                          # Center the 8-column box
                          column(8, offset = 2, div(class = "bento-box", 
                                                    plotOutput("rq3_correlation", height = "600px")))
                        )
               ), # <-- Comma 3 (after RQ3 Panel)
               
               # --- RQ4 ---
               tabPanel("RQ4: Top Anomalies",
                        fluidRow(
                          column(12, 
                                 div(class = "bento-box",
                                     h4("Top 5 Anomaly Analysis"),
                                     p("Select a rank to view the state's seasonal pattern against the national average."),
                                     
                                     radioButtons("anomaly_rank_selector", 
                                                  label = "Select Anomaly Rank:",
                                                  choices = c("Rank 1" = 1, 
                                                              "Rank 2" = 2, 
                                                              "Rank 3" = 3, 
                                                              "Rank 4" = 4, 
                                                              "Rank 5" = 5),
                                                  selected = 1,
                                                  inline = TRUE), # Comma after radioButtons
                                     
                                     hr(), # Comma after hr()
                                     
                                     plotOutput("rq4_anomaly_plot", height = "500px")
                                 ) # end div
                          ) # end column
                        ) # end fluidRow
               ) # <-- No comma here (last panel)
               
             ) # end tabsetPanel
           ) # end fluidPage
  ), # --- End of Tab 3 (Data Analysis) ---
) # <--- THIS IS THE MISSING PARENTHESIS TO CLOSE navbarPage( ...

# ==============================================================================
# --- 5. SHINY SERVER: Define the server logic ---
# ==============================================================================
# (This section is unchanged)

server <- function(input, output) {
  
  # --- SERVER LOGIC FOR TAB 1: INTERACTIVE MAPS ---
  
  selected_month_name <- reactive({
    month.name[input$month_slider]
  })
  
  output$map_plot_heatwave <- renderPlot({
    create_vulnerability_map(
      data_sf = map_and_data_heatwaves,
      base_map_sf = india_states_sf,
      month_name = selected_month_name(),
      main_title = "State-wise Heatwave Vulnerability Index (NVI)",
      color_palette = "YlOrRd"
    )
  }, bg = "transparent") # Set plot output background to transparent
  
  output$map_plot_lightning <- renderPlot({
    create_vulnerability_map(
      data_sf = map_and_data_lightning,
      base_map_sf = india_states_sf,
      month_name = selected_month_name(),
      main_title = "State-wise Lightning Vulnerability Index (NVI)",
      color_palette = "Purples"
    )
  }, bg = "transparent") # Set plot output background to transparent
  
  output$map_plot_coldwave <- renderPlot({
    create_vulnerability_map(
      data_sf = map_and_data_coldwaves,
      base_map_sf = india_states_sf,
      month_name = selected_month_name(),
      main_title = "State-wise Cold Wave Vulnerability Index (NVI)",
      color_palette = "YlGnBu"
    )
  }, bg = "transparent") # Set plot output background to transparent
  
  # --- SERVER LOGIC FOR TAB 2: DATA ANALYSIS ---
  
  output$rq1_box_heat <- renderPlot({
    plot_rq1_boxplot("Heatwave", df_long_anom)
  }, bg = "transparent")
  
  output$rq1_box_cold <- renderPlot({
    plot_rq1_boxplot("Coldwave", df_long_anom)
  }, bg = "transparent")
  
  output$rq1_box_light <- renderPlot({
    plot_rq1_boxplot("LightVul", df_long_anom)
  }, bg = "transparent")
  
  output$rq1_bubble_heat <- renderPlot({
    plot_rq1_bubbleplot("Heatwave", df_long_anom)
  }, bg = "transparent")
  
  output$rq1_bubble_cold <- renderPlot({
    plot_rq1_bubbleplot("Coldwave", df_long_anom)
  }, bg = "transparent")
  
  output$rq1_bubble_light <- renderPlot({
    plot_rq1_bubbleplot("LightVul", df_long_anom)
  }, bg = "transparent")
  
  output$rq2_hotspot <- renderPlot({
    plot_hotspot_index(df_hotspot_index)
  }, bg = "transparent")
  
  output$rq3_correlation <- renderPlot({
    plot_correlation_heatmap(corr_melt, nrow(df_corr_data))
  }, bg = "transparent")
  
  output$rq4_anomaly_plot <- renderPlot({
    # Make sure the input is not null before trying to plot
    req(input$anomaly_rank_selector)
    
    # Convert the character input from radio buttons to a number
    rank_to_plot <- as.numeric(input$anomaly_rank_selector)
    
    # Call the plotting function with the selected rank
    plot_nth_anomaly(
      n = rank_to_plot,
      top_anomalies_df = top_10_anomalies,
      df_base = df_final,
      df_stats = national_monthly_stats
    )
  }, bg = "transparent") # Don't forget the transparent background!
  
} # --- End of Server Function
# ==============================================================================
# --- 6. RUN THE APP ---
# ==============================================================================
shinyApp(ui = ui, server = server)