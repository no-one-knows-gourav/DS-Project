# Load required libraries
# Ensure you have these installed: install.packages(c("rvest", "httr", "jsonlite"))
library(rvest)
library(httr)
library(jsonlite)

#' Scrape and Process IMD Heat Wave Data from Multiple URLs
#'
#' This function takes a vector of base URLs and a corresponding vector of time periods (e.g., months).
#' It scrapes heat wave vulnerability data from each URL, processes it, adds a column for the time period,
#' and then combines all the data into a single data frame.
#'
#' @param base_urls A character vector of base URLs for the IMD Heat Wave Atlas pages.
#'                  (e.g., "https://imdpune.gov.in/hazardatlas/heat_wave/annual/nvi/")
#' @param time_periods A character vector of the same length as base_urls, where each element
#'                     is a string describing the time period for the corresponding URL (e.g., "Annual", "March").
#'
#' @return A single data frame containing the combined and cleaned data from all provided URLs,
#'         with an additional column indicating the time period. Returns NULL if no data could be scraped.
scrape_imd_data <- function(base_urls, time_periods) {
  
  # --- Input Validation ---
  if (length(base_urls) != length(time_periods)) {
    stop("Error: The 'base_urls' and 'time_periods' vectors must be the same length.")
  }
  
  # List to store data frames from each URL
  all_data_list <- list()
  
  # --- Loop through each URL ---
  for (i in 1:length(base_urls)) {
    base_url <- base_urls[i]
    time_period <- time_periods[i]
    
    cat(paste0("\n--- Processing: ", time_period, " (", base_url, ") ---\n"))
    
    # Initialize a variable to hold the extracted data for the current iteration
    heat_wave_data <- NULL 
    
    # --- Data Extraction Logic (based on your provided code) ---
    tryCatch({
      main_page_url <- paste0(base_url, "index.html")
      webpage <- read_html(main_page_url)
      
      # Find all script tags and their sources
      script_sources <- webpage %>%
        html_elements("script") %>%
        html_attr("src")
      
      script_sources <- script_sources[!is.na(script_sources)]
      
      # Find the target script (assuming it contains 'NormalizedVulnerabilityIndex')
      target_script <- script_sources[grepl("NormalizedVulnerabilityIndex", script_sources)]
      
      if (length(target_script) > 0) {
        cat("Found target data script:", target_script[1], "\n")
        
        # Construct the full URL for the script
        full_script_url <- if (!grepl("^https?://", target_script[1])) {
          paste0(base_url, target_script[1])
        } else {
          target_script[1]
        }
        
        # Fetch the script content
        script_response <- GET(full_script_url)
        
        if (status_code(script_response) == 200) {
          js_content <- content(script_response, "text", encoding = "UTF-8")
          
          # Extract JSON string from the JavaScript variable assignment
          json_start <- regexpr("\\{", js_content)[1]
          json_end <- tail(gregexpr("\\}", js_content)[[1]], 1)[1]
          
          if (json_start > 0 && json_end > json_start) {
            json_string <- substr(js_content, json_start, json_end)
            
            # Parse the JSON
            heat_wave_data <- fromJSON(json_string, flatten = TRUE)
            cat("Successfully extracted and parsed JSON data.\n")
          } else {
            cat("Warning: Could not extract JSON from JavaScript file.\n")
          }
        } else {
          cat("Warning: Failed to fetch script file. Status:", status_code(script_response), "\n")
        }
      } else {
        cat("Warning: No 'NormalizedVulnerabilityIndex' script found on the page.\n")
      }
      
    }, error = function(e) {
      cat("An error occurred while processing the URL:", e$message, "\n")
    })
    
    # --- Data Cleaning and Processing ---
    if (!is.null(heat_wave_data) && "features" %in% names(heat_wave_data)) {
      # Extract the features data frame
      heat_wave_df <- heat_wave_data$features
      
      # Remove specified geometry columns and the 'type' column
      cols_to_remove <- c("geometry.type", "geometry.coordinates", "type")
      heat_wave_df <- heat_wave_df[, !names(heat_wave_df) %in% cols_to_remove]
      
      # Add the month/time period column
      heat_wave_df$time_period <- time_period
      
      # Add the cleaned data frame to our list
      all_data_list[[time_period]] <- heat_wave_df
      
      cat("Cleaned and stored data for", time_period, "\n")
    } else {
      cat("Skipping data processing for", time_period, "due to missing data.\n")
    }
  } # End of for loop
  
  # --- Combine all data frames ---
  if (length(all_data_list) > 0) {
    cat("\n--- Combining all datasets ---\n")
    # Using do.call and rbind to combine all data frames in the list
    combined_df <- do.call(rbind, all_data_list)
    # Reset row names for a clean final data frame
    rownames(combined_df) <- NULL
    cat("Successfully combined", length(all_data_list), "datasets into one.\n")
    return(combined_df)
  } else {
    cat("\n--- No data was successfully scraped. Returning NULL. ---\n")
    return(NULL)
  }
}

# --- Example Usage ---

# -------- HEAT WAVES -------------

# 1. Define the vectors for URLs and their corresponding names (months)
urls_to_scrape_heatwaves <- c(
  "https://imdpune.gov.in/hazardatlas/heat_wave/april/nvi/",
  "https://imdpune.gov.in/hazardatlas/heat_wave/may/nvi/", 
  "https://imdpune.gov.in/hazardatlas/heat_wave/june/nvi/", 
  "https://imdpune.gov.in/hazardatlas/heat_wave/july/nvi/"
)
time_periods_to_scrape_heatwaves <- c("April", "May", "June", "July")

# 2. Call the function with these vectors
final_data_heatwaves <- scrape_imd_data(urls_to_scrape_heatwaves, time_periods_to_scrape_heatwaves)

# 3. Inspect the final combined dataset
if (!is.null(final_data_heatwaves)) {
  cat("\n--- Scraping Complete. Final Data Summary: ---\n")
  cat("Dimensions of the combined data frame:", dim(final_data_heatwaves), "\n")
  cat("Column names:", paste(names(final_data_heatwaves), collapse = ", "), "\n\n")
  
  cat("First 6 rows of the combined data:\n")
  print(head(final_data_heatwaves))
  
  cat("\nLast 6 rows of the combined data:\n")
  print(tail(final_data_heatwaves))
  
  cat("\nFrequency of data by time period:\n")
  print(table(final_data_heatwaves$time_period))
}

library(dplyr)

# Assume 'final_data' is a pre-existing data frame

# Group by state and time period to calculate the average vulnerability index
state_month_avg_nvi_heatwaves <- final_data_heatwaves %>%
  group_by(`properties.STATE`, time_period) %>%
  summarise(
    # Calculate the mean of the vulnerability index, removing any NA values
    # The column name is wrapped in backticks because it contains spaces
    avg_NVI = mean(`properties.Normalized Vulnerability Index`, na.rm = TRUE),
    # Count the number of districts (rows) in each group
    n_districts = n(),
    # .groups = 'drop' is good practice to avoid downstream issues with grouped data frames
    .groups = 'drop'
  ) %>%
  # Arrange the results by state and then by the time period
  arrange(`properties.STATE`, time_period)

# Print the resulting summary table to the console
print(state_month_avg_nvi_heatwaves)

# --- (Optional) Save the aggregated data to a CSV file for future use ---
write.csv(state_month_avg_nvi_heatwaves, "state_month_avg_nvi_heatwaves.csv", row.names = FALSE) 

# ---------- LIGHTNING ----------- 

# 1. Define the vectors for URLs and their corresponding names (months)
urls_to_scrape_lightvul <- c(
  "https://imdpune.gov.in/hazardatlas/lightvul/february/nvi/",
  "https://imdpune.gov.in/hazardatlas/lightvul/march/nvi/", 
  "https://imdpune.gov.in/hazardatlas/lightvul/april/nvi/", 
  "https://imdpune.gov.in/hazardatlas/lightvul/may/nvi/", 
  "https://imdpune.gov.in/hazardatlas/lightvul/june/nvi/",
  "https://imdpune.gov.in/hazardatlas/lightvul/july/nvi/",
  "https://imdpune.gov.in/hazardatlas/lightvul/august/nvi/",
  "https://imdpune.gov.in/hazardatlas/lightvul/september/nvi/",
  "https://imdpune.gov.in/hazardatlas/lightvul/october/nvi/"
)
time_periods_to_scrape_lightvul <- c("February", "March", "April", "May", "June", "July", "August", "September", "October")

# 2. Call the function with these vectors
final_data_lightvul <- scrape_imd_data(urls_to_scrape_lightvul, time_periods_to_scrape_lightvul)

# 3. Inspect the final combined dataset
if (!is.null(final_data_lightvul)) {
  cat("\n--- Scraping Complete. Final Data Summary: ---\n")
  cat("Dimensions of the combined data frame:", dim(final_data_lightvul), "\n")
  cat("Column names:", paste(names(final_data_lightvul), collapse = ", "), "\n\n")
  
  cat("First 6 rows of the combined data:\n")
  print(head(final_data_lightvul))
  
  cat("\nLast 6 rows of the combined data:\n")
  print(tail(final_data_lightvul))
  
  cat("\nFrequency of data by time period:\n")
  print(table(final_data_lightvul$time_period))
}

# Group by state and time period to calculate the average vulnerability index
state_month_avg_nvi_lightvul <- final_data_lightvul %>%
  group_by(`properties.STATE`, time_period) %>%
  summarise(
    # Calculate the mean of the vulnerability index, removing any NA values
    # The column name is wrapped in backticks because it contains spaces
    avg_NVI = mean(`properties.Lightning_Normalized Vulnerability Index`, na.rm = TRUE),
    # Count the number of districts (rows) in each group
    n_districts = n(),
    # .groups = 'drop' is good practice to avoid downstream issues with grouped data frames
    .groups = 'drop'
  ) %>%
  # Arrange the results by state and then by the time period
  arrange(`properties.STATE`, time_period)

# Print the resulting summary table to the console
print(state_month_avg_nvi_lightvul)

# --- (Optional) Save the aggregated data to a CSV file for future use ---
write.csv(state_month_avg_nvi_lightvul, "state_month_avg_nvi_lightvul.csv", row.names = FALSE) 

# ------- COLD WAVES ----------- 

# 1. Define the vectors for URLs and their corresponding names (months)
urls_to_scrape_coldwaves <- c(
  "https://imdpune.gov.in/hazardatlas/cold_wave/december/nvi/",
  "https://imdpune.gov.in/hazardatlas/cold_wave/january/nvi/", 
  "https://imdpune.gov.in/hazardatlas/cold_wave/february/nvi/", 
  "https://imdpune.gov.in/hazardatlas/cold_wave/march/nvi/"
)
time_periods_to_scrape_coldwaves <- c("December", "January", "February", "March")

# 2. Call the function with these vectors
final_data_coldwaves <- scrape_imd_data(urls_to_scrape_coldwaves, time_periods_to_scrape_coldwaves)

# 3. Inspect the final combined dataset
if (!is.null(final_data_coldwaves)) {
  cat("\n--- Scraping Complete. Final Data Summary: ---\n")
  cat("Dimensions of the combined data frame:", dim(final_data_coldwaves), "\n")
  cat("Column names:", paste(names(final_data_coldwaves), collapse = ", "), "\n\n")
  
  cat("First 6 rows of the combined data:\n")
  print(head(final_data_coldwaves))
  
  cat("\nLast 6 rows of the combined data:\n")
  print(tail(final_data_coldwaves))
  
  cat("\nFrequency of data by time period:\n")
  print(table(final_data_coldwaves$time_period))
}

# Group by state and time period to calculate the average vulnerability index
state_month_avg_nvi_coldwaves <- final_data_coldwaves %>%
  group_by(`properties.STATE`, time_period) %>%
  summarise(
    # Calculate the mean of the vulnerability index, removing any NA values
    # The column name is wrapped in backticks because it contains spaces
    avg_NVI = mean(`properties.Normalized Vulnerability Index`, na.rm = TRUE),
    # Count the number of districts (rows) in each group
    n_districts = n(),
    # .groups = 'drop' is good practice to avoid downstream issues with grouped data frames
    .groups = 'drop'
  ) %>%
  # Arrange the results by state and then by the time period
  arrange(`properties.STATE`, time_period)

# Print the resulting summary table to the console
print(state_month_avg_nvi_coldwaves)

# --- (Optional) Save the aggregated data to a CSV file for future use ---
write.csv(state_month_avg_nvi_coldwaves, "state_month_avg_nvi_coldwaves.csv", row.names = FALSE) 

