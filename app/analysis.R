# --- ANALYSIS SCRIPT (analysis.R) ---
# This script assumes the following dataframes already exist in the environment:
# - state_month_avg_nvi_heatwaves
# - state_month_avg_nvi_lightvul
# - state_month_avg_nvi_coldwaves

message("--- analysis.R: Loading analysis libraries... ---")
# Load the necessary library for data manipulation
library(tidyverse)
library(reshape2) # Required for melting the correlation matrix

# --- Configuration ---
NVI_TYPES <- c("Coldwave", "Heatwave", "LightVul")
MONTH_ORDER <- c(
  "January", "February", "March", "April", "May", "June", 
  "July", "August", "September", "October", "November", "December"
)

# --- Step 1: Clean Data (using in-memory data) ---
message("--- analysis.R: Step 1 - Cleaning data... ---")
# MODIFIED: Uses dataframes already loaded by app.R instead of read_csv
df_cold <- state_month_avg_nvi_coldwaves %>%
  select(State = `properties.STATE`, Month = time_period, avg_NVI) %>%
  rename(Coldwave_NVI = avg_NVI)

df_heat <- state_month_avg_nvi_heatwaves %>%
  select(State = `properties.STATE`, Month = time_period, avg_NVI) %>%
  rename(Heatwave_NVI = avg_NVI)

df_light <- state_month_avg_nvi_lightvul %>%
  select(State = `properties.STATE`, Month = time_period, avg_NVI) %>%
  rename(LightVul_NVI = avg_NVI)


# --- Step 2: Merge, Flag Original Data, and Impute ---
message("--- analysis.R: Step 2 - Merging and imputing... ---")
merged_df <- df_cold %>%
  full_join(df_heat, by = c("State", "Month")) %>%
  full_join(df_light, by = c("State", "Month"))

for (nvi in NVI_TYPES) {
  nvi_col <- paste0(nvi, "_NVI")
  flag_col <- paste0(nvi, "_OBSERVED")
  merged_df <- merged_df %>%
    mutate(!!flag_col := !is.na(!!sym(nvi_col)))
}
nvi_cols <- paste0(NVI_TYPES, "_NVI")
merged_df <- merged_df %>%
  mutate(across(all_of(nvi_cols), ~replace_na(.x, 0.0)))
merged_df <- merged_df %>%
  mutate(
    Month = factor(Month, levels = MONTH_ORDER, ordered = TRUE),
    Month_Num = as.numeric(Month)
  )

# --- Step 3: Calculate National Monthly Statistics (Baseline) ---
message("--- analysis.R: Step 3 - Calculating national stats... ---")
national_monthly_stats <- merged_df %>%
  group_by(Month_Num) %>%
  summarise(
    Coldwave_NVI_Mu = mean(Coldwave_NVI, na.rm = TRUE),
    Coldwave_NVI_Sigma = sd(Coldwave_NVI, na.rm = TRUE),
    Coldwave_NVI_IQR = IQR(Coldwave_NVI, na.rm = TRUE),
    Heatwave_NVI_Mu = mean(Heatwave_NVI, na.rm = TRUE),
    Heatwave_NVI_Sigma = sd(Heatwave_NVI, na.rm = TRUE),
    Heatwave_NVI_IQR = IQR(Heatwave_NVI, na.rm = TRUE),
    LightVul_NVI_Mu = mean(LightVul_NVI, na.rm = TRUE),
    LightVul_NVI_Sigma = sd(LightVul_NVI, na.rm = TRUE),
    LightVul_NVI_IQR = IQR(LightVul_NVI, na.rm = TRUE)
  )

# --- Step 4: Merge Statistics and Calculate Z-Scores ---
message("--- analysis.R: Step 4 - Calculating Z-Scores... ---")
df_final <- merged_df %>%
  left_join(national_monthly_stats, by = "Month_Num")

for (nvi in NVI_TYPES) {
  nvi_col <- paste0(nvi, "_NVI")
  mu_col <- paste0(nvi, "_NVI_Mu")
  sigma_col <- paste0(nvi, "_NVI_Sigma")
  z_score_col <- paste0(nvi, "_ZScore")
  abs_z_score_col <- paste0(nvi, "_AbsZScore")
  
  df_final <- df_final %>%
    mutate(!!z_score_col := 
             ifelse(!!sym(sigma_col) != 0, 
                    (!!sym(nvi_col) - !!sym(mu_col)) / !!sym(sigma_col), 
                    0)
    ) %>%
    mutate(!!abs_z_score_col := abs(!!sym(z_score_col)))
}

# --- Step 5: Final Output (Verification) ---
# (Printing and saving to CSV is commented out for the Shiny app)
# df_output <- df_final %>% ...
# print(head(df_output, 10))
# write_csv(df_final, "nvi_zscore_results.csv")

# --- Step 6: Create Anomaly-Focused Long Dataframe ---
message("--- analysis.R: Step 6 - Creating long dataframe... ---")
df_long_anom <- df_final %>%
  pivot_longer(
    cols = starts_with(NVI_TYPES), 
    names_to = c("NVI_Hazard", ".value"),
    names_pattern = "(.+?)_(NVI|ZScore|AbsZScore|OBSERVED)"
  ) %>%
  rename(NVI_Value = NVI,
         Z_Score = ZScore,
         Abs_Z_Score = AbsZScore,
         Observed_Flag = OBSERVED) %>%
  filter(Observed_Flag == TRUE) %>%
  filter(Abs_Z_Score > 0.001)

df_state_avg <- df_long_anom %>%
  group_by(State, NVI_Hazard) %>%
  summarise(Avg_NVI = mean(NVI_Value, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(
    names_from = NVI_Hazard,
    values_from = Avg_NVI,
    names_prefix = "Avg_"
  )

# ==============================================================================
# --- RESEARCH QUESTION 1: EDA FOR DRASTIC VARIATIONS (Seasonal/Spatial) ---
# ==============================================================================
message("--- analysis.R: Step 7 - Defining RQ1 plots... ---")

# 7.1 Function to Create Boxplot with Max/Min Highlights
plot_rq1_boxplot <- function(nvi_hazard, df_long) {
  df_plot <- df_long %>% 
    filter(NVI_Hazard == nvi_hazard) %>%
    mutate(Month = factor(Month, levels = MONTH_ORDER, ordered = TRUE))
  
  stats_points <- df_plot %>%
    group_by(Month) %>%
    summarise(
      max_NVI = max(NVI_Value, na.rm = TRUE),
      min_NVI = min(NVI_Value, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    left_join(df_plot %>% select(Month, State, NVI_Value), 
              by = c("Month", "max_NVI" = "NVI_Value")) %>%
    rename(max_State = State) %>%
    left_join(df_plot %>% select(Month, State, NVI_Value), 
              by = c("Month", "min_NVI" = "NVI_Value")) %>%
    rename(min_State = State) %>%
    distinct(Month, .keep_all = TRUE)
  
  max_points <- stats_points %>%
    select(Month, NVI_Value = max_NVI, States = max_State) %>%
    rename(State_Annotation = States) 
  
  p <- ggplot(df_plot, aes(x = Month, y = NVI_Value, color = Month)) +
    geom_boxplot(outlier.shape = NA) + 
    geom_jitter(width = 0.1, alpha = 0.5) + 
    geom_text(
      data = max_points,
      aes(x = Month, y = NVI_Value, label = paste0("Max: ", round(NVI_Value, 3), "\n", State_Annotation)),
      size = 2.5, hjust = 0.5, vjust = -0.2, color = "black", inherit.aes = FALSE
    ) +
    labs(
      title = paste0("Seasonal Distribution and State Variation of ", nvi_hazard, " NVI"),
      subtitle = "Boxplots show median, quartiles, and range across all states for each month.",
      x = "Month", y = paste0(nvi_hazard, " NVI")
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1), 
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA)
    )
  
  return(p) # Return plot object for Shiny
}


# 7.2 Function to Create Bubble/Scatter Plot with Max Highlights
plot_rq1_bubbleplot <- function(nvi_hazard, df_long) {
  df_plot <- df_long %>% 
    filter(NVI_Hazard == nvi_hazard) %>%
    mutate(Month = factor(Month, levels = MONTH_ORDER, ordered = TRUE))
  
  max_points <- df_plot %>%
    group_by(Month) %>%
    filter(NVI_Value == max(NVI_Value, na.rm = TRUE)) %>%
    ungroup() %>%
    distinct(Month, .keep_all = TRUE) %>% # Handle ties
    rename(State_Annotation = State) 
  
  p <- ggplot(df_plot, aes(x = Month, y = NVI_Value, color = State, size = NVI_Value)) +
    geom_point(alpha = 0.7) +
    geom_text(
      data = max_points,
      aes(x = Month, y = NVI_Value, label = State_Annotation),
      size = 2.5, hjust = 0.5, vjust = -1.5, color = "red", inherit.aes = FALSE 
    ) +
    guides(size = "none") +
    labs(
      title = paste0("Bubble Plot of ", nvi_hazard, " NVI: State-Month Distribution"),
      subtitle = "Point size reflects NVI magnitude. Shows which state drives monthly extremes.",
      x = "Months", y = paste0(nvi_hazard, " NVI"), color = "State"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.05, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1), 
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA)
    )
  
  return(p) # Return plot object for Shiny
}

# (Driver functions 7.3 and 7.4 are not needed as Shiny calls the main functions directly)

# ==============================================================================
# --- RESEARCH QUESTION 2: HEURISTIC HOTSPOT GROUPING ---
# ==============================================================================
message("--- analysis.R: Step 8 - Defining RQ2 plots... ---")

# 8.1 State Feature Engineering
df_state_features <- df_long_anom %>%
  group_by(State, NVI_Hazard) %>%
  summarise(
    Mean_NVI = mean(NVI_Value, na.rm = TRUE),
    SD_NVI = sd(NVI_Value, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_wider(
    names_from = NVI_Hazard,
    values_from = c(Mean_NVI, SD_NVI)
  )

feature_cols <- c(
  "Mean_NVI_Coldwave", "SD_NVI_Coldwave",
  "Mean_NVI_Heatwave", "SD_NVI_Heatwave",
  "Mean_NVI_LightVul", "SD_NVI_LightVul"
)
df_state_features_clean <- df_state_features %>%
  filter(complete.cases(!!!syms(feature_cols)))

# 8.2 Min-Max Normalization (Scaling)
min_max_normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}
df_normalized <- df_state_features_clean %>%
  mutate(across(all_of(feature_cols), min_max_normalize, .names = "Norm_{.col}"))
norm_cols <- paste0("Norm_", feature_cols)

# 8.3 Calculate Composite Hotspot Index (H)
df_hotspot_index <- df_normalized %>%
  rowwise() %>%
  mutate(
    Hotspot_Index = mean(c(!!!syms(norm_cols)), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(desc(Hotspot_Index))

# 8.4 Plot Final Hotspot Index Bar Plot
plot_hotspot_index <- function(df_index) {
  p <- ggplot(df_index, aes(x = reorder(State, Hotspot_Index), y = Hotspot_Index, fill = Hotspot_Index)) +
    geom_col() +
    coord_flip() + 
    scale_fill_gradient(low = "lightblue", high = "red") +
    labs(
      title = "State Ranking by Composite Hotspot Index (RQ2)",
      subtitle = "Index = Average of 6 Normalized NVI Features (Mean & Std Dev for all 3 Hazards)",
      x = "State",
      y = "Composite Hotspot Index (0 = Lowest Risk, 1 = Highest Risk)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, face = "italic"),
      legend.position = "none", 
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA)
    )
  
  return(p) # Return plot object for Shiny
}

# ==============================================================================
# --- RESEARCH QUESTION 3: CORRELATION ANALYSIS ---
# ==============================================================================
message("--- analysis.R: Step 9 - Defining RQ3 plots... ---")

# 9.1 Data Preparation for Correlation
df_corr_data <- df_state_avg %>%
  filter(!is.na(Avg_Coldwave) & !is.na(Avg_Heatwave) & !is.na(Avg_LightVul)) %>%
  select(Avg_Coldwave, Avg_Heatwave, Avg_LightVul) %>%
  rename(
    `Coldwaves VI` = Avg_Coldwave,
    `Heatwaves VI` = Avg_Heatwave,
    `Lightvul VI` = Avg_LightVul
  )

# 9.2 Calculate Correlation Matrix
corr_matrix <- cor(df_corr_data)
corr_melt <- melt(corr_matrix)

# 9.3 Define Plotting function for Correlation Heatmap
plot_correlation_heatmap <- function(corr_melt_df, num_states) {
  p <- ggplot(corr_melt_df, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
      low = "red", mid = "white", high = "deepskyblue",
      midpoint = 0, limit = c(-1, 1), space = "Lab",
      name = "Correlation"
    ) +
    geom_text(aes(label = round(value, 4)), color = "black", size = 4) +
    theme_minimal() +
    labs(
      title = "Correlation Matrix between State-Wise Average Vulnerability Indices",
      subtitle = paste("Based on annual average NVI for", num_states, "states with observed data for all three hazards."),
      x = "Hazard 1",
      y = "Hazard 2"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid = element_blank(), 
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA)
    ) +
    coord_fixed()
  
  return(p) # Return plot object for Shiny
}

# ==============================================================================
# --- RESEARCH QUESTION 4: ANOMALY DETECTION ---
# ==============================================================================
message("--- analysis.R: Step 10 - Defining RQ4 plots... ---")

# 10.1 Rank and select the top 10 anomalies
top_10_anomalies <- df_long_anom %>%
  arrange(desc(Abs_Z_Score)) %>%
  head(10)

# 10.2 Create Plotting Function for Contextual Line Plot
plot_anomaly_context <- function(anomaly_row, df_base, df_stats) {
  anom_state <- anomaly_row$State
  anom_month <- anomaly_row$Month
  anom_hazard <- anomaly_row$NVI_Hazard
  anom_zscore <- anomaly_row$Abs_Z_Score
  nvi_col <- paste0(anom_hazard, "_NVI")
  mu_col <- paste0(anom_hazard, "_NVI_Mu")
  
  plot_data <- df_base %>%
    select(State, Month_Num, !!nvi_col) %>%
    rename(Observed_NVI = !!nvi_col) %>%
    left_join(df_stats %>% select(Month_Num, !!mu_col), by = "Month_Num") %>%
    rename(National_Mean_NVI = !!mu_col) %>%
    filter(State == anom_state) %>%
    pivot_longer(
      cols = c(Observed_NVI, National_Mean_NVI),
      names_to = "Series",
      values_to = "NVI"
    )
  
  p <- ggplot(plot_data, aes(x = Month_Num, y = NVI, color = Series)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_point(data = filter(plot_data, Month_Num == anomaly_row$Month_Num, Series == "Observed_NVI"), 
               color = "red", size = 4) +
    scale_x_continuous(breaks = 1:12, labels = MONTH_ORDER) +
    labs(
      title = paste0(anom_hazard, " NVI Seasonal Pattern: ", anom_state, " (Anomaly Z=", round(anom_zscore, 2), ")"),
      subtitle = paste0("Anomalous Month: ", anom_month),
      x = "Month",
      y = paste0(anom_hazard, " NVI"),
      color = "Data Series"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA)
    )
  
  return(p) # Return plot object for Shiny
}

# 10.3 Function to plot the Nth anomaly
plot_nth_anomaly <- function(n, top_anomalies_df, df_base, df_stats) {
  
  if (n < 1 || n > nrow(top_anomalies_df) || is.na(n) || nrow(top_anomalies_df) == 0) {
    # Return an empty plot with an error message
    return(
      ggplot() + 
        labs(title = paste("Error: Anomaly index", n, "is out of bounds or no anomalies found.")) +
        theme_minimal() + 
        theme(
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA)
        )
    )
  }
  
  anomaly_row <- top_anomalies_df[n, ]
  p <- plot_anomaly_context(anomaly_row, df_base, df_stats)
  
  return(p) # Return plot object for Shiny
}

message("--- analysis.R: All analysis objects and functions created. ---")