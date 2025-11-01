## Project Methodology: India Climate Vulnerability Dashboard
This application provides a comprehensive visualization and analysis of climate hazard vulnerability across major Indian states. Our core goal is to move beyond simple hazard counting and quantify the Normalized Vulnerability Index (NVI) for three critical, contrasting hazard types: Heatwaves, Coldwaves, and Lightning.
The analysis is structured around four distinct research questions (RQs), designed to provide granular insights for policy and risk management. All data is processed at the State-Month-Hazard level, providing a detailed temporal and geographical understanding of risk.
### Research Question 1: Temporal Distribution and State-Level Outliers
This phase focuses on understanding the monthly distribution of vulnerability and identifying states that experience unusually high or low NVI values relative to the national average for that month.
Distribution Analysis: Boxplots and bubble plots are generated to visualize the distribution of NVI values across all states for each month and hazard type, revealing seasonal patterns and variance.
Outlier Detection: For every month and hazard type, we calculate the Mean $$\bar{x}$$ and Standard Deviation $$\sigma$$ of the NVI values across all states. We then identify states whose NVI values lie outside the range of $$[\bar{x} - 2\sigma, \bar{x} + 2\sigma]$$. These outliers represent states exhibiting statistically unusual vulnerability during that specific month.
### Research Question 2: Hotspot Identification via Composite Index
This phase aims to create a single, quantifiable measure to rank states based on their long-term susceptibility to all three climate hazards combined, helping to identify "hotspot" regions.
Feature Engineering: We filter the data to include only states with complete NVI data across all three hazards. Six core features are calculated for each state:
$$\text{Annual Mean NVI} \quad (\bar{x}_{\text{Heatwave}}, \bar{x}_{\text{Coldwave}}, \bar{x}_{\text{Lightning}})$$$$\text{Annual Standard Deviation NVI} \quad (\sigma_{\text{Heatwave}}, \sigma_{\text{Coldwave}}, \sigma_{\text{Lightning}})$$
Heuristic Hotspot Index (HHI): The HHI is calculated as the mean of these six features. A higher HHI value indicates a state with higher overall vulnerability and greater variability across the three hazard types.
Visualization: States are ranked by their HHI values and displayed in a bar plot, clearly identifying regions that are most at risk and can be grouped together as priority hotspots.
### Research Question 3: Inter-Hazard Correlation Analysis
This question explores the relationship between the three hazard types, determining if high vulnerability to one hazard (ee.g., Heatwaves) correlates with high or low vulnerability to another (e.g., Coldwaves or Lightning).
Data Preparation: The analysis is restricted to states with complete NVI data for all hazards.
Correlation Matrix: A Correlation Matrix and Correlation Heatmap are generated to visualize the pairwise linear relationship between the NVI values of Heatwaves, Coldwaves, and Lightning. This helps establish if resource allocation should be planned independently or jointly.
### Research Question 4: Absolute Anomaly Detection
This phase pinpoints the top ten most anomalous State-Month-Hazard combinations, representing the most extreme vulnerability events observed across the dataset.
Z-Score Calculation: For every State-Month-Hazard observation, we calculate the absolute Z-score $$|Z|$$ using the state-specific, hazard-specific, long-term mean and standard deviation:
$$[|Z| = \left| \frac{x_{i} - \bar{x}_{\text{State, Hazard}}}{\sigma_{\text{State, Hazard}}} \right|]$$

where $x_i$ is the NVI value for a specific month, $\bar{x}$ is the annual mean NVI for that State and Hazard, and $\sigma$ is the annual standard deviation.
Top 10 Anomalies: Observations are ordered by their absolute Z-score, and the top 10 are selected. These ten rows represent the most statistically significant deviations from a state's expected vulnerability pattern, indicating critical points for further investigation.
