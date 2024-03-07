#install.packages("dplyr")
#install.packages("ggplot2")

library(dplyr)
library(ggplot2)

setwd("Z:/HU onedrive/OneDrive - Humboldt-Universitaet zu Berlin, CMS/WiSe2324/Thesis Seminar/Paper_Replication")
data <- read.csv("compustat_usa_1999_2006_1dec08_csv_cleaned.csv", sep = ";")
write.csv(data, "org_data.csv", row.names = FALSE)

# construct no_gap data set
company_counts <- table(data$smbl)

# Get the companies with at least 8 observations
valid_companies <- names(company_counts)[company_counts == 8]

# Filter the original dataset to keep only the valid companies
no_gap_firms <- data[data$smbl %in% valid_companies, ]
#change NA to 0
no_gap_firms[is.na(no_gap_firms)] <- 0
# >>>>>>>>>>>>>>>>>>>>

#change NA to 0
data[is.na(data)] <- 0

# Filter the data to keep only rows where DNUM is between 6000 and 6700 (finance, insurance, real state) or between 4000 and 4900 (transportation & public utilities)
data <- data %>%
  filter(!(dnum >= 6000 & dnum <= 6700 | dnum >= 4000 & dnum <= 4900))


#   function of table 1
process_table1_data <- function(data) {
  # Subset the data
  table1_data <- subset(data, select = c(dnum, yeara, data162, data2, data3, data68, data4, data8, data31, data32, data33, data69, data6, data34, data70, data71, data72, data5, data9, data75, data35, data38, data181, data130, data60, data216, data6, data111))
  
  # Replace NA values with 0
  table1_data[is.na(table1_data)] <- 0
  
  # Get column names except the first two
  columns_to_divide <- names(table1_data)[-c(1, 2)]
  
  # Divide columns by total assets(#6)
  table1_data[, columns_to_divide] <- table1_data[, columns_to_divide] / table1_data$data6
  
  # Group the data by year and calculate the average for each column
  yearly_avg <- table1_data %>%
    group_by(yeara) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE))
  
  # Group the data by year and calculate the number of observations in each year
  yearly_counts <- table1_data %>%
    group_by(yeara) %>%
    summarise(count = n())
  
  # Merge the average and count data
  yearly_avg <- left_join(yearly_avg, yearly_counts, by = "yeara")
  
  return(list(table1_data = table1_data, yearly_avg = yearly_avg))
}

#normalizing variables of no gap firms by total assets
no_gap_firms_divided <- process_table1_data(no_gap_firms)
table1_no_gap <- no_gap_firms_divided$table1_data

# gap allowed data
table1 <- process_table1_data(data)
table1_divided <- table1$table1_data
table1_average <- table1$yearly_avg
write.csv(table1_average, "table1.csv", row.names = FALSE)

# function to process table 2
process_table2_data <- function(data) {
  # Construct Table 2 with additional columns
  table2 <- data %>%
    mutate(
      `Cash dividends` = data127,
      `Investments` = ifelse(data318 >= 1 & data318 <= 3,
                             data128 + data113 + data129 + data219 - data107 - data109,
                             data128 + data113 + data129 - data107 - data109 - data309 - data310),
      `ChangeWC` = ifelse(data31 == 1,
                          data236 + data274 + data301,
                          ifelse(data318 %in% c(2, 3),
                                 -data236 + data274 - data301,
                                 ifelse(data318 == 7,
                                        -data302 - data303 - data304 - data305 - data307 + data274 - data312 - data301,
                                        NA))), # NA if none of the conditions are met
      `InternalCF` = ifelse(data318 %in% c(1, 2, 3),
                            data123 + data124 + data125 + data126 + data106 + data213 + data217 + data218,
                            ifelse(data318 == 7,
                                   data123 + data124 + data125 + data126 + data106 + data213 + data217 + data314,
                                   NA)), # NA if none of the conditions are met
      `Financing_deficit` = `Cash dividends` + `Investments` + `ChangeWC` - `InternalCF`,
      `net_debt_issued` = data111 - data114,
      `net_equity_issued` = data108 - data115,
      `net_external_finance` = `net_debt_issued` + `net_equity_issued`,
      `gross_debt_issued` = data111,
      `long_term_debt_total` = data9,
      `year` = `yeara`
    ) %>%
    select(`year`, `Cash dividends`, `Investments`, `ChangeWC`, `InternalCF`, `Financing_deficit`, `net_debt_issued`, `net_equity_issued`, `net_external_finance`, `gross_debt_issued`, `long_term_debt_total`)
  
  # Calculate net_debt_issued and net_equity_issued
  table2_divided <- table2 %>%
    mutate(across(-year, ~./data$data6))
  
  # Calculate the yearly averages
  table2_yearly_average <- table2_divided %>%
    group_by(year) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE))
  
  # Calculate the count of observations for each year
  yearly_counts <- table2_divided %>%
    group_by(year) %>%
    summarise(count = n())
  
  # Append the counts to the final table
  final_table2 <- table2_yearly_average %>%
    left_join(yearly_counts, by = "year")
  
  return(list(table2_divided = table2_divided, final_table2 = final_table2))
}

table2_data <- process_table2_data(data)

# Accessing the processed table2_divided
table2_divided <- table2_data$table2_divided

# Accessing the processed final_table2
final_table2 <- table2_data$final_table2
write.csv(final_table2, "table2.csv", row.names = FALSE)

# table 2 for no gap data
table2_data_no_gap <- process_table2_data(no_gap_firms)

# Accessing the processed table2_divided
table2_divided_no_gap <- table2_data_no_gap$table2_divided

# plot graph 1 ####################
# Convert year to factor
final_table2$year <- as.factor(final_table2$year)

# Create a line plot
line_plot <- ggplot(final_table2, aes(x = year)) +
  geom_line(aes(y = Financing_deficit, color = "Financing Deficit", group = 1), linetype = "solid", size = 1) +
  geom_line(aes(y = net_debt_issued, color = "Net Debt Issued", group = 1), linetype = "dashed", size = 0.5) +
  geom_line(aes(y = net_equity_issued, color = "Net Equity Issued", group = 1), linetype = "dashed", size = 1.5) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +  # Add a solid line at y = 0
  labs(x = "Year", y = "Value") +
  scale_color_manual(values = c("Financing Deficit" = "black", "Net Debt Issued" = "black", "Net Equity Issued" = "black")) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white")) +
  ylim(-0.05, 0.25) +
  scale_y_continuous(breaks = seq(-0.05, 0.25, by = 0.05))

# Print the line plot
print(line_plot)
# Define the file path and name for the saved plot
file_path <- "Z:/HU onedrive/OneDrive - Humboldt-Universitaet zu Berlin, CMS/WiSe2324/Thesis Seminar/Paper_Replication/graph1.png"

# Save the plot
ggsave(filename = file_path, plot = line_plot, width = 10, height = 6, units = "in", dpi = 300)

################## end of graph 1 ############

# Table 3 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#############################################################################################
#3
# Perform regressions
regression_results <- data.frame(
  Model = character(),
  Number_of_Samples = numeric(),
  R_Squared = numeric(),
  Coefficients = character(),
  Constant = numeric(),
  Significance_Level = character(),
  stringsAsFactors = FALSE
)

# Regression for no gap data
# Column 1 regression
regression_model_1 <- lm(net_debt_issued ~ Financing_deficit, data = table2_divided_no_gap)
regression_results[1, ] <- c(
  "net debt issued (1) no gap", 
  length(regression_model_1$residuals), 
  summary(regression_model_1)$r.squared, 
  paste(names(coef(regression_model_1)), ": ", coef(regression_model_1), sep = "", collapse = ", "),
  coef(regression_model_1)[1],
  ifelse(summary(regression_model_1)$coefficients[2, 4] < 0.05, "Significant", "Not Significant")
)

# Column 2 regression
regression_model_2 <- lm(gross_debt_issued ~ Financing_deficit, data = table2_divided_no_gap)
regression_results[2, ] <- c(
  "gross debt issued (2) no gap", 
  length(regression_model_2$residuals), 
  summary(regression_model_2)$r.squared, 
  paste(names(coef(regression_model_2)), ": ", coef(regression_model_2), sep = "", collapse = ", "),
  coef(regression_model_2)[1],
  ifelse(summary(regression_model_2)$coefficients[2, 4] < 0.05, "Significant", "Not Significant")
)

# Column 3 regression
table2_divided_no_gap$change_long_term_debt_total <- c(NA, diff(table2_divided_no_gap$long_term_debt_total))
regression_model_3 <- lm(change_long_term_debt_total ~ Financing_deficit, data = table2_divided_no_gap)
regression_results[3, ] <- c(
  "Change in debt ratio (3) no gap", 
  length(regression_model_3$residuals), 
  summary(regression_model_3)$r.squared, 
  paste(names(coef(regression_model_3)), ": ", coef(regression_model_3), sep = "", collapse = ", "),
  coef(regression_model_3)[1],
  ifelse(summary(regression_model_3)$coefficients[2, 4] < 0.05, "Significant", "Not Significant")
)

# Regression for gap allowed data (original data)
# Column 4 regression
regression_model_4 <- lm(net_debt_issued ~ Financing_deficit, data = table2_divided)
regression_results[4, ] <- c(
  "net_debt_issued (4)", 
  length(regression_model_4$residuals), 
  summary(regression_model_4)$r.squared, 
  paste(names(coef(regression_model_4)), ": ", coef(regression_model_4), sep = "", collapse = ", "),
  coef(regression_model_4)[1],
  ifelse(summary(regression_model_4)$coefficients[2, 4] < 0.05, "Significant", "Not Significant")
)

# Column 5 regression
regression_model_5 <- lm(gross_debt_issued ~ Financing_deficit, data = table2_divided)
regression_results[5, ] <- c(
  "gross_debt_issued (5)", 
  length(regression_model_5$residuals), 
  summary(regression_model_5)$r.squared, 
  paste(names(coef(regression_model_5)), ": ", coef(regression_model_5), sep = "", collapse = ", "),
  coef(regression_model_5)[1],
  ifelse(summary(regression_model_5)$coefficients[2, 4] < 0.05, "Significant", "Not Significant")
)

# Column 6 regression
table2_divided$change_long_term_debt_total <- c(NA, diff(table2_divided$long_term_debt_total))
regression_model_6 <- lm(change_long_term_debt_total ~ Financing_deficit, data = table2_divided)
regression_results[6, ] <- c(
  "Change in debt ratio (6)", 
  length(regression_model_6$residuals), 
  summary(regression_model_6)$r.squared, 
  paste(names(coef(regression_model_6)), ": ", coef(regression_model_6), sep = "", collapse = ", "),
  coef(regression_model_6)[1],
  ifelse(summary(regression_model_6)$coefficients[2, 4] < 0.05, "Significant", "Not Significant")
)

# Write results to CSV
write.csv(regression_results, "regression_results.csv", row.names = FALSE)


