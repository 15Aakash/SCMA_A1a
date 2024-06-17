# Set the working directory and verify it
setwd('C:/Users/Aakash/Desktop/SCMA')
getwd()

# Function to install and load libraries
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Load required libraries
libraries <- c("dplyr", "readr", "readxl", "tidyr", "ggplot2", "BSDA", "glue")
lapply(libraries, install_and_load)

# Reading the file into R
data <- read.csv("C:/Users/Aakash/Desktop/SCMA/NSSO68.csv")
# Display the first few rows of the data
head(data)

# Filtering for Meghalaya data
df <- data %>%
  filter(state_1 == "MEG")

# Display dataset info
cat("Dataset Information:\n")
print(names(df))
print(head(df))
print(dim(df))

# Finding missing values
missing_info <- colSums(is.na(df))
cat("Missing Values Information:\n")
print(missing_info)

# Sub-setting the data
megData <- df %>%
  select(state_1, District, Region, Sector, State_Region, Meals_At_Home, ricepds_v, Wheatpds_q, chicken_q, pulsep_q, wheatos_q, No_of_Meals_per_day)

# Check for missing values in the subset
cat("Missing Values in Subset:\n")
print(colSums(is.na(megData)))

# (1) HANDLING MISSING VALUES
# Impute missing values with mean for specific columns
impute_with_mean <- function(column) {
  if (any(is.na(column))) {
    column[is.na(column)] <- mean(column, na.rm = TRUE)
  }
  return(column)
}
megData$Meals_At_Home <- impute_with_mean(megData$Meals_At_Home)
megData$No_of_Meals_per_day <- impute_with_mean(megData$No_of_Meals_per_day)

# Check for missing values after imputation
cat("Missing Values After Imputation:\n")
print(colSums(is.na(megData)))

# (2)CHECK FOR OUTLIERS
remove_outliers <- function(df, column_name) {
  Q1 <- quantile(df[[column_name]], 0.25)
  Q3 <- quantile(df[[column_name]], 0.75)
  IQR <- Q3 - Q1
  lower_threshold <- Q1 - (1.5 * IQR)
  upper_threshold <- Q3 + (1.5 * IQR)
  df <- subset(df, df[[column_name]] >= lower_threshold & df[[column_name]] <= upper_threshold)
  return(df)
}

outlier_columns <- c("ricepds_v", "chicken_q")
for (col in outlier_columns) {
  megData <- remove_outliers(megData, col)
}

# (4)RENAME DISTRICTS AND SECTORS USING CODES FROM APPENDIX OF NSSA 68TH ROUND DATA
district_mapping <- c("6" = "East Khasi Hills" , "7" = "Jaintia Hills" ,"4" = "West Khasi Hills" , "1" = "West Garo Hills" , "5" = "Ri Bhoi" , "2" = "East Garo Hills", "3" = "South Garo Hills")
sector_mapping <- c("2" = "URBAN", "1" = "RURAL")

megData$District <- as.character(megData$District)
megData$Sector <- as.character(megData$Sector)
megData$District <- ifelse(megData$District %in% names(district_mapping), district_mapping[megData$District], megData$District)
megData$Sector <- ifelse(megData$Sector %in% names(sector_mapping), sector_mapping[megData$Sector], megData$Sector)


# (5)SUMMARIZING VARIABLES
megData$total_consumption <- rowSums(megData[, c("ricepds_v", "Wheatpds_q", "chicken_q", "pulsep_q", "wheatos_q")], na.rm = TRUE)

# Summarize and display top and bottom consuming districts and regions
summarize_consumption <- function(group_col) {
  summary <- megData %>%
    group_by(across(all_of(group_col))) %>%
    summarise(total = sum(total_consumption)) %>%
    arrange(desc(total))
  return(summary)
}

district_summary <- summarize_consumption("District")
region_summary <- summarize_consumption("Sector")

# (6) DISPLAYING TOP AND BOTTOM 3 DISTRICTS OF CONSUMPTION
cat("Top 3 Consuming Districts:\n")
print(head(district_summary, 3))
cat("Bottom 3 Consuming Districts:\n")
print(tail(district_summary, 3))

cat("Region Consumption Summary:\n")
print(region_summary)

# (7) TEST FOR DIFFERENCES IN MEAN CONSUMPTION AMONG RURAL AND URBAN
rural <- megData %>%
  filter(Sector == "RURAL") %>%
  select(total_consumption)

urban <- megData %>%
  filter(Sector == "URBAN") %>%
  select(total_consumption)


mean_rural <- mean(rural$total_consumption)
mean_urban <- mean(urban$total_consumption)

# Z-TEST :
z_test_result <- BSDA::z.test(rural$total_consumption, urban$total_consumption, 
                              alternative = "two.sided", mu = 0, sigma.x = sd_rural, sigma.y = sd_urban, conf.level = 0.95)

cat("Z STATISTIC:\n")
print(z_test_result$statistic)
print(z_test_result$method)

cat(glue::glue("P value is :{z_test_result$p.value}"))

# (8) OUTPUT BASED ON P VALUE OBTAINED
if (z_test_result$p.value < 0.05) {
  cat(glue::glue("P value is < 0.05 :Therefore we reject the null hypothesis.\n"))
  cat(glue::glue("Which means there is a significant difference between mean consumptions of urban and rural.\n"))
  cat(glue::glue("The mean consumption in Rural areas is {mean_rural} and in Urban areas its {mean_urban}\n"))
} else {
  cat(glue::glue("P value is >= 0.05:Therefore we fail to reject the null hypothesis.\n"))
  cat(glue::glue("There is no significant difference between mean consumptions of urban and rural.\n"))
  cat(glue::glue("The mean consumption in Rural area is {mean_rural} and in Urban area its {mean_urban}\n"))
}
