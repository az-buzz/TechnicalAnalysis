# AI Assistance Declaration: I used Gemini for ideation, formula drafting, and layout suggestions. Prompts used: See reflection text file. I verified outputs using self spot-checks and RStudio summary functions. All final calculations are done by myself. I am responsible for the accuracy and originality of this work.

# Load required library
library(dplyr)
library(stringr)

# --- 0. Generate Synthetic 'Messy' Data (Since we don't have the original file) ---
# This ensures your script runs perfectly without needing an external CSV.
set.seed(42)
sales_data_dirty <- data.frame(
  CustomerID = c(101, 102, NA, 104, 105, 101, 107, NA, 109, 110),
  Region = c("NORTH", "south ", " North", "East", "WEST", "NORTH", "east", "south", "West", "UNKNOWN"),
  Revenue = c(250, 300, 150, 999999, 400, 250, 350, -50, 500, 450), # 999999 and -50 are outliers
  stringsAsFactors = FALSE
)

# --- 1. Explore and Describe Dataset Cleanliness Issues ---
cat("--- BEFORE CLEANING: RAW DATA SUMMARY ---\n")
glimpse(sales_data_dirty)
summary(sales_data_dirty)

# Check for missing values specifically
cat("\nMissing Customer IDs:", sum(is.na(sales_data_dirty$CustomerID)), "\n")

# --- 2. Implement AI Suggestions and Debug (Cleaning Pipeline) ---
sales_data_clean <- sales_data_dirty %>%
  # Fix Issue 1: Remove missing Customer IDs
  filter(!is.na(CustomerID)) %>%
  # Fix Issue 2: Remove duplicated rows
  distinct() %>%
  # Fix Issue 3: Standardize inconsistent Region formatting (trim whitespace, title case)
  mutate(Region = str_to_title(trimws(Region))) %>%
  # Fix Issue 4: Filter out extreme outliers in Revenue (e.g., negative values or abnormally high)
  filter(Revenue > 0 & Revenue < 10000)

# --- 3. Verification & Evaluation ---
cat("\n--- AFTER CLEANING: CLEAN DATA SUMMARY ---\n")
glimpse(sales_data_clean)
summary(sales_data_clean)
cat("\nMissing Customer IDs Remaining:", sum(is.na(sales_data_clean$CustomerID)), "\n")
