##############################################################
# Quantium Chips Analysis
# Author: Geraldine Nyika
# Purpose: Clean, explore, and analyze chip purchasing data
# Output: Cleaned datasets, summary tables, and visualizations
##############################################################

# ================================
# 1. Load Required Libraries
# ================================
library(tidyverse)
library(readxl)
library(janitor)
library(ggplot2)

# ================================
# 2. Import Raw Data
# ================================
transactions <- read_excel("Data/QVI_transaction_data.xlsx") %>% 
  clean_names()

customers <- read_csv("Data/QVI_purchase_behaviour.csv") %>% 
  clean_names()

# ================================
# 3. Clean Transaction Data
# ================================

# Convert Excel date numeric → real date
transactions <- transactions %>%
  mutate(date = as.Date(date, origin = "1899-12-30"))

# Remove extra spaces in product names
transactions <- transactions %>%
  mutate(prod_name = str_squish(prod_name))

# Extract pack size from product name
transactions <- transactions %>%
  mutate(pack_size = as.numeric(str_extract(prod_name, "\\d+")))

# Extract brand (first word)
transactions <- transactions %>%
  mutate(brand = word(prod_name, 1))

# Standardize brand names
transactions <- transactions %>%
  mutate(brand_clean = case_when(
    brand %in% c("Smiths", "Smith") ~ "Smith's",
    brand %in% c("Doritos", "Dorito") ~ "Doritos",
    brand == "Kettle" ~ "Kettle",
    brand == "Pringles" ~ "Pringles",
    brand == "Thins" ~ "Thins",
    brand == "Cobs" ~ "Cobs",
    brand == "Twisties" ~ "Twisties",
    brand == "Cheezels" ~ "Cheezels",
    brand == "Cheetos" ~ "Cheetos",
    brand == "CCs" ~ "CC's",
    
    brand %in% c("Natural", "NCC") ~ "Natural Chip Co",
    brand %in% c("Grain", "GrnWves") ~ "Grain Waves",
    brand %in% c("Sunbites", "Snbts") ~ "Sunbites",
    
    brand %in% c("Red", "RRD") ~ "Red Rock Deli",
    brand == "Old" ~ "Old El Paso",
    brand %in% c("WW", "Woolworths") ~ "Woolworths",
    brand == "French" ~ "French Fries",
    brand == "Tostitos" ~ "Tostitos",
    
    TRUE ~ brand
  ))

# ================================
# 4. Filter Out Problematic Data
# ================================
transactions <- transactions %>%
  filter(pack_size > 20 & pack_size < 500) %>%   # realistic chip sizes
  filter(prod_qty > 0 & prod_qty < 10)           # remove bulk errors

# ================================
# 5. Merge with Customer Data
# ================================
merged <- transactions %>%
  inner_join(customers, by = "lylty_card_nbr")

# Add unit price feature
merged <- merged %>%
  mutate(unit_price = tot_sales / prod_qty)

# ================================
# 6. Summaries & Segment Analysis
# ================================

# 6.1 Sales by Lifestage
sales_lifestage <- merged %>%
  group_by(lifestage) %>%
  summarise(total_sales = sum(tot_sales), .groups = "drop") %>%
  arrange(desc(total_sales))

# 6.2 Sales by Premium Customer Type
sales_premium <- merged %>%
  group_by(premium_customer) %>%
  summarise(total_sales = sum(tot_sales), .groups = "drop") %>%
  arrange(desc(total_sales))

# 6.3 Cross-Segment Summary
segment_summary <- merged %>%
  group_by(lifestage, premium_customer) %>%
  summarise(
    total_sales = sum(tot_sales),
    avg_price = mean(unit_price),
    qty = sum(prod_qty),
    transactions = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(total_sales))

# ================================
# 7. Pack Size Analysis
# ================================
packsize_summary <- merged %>%
  group_by(pack_size) %>%
  summarise(
    total_sales = sum(tot_sales),
    avg_price = mean(unit_price),
    qty = sum(prod_qty),
    transactions = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(total_sales))

# ================================
# 8. Brand Performance
# ================================
brand_summary <- merged %>%
  group_by(brand_clean) %>%
  summarise(
    total_sales = sum(tot_sales),
    avg_price = mean(unit_price),
    qty = sum(prod_qty),
    transactions = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(total_sales))

# ================================
# 9. Export Summary Tables
# ================================
write_csv(sales_lifestage,  "outputs/summary_tables/sales_lifestage.csv")
write_csv(sales_premium,    "outputs/summary_tables/sales_premium.csv")
write_csv(segment_summary,  "outputs/summary_tables/segment_summary.csv")
write_csv(packsize_summary, "outputs/summary_tables/packsize_summary.csv")
write_csv(brand_summary,    "outputs/summary_tables/brand_summary.csv")

# ================================
# 10. Export Cleaned Data
# ================================
write_csv(transactions, "cleaned_data/transactions_cleaned.csv")
write_csv(customers,    "cleaned_data/customers_cleaned.csv")
write_csv(merged,       "cleaned_data/merged_cleaned.csv")

##############################################################
# End of Analysis Script
##############################################################

