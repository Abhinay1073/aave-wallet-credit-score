# score_wallets.R

# Load libraries
library(jsonlite)
library(dplyr)
library(ggplot2)
library(scales)

# STEP 1: Load JSON file
json_file <- "C:/Users/Abhinay/Downloads/user-wallet-transactions.json"

# Read JSON (flatten nested structure)
df <- fromJSON(json_file, flatten = TRUE)

# STEP 2: Select & rename relevant columns
df_clean <- df %>%
  select(
    walletAddress = userWallet,
    type          = `actionData.type`,
    amount        = `actionData.amount`,
    timestamp     = timestamp
  )

# STEP 3: Convert data types
df_clean$timestamp <- as.POSIXct(df_clean$timestamp, origin = "1970-01-01", tz = "UTC")
df_clean$amount <- as.numeric(as.character(df_clean$amount))

# STEP 4: Feature engineering
features <- df_clean %>%
  group_by(walletAddress) %>%
  summarise(
    total_tx          = n(),
    deposits          = sum(type == "deposit"),
    borrows           = sum(type == "borrow"),
    repays            = sum(type == "repay"),
    redeems           = sum(type == "redeemunderlying"),
    liquidations      = sum(type == "liquidationcall"),
    avg_amt           = mean(amount, na.rm = TRUE),
    repay_ratio       = ifelse(borrows > 0, repays / borrows, 0),
    liquidation_ratio = ifelse(total_tx > 0, liquidations / total_tx, 0),
    .groups = "drop"
  )

# STEP 5: Improved Credit Scoring Logic
df_features <- features %>%
  mutate(
    score_raw = (
      (deposits * 8 + repays * 10 + redeems * 4) -
        (borrows * 6 + liquidations * 15)
    ) +
      (repays * repay_ratio * 5) -
      (liquidations * liquidation_ratio * 10) +
      (log1p(avg_amt) * 2),
    
    score_norm = pmin(pmax(score_raw, 0), 1000),
    credit_score = round(rescale(score_norm, to = c(0, 1000)))
  )

# STEP 6: Save results
output_dir <- "C:/Users/Abhinay/Desktop/Aave"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

write.csv(
  df_features[, c("walletAddress", "credit_score")],
  file.path(output_dir, "wallet_credit_scores.csv"),
  row.names = FALSE
)

# STEP 7: Improved Plot for Credit Score Distribution
df_features$score_bucket <- cut(
  df_features$credit_score,
  breaks = seq(0, 1000, by = 100),
  include.lowest = TRUE,
  right = FALSE,
  labels = paste(seq(0, 900, by = 100), seq(99, 999, by = 100), sep = "-")
)

score_summary <- df_features %>%
  group_by(score_bucket) %>%
  summarise(wallet_count = n(), .groups = "drop")

# Save improved chart
png(file.path(output_dir, "credit_score_distribution_improved.png"), width = 900, height = 600)

ggplot(score_summary, aes(x = score_bucket, y = wallet_count)) +
  geom_col(fill = "#1F77B4", width = 0.7) +
  geom_text(aes(label = wallet_count), vjust = -0.3, color = "black", size = 4) +
  labs(
    title = "Credit Score Distribution (Grouped)",
    x = "Credit Score Range",
    y = "Number of Wallets"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

dev.off()

# STEP 8: Optional diagnostic check
print(summary(df_features$credit_score))
print(table(df_features$credit_score))
