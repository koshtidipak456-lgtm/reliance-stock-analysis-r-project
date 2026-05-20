# ============================================================================
#        RELIANCE INDUSTRIES LIMITED - COMPLETE R PROJECT
#        Company: Reliance Industries (RELIANCE.NS)
#        All Parts Combined in One Script
# ============================================================================
# CONTENTS:
#   PART 01A : Financial Data Acquisition & Handling (CSV, Excel, API, Cleaning)
#   PART 01B : Data Visualisation (Line, Bar, Financial Charts using ggplot2)
#   PART 01C : Basic Time Series Analysis (ARIMA, Trends, Decomposition)
#   PART 02  : Algorithmic Trading
# ============================================================================
# ===========================================================================
#                     INSTALL & LOAD ALL PACKAGES
# ===========================================================================
packages <- c(
  "quantmod", "tidyverse", "ggplot2", "readxl", "writexl",
  "zoo", "lubridate", "scales", "gridExtra", "reshape2",
  "forecast", "tseries", "urca",
  "TTR", "PerformanceAnalytics", "janitor"
)
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}
cat("\n=== All packages loaded successfully ===\n\n")
# ===========================================================================
#  PART 01A: FINANCIAL DATA ACQUISITION & HANDLING
# ===========================================================================
cat("############################################################\n")
cat("#  PART 01A: FINANCIAL DATA ACQUISITION & HANDLING         #\n")
cat("############################################################\n\n")
# --- 1. Download Data from Yahoo Finance API ---
cat("--- Downloading Reliance Industries data from Yahoo Finance API ---\n")
symbol     <- "RELIANCE.NS"
start_date <- Sys.Date() - (365 * 10)
end_date   <- Sys.Date()
getSymbols(symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = TRUE)
# Convert xts to data frame
reliance_xts <- get(symbol)
reliance_df  <- data.frame(Date = index(reliance_xts), coredata(reliance_xts))
# Clean column names (remove "RELIANCE.NS." prefix)
colnames(reliance_df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
cat("Data downloaded successfully!\n")
cat("Date Range :", as.character(min(reliance_df$Date)), "to", as.character(max(reliance_df$Date)), "\n")
cat("Total Rows :", nrow(reliance_df), "\n")
cat("Total Cols :", ncol(reliance_df), "\n\n")
# --- 2. View Data Structure ---
cat("--- Data Structure ---\n")
str(reliance_df)
cat("\n--- First 6 Rows ---\n")
print(head(reliance_df))
cat("\n--- Last 6 Rows ---\n")
print(tail(reliance_df))
cat("\n--- Summary Statistics ---\n")
print(summary(reliance_df))
# --- 3. Save Data to CSV ---
csv_path <- "Reliance_Stock_Data.csv"
write.csv(reliance_df, file = csv_path, row.names = FALSE)
cat("\nData saved to CSV:", csv_path, "\n")
# --- 4. Save Data to Excel ---
excel_path <- "Reliance_Stock_Data.xlsx"
write_xlsx(reliance_df, path = excel_path)
cat("Data saved to Excel:", excel_path, "\n")
# --- 5. Read Data Back from CSV ---
cat("\n--- Reading Data from CSV ---\n")
reliance_csv <- read.csv(csv_path, stringsAsFactors = FALSE)
reliance_csv$Date <- as.Date(reliance_csv$Date)
cat("CSV rows:", nrow(reliance_csv), "| Cols:", ncol(reliance_csv), "\n")
print(head(reliance_csv, 3))
# --- 6. Read Data Back from Excel ---
cat("\n--- Reading Data from Excel ---\n")
reliance_excel <- read_excel(excel_path)
reliance_excel$Date <- as.Date(reliance_excel$Date)
cat("Excel rows:", nrow(reliance_excel), "| Cols:", ncol(reliance_excel), "\n")
print(head(reliance_excel, 3))
# --- 7. Data Cleaning ---
cat("\n\n========== DATA CLEANING ==========\n")
# 7a. Check for missing values
cat("\n--- Missing Values per Column ---\n")
missing_counts <- colSums(is.na(reliance_df))
print(missing_counts)
cat("Total missing values:", sum(missing_counts), "\n")
# 7b. Handle missing values using forward fill (na.locf)
reliance_clean <- reliance_df
reliance_clean$Open     <- na.locf(reliance_clean$Open, na.rm = FALSE)
reliance_clean$High     <- na.locf(reliance_clean$High, na.rm = FALSE)
reliance_clean$Low      <- na.locf(reliance_clean$Low, na.rm = FALSE)
reliance_clean$Close    <- na.locf(reliance_clean$Close, na.rm = FALSE)
reliance_clean$Volume   <- na.locf(reliance_clean$Volume, na.rm = FALSE)
reliance_clean$Adjusted <- na.locf(reliance_clean$Adjusted, na.rm = FALSE)
# Remove any remaining NAs
reliance_clean <- na.omit(reliance_clean)
cat("\nAfter cleaning - Missing values:", sum(is.na(reliance_clean)), "\n")
cat("After cleaning - Total rows:", nrow(reliance_clean), "\n")
# 7c. Check for duplicate dates
cat("\n--- Duplicate Dates Check ---\n")
dup_count <- sum(duplicated(reliance_clean$Date))
cat("Duplicate dates found:", dup_count, "\n")
if (dup_count > 0) {
  reliance_clean <- reliance_clean[!duplicated(reliance_clean$Date), ]
  cat("Duplicates removed. Rows remaining:", nrow(reliance_clean), "\n")
}
# 7d. Check data types
cat("\n--- Data Types ---\n")
print(sapply(reliance_clean, class))
# 7e. Check for negative prices (anomalies)
cat("\n--- Anomaly Check (Negative Prices) ---\n")
cat("Negative Open prices:", sum(reliance_clean$Open < 0, na.rm = TRUE), "\n")
cat("Negative Close prices:", sum(reliance_clean$Close < 0, na.rm = TRUE), "\n")
cat("Negative Volume:", sum(reliance_clean$Volume < 0, na.rm = TRUE), "\n")
# 7f. Sort by date
reliance_clean <- reliance_clean %>% arrange(Date)
# 7g. Add derived columns
reliance_clean <- reliance_clean %>%
  mutate(
    Daily_Return     = (Close - lag(Close)) / lag(Close) * 100,
    Daily_Return_Pct = Daily_Return,
    Log_Return       = log(Close / lag(Close)) * 100,
    Price_Range      = High - Low,
    Year             = year(Date),
    Month            = month(Date, label = TRUE),
    Day_of_Week      = wday(Date, label = TRUE),
    Quarter          = quarter(Date)
  )
cat("\n--- Derived Columns Added ---\n")
cat("New columns: Daily_Return, Log_Return, Price_Range, Year, Month, Day_of_Week, Quarter\n")
print(head(reliance_clean))
# 7h. Save cleaned data
write.csv(reliance_clean, file = "Reliance_Clean_Data.csv", row.names = FALSE)
write_xlsx(reliance_clean, path = "Reliance_Clean_Data.xlsx")
cat("\nCleaned data saved to CSV and Excel.\n")
cat("\n========== PART 01A COMPLETE ==========\n\n")
# ===========================================================================
#  PART 01B: DATA VISUALISATION (ggplot2)
# ===========================================================================
cat("############################################################\n")
cat("#  PART 01B: DATA VISUALISATION (ggplot2)                  #\n")
cat("############################################################\n\n")
# Use cleaned data going forward
reliance <- reliance_clean
# Custom theme for all plots
theme_reliance <- theme_minimal() +
  theme(
    plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey40"),
    axis.title    = element_text(size = 11),
    axis.text     = element_text(size = 9),
    legend.position = "bottom"
  )
# ---- CHART 1: Closing Price Line Chart ----
cat("--- Chart 1: Closing Price Line Chart ---\n")
p1 <- ggplot(reliance, aes(x = Date, y = Close)) +
  geom_line(color = "#1E88E5", linewidth = 0.5) +
  labs(
    title    = "Reliance Industries - Closing Price Over Time",
    subtitle = paste(min(reliance$Date), "to", max(reliance$Date)),
    x = "Date", y = "Closing Price (INR)"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") +
  scale_y_continuous(labels = comma) +
  theme_reliance +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p1)
ggsave("Chart01_Closing_Price_Line.png", p1, width = 12, height = 6, dpi = 300)
# ---- CHART 2: OHLC Line Chart ----
cat("--- Chart 2: OHLC Line Chart ---\n")
ohlc_long <- reliance %>%
  select(Date, Open, High, Low, Close) %>%
  pivot_longer(cols = c(Open, High, Low, Close), names_to = "Price_Type", values_to = "Price")
p2 <- ggplot(ohlc_long, aes(x = Date, y = Price, color = Price_Type)) +
  geom_line(linewidth = 0.4, alpha = 0.7) +
  scale_color_manual(values = c("Open" = "#43A047", "High" = "#E53935",
                                "Low" = "#1E88E5", "Close" = "#FF8F00")) +
  labs(
    title = "Reliance Industries - OHLC Prices",
    x = "Date", y = "Price (INR)", color = "Price Type"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") +
  scale_y_continuous(labels = comma) +
  theme_reliance +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p2)
ggsave("Chart02_OHLC_Lines.png", p2, width = 12, height = 6, dpi = 300)
# ---- CHART 3: Monthly Volume Bar Plot ----
cat("--- Chart 3: Monthly Volume Bar Plot ---\n")
monthly_vol <- reliance %>%
  mutate(YearMonth = floor_date(Date, "month")) %>%
  group_by(YearMonth) %>%
  summarise(Avg_Volume = mean(Volume, na.rm = TRUE), .groups = "drop")
p3 <- ggplot(monthly_vol, aes(x = YearMonth, y = Avg_Volume)) +
  geom_bar(stat = "identity", fill = "#26A69A", alpha = 0.8) +
  labs(
    title = "Reliance Industries - Average Monthly Trading Volume",
    x = "Month", y = "Average Volume"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") +
  scale_y_continuous(labels = comma) +
  theme_reliance +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p3)
ggsave("Chart03_Monthly_Volume_Bar.png", p3, width = 12, height = 6, dpi = 300)
# ---- CHART 4: Yearly Average Closing Price ----
cat("--- Chart 4: Yearly Average Closing Price ---\n")
yearly_avg <- reliance %>%
  group_by(Year) %>%
  summarise(Avg_Close = mean(Close, na.rm = TRUE), .groups = "drop")
p4 <- ggplot(yearly_avg, aes(x = factor(Year), y = Avg_Close)) +
  geom_bar(stat = "identity", fill = "#7E57C2", alpha = 0.85) +
  geom_text(aes(label = round(Avg_Close, 0)), vjust = -0.5, size = 3.5) +
  labs(
    title = "Reliance Industries - Yearly Average Closing Price",
    x = "Year", y = "Average Closing Price (INR)"
  ) +
  scale_y_continuous(labels = comma) +
  theme_reliance
print(p4)
ggsave("Chart04_Yearly_Avg_Close_Bar.png", p4, width = 10, height = 6, dpi = 300)
# ---- CHART 5: Daily Returns ----
cat("--- Chart 5: Daily Returns Line Chart ---\n")
p5 <- ggplot(reliance, aes(x = Date, y = Daily_Return)) +
  geom_line(color = "#EF5350", linewidth = 0.3, alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "Reliance Industries - Daily Returns (%)",
    x = "Date", y = "Daily Return (%)"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") +
  theme_reliance +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p5)
ggsave("Chart05_Daily_Returns.png", p5, width = 12, height = 6, dpi = 300)
# ---- CHART 6: Returns Distribution Histogram ----
cat("--- Chart 6: Returns Distribution Histogram ---\n")
p6 <- ggplot(reliance %>% filter(!is.na(Daily_Return)), aes(x = Daily_Return)) +
  geom_histogram(bins = 80, fill = "#42A5F5", color = "white", alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8) +
  labs(
    title = "Reliance Industries - Distribution of Daily Returns",
    x = "Daily Return (%)", y = "Frequency"
  ) +
  theme_reliance
print(p6)
ggsave("Chart06_Returns_Histogram.png", p6, width = 10, height = 6, dpi = 300)
# ---- CHART 7: Moving Averages (50 & 200 Day) ----
cat("--- Chart 7: Moving Averages ---\n")
reliance <- reliance %>%
  arrange(Date) %>%
  mutate(
    MA_50  = rollmean(Close, k = 50, fill = NA, align = "right"),
    MA_200 = rollmean(Close, k = 200, fill = NA, align = "right")
  )
p7 <- ggplot(reliance, aes(x = Date)) +
  geom_line(aes(y = Close, color = "Close"), linewidth = 0.4) +
  geom_line(aes(y = MA_50, color = "50-Day MA"), linewidth = 0.7) +
  geom_line(aes(y = MA_200, color = "200-Day MA"), linewidth = 0.7) +
  scale_color_manual(values = c("Close" = "grey60", "50-Day MA" = "#FF7043", "200-Day MA" = "#26A69A")) +
  labs(
    title = "Reliance Industries - 50-Day & 200-Day Moving Averages",
    x = "Date", y = "Price (INR)", color = "Legend"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") +
  scale_y_continuous(labels = comma) +
  theme_reliance +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p7)
ggsave("Chart07_Moving_Averages.png", p7, width = 12, height = 6, dpi = 300)
# ---- CHART 8: Candlestick Chart (Last 90 Days) ----
cat("--- Chart 8: Candlestick Chart ---\n")
recent <- reliance %>% tail(90) %>%
  mutate(Direction = ifelse(Close >= Open, "Up", "Down"))
p8 <- ggplot(recent) +
  geom_segment(aes(x = Date, xend = Date, y = Low, yend = High, color = Direction), linewidth = 0.4) +
  geom_rect(aes(xmin = Date - 0.3, xmax = Date + 0.3,
                ymin = pmin(Open, Close), ymax = pmax(Open, Close),
                fill = Direction), color = NA) +
  scale_fill_manual(values = c("Up" = "#4CAF50", "Down" = "#F44336")) +
  scale_color_manual(values = c("Up" = "#4CAF50", "Down" = "#F44336")) +
  labs(
    title = "Reliance Industries - Candlestick Chart (Last 90 Trading Days)",
    x = "Date", y = "Price (INR)"
  ) +
  scale_y_continuous(labels = comma) +
  theme_reliance
print(p8)
ggsave("Chart08_Candlestick.png", p8, width = 12, height = 6, dpi = 300)
# ---- CHART 9: Monthly Returns Box Plot ----
cat("--- Chart 9: Monthly Returns Box Plot ---\n")
p9 <- ggplot(reliance %>% filter(!is.na(Daily_Return)), aes(x = Month, y = Daily_Return, fill = Month)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Reliance Industries - Monthly Distribution of Daily Returns",
    x = "Month", y = "Daily Return (%)"
  ) +
  theme_reliance +
  theme(legend.position = "none")
print(p9)
ggsave("Chart09_Monthly_Boxplot.png", p9, width = 10, height = 6, dpi = 300)
# ---- CHART 10: Price and Volume Combined ----
cat("--- Chart 10: Price and Volume Combined ---\n")
scale_factor <- max(reliance$Close, na.rm = TRUE) / max(reliance$Volume, na.rm = TRUE)
p10 <- ggplot(reliance, aes(x = Date)) +
  geom_bar(aes(y = Volume * scale_factor), stat = "identity", fill = "#B0BEC5", alpha = 0.5) +
  geom_line(aes(y = Close), color = "#1565C0", linewidth = 0.5) +
  scale_y_continuous(
    name = "Closing Price (INR)", labels = comma,
    sec.axis = sec_axis(~ . / scale_factor, name = "Volume", labels = comma)
  ) +
  labs(title = "Reliance Industries - Price vs Volume", x = "Date") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") +
  theme_reliance +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p10)
ggsave("Chart10_Price_Volume.png", p10, width = 12, height = 6, dpi = 300)
cat("\n========== PART 01B COMPLETE: 10 Charts Saved ==========\n\n")
# ===========================================================================
#  PART 01C: BASIC TIME SERIES ANALYSIS
# ===========================================================================
cat("############################################################\n")
cat("#  PART 01C: BASIC TIME SERIES ANALYSIS                    #\n")
cat("############################################################\n\n")
# --- Monthly Average Data ---
monthly_data <- reliance %>%
  mutate(YearMonth = floor_date(Date, "month")) %>%
  group_by(YearMonth) %>%
  summarise(
    Avg_Close  = mean(Close, na.rm = TRUE),
    Avg_Volume = mean(Volume, na.rm = TRUE),
    .groups    = "drop"
  )
# ---- TREND ANALYSIS ----
cat("========== TREND ANALYSIS ==========\n\n")
monthly_data$Time_Index <- 1:nrow(monthly_data)
trend_model <- lm(Avg_Close ~ Time_Index, data = monthly_data)
cat("--- Linear Trend Model ---\n")
print(summary(trend_model))
cat("\nTrend Slope (per month):", coef(trend_model)[2], "INR\n")
cat("R-squared:", summary(trend_model)$r.squared, "\n\n")
p_trend <- ggplot(monthly_data, aes(x = YearMonth, y = Avg_Close)) +
  geom_line(color = "#1E88E5", linewidth = 0.6) +
  geom_smooth(method = "lm", color = "#E53935", linetype = "dashed", se = TRUE, alpha = 0.2) +
  labs(
    title = "Reliance Industries - Long-term Price Trend",
    subtitle = "Monthly Average Closing Price with Linear Trend",
    x = "Date", y = "Average Close (INR)"
  ) +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") +
  theme_reliance +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p_trend)
ggsave("TS_Chart01_Trend_Analysis.png", p_trend, width = 12, height = 6, dpi = 300)
# ---- SEASONAL DECOMPOSITION ----
cat("========== SEASONAL DECOMPOSITION ==========\n\n")
close_ts <- ts(monthly_data$Avg_Close,
               start = c(year(min(monthly_data$YearMonth)), month(min(monthly_data$YearMonth))),
               frequency = 12)
# Additive Decomposition
decomp_add <- decompose(close_ts, type = "additive")
png("TS_Chart02_Decomposition_Additive.png", width = 1200, height = 800, res = 150)
plot(decomp_add, col = "#1565C0")
title(main = "Reliance - Additive Seasonal Decomposition", col.main = "black")
dev.off()
cat("Additive decomposition saved.\n")
# Multiplicative Decomposition
decomp_mult <- decompose(close_ts, type = "multiplicative")
png("TS_Chart03_Decomposition_Multiplicative.png", width = 1200, height = 800, res = 150)
plot(decomp_mult, col = "#C62828")
title(main = "Reliance - Multiplicative Seasonal Decomposition", col.main = "black")
dev.off()
cat("Multiplicative decomposition saved.\n")
# STL Decomposition
stl_decomp <- stl(close_ts, s.window = "periodic")
png("TS_Chart04_STL_Decomposition.png", width = 1200, height = 800, res = 150)
plot(stl_decomp, col = "#2E7D32")
title(main = "Reliance - STL Decomposition", col.main = "black")
dev.off()
cat("STL decomposition saved.\n")
# Seasonal Component Bar Plot
seasonal_vals <- data.frame(
  Month    = factor(month.abb, levels = month.abb),
  Seasonal = as.numeric(decomp_add$figure)
)
p_seasonal <- ggplot(seasonal_vals, aes(x = Month, y = Seasonal, fill = Seasonal > 0)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  scale_fill_manual(values = c("TRUE" = "#4CAF50", "FALSE" = "#F44336")) +
  labs(title = "Reliance - Seasonal Component (Monthly)", x = "Month", y = "Seasonal Effect") +
  theme_reliance + theme(legend.position = "none")
print(p_seasonal)
ggsave("TS_Chart05_Seasonal_Component.png", p_seasonal, width = 10, height = 6, dpi = 300)
# ---- STATIONARITY TESTING ----
cat("\n========== STATIONARITY TESTING ==========\n\n")
cat("--- ADF Test: Raw Closing Price ---\n")
adf_raw <- adf.test(na.omit(close_ts))
print(adf_raw)
cat("Result:", ifelse(adf_raw$p.value < 0.05, "STATIONARY", "NON-STATIONARY"), "\n\n")
diff_close <- diff(close_ts)
cat("--- ADF Test: First Differenced ---\n")
adf_diff <- adf.test(na.omit(diff_close))
print(adf_diff)
cat("Result:", ifelse(adf_diff$p.value < 0.05, "STATIONARY", "NON-STATIONARY"), "\n\n")
png("TS_Chart06_Stationarity.png", width = 1200, height = 800, res = 150)
par(mfrow = c(2, 1))
plot(close_ts, main = "Original Monthly Close Price", ylab = "Price (INR)", col = "#1565C0")
plot(diff_close, main = "First Differenced Close Price", ylab = "Differenced", col = "#C62828")
par(mfrow = c(1, 1))
dev.off()
# ---- ACF & PACF ----
cat("========== ACF & PACF ==========\n\n")
png("TS_Chart07_ACF_PACF.png", width = 1200, height = 800, res = 150)
par(mfrow = c(2, 2))
acf(na.omit(close_ts), main = "ACF - Original", lag.max = 36, col = "#1565C0")
pacf(na.omit(close_ts), main = "PACF - Original", lag.max = 36, col = "#1565C0")
acf(na.omit(diff_close), main = "ACF - Differenced", lag.max = 36, col = "#C62828")
pacf(na.omit(diff_close), main = "PACF - Differenced", lag.max = 36, col = "#C62828")
par(mfrow = c(1, 1))
dev.off()
cat("ACF/PACF plots saved.\n")
# ---- ARIMA MODELLING ----
cat("\n========== ARIMA MODELLING ==========\n\n")
arima_model <- auto.arima(close_ts, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
cat("ARIMA Model Summary:\n")
print(summary(arima_model))
png("TS_Chart08_ARIMA_Residuals.png", width = 1200, height = 800, res = 150)
checkresiduals(arima_model)
dev.off()
cat("Residual diagnostics saved.\n")
cat("\n--- Ljung-Box Test ---\n")
lb_test <- Box.test(residuals(arima_model), type = "Ljung-Box", lag = 20)
print(lb_test)
cat("Result:", ifelse(lb_test$p.value > 0.05, "Residuals independent (GOOD)", "Autocorrelation present (BAD)"), "\n")
# ---- ARIMA FORECAST ----
cat("\n========== ARIMA FORECAST (12 Months) ==========\n\n")
forecast_result <- forecast(arima_model, h = 12)
print(forecast_result)
png("TS_Chart09_ARIMA_Forecast.png", width = 1200, height = 600, res = 150)
plot(forecast_result, main = "Reliance - ARIMA Forecast (12 Months)",
     xlab = "Time", ylab = "Price (INR)", col = "#1565C0", fcol = "#E53935",
     shadecols = c("#FFCDD2", "#EF9A9A"))
grid(col = "grey90")
dev.off()
fc_df <- data.frame(
  Date     = seq(max(monthly_data$YearMonth) + months(1), by = "month", length.out = 12),
  Forecast = as.numeric(forecast_result$mean),
  Lo80 = as.numeric(forecast_result$lower[, 1]),
  Hi80 = as.numeric(forecast_result$upper[, 1]),
  Lo95 = as.numeric(forecast_result$lower[, 2]),
  Hi95 = as.numeric(forecast_result$upper[, 2])
)
p_forecast <- ggplot() +
  geom_line(data = monthly_data, aes(x = YearMonth, y = Avg_Close), color = "#1565C0", linewidth = 0.5) +
  geom_ribbon(data = fc_df, aes(x = Date, ymin = Lo95, ymax = Hi95), fill = "#FFCDD2", alpha = 0.5) +
  geom_ribbon(data = fc_df, aes(x = Date, ymin = Lo80, ymax = Hi80), fill = "#EF9A9A", alpha = 0.5) +
  geom_line(data = fc_df, aes(x = Date, y = Forecast), color = "#E53935", linewidth = 0.8, linetype = "dashed") +
  geom_point(data = fc_df, aes(x = Date, y = Forecast), color = "#E53935", size = 2) +
  labs(
    title = "Reliance - ARIMA Price Forecast (Next 12 Months)",
    x = "Date", y = "Monthly Avg Close (INR)"
  ) +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") +
  theme_reliance +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p_forecast)
ggsave("TS_Chart10_ARIMA_Forecast_ggplot.png", p_forecast, width = 12, height = 6, dpi = 300)
cat("\n--- Model Accuracy ---\n")
print(accuracy(arima_model))
cat("\n========== PART 01C COMPLETE ==========\n\n")
# ===========================================================================
#  PART 02: ALGORITHMIC TRADING
# ===========================================================================
cat("############################################################\n")
cat("#  PART 02: ALGORITHMIC TRADING                            #\n")
cat("############################################################\n\n")
# ---- STRATEGY 1: SMA 50/200 CROSSOVER ----
cat("========== STRATEGY 1: SMA 50/200 CROSSOVER ==========\n\n")
reliance <- reliance %>%
  mutate(
    SMA_50  = SMA(Close, n = 50),
    SMA_200 = SMA(Close, n = 200)
  )
reliance <- reliance %>%
  mutate(
    SMA_Signal = case_when(
      SMA_50 > SMA_200 & lag(SMA_50) <= lag(SMA_200) ~  1,
      SMA_50 < SMA_200 & lag(SMA_50) >= lag(SMA_200) ~ -1,
      TRUE ~ 0
    ),
    SMA_Position = ifelse(SMA_50 > SMA_200, 1, 0)
  )
reliance <- reliance %>%
  mutate(
    Daily_Ret      = Close / lag(Close) - 1,
    SMA_Strategy   = lag(SMA_Position) * Daily_Ret,
    SMA_Cumulative = cumprod(1 + ifelse(is.na(SMA_Strategy), 0, SMA_Strategy)),
    BH_Cumulative  = cumprod(1 + ifelse(is.na(Daily_Ret), 0, Daily_Ret))
  )
cat("Buy Signals (Golden Cross):", sum(reliance$SMA_Signal == 1, na.rm = TRUE), "\n")
cat("Sell Signals (Death Cross):", sum(reliance$SMA_Signal == -1, na.rm = TRUE), "\n")
buy_pts  <- reliance %>% filter(SMA_Signal == 1)
sell_pts <- reliance %>% filter(SMA_Signal == -1)
p_sma <- ggplot(reliance %>% filter(!is.na(SMA_200)), aes(x = Date)) +
  geom_line(aes(y = Close, color = "Close"), linewidth = 0.4) +
  geom_line(aes(y = SMA_50, color = "SMA 50"), linewidth = 0.6) +
  geom_line(aes(y = SMA_200, color = "SMA 200"), linewidth = 0.6) +
  geom_point(data = buy_pts, aes(x = Date, y = Close), color = "green", shape = 24, size = 3, fill = "green") +
  geom_point(data = sell_pts, aes(x = Date, y = Close), color = "red", shape = 25, size = 3, fill = "red") +
  scale_color_manual(values = c("Close" = "grey60", "SMA 50" = "#FF7043", "SMA 200" = "#1E88E5")) +
  labs(
    title = "Strategy 1: SMA 50/200 Crossover - Buy & Sell Signals",
    subtitle = "Green = Buy (Golden Cross) | Red = Sell (Death Cross)",
    x = "Date", y = "Price (INR)", color = "Legend"
  ) +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") +
  theme_reliance +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p_sma)
ggsave("Algo_Chart01_SMA_Crossover.png", p_sma, width = 14, height = 7, dpi = 300)
p_sma_cum <- ggplot(reliance %>% filter(!is.na(SMA_200)), aes(x = Date)) +
  geom_line(aes(y = SMA_Cumulative, color = "SMA Strategy"), linewidth = 0.7) +
  geom_line(aes(y = BH_Cumulative, color = "Buy & Hold"), linewidth = 0.7) +
  scale_color_manual(values = c("SMA Strategy" = "#4CAF50", "Buy & Hold" = "#1565C0")) +
  labs(
    title = "Strategy 1: Cumulative Returns - SMA Crossover vs Buy & Hold",
    x = "Date", y = "Growth of Rs.1", color = "Strategy"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") +
  theme_reliance +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p_sma_cum)
ggsave("Algo_Chart02_SMA_Cumulative.png", p_sma_cum, width = 12, height = 6, dpi = 300)
# ---- STRATEGY 2: RSI ----
cat("\n========== STRATEGY 2: RSI (14) ==========\n\n")
reliance <- reliance %>% mutate(RSI_14 = RSI(Close, n = 14))
reliance <- reliance %>%
  mutate(
    RSI_Signal = case_when(
      RSI_14 < 30 & lag(RSI_14) >= 30 ~  1,
      RSI_14 > 70 & lag(RSI_14) <= 70 ~ -1,
      TRUE ~ 0
    ),
    RSI_Position = case_when(
      RSI_14 < 30  ~ 1,
      RSI_14 > 70  ~ 0,
      TRUE ~ NA_real_
    )
  )
reliance$RSI_Position <- na.locf(reliance$RSI_Position, na.rm = FALSE)
reliance$RSI_Position[is.na(reliance$RSI_Position)] <- 0
reliance <- reliance %>%
  mutate(
    RSI_Strategy   = lag(RSI_Position) * Daily_Ret,
    RSI_Cumulative = cumprod(1 + ifelse(is.na(RSI_Strategy), 0, RSI_Strategy))
  )
cat("RSI Buy Signals:", sum(reliance$RSI_Signal == 1, na.rm = TRUE), "\n")
cat("RSI Sell Signals:", sum(reliance$RSI_Signal == -1, na.rm = TRUE), "\n")
p_rsi <- ggplot(reliance %>% filter(!is.na(RSI_14)), aes(x = Date, y = RSI_14)) +
  geom_line(color = "#7B1FA2", linewidth = 0.4) +
  geom_hline(yintercept = 70, linetype = "dashed", color = "red", linewidth = 0.6) +
  geom_hline(yintercept = 30, linetype = "dashed", color = "green", linewidth = 0.6) +
  geom_hline(yintercept = 50, linetype = "dotted", color = "grey50") +
  annotate("rect", xmin = min(reliance$Date), xmax = max(reliance$Date), ymin = 70, ymax = 100, fill = "red", alpha = 0.05) +
  annotate("rect", xmin = min(reliance$Date), xmax = max(reliance$Date), ymin = 0, ymax = 30, fill = "green", alpha = 0.05) +
  labs(
    title = "Strategy 2: RSI (14-Day)", subtitle = "Red Zone > 70 (Sell) | Green Zone < 30 (Buy)",
    x = "Date", y = "RSI Value"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") +
  theme_reliance +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p_rsi)
ggsave("Algo_Chart03_RSI.png", p_rsi, width = 12, height = 6, dpi = 300)
# ---- STRATEGY 3: MACD ----
cat("\n========== STRATEGY 3: MACD (12,26,9) ==========\n\n")
macd_vals <- MACD(reliance$Close, nFast = 12, nSlow = 26, nSig = 9)
reliance$MACD      <- macd_vals[, "macd"]
reliance$MACD_Sig  <- macd_vals[, "signal"]
reliance$MACD_Hist <- reliance$MACD - reliance$MACD_Sig
reliance <- reliance %>%
  mutate(
    MACD_Signal   = case_when(
      MACD > MACD_Sig & lag(MACD) <= lag(MACD_Sig) ~  1,
      MACD < MACD_Sig & lag(MACD) >= lag(MACD_Sig) ~ -1,
      TRUE ~ 0
    ),
    MACD_Position = ifelse(MACD > MACD_Sig, 1, 0),
    MACD_Strategy   = lag(MACD_Position) * Daily_Ret,
    MACD_Cumulative = cumprod(1 + ifelse(is.na(MACD_Strategy), 0, MACD_Strategy))
  )
cat("MACD Buy Signals:", sum(reliance$MACD_Signal == 1, na.rm = TRUE), "\n")
cat("MACD Sell Signals:", sum(reliance$MACD_Signal == -1, na.rm = TRUE), "\n")
p_macd <- ggplot(reliance %>% filter(!is.na(MACD)), aes(x = Date)) +
  geom_line(aes(y = MACD, color = "MACD"), linewidth = 0.5) +
  geom_line(aes(y = MACD_Sig, color = "Signal Line"), linewidth = 0.5) +
  geom_bar(aes(y = MACD_Hist, fill = MACD_Hist > 0), stat = "identity", alpha = 0.4) +
  scale_color_manual(values = c("MACD" = "#1565C0", "Signal Line" = "#E53935")) +
  scale_fill_manual(values = c("TRUE" = "#4CAF50", "FALSE" = "#F44336"), guide = "none") +
  labs(title = "Strategy 3: MACD (12, 26, 9)", x = "Date", y = "MACD Value", color = "Legend") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") +
  theme_reliance +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p_macd)
ggsave("Algo_Chart04_MACD.png", p_macd, width = 12, height = 6, dpi = 300)
# ---- ALL STRATEGIES COMPARISON ----
cat("\n========== STRATEGY COMPARISON ==========\n\n")
p_compare <- ggplot(reliance %>% filter(!is.na(SMA_200)), aes(x = Date)) +
  geom_line(aes(y = BH_Cumulative, color = "Buy & Hold"), linewidth = 0.7) +
  geom_line(aes(y = SMA_Cumulative, color = "SMA 50/200"), linewidth = 0.7) +
  geom_line(aes(y = RSI_Cumulative, color = "RSI (14)"), linewidth = 0.7) +
  geom_line(aes(y = MACD_Cumulative, color = "MACD"), linewidth = 0.7) +
  scale_color_manual(values = c("Buy & Hold" = "#1565C0", "SMA 50/200" = "#4CAF50",
                                "RSI (14)" = "#7B1FA2", "MACD" = "#FF7043")) +
  labs(
    title = "Reliance - Strategy Comparison: Cumulative Returns",
    subtitle = "Growth of Rs.1 invested", x = "Date", y = "Cumulative Return", color = "Strategy"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") +
  theme_reliance +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p_compare)
ggsave("Algo_Chart05_Strategy_Comparison.png", p_compare, width = 14, height = 7, dpi = 300)
# ---- PERFORMANCE METRICS ----
cat("\n========== PERFORMANCE METRICS ==========\n\n")
calc_metrics <- function(returns, name) {
  ret <- na.omit(returns)
  total_ret  <- prod(1 + ret) - 1
  annual_ret <- (1 + total_ret)^(252 / length(ret)) - 1
  annual_vol <- sd(ret) * sqrt(252)
  sharpe     <- annual_ret / annual_vol
  max_dd     <- maxDrawdown(xts(ret, order.by = reliance$Date[!is.na(returns)]))
  win_rate   <- sum(ret > 0) / length(ret) * 100
  data.frame(
    Strategy = name, Total_Return = round(total_ret * 100, 2),
    Annual_Return = round(annual_ret * 100, 2), Annual_Volatility = round(annual_vol * 100, 2),
    Sharpe_Ratio = round(sharpe, 3), Max_Drawdown = round(max_dd * 100, 2),
    Win_Rate = round(win_rate, 2)
  )
}
metrics <- bind_rows(
  calc_metrics(reliance$Daily_Ret, "Buy & Hold"),
  calc_metrics(reliance$SMA_Strategy, "SMA 50/200"),
  calc_metrics(reliance$RSI_Strategy, "RSI (14)"),
  calc_metrics(reliance$MACD_Strategy, "MACD (12,26,9)")
)
cat("--- Performance Table ---\n")
print(metrics, row.names = FALSE)
write.csv(metrics, "Algo_Performance_Metrics.csv", row.names = FALSE)
# ---- BOLLINGER BANDS ----
cat("\n========== BOLLINGER BANDS ==========\n\n")
bb <- BBands(reliance$Close, n = 20, sd = 2)
reliance$BB_Upper  <- bb[, "up"]
reliance$BB_Middle <- bb[, "mavg"]
reliance$BB_Lower  <- bb[, "dn"]
recent_bb <- reliance %>% tail(365)
p_bb <- ggplot(recent_bb, aes(x = Date)) +
  geom_ribbon(aes(ymin = BB_Lower, ymax = BB_Upper), fill = "#E3F2FD", alpha = 0.6) +
  geom_line(aes(y = Close, color = "Close"), linewidth = 0.5) +
  geom_line(aes(y = BB_Upper, color = "Upper Band"), linewidth = 0.4, linetype = "dashed") +
  geom_line(aes(y = BB_Middle, color = "Middle Band"), linewidth = 0.4) +
  geom_line(aes(y = BB_Lower, color = "Lower Band"), linewidth = 0.4, linetype = "dashed") +
  scale_color_manual(values = c("Close" = "#1565C0", "Upper Band" = "#E53935",
                                "Middle Band" = "#FF8F00", "Lower Band" = "#4CAF50")) +
  labs(title = "Reliance - Bollinger Bands (Last 1 Year)", x = "Date", y = "Price (INR)", color = "Legend") +
  scale_y_continuous(labels = comma) +
  theme_reliance
print(p_bb)
ggsave("Algo_Chart06_Bollinger_Bands.png", p_bb, width = 12, height = 6, dpi = 300)
# ===========================================================================
#                         FINAL SUMMARY
# ===========================================================================
cat("\n\n============================================================\n")
cat("   RELIANCE INDUSTRIES - COMPLETE R PROJECT FINISHED!\n")
cat("============================================================\n")
cat(" Company  : Reliance Industries Limited (RELIANCE.NS)\n")
cat(" Data     : 10 years from Yahoo Finance\n")
cat("------------------------------------------------------------\n")
cat(" PART 01A : Data downloaded, saved (CSV + Excel), cleaned\n")
cat(" PART 01B : 10 ggplot2 visualisation charts created\n")
cat(" PART 01C : Trend, Decomposition, ADF, ACF/PACF, ARIMA done\n")
cat(" PART 02  : 3 Algo strategies + Bollinger Bands backtested\n")
cat("------------------------------------------------------------\n")
cat(" Total Charts : 26\n")
cat(" Total Files  : CSV, Excel, PNG charts, Performance metrics\n")
cat("============================================================\n")