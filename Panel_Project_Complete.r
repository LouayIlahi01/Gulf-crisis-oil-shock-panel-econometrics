# ============================================================================
# Project: Geopolitics, Oil Shock, and Macroeconomics - Panel Data Analysis
# ============================================================================

# ============================================================================
# 1. INSTALL AND LOAD PACKAGES
# ============================================================================

# Auto-install any missing packages
packages <- c("tidyverse", "plm", "lmtest", "stargazer", "ggplot2", "car")
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed])

# Load all packages
library(tidyverse)
library(plm)
library(lmtest)
library(stargazer)
library(ggplot2)
library(car)

# ============================================================================
# 2. LOAD AND PREPARE THE DATA
# ============================================================================

# Load the dataset — update path to where your CSV is saved
data_raw <- read.csv("C:/Users/louay/Downloads/panel_data_final (5).csv",
                     stringsAsFactors = FALSE)

# Fix column names to lowercase (required for the script to work)
names(data_raw) <- tolower(names(data_raw))

# Rename IndPro to ipi (required by the model formula)
names(data_raw)[names(data_raw) == "indpro"] <- "ipi"

# Inspect the data
str(data_raw)
head(data_raw)

# Convert date to Date format
data_raw$date <- as.Date(data_raw$date)

# Sort by country and date (important for lag calculation)
data_raw <- data_raw %>% arrange(country, date)

# ============================================================================
# 3. CREATE DERIVED VARIABLES
# ============================================================================

data_processed <- data_raw %>%
  group_by(country) %>%
  mutate(
    ln_cpi        = log(cpi),
    ln_ipi        = log(ipi),
    ln_neer       = log(neer),
    post_crisis   = ifelse(date >= as.Date("2026-03-01"), 1, 0),
    oil_pct_change = (brent - dplyr::lag(brent)) / dplyr::lag(brent) * 100,
    oil_pct_change = ifelse(is.na(oil_pct_change), 0, oil_pct_change)
  ) %>%
  ungroup() %>%
  filter(!is.na(ln_cpi), !is.na(ln_ipi), !is.na(oil_pct_change))

# Verify
str(data_processed)

# ============================================================================
# 4. EXPLORATORY DATA ANALYSIS (EDA)
# ============================================================================

# Plot 1: Brent crude oil price over time with crisis marker
ggplot(data_processed, aes(x = date, y = brent)) +
  geom_line(color = "darkred", linewidth = 1) +
  geom_vline(xintercept = as.numeric(as.Date("2026-02-27")),
             linetype = "dashed", color = "blue") +
  annotate("text", x = as.Date("2026-02-27"),
           y = max(data_processed$brent),
           label = "Crisis Start", hjust = -0.1) +
  labs(title = "Brent Crude Oil Price: Pre and Post 2026 Gulf Crisis",
       x = "Date", y = "USD per Barrel") +
  theme_minimal()

# Plot 2: Log CPI trends by country group
ggplot(data_processed, aes(x = date, y = ln_cpi, color = group)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  geom_vline(xintercept = as.numeric(as.Date("2026-02-27")),
             linetype = "dashed") +
  labs(title = "Log CPI Trends by Country Group",
       x = "Date", y = "Log(CPI)") +
  theme_minimal()

# ============================================================================
# 5. DECLARE PANEL DATA FRAME
# ============================================================================

pdata <- pdata.frame(data_processed, index = c("country", "date"))

# ============================================================================
# 6. PANEL REGRESSIONS
# ============================================================================

# Model 1: Pooled OLS
pols_model <- plm(ln_cpi ~ oil_pct_change + ln_neer + policy_rate + vix + post_crisis,
                  data = pdata, model = "pooling")

# Model 2: Fixed Effects
fe_model <- plm(ln_cpi ~ oil_pct_change + ln_neer + policy_rate + vix + post_crisis,
                data = pdata, model = "within")

# Model 3: Random Effects
re_model <- plm(ln_cpi ~ oil_pct_change + ln_neer + policy_rate + vix + post_crisis,
                data = pdata, model = "random")

# Display summaries
summary(pols_model)
summary(fe_model)
summary(re_model)

# ============================================================================
# 7. DIAGNOSTIC TESTS
# ============================================================================

# Hausman Test: FE vs RE
# H0: RE is consistent. If p < 0.05 → prefer FE
hausman_test <- phtest(fe_model, re_model)
print(hausman_test)

# Breusch-Pagan Test: POLS vs RE
# H0: POLS is sufficient. If p < 0.05 → prefer RE over POLS
bp_test <- plmtest(pols_model, effect = "individual", type = "bp")
print(bp_test)

# Robust Standard Errors (Arellano 1987)
coeftest(fe_model,   vcov = vcovHC(fe_model,   method = "arellano", type = "HC3"))
coeftest(re_model,   vcov = vcovHC(re_model,   method = "arellano", type = "HC3"))
coeftest(pols_model, vcov = vcovHC(pols_model, type = "HC3"))

# ============================================================================
# 8. HETEROGENEITY ANALYSIS: IMPORTERS VS. EXPORTERS
# ============================================================================

# Create proper pdata.frames for each subgroup
importers_data <- pdata.frame(
  data_processed[data_processed$oil_balance == "importer", ],
  index = c("country", "date"))

exporters_data <- pdata.frame(
  data_processed[data_processed$oil_balance == "exporter", ],
  index = c("country", "date"))

# Fixed Effects for each subgroup
fe_importers <- plm(ln_cpi ~ oil_pct_change + ln_neer + policy_rate + vix + post_crisis,
                    data = importers_data, model = "within")

fe_exporters <- plm(ln_cpi ~ oil_pct_change + ln_neer + policy_rate + vix + post_crisis,
                    data = exporters_data, model = "within")

# Side-by-side comparison table
stargazer(fe_importers, fe_exporters,
          title = "Fixed Effects Results: Oil Importers vs. Exporters",
          column.labels = c("Importers", "Exporters"),
          type = "text",
          keep.stat = c("n", "rsq"))

# ============================================================================
# 9. INTERPRETATION NOTES
# ============================================================================

# - Hausman p < 0.05 → Fixed Effects preferred over Random Effects
# - BP test p < 0.05 → Random Effects preferred over Pooled OLS
# - Combined: Fixed Effects is the best model
# - oil_pct_change: not statistically significant in either group
#   → oil shock transmits indirectly via NEER and monetary policy
# - post_crisis: +0.092*** for importers vs +0.020** for exporters
#   → importers suffered significantly more inflation post-crisis
# - policy_rate: positive for importers (stagflation), negative for exporters
#   → monetary policy worked differently across groups
# - ln_neer: significant only for importers
#   → currency depreciation amplifies imported inflation
