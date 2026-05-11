# ------------------------------------------------------------------------------
# Test script: Parental Investment (PI) factor analysis
# Loads master data, runs CFA with anchor fixed to 1, reports factor loadings.
# Use this to check that the anchor loading is 1 and to simplify/rewrite the model.
# Run from WB_TIMMS project root. No dependency on running full WB_ANALYSIS.
# ------------------------------------------------------------------------------

library(dplyr)
library(lavaan)
library(ggplot2)

# ---- Config ----
parental_items_all <- sprintf("ASBH01%s", LETTERS[1:18])
exclude_items <- c("ASBH01J", "ASBH01K", "ASBH01L", "ASBH01M", "ASBH01N", "ASBH01O", "ASBH01P", "ASBH01Q", "ASBH01R")
parental_items <- setdiff(parental_items_all, exclude_items)
parental_anchor <- "ASBH01E"

# ---- Load one year of data from master ----
base_dir <- getwd()
master_path <- file.path(base_dir, "data/processed_data/master_refactored.RData")

load(master_path)
df <- master_refactored %>%
  dplyr::filter(year == 2019) %>% mutate(across(everything(), ~ ifelse(. %in% c(998, 999), NA, .)))

df <- df %>%
  mutate(across(all_of(parental_items), 
                ~ ifelse(.x == 2, 1, 0),
                .names = "{.col}_bin"))

# ---- build model, fit, return fit and loadings ----
# Model: latent I =~ 1*anchor + item2 + item3 + ... (anchor loading fixed to 1).
#        anchor ~ 0*1 fixes intercept of anchor to 0 for identification.
# Returns list(fit = lavaan object, loadings = data frame of factor loadings).

model <- 'I =~ 1*ASBH01E + ASBH01A + ASBH01B + ASBH01C + ASBH01D + ASBH01F + ASBH01G + ASBH01H + ASBH01I
    ASBH01E ~ 0*1' 

# + ASBH01J + ASBH01K + ASBH01L + ASBH01M + ASBH01N + ASBH01O + ASBH01P + ASBH01Q + ASBH01R
#  ASBH01E ~ 0*1
  
fit <- lavaan::cfa(model, data = df, ordered = parental_items, std.lv = FALSE)

# Extract loadings (lhs = "I", op = "=~"); anchor should be 1
pe <- lavaan::parameterEstimates(fit)
loadings <- pe %>% dplyr::filter(lhs == "I", op == "=~") %>% dplyr::select(lhs, op, rhs, est, se, z, pvalue)


print(summary(fit, fit.measures = TRUE, standardized = TRUE))
print(lavaan::parameterEstimates(fit, standardized = TRUE))
print(loadings)

#########################


library(psych)
items_bin <- df %>% select(ASBH01A_bin:ASBH01I_bin)  # all 9
tetcor <- tetrachoric(items_bin)
fa.parallel(tetcor$rho, n.obs = nrow(df))  # scree plot
fa(tetcor$rho, nfactors = 2, rotate = "oblimin", n.obs = nrow(df))


########################

model_bin <- 'I =~ ASBH01C_bin + ASBH01F_bin + ASBH01G_bin + ASBH01I_bin'

fit_bin <- cfa(model_bin, 
               data = df, 
               ordered = TRUE,          # tells lavaan these are binary/ordinal
               std.lv = FALSE)           # fixes first loading to 1 (default)

modindices(fit_bin, sort = TRUE, minimum.value = 10)


# View summary with fit indices and standardised estimates
summary(fit_bin, fit.measures = TRUE, standardized = TRUE)

# Extract all parameter estimates
pe_bin <- parameterEstimates(fit_bin, standardized = TRUE)

# Extract just the factor loadings
loadings_bin <- pe_bin %>% 
  filter(lhs == "I", op == "=~") %>% 
  select(lhs, rhs, est, se, z, pvalue, std.lv, std.all)

print(loadings_bin)

df$PI_index_bin <- as.numeric(lavaan::predict(fit_bin))

summary_bin <- df %>%
  group_by(IDCNTRY, CountryName) %>%
  summarise(
    mean_PI = mean(PI_index_bin, na.rm = TRUE),
    sd_PI   = sd(PI_index_bin, na.rm = TRUE),
    p1      = quantile(PI_index_bin, 0.01, na.rm = TRUE),
    p25     = quantile(PI_index_bin, 0.25, na.rm = TRUE),
    median  = median(PI_index_bin, na.rm = TRUE),
    p75     = quantile(PI_index_bin, 0.75, na.rm = TRUE),
    p99     = quantile(PI_index_bin, 0.99, na.rm = TRUE),
    .groups = "drop"
  )






##########################

df$pi_mean <- rowMeans(df[, parental_items], na.rm = TRUE)

df$pi_factor <- NA_real_
df[complete.cases(df[, parental_items]), "pi_factor"] <- as.vector(lavaan::lavPredict(fit))

table <- df %>% group_by(IDCNTRY, CountryName) %>%
  summarise(
    mean_PI = mean(PI_index, na.rm = TRUE),
    sd_PI = sd(PI_index, na.rm = TRUE),
    p1 = quantile(PI_index, 0.01, na.rm = TRUE),
    p25 = quantile(PI_index, 0.25, na.rm = TRUE),
    median = median(PI_index, na.rm = TRUE),
    p75 = quantile(PI_index, 0.75, na.rm = TRUE),
    p99 = quantile(PI_index, 0.99, na.rm = TRUE),
    .groups = "drop"
  )

table

r <- cor(df$pi_factor, df$pi_mean, use = "pairwise.complete.obs")

ggplot(df, aes(x = pi_mean, y = pi_factor)) +
  geom_point(alpha = 0.3, shape = 16, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, colour = "darkred", linewidth = 1, fill = "grey75", alpha = 0.3) +
  labs(x = "Mean of items (pi_mean)", y = "Factor score (pi_factor)", title = "Parental investment: factor score vs mean of items") +
  theme_minimal(base_size = 12)
