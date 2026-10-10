# =============================================================================
# Global Population Growth Analysis and Prediction Using R
# Dataset : Our World in Data - Population (Entity, Code, Year, Population)
# Author  : Zore  |  MCA, Karpagam College of Engineering
# Run     : set the working directory to the project root, then
#           source("scripts/population_analysis.R")
# Needs   : install.packages(c("ggplot2", "corrplot", "randomForest"))
# =============================================================================

library(ggplot2)
library(corrplot)
library(randomForest)

set.seed(42)
dir.create("outputs/plots", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/logs",  recursive = TRUE, showWarnings = FALSE)

# helper: run an expression, print it and save its console output to a log file
save_log <- function(name, expr) {
  out <- capture.output(print(expr))
  writeLines(out, file.path("outputs/logs", paste0(name, ".txt")))
  cat(out, sep = "\n")
}
save_plot <- function(name, plot_fun, w = 1600, h = 1000) {
  png(file.path("outputs/plots", paste0(name, ".png")), width = w, height = h, res = 160)
  plot_fun()
  dev.off()
}

# -----------------------------------------------------------------------------
# 1. LOADING THE DATASET
# -----------------------------------------------------------------------------
pop <- read.csv("data/population.csv", stringsAsFactors = FALSE)
save_log("00_head", head(pop))

# -----------------------------------------------------------------------------
# 2. ANATOMY, STRUCTURE AND SUMMARY
# -----------------------------------------------------------------------------
save_log("01_dim",     dim(pop))
save_log("02_names",   names(pop))
writeLines(capture.output(str(pop)), "outputs/logs/03_str.txt")
cat(readLines("outputs/logs/03_str.txt"), sep = "\n")
save_log("04_summary", summary(pop))
cat("Number of entities:", length(unique(pop$Entity)), "\n")

# -----------------------------------------------------------------------------
# 3. DATA CLEANING
# -----------------------------------------------------------------------------
# 3.1 Missing values
save_log("05_missing", colSums(is.na(pop)))

# 3.2 Duplicates (whole rows and Entity-Year pairs)
dup_rows <- sum(duplicated(pop))
dup_keys <- sum(duplicated(pop[, c("Entity", "Year")]))
save_log("06_duplicates", c(duplicate_rows = dup_rows, duplicate_entity_year = dup_keys))

# 3.3 Inconsistent observations: blank codes, non-positive populations
save_log("07_inconsistent",
         c(blank_codes = sum(pop$Code == ""),
           non_positive_population = sum(pop$Population <= 0),
           years_before_1800 = sum(pop$Year < 1800)))

# 3.4 Keep 1800-2023 (annual data); flag aggregates (World, continents, UN/income groups)
pop <- pop[pop$Year >= 1800, ]
pop$Type <- ifelse(pop$Entity == "World", "World",
            ifelse(pop$Entity %in% c("Africa", "Asia", "Europe", "North America",
                                     "South America", "Oceania"), "Continent",
            ifelse(grepl("^[A-Z]{3}$", pop$Code), "Country", "Other")))   # ISO-3 codes only
save_log("08_entity_types", table(pop[!duplicated(pop$Entity), "Type"]))

# 3.5 Feature engineering (population growth per entity)
pop <- pop[order(pop$Entity, pop$Year), ]
pop$Growth     <- ave(pop$Population, pop$Entity, FUN = function(x) c(NA, diff(x)))
pop$GrowthRate <- 100 * pop$Growth / (pop$Population - pop$Growth)   # % per year
pop$LogPop     <- log10(pop$Population)

write.csv(pop, "data/population_clean_1800_2023.csv", row.names = FALSE)

world <- pop[pop$Entity == "World", c("Year", "Population", "Growth", "GrowthRate", "LogPop")]
write.csv(world, "data/world_population_1800_2023.csv", row.names = FALSE)
save_log("09_world_head", head(world))

# -----------------------------------------------------------------------------
# 4. CONTROL STRUCTURES, FUNCTIONS, LISTS AND DATA FRAMES (assignment Q2)
# -----------------------------------------------------------------------------
# 4.0 Basic data structures on this dataset
years  <- world$Year                                              # numeric (integer) vector
first5 <- list(name = "World", years = years[1:5])                # list
m      <- as.matrix(world[1:3, c("Year", "Population")])          # matrix
f      <- factor(pop$Type)                                        # factor
writeLines(capture.output(str(world)), "outputs/logs/10a_str_world.txt")
save_log("10b_structures", list(vector = years[1:5], list = first5, matrix = m,
                                factor_levels = levels(f), data_frame_class = class(world)))

# 4.1 Function: compound annual growth rate between two values
cagr <- function(p_start, p_end, years) {
  100 * ((p_end / p_start)^(1 / years) - 1)
}

# 4.2 if / else if / else : classify the growth era of a year
growth_era <- function(rate) {
  if (is.na(rate)) {
    "Unknown"
  } else if (rate < 0.5) {
    "Slow (<0.5%)"
  } else if (rate < 1.5) {
    "Moderate (0.5-1.5%)"
  } else {
    "Rapid (>=1.5%)"
  }
}

# 4.3 for loop : growth summary per period, stored in a list
periods <- list(c(1800, 1849), c(1850, 1899), c(1900, 1949), c(1950, 1999), c(2000, 2023))
period_summary <- list()
for (p in periods) {
  sub   <- world[world$Year >= p[1] & world$Year <= p[2], ]
  label <- paste(p[1], p[2], sep = "-")
  period_summary[[label]] <- list(mean_growth_rate = mean(sub$GrowthRate, na.rm = TRUE),
                                  max_growth_rate  = max(sub$GrowthRate,  na.rm = TRUE),
                                  cagr = cagr(sub$Population[1], tail(sub$Population, 1), nrow(sub) - 1))
}
period_df <- do.call(rbind, lapply(names(period_summary), function(n)
  data.frame(Period = n, as.data.frame(period_summary[[n]]))))
period_df[, -1] <- round(period_df[, -1], 3)
save_log("10_period_summary", period_df)

# 4.4 sapply with the function: era of every year, then tabulate
world$Era <- sapply(world$GrowthRate, growth_era)
save_log("11_era_table", table(world$Era))

# 4.5 while loop : first year the world population crossed each milestone
targets <- c(1e9, 2e9, 4e9, 6e9, 8e9); i <- 1; crossing <- list()
while (i <= length(targets)) {
  yr <- world$Year[which(world$Population >= targets[i])[1]]
  crossing[[paste0(targets[i] / 1e9, " billion")]] <- yr
  i <- i + 1
}
save_log("12_billion_milestones", unlist(crossing))

# -----------------------------------------------------------------------------
# 5. DATA VISUALISATION
# -----------------------------------------------------------------------------
# 5.0 Descriptive statistics of the World series
desc_fun <- function(x) c(Mean = mean(x, na.rm = TRUE), Median = median(x, na.rm = TRUE),
                          SD = sd(x, na.rm = TRUE), Min = min(x, na.rm = TRUE), Max = max(x, na.rm = TRUE))
desc <- rbind("Population (billions)" = desc_fun(world$Population / 1e9),
              "GrowthRate (% per year)" = desc_fun(world$GrowthRate))
save_log("10c_descriptive", round(desc, 3))

p1 <- ggplot(world, aes(Year, Population / 1e9)) +
  geom_line(colour = "#0F766E", linewidth = 1.2) +
  labs(title = "World Population 1800-2023", x = "Year", y = "Population (billions)") +
  theme_minimal(base_size = 14)
ggsave("outputs/plots/01_world_population.png", p1, width = 9, height = 5.5, dpi = 160)

p2 <- ggplot(world[-1, ], aes(Year, GrowthRate)) +
  geom_line(colour = "#1E3A8A", linewidth = 1.1) +
  labs(title = "World Annual Population Growth Rate", x = "Year", y = "Growth rate (% per year)") +
  theme_minimal(base_size = 14)
ggsave("outputs/plots/02_growth_rate.png", p2, width = 9, height = 5.5, dpi = 160)

cont <- pop[pop$Type == "Continent", ]
p3 <- ggplot(cont, aes(Year, Population / 1e9, colour = Entity)) +
  geom_line(linewidth = 1) +
  labs(title = "Population by Continent", x = "Year", y = "Population (billions)", colour = "") +
  theme_minimal(base_size = 14)
ggsave("outputs/plots/03_continents.png", p3, width = 9, height = 5.5, dpi = 160)

# Correlation matrix (continent-level numeric variables)
num_vars <- cont[!is.na(cont$GrowthRate), c("Year", "Population", "Growth", "GrowthRate", "LogPop")]
cor_mat  <- cor(num_vars, use = "pairwise.complete.obs")
save_log("13_cor_matrix", round(cor_mat, 3))
save_plot("04_correlation", function() corrplot(cor_mat, method = "color", addCoef.col = "black",
                                                tl.col = "black", tl.cex = 1, number.cex = 0.9), 1300, 1100)

# Boxplots for outliers
save_plot("05_boxplot_growthrate", function() {
  par(mar = c(8, 5, 4, 2))
  boxplot(GrowthRate ~ Entity, data = cont[!is.na(cont$GrowthRate), ], col = "#99F6E4",
          main = "Annual Growth Rate by Continent (1801-2023)", ylab = "Growth rate (%)", xlab = "", las = 2)
})
countries23 <- pop[pop$Type == "Country" & pop$Year == 2023, ]
save_plot("06_boxplot_country_pop", function() {
  boxplot(countries23$LogPop, col = "#BFDBFE", horizontal = TRUE,
          main = "Country Population in 2023 (log10 scale)", xlab = "log10(Population)")
})
out_stats <- boxplot.stats(world$GrowthRate[-1])$out
save_log("14_outliers", c(world_growth_outliers = length(out_stats),
                          country_pop_outliers = length(boxplot.stats(countries23$LogPop)$out)))

# -----------------------------------------------------------------------------
# 6. DATA SPLITTING (chronological 80 / 20)
# -----------------------------------------------------------------------------
n       <- nrow(world)
n_train <- floor(0.8 * n)
train   <- world[1:n_train, ]
test    <- world[(n_train + 1):n, ]
save_log("15_split", c(total = n, train = nrow(train), test = nrow(test),
                       train_years = paste(range(train$Year), collapse = "-"),
                       test_years  = paste(range(test$Year),  collapse = "-")))

# -----------------------------------------------------------------------------
# 7. MODEL TRAINING (assignment Q4)
# -----------------------------------------------------------------------------
metrics <- function(actual, pred) {
  c(RMSE = sqrt(mean((actual - pred)^2)),
    MAE  = mean(abs(actual - pred)),
    R2   = 1 - sum((actual - pred)^2) / sum((actual - mean(actual))^2),
    MAPE = 100 * mean(abs((actual - pred) / actual)))
}

lm_model   <- lm(Population ~ Year, data = train)                 # 7.1 simple linear
save_log("16_lm_summary", summary(lm_model))
poly_model <- lm(Population ~ poly(Year, 3), data = train)       # 7.2 polynomial (deg 3)
save_log("17_poly_summary", summary(poly_model))
log_model  <- lm(log(Population) ~ Year, data = train)           # 7.3 log-linear (exponential)
save_log("18_loglinear_summary", summary(log_model))
rf_model   <- randomForest(Population ~ Year, data = train, ntree = 500)   # 7.4 random forest
save_log("19_rf_model", rf_model)

# -----------------------------------------------------------------------------
# 8. PREDICTION AND EVALUATION (assignment Q5)
# -----------------------------------------------------------------------------
test$Pred_LM   <- predict(lm_model,   newdata = test)
test$Pred_Poly <- predict(poly_model, newdata = test)
test$Pred_Log  <- exp(predict(log_model, newdata = test))
test$Pred_RF   <- predict(rf_model,   newdata = test)

results <- rbind(
  "Linear Regression"     = metrics(test$Population, test$Pred_LM),
  "Polynomial (degree 3)" = metrics(test$Population, test$Pred_Poly),
  "Log-linear Regression" = metrics(test$Population, test$Pred_Log),
  "Random Forest"         = metrics(test$Population, test$Pred_RF))
results_df <- data.frame(Model = rownames(results), round(results, 3), row.names = NULL)
write.csv(results_df, "outputs/model_comparison.csv", row.names = FALSE)
save_log("20_model_comparison", format(results_df, big.mark = ",", scientific = FALSE))

train_r2 <- c(LM = summary(lm_model)$r.squared, Poly = summary(poly_model)$r.squared,
              Log = summary(log_model)$r.squared, RF = mean(rf_model$rsq))
save_log("21_train_r2", round(train_r2, 4))

write.csv(test[, c("Year", "Population", "Pred_LM", "Pred_Poly", "Pred_Log", "Pred_RF")],
          "outputs/test_predictions.csv", row.names = FALSE)

avp <- function(col, title, colour, file) {
  d <- data.frame(Year = test$Year, Actual = test$Population / 1e9, Predicted = test[[col]] / 1e9)
  g <- ggplot(d, aes(Year)) +
    geom_line(aes(y = Actual,    colour = "Actual"),    linewidth = 1.2) +
    geom_line(aes(y = Predicted, colour = "Predicted"), linewidth = 1.2, linetype = "dashed") +
    scale_colour_manual(values = c(Actual = "#111827", Predicted = colour)) +
    labs(title = title, x = "Year (test period)", y = "Population (billions)", colour = "") +
    theme_minimal(base_size = 14)
  ggsave(file.path("outputs/plots", file), g, width = 9, height = 5.5, dpi = 160)
}
avp("Pred_LM",   "Linear Regression: Actual vs Predicted",     "#DC2626", "07_lm_actual_vs_pred.png")
avp("Pred_Poly", "Polynomial Regression: Actual vs Predicted", "#7C3AED", "08_poly_actual_vs_pred.png")
avp("Pred_Log",  "Log-linear Regression: Actual vs Predicted", "#EA580C", "09_log_actual_vs_pred.png")
avp("Pred_RF",   "Random Forest: Actual vs Predicted",         "#0F766E", "10_rf_actual_vs_pred.png")

full <- data.frame(Year = world$Year, Actual = world$Population / 1e9,
                   LM = predict(lm_model, world) / 1e9, Poly = predict(poly_model, world) / 1e9,
                   Log = exp(predict(log_model, world)) / 1e9, RF = predict(rf_model, world) / 1e9)
pf <- ggplot(full, aes(Year)) +
  geom_line(aes(y = Actual, colour = "Actual"), linewidth = 1.4) +
  geom_line(aes(y = LM,   colour = "Linear"),        linetype = "dashed") +
  geom_line(aes(y = Poly, colour = "Polynomial"),    linetype = "dashed") +
  geom_line(aes(y = Log,  colour = "Log-linear"),    linetype = "dashed") +
  geom_line(aes(y = RF,   colour = "Random Forest"), linetype = "dashed") +
  geom_vline(xintercept = max(train$Year), colour = "grey40") +
  scale_colour_manual(values = c(Actual = "#111827", Linear = "#DC2626", Polynomial = "#7C3AED",
                                 "Log-linear" = "#EA580C", "Random Forest" = "#0F766E")) +
  labs(title = "All Models vs Actual (grey line = train/test split)", x = "Year",
       y = "Population (billions)", colour = "") + theme_minimal(base_size = 14)
ggsave("outputs/plots/11_all_models.png", pf, width = 9, height = 5.5, dpi = 160)

# -----------------------------------------------------------------------------
# 9. STATISTICAL PROBLEMS: regression diagnostics (assignment Q3)
# -----------------------------------------------------------------------------
res     <- residuals(lm_model)
dw      <- sum(diff(res)^2) / sum(res^2)                    # Durbin-Watson statistic
bp_aux  <- lm(res^2 ~ train$Year)                           # Breusch-Pagan (studentised) test
bp_stat <- length(res) * summary(bp_aux)$r.squared
bp_p    <- pchisq(bp_stat, df = 1, lower.tail = FALSE)
sh      <- shapiro.test(res)
vif_raw <- 1 / (1 - cor(train$Year, train$Year^2)^2)        # multicollinearity: Year vs Year^2
yr_c    <- train$Year - mean(train$Year)
vif_cen <- 1 / (1 - cor(yr_c, yr_c^2)^2)                    # after centring
skew    <- mean((train$Population - mean(train$Population))^3) / sd(train$Population)^3
diag_tbl <- data.frame(
  Check = c("Durbin-Watson statistic (autocorrelation)", "Breusch-Pagan chi-square",
            "Breusch-Pagan p-value (heteroscedasticity)", "Shapiro-Wilk p-value (normal residuals)",
            "VIF: Year vs Year^2 (raw)", "VIF: Year vs Year^2 (centred)",
            "Skewness of Population (train)", "Missing World values"),
  Value = c(format(round(dw, 4), nsmall = 4), format(round(bp_stat, 3), nsmall = 3),
            format(signif(bp_p, 3), scientific = TRUE), format(signif(sh$p.value, 3), scientific = TRUE),
            format(round(vif_raw, 1), big.mark = ","), format(round(vif_cen, 2), nsmall = 2),
            format(round(skew, 3), nsmall = 3), as.character(sum(is.na(world$Population)))))
save_log("22_diagnostics", diag_tbl)

save_plot("12_residual_diagnostics", function() {
  par(mfrow = c(1, 2))
  plot(fitted(lm_model), res, pch = 16, col = "#0F766E", main = "Residuals vs Fitted (Linear)",
       xlab = "Fitted values", ylab = "Residuals"); abline(h = 0, lty = 2)
  qqnorm(res, pch = 16, col = "#1E3A8A", main = "Normal Q-Q (Linear residuals)"); qqline(res)
}, 1800, 900)

# -----------------------------------------------------------------------------
# 10. FUTURE FORECAST (refit on the full 1800-2023 history)
# -----------------------------------------------------------------------------
best <- results_df$Model[which.min(results_df$RMSE)]
cat("\nBest model on the test set:", best, "\n")
future <- data.frame(Year = 2024:2050)
future$Polynomial <- predict(lm(Population ~ poly(Year, 3), data = world), newdata = future)
future$LogLinear  <- exp(predict(lm(log(Population) ~ Year, data = world), newdata = future))
write.csv(future, "outputs/forecast_2024_2050.csv", row.names = FALSE)
fut <- future[future$Year %in% c(2030, 2040, 2050), ]
fut[, -1] <- format(round(fut[, -1]), big.mark = ",", scientific = FALSE)
save_log("23_future_forecast", fut)
cat("\nAll outputs saved in outputs/plots and outputs/logs\n")
