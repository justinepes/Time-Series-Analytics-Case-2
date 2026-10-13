# Case 2: Structural VAR Models and Local Projections
# Time Series Analytics

# ----------------------------------------------------------------
# Script overview
# Part I   Data exploration, transformations and VAR analysis
# Part II  Structural VARs and local projections
# Part III Monetary policy shock proxies and IRF comparisons
#
# Original calculations, settings, object names and commands retained.
# ----------------------------------------------------------------

rm(list=ls())

# Packages
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(bootUR)
library(vars)
library(gsl)
library(svars)
library(lpirfs)

# Data exploration

# ================================================================
# 01. Data loading and preparation

# ================================================================
# Load data
fred_data <-  read_csv("~/MU/Time Series Analytics/Case/FRED_md.csv")
names(fred_data)[1] <- "date"
fred_data <- fred_data %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
  mutate(across(-date, as.numeric))

tcode <- fred_data[1, ]  %>% dplyr::select("date", "INDPRO", "CPIAUCSL", "FEDFUNDS")

fred_data <- fred_data %>% dplyr::select("date", "INDPRO", "CPIAUCSL", "FEDFUNDS")

fred_data <- fred_data %>% filter(date <= as.Date("2019-12-01"))

head(fred_data)
tail(fred_data)
summary(fred_data)
str(fred_data)

# ================================================================
# 02. Exploratory data analysis

# ================================================================
# Time series plot of the variables
par(mfrow = c(3, 1))

plot(fred_data$date, fred_data$INDPRO,
     type = "l",
     main = "Industrial Production",
     xlab = "Date",
     ylab = "Index")

plot(fred_data$date, fred_data$CPIAUCSL,
     type = "l",
     main = "Consumer Price Index",
     xlab = "Date",
     ylab = "Index")

plot(fred_data$date, fred_data$FEDFUNDS,
     type = "l",
     main = "Federal Funds Rate",
     xlab = "Date",
     ylab = "Percent")

# ================================================================
# 03. Presentation plots: original series

# ================================================================
# We let AI make a nicer plot for the presentation:
# Data exploration: presentation figure

plot_data <- fred_data %>%
  dplyr::select(date, INDPRO, CPIAUCSL, FEDFUNDS) %>%
  pivot_longer(
    cols = -date,
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    variable = factor(
      variable,
      levels = c("INDPRO", "CPIAUCSL", "FEDFUNDS"),
      labels = c(
        "Industrial Production",
        "Consumer Price Index",
        "Federal Funds Rate"
      )
    )
  )

# Create presentation-quality time-series plots
data_plot <- ggplot(
  plot_data,
  aes(x = date, y = value)
) +

  # Plot each time series
  geom_line(
    linewidth = 0.7
  ) +

  # Separate panel for each variable
  # Free y-scales because the variables have different units
  facet_wrap(
    ~ variable,
    ncol = 3,
    scales = "free_y"
  ) +

  # Show years at 10-year intervals
  scale_x_date(
    date_breaks = "10 years",
    date_labels = "%Y"
  ) +

  # No overall axis labels needed
  labs(
    x = NULL,
    y = NULL
  ) +

  # Clean presentation theme
  theme_minimal(
    base_size = 13
  ) +

  theme(
    # Remove minor grid lines
    panel.grid.minor = element_blank(),

    # Keep major grid lines subtle
    panel.grid.major = element_line(
      linewidth = 0.3
    ),

    # Panel titles
    strip.text = element_text(
      size = 13,
      face = "bold"
    ),

    # Remove grey background behind panel titles
    strip.background = element_blank(),

    # Axis labels
    axis.text = element_text(
      size = 10
    ),

    # Add some space between the three graphs
    panel.spacing = grid::unit(
      1.5,
      "lines"
    ),

    # White background for Canva
    plot.background = element_rect(
      fill = "white",
      colour = NA
    )
  )

# Display the figure in RStudio
data_plot

ggsave(
  filename = "fred_data_exploration.png",
  plot = data_plot,
  width = 13,
  height = 4,
  dpi = 300,
  bg = "white"
)

# ================================================================
# 04. Histograms and autocorrelation

# ================================================================
# histograms of the variables

hist(fred_data$INDPRO,
     main = "Industrial Production",
     xlab = "INDPRO")

hist(fred_data$CPIAUCSL,
     main = "Consumer Price Index",
     xlab = "CPIAUCSL")

hist(fred_data$FEDFUNDS,
     main = "Federal Funds Rate",
     xlab = "FEDFUNDS")

# autocorrelation plots

acf(fred_data$INDPRO,
    main = "ACF: Industrial Production")

acf(fred_data$CPIAUCSL,
    main = "ACF: Consumer Price Index")

acf(fred_data$FEDFUNDS,
    main = "ACF: Federal Funds Rate")

# ================================================================
# 05. Transformations and comparison with original series

# ================================================================
par(mfrow = c(2, 3)) # compare the transformed variables with their "originals"

plot(fred_data$date, fred_data$INDPRO,
     type = "l",
     main = "Industrial Production",
     xlab = "Date",
     ylab = "Index")

plot(fred_data$date, fred_data$CPIAUCSL,
     type = "l",
     main = "Consumer Price Index",
     xlab = "Date",
     ylab = "Index")

plot(fred_data$date, fred_data$FEDFUNDS,
     type = "l",
     main = "Federal Funds Rate",
     xlab = "Date",
     ylab = "Percent")

# perform transformation based on t codes by McCracken and Ng (2016)
# t-code 5: taking the difference of the log
fred_data$trans_INDPRO <- c(NA, diff(log(fred_data$INDPRO)))

plot(fred_data$date, fred_data$trans_INDPRO,
     type = "l",
     main = "Transformed Industrial Production",
     xlab = "Date",
     ylab = "Δ log(INDPRO)")

# t-code 6: taking the second difference of the log
fred_data$trans_CPIAUCSL <- c(
  NA,
  NA,
  diff(diff(log(fred_data$CPIAUCSL)))
)

plot(fred_data$date, fred_data$trans_CPIAUCSL,
     type = "l",
     main = "Transformed CPI",
     xlab = "Date",
     ylab = expression(Delta^2 * log(CPI)))

# t-code 2: taking the first difference

fred_data$trans_FEDFUNDS <- c(NA, diff(fred_data$FEDFUNDS))

plot(fred_data$date, fred_data$trans_FEDFUNDS,
     type = "l",
     main = "Transformed Federal Funds",
     xlab = "Date",
     ylab = "Δ FEDFUNDS")

par(mfrow = c(1, 1)) # reset plot layout

# ================================================================
# 06. Presentation plots: transformed series (AI)

# ================================================================
transformed_plot_data <- fred_data %>%
  dplyr::select(
    date,
    trans_INDPRO,
    trans_CPIAUCSL,
    trans_FEDFUNDS
  ) %>%
  tidyr::pivot_longer(
    cols = -date,
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    variable = factor(
      variable,
      levels = c(
        "trans_INDPRO",
        "trans_CPIAUCSL",
        "trans_FEDFUNDS"
      ),
      labels = c(
        "Δ log(INDPRO)",
        "Δ² log(CPI)",
        "Δ FEDFUNDS"
      )
    )
  )

transformed_plot <- ggplot(
  transformed_plot_data,
  aes(x = date, y = value)
) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.4,
    linetype = "dashed"
  ) +
  geom_line(
    linewidth = 0.6
  ) +
  facet_wrap(
    ~ variable,
    ncol = 3,
    scales = "free_y"
  ) +
  scale_x_date(
    date_breaks = "10 years",
    date_labels = "%Y"
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal(
    base_size = 13
  ) +
  theme(
    panel.grid.minor = element_blank(),

    panel.grid.major = element_line(
      linewidth = 0.3
    ),

    strip.text = element_text(
      size = 13,
      face = "bold"
    ),

    strip.background = element_blank(),

    axis.text = element_text(
      size = 10
    ),

    panel.spacing = grid::unit(
      1.5,
      "lines"
    ),

    plot.background = element_rect(
      fill = "white",
      colour = NA
    )
  )

# Display
transformed_plot

# Save for Canva
ggsave(
  filename = "fred_transformed_series.png",
  plot = transformed_plot,
  width = 13,
  height = 4,
  dpi = 300,
  bg = "white"
)

# ================================================================
# 07. Unit root tests (Pantula principle)

# ================================================================
# We implement the Pantula Principle by using ADF test (Augmented Dickey-Fuller
# test), which checks whether a time series is stationary or has a unit root
# H₀: unit root-> non-stationary
# H₁: no unit root-> stationary

# INDPRO # --------------------------------------------------------------------

indpro_dlog  <- na.omit(diff(log(fred_data$INDPRO)))
indpro_log <- na.omit(log(fred_data$INDPRO))

# dmax = 1
# H0: unit root in Δlog(INDPRO)
adf_indpro_d1 <- adf(indpro_dlog, deterministics = "intercept")
adf_indpro_d1$statistic
adf_indpro_d1$p.value

boot_adf_indpro_d1 <- boot_adf(indpro_dlog, deterministics = "intercept")
boot_adf_indpro_d1$statistic
boot_adf_indpro_d1$p.value

# We reject, set d = 0
# H0: unit root in log(INDPRO)
adf_indpro_log <- adf(indpro_log, deterministics = "trend")
adf_indpro_log$statistic
adf_indpro_log$p.value

boot_adf_indpro_log <- boot_adf(indpro_log, deterministics = "trend")
boot_adf_indpro_log$statistic
boot_adf_indpro_log$p.value
# We fail to reject, series is considered I(1)
# This is consistent with the transformation code suggested by McCracken and Ng.

# CPI # -----------------------------------------------------------------------

cpi_d2log <- na.omit(fred_data$trans_CPIAUCSL)
cpi_dlog  <- na.omit(diff(log(fred_data$CPIAUCSL)))
cpi_log <- na.omit(log(fred_data$CPIAUCSL))

# dmax = 2
# Step 1 - H0: unit root in Δ²log(CPI)
adf_cpi_d2 <- adf(cpi_d2log, deterministics = "none")
adf_cpi_d2$statistic
adf_cpi_d2$p.value

boot_adf_cpi_d2 <- boot_adf(cpi_d2log, deterministics = "none")
boot_adf_cpi_d2$statistic
boot_adf_cpi_d2$p.value
# We reject, set d = 1

# Step 2 - H0: unit root in Δlog(CPI)
adf_cpi_d1 <- adf(cpi_dlog, deterministics = "intercept")
adf_cpi_d1$statistic
adf_cpi_d1$p.value

boot_adf_cpi_d1 <- boot_adf(cpi_dlog, deterministics = "intercept")
boot_adf_cpi_d1$statistic
boot_adf_cpi_d1$p.value

# We fail to reject, series is considered I(2)
# This is consistent with the transformation code suggested by McCracken and Ng.

# FEDFUNDS #-------------------------------------------------------------------

ff_diff  <- na.omit(fred_data$trans_FEDFUNDS)
ff <- na.omit(fred_data$FEDFUNDS)

# dmax = 1
# H0: unit root in ΔFEDFUNDS
adf_ff_d1 <- adf(ff_diff, deterministics = "none")
adf_ff_d1$statistic
adf_ff_d1$p.value

boot_adf_ff_d1 <- boot_adf(ff_diff, deterministics = "none")
boot_adf_ff_d1$statistic
boot_adf_ff_d1$p.value

# We reject, set d = 0
# H0: unit root in FEDFUNDS level
adf_ff <- adf(ff, deterministics = "intercept")
adf_ff$statistic
adf_ff$p.value

boot_adf_ff <- boot_adf(ff, deterministics = "intercept")
boot_adf_ff$statistic
boot_adf_ff$p.value

# We fail to reject, series is considered I(1)
# This is consistent with the transformation code suggested by McCracken and Ng.

# Since our results are consistent with the transformations of McCracken and Ng
# we use those transformations such that we have a stationary time series from
# now on

# ================================================================
# 08. VAR estimation and lag selection

# ================================================================
var_data <- fred_data %>%
  dplyr::select(trans_INDPRO, trans_CPIAUCSL, trans_FEDFUNDS) %>%
  na.omit()

lag_selection <- VARselect(var_data, lag.max = 30, type = "const")

lag_selection$selection
lag_selection$criteria["AIC(n)", ]
lag_selection$criteria["SC(n)", ]
lag_selection$criteria["HQ(n)", ]

# Choose the lag order by the criteria that we choose
p_selected <- lag_selection$selection["AIC(n)"]

var_model <- VAR(var_data, p = p_selected, type = "const")
summary(var_model)

# Coefficients per equation
var_model$varresult$trans_INDPRO
var_model$varresult$trans_CPIAUCSL
var_model$varresult$trans_FEDFUNDS

# ================================================================
# 09. VAR model diagnostics

# ================================================================
# Evaluate the model # --------------------------------------------------------

# Serial correlation
# H0: no serial correlation
serial_test <- serial.test(var_model, lags.pt = 28, type = "PT.asymptotic")
serial_test

for (p in 1:30) {
  model_p <- VAR(var_data, p = p, type = "const")
  st <- serial.test(model_p, lags.pt = 30, type = "PT.asymptotic")
  cat("p =", p, " -> p-value:", st$serial$p.value, "\n")
}

# ================================================================
# 10. Pairwise Granger causality

# ================================================================
granger_pair <- function(var_model, cause, response) {

  # Get equation for response variable
  model <- var_model$varresult[[response]]

  # Find coefficients for all lags of cause variable
  idx <- grep(
    paste0("^", cause, "\\.l[0-9]+$"),
    names(coef(model))
  )

  # Coefficients being tested
  b <- coef(model)[idx]

  # Corresponding covariance matrix
  V <- vcov(model)[idx, idx, drop = FALSE]

  # Number of restrictions
  q <- length(idx)

  # Joint F statistic
  F_stat <- as.numeric(t(b) %*% solve(V) %% b / q)

  # p-value
  p_value <- pf(
    F_stat,
    df1 = q,
    df2 = df.residual(model),
    lower.tail = FALSE
  )

  cat(
    cause, "->", response,
    "\nF =", F_stat,
    "\ndf1 =", q,
    "\ndf2 =", df.residual(model),
    "\np-value =", p_value,
    "\n\n"
  )
}

# INDPRO -> CPI
granger_pair(var_model, "trans_INDPRO", "trans_CPIAUCSL")

# CPI -> INDPRO
granger_pair(var_model, "trans_CPIAUCSL", "trans_INDPRO")

# INDPRO -> FEDFUNDS
granger_pair(var_model, "trans_INDPRO", "trans_FEDFUNDS")

# FEDFUNDS -> INDPRO
granger_pair(var_model, "trans_FEDFUNDS", "trans_INDPRO")

# CPI -> FEDFUNDS
granger_pair(var_model, "trans_CPIAUCSL", "trans_FEDFUNDS")

# FEDFUNDS -> CPI
granger_pair(var_model, "trans_FEDFUNDS", "trans_CPIAUCSL")

# ================================================================
# 11. REDUCED-FORM IMPULSE RESPONSE FUNCTIONS

# ================================================================
# Reduced-form IRFs use non-orthogonalized VAR innovations.
# These shocks are not necessarily exogenous monetary policy shocks.
# ----------------------------------------------------------------
# 11.1 Estimate reduced-form IRFs
# ----------------------------------------------------------------
irf_indpro_reduced <- irf(
  var_model,
  impulse = "trans_INDPRO",
  n.ahead = 36,
  ortho = FALSE,
  boot = TRUE,
  ci = 0.95,
  runs = 1000
)

irf_cpi_reduced <- irf(
  var_model,
  impulse = "trans_CPIAUCSL",
  n.ahead = 36,
  ortho = FALSE,
  boot = TRUE,
  ci = 0.95,
  runs = 1000
)

irf_ff_reduced <- irf(
  var_model,
  impulse = "trans_FEDFUNDS",
  n.ahead = 36,
  ortho = FALSE,
  boot = TRUE,
  ci = 0.95,
  runs = 1000
)

# Plot all reduced-form IRFs
plot(irf_indpro_reduced)
plot(irf_cpi_reduced)
plot(irf_ff_reduced)
# ----------------------------------------------------------------
# 11.2 Convert reduced-form FEDFUNDS IRFs back to levels
# ----------------------------------------------------------------
# INDPRO: Δlog -> log (cumulate once)
# CPI: Δ²log -> log (cumulate twice)
# FEDFUNDS: Δ -> level (cumulate once)

reduced_indpro <- irf_ff_reduced$irf$trans_FEDFUNDS[
  , "trans_INDPRO"
]

reduced_cpi <- irf_ff_reduced$irf$trans_FEDFUNDS[
  , "trans_CPIAUCSL"
]

reduced_ff <- irf_ff_reduced$irf$trans_FEDFUNDS[
  , "trans_FEDFUNDS"
]

reduced_indpro_level <- 100 * cumsum(reduced_indpro)
reduced_cpi_level <- 100 * cumsum(cumsum(reduced_cpi))
reduced_ff_level <- cumsum(reduced_ff)
# ----------------------------------------------------------------
# 11.3 Plot reduced-form responses in levels
# ----------------------------------------------------------------
par(mfrow = c(3, 1))

plot(
  0:36,
  reduced_indpro_level,
  type = "l",
  lwd = 2,
  xlab = "Months after shock",
  ylab = "Response (%)",
  main = "Reduced-form: INDPRO response to FEDFUNDS shock"
)
abline(h = 0, lty = 3)

plot(
  0:36,
  reduced_cpi_level,
  type = "l",
  lwd = 2,
  xlab = "Months after shock",
  ylab = "Response (%)",
  main = "Reduced-form: CPI response to FEDFUNDS shock"
)
abline(h = 0, lty = 3)

plot(
  0:36,
  reduced_ff_level,
  type = "l",
  lwd = 2,
  xlab = "Months after shock",
  ylab = "Response (percentage points)",
  main = "Reduced-form: FEDFUNDS response to FEDFUNDS shock"
)
abline(h = 0, lty = 3)

par(mfrow = c(1, 1))

#

# ================================================================
# 12. Structural VAR: recursive identification and IRFs

# ================================================================
# install.packages("gsl")
# install.packages("svars")
# Function id.chol was given by AI

svar_model <- id.chol(
  var_model,
  order_k = c(
    "trans_INDPRO",
    "trans_CPIAUCSL",
    "trans_FEDFUNDS"
  )
)

# Display results
summary(svar_model)

# Estimated structural impact matrix
svar_model$B

# Check ordering
svar_model$order_k

# 12.2 Structural impulse response functions

svar_irf <- irf(
  svar_model,
  n.ahead = 36
)

# Plot all structural IRFs
plot(
  svar_irf,
  scales = "free_y"
)

# We add bootstrap constraints (by AI)
svar_boot <- wild.boot(svar_model, n.ahead = 36, nboot = 500)
plot(svar_boot)

svar_model_2 <- id.chol(
  var_model,
  order_k = c("trans_CPIAUCSL", "trans_INDPRO", "trans_FEDFUNDS")
)
irf_2 <- irf(svar_model_2, n.ahead = 36)

plot(svar_irf, scales = "free_y")
plot(irf_2, scales = "free_y")

# Optional: check how large the contemporaneous correlations are to begin with,
# which tells you how much the ordering *should* matter
cov2cor(summary(var_model)$covres)

# ================================================================
# 13. REDUCED-FORM VAR VS. LOCAL PROJECTIONS (With help from AI)

# ================================================================
library(vars)
library(ggplot2)
library(dplyr)

set.seed(123)

# Settings
p <- as.integer(p_selected)
H <- 36
B <- 1000
block_length <- 48

Y <- as.data.frame(var_data)
variables <- names(Y)
# ----------------------------------------------------------------
# 17.1 Estimate Local Projections
# ----------------------------------------------------------------
estimate_lp <- function(Y) {

  T <- nrow(Y)

  # Estimate VAR innovations
  U <- as.data.frame(residuals(VAR(Y, p = p, type = "const")))

  # Store responses: 3 variables x 37 horizons
  responses <- matrix(NA_real_, nrow = 3, ncol = H + 1)

  for (h in 0:H) {

    t <- (p + 1):(T - h)

    # Contemporaneous VAR innovations
    data_h <- data.frame(
      ff = U$trans_FEDFUNDS[t - p],
      indpro = U$trans_INDPRO[t - p],
      cpi = U$trans_CPIAUCSL[t - p]
    )

    # Add lagged controls
    for (lag in 1:p) {
      for (v in variables) {
        data_h[[paste0(v, lag)]] <- Y[t - lag, v]
      }
    }

    # Estimate one regression per response variable
    for (j in 1:3) {

      data_h$y <- Y[t + h, j]

      model <- lm(y ~ ., data = data_h)

      responses[j, h + 1] <- coef(model)["ff"]
    }
  }

  responses
}

# Original LP estimates
lp_original <- estimate_lp(Y)
# ----------------------------------------------------------------
# 13.2 Moving block bootstrap
# ----------------------------------------------------------------
bootstrap_sample <- function(Y) {

  T <- nrow(Y)

  # Randomly select starting points of blocks
  starts <- sample(
    1:(T - block_length + 1),
    ceiling(T / block_length),
    replace = TRUE
  )

  # Join blocks and keep the original sample length
  indices <- unlist(
    lapply(starts, function(s) s:(s + block_length - 1))
  )[1:T]

  Y[indices, , drop = FALSE]
}

# Repeat LP estimation on bootstrap samples
lp_boot <- replicate(
  B,
  estimate_lp(bootstrap_sample(Y)),
  simplify = "array"
)

# 95% bootstrap confidence intervals
lp_lower <- apply(lp_boot, c(1, 2), quantile, probs = 0.025)
lp_upper <- apply(lp_boot, c(1, 2), quantile, probs = 0.975)
# ----------------------------------------------------------------
# 13.3 Combine VAR and LP results
# ----------------------------------------------------------------
# Reduced-form VAR results from Section 11
var_irf <- irf_ff_reduced

var_data_plot <- data.frame(
  horizon = rep(0:H, times = 3),
  variable = rep(variables, each = H + 1),
  response = as.vector(var_irf$irf$trans_FEDFUNDS[, variables]),
  lower = as.vector(var_irf$Lower$trans_FEDFUNDS[, variables]),
  upper = as.vector(var_irf$Upper$trans_FEDFUNDS[, variables]),
  method = "VAR"
)

# LP results
lp_data_plot <- data.frame(
  horizon = rep(0:H, times = 3),
  variable = rep(variables, each = H + 1),
  response = as.vector(t(lp_original)),
  lower = as.vector(t(lp_lower)),
  upper = as.vector(t(lp_upper)),
  method = "Local Projections"
)

# Combine
comparison_data <- bind_rows(var_data_plot, lp_data_plot)

comparison_data$variable <- factor(
  comparison_data$variable,
  levels = variables,
  labels = c(
    "Industrial Production",
    "Consumer Price Index",
    "Federal Funds Rate"
  )
)
# ----------------------------------------------------------------
# 13.4 Plot
# ----------------------------------------------------------------
comparison_plot <- ggplot(
  comparison_data,
  aes(horizon, response, colour = method, fill = method)
) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.12, colour = NA) +
  geom_line(linewidth = 1) +
  facet_wrap(~variable, ncol = 3, scales = "free_y") +
  scale_colour_manual(values = c(
    "VAR" = "#174A7E",
    "Local Projections" = "#D97732"
  )) +
  scale_fill_manual(values = c(
    "VAR" = "#174A7E",
    "Local Projections" = "#D97732"
  )) +
  scale_x_continuous(breaks = seq(0, 36, 6)) +
  labs(
    title = "Reduced-form IRFs: VAR vs. Local Projections",
    subtitle = "Responses to a unit FEDFUNDS innovation",
    x = "Months after impulse",
    y = "Response (transformed units)",
    colour = NULL,
    fill = NULL,
    caption = "95% bootstrap CIs"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 17),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

comparison_plot

ggsave(
  "VAR_vs_LP_bootstrap.png",
  comparison_plot,
  width = 13,
  height = 5,
  dpi = 300
)

# ================================================================
# 14. STRUCTURAL LOCAL PROJECTIONS: CHOLESKY IDENTIFICATION

# ================================================================
# AI was used to assist with implementing the structural LPs,
# constructing bootstrap confidence intervals and plotting the results.

library(vars)
library(lpirfs)
library(ggplot2)
library(dplyr)

set.seed(123)
# ----------------------------------------------------------------
# 14.1 Settings
# ----------------------------------------------------------------
variables <- c(
  "trans_INDPRO",
  "trans_CPIAUCSL",
  "trans_FEDFUNDS"
)

labels <- c(
  "Industrial Production",
  "Consumer Price Index",
  "Federal Funds Rate"
)

# The column ordering determines the recursive identification
Y <- as.data.frame(var_data[, variables])

p <- as.integer(p_selected)
H <- 36

B <- 500
block_length <- 48

K <- ncol(Y)
T_obs <- nrow(Y)

stopifnot(
  block_length > p,
  block_length < T_obs,
  T_obs > p + H
)
# ----------------------------------------------------------------
# 14.2 Structural Local Projections (Cholesky)
# ----------------------------------------------------------------
lp_structural <- lpirfs::lp_lin(
  endog_data = Y,
  lags_endog_lin = p,
  trend = 0,
  shock_type = 1,
  confint = 1.96,
  hor = H + 1
)

# shock_type = 1 gives a unit-normalized shock.
# FEDFUNDS is the third variable in the ordering.
# The third impulse is therefore the FEDFUNDS structural shock.

# Keep horizons 0 to 36
lp_original <- lp_structural$irf_lin_mean[, 1:(H + 1), 3]

# Check the contemporaneous responses
print(lp_original[, 1])
# ----------------------------------------------------------------
# 14.3 Moving block bootstrap for structural LPs
# ----------------------------------------------------------------
# Resample blocks of consecutive observations
bootstrap_sample <- function(data) {

  T <- nrow(data)

  starts <- sample(
    1:(T - block_length + 1),
    ceiling(T / block_length),
    replace = TRUE
  )

  indices <- unlist(
    lapply(starts, function(s) s:(s + block_length - 1))
  )[1:T]

  data[indices, , drop = FALSE]
}

# Store bootstrap responses:
# variables x horizons x bootstrap replications
lp_boot <- array(
  NA_real_,
  dim = c(K, H + 1, B)
)

# Re-estimate structural LPs for every bootstrap sample
for (b in 1:B) {

  Y_boot <- bootstrap_sample(Y)

  lp_fit <- tryCatch(
    lpirfs::lp_lin(
      endog_data = Y_boot,
      lags_endog_lin = p,
      trend = 0,
      shock_type = 1,
      confint = 1.96,
      hor = H + 1
    ),
    error = function(e) NULL
  )

  if (!is.null(lp_fit)) {
    lp_boot[, , b] <- lp_fit$irf_lin_mean[, 1:(H + 1), 3]
  }

  if (b %% 100 == 0) {
    cat("Bootstrap iteration:", b, "of", B, "\n")
  }
}

# Check successful bootstrap replications
successful <- sum(!is.na(lp_boot[1, 1, ]))

cat("Successful replications:", successful, "of", B, "\n")

if (successful < 0.95 * B) {
  stop("Too many bootstrap replications failed.")
}

# Calculate 95% percentile bootstrap confidence intervals
lp_lower <- apply(
  lp_boot,
  c(1, 2),
  quantile,
  probs = 0.025,
  na.rm = TRUE
)

lp_upper <- apply(
  lp_boot,
  c(1, 2),
  quantile,
  probs = 0.975,
  na.rm = TRUE
)
# ----------------------------------------------------------------
# 14.4 Structural VAR (same Cholesky ordering)
# ----------------------------------------------------------------
# The existing VAR uses the same variable ordering.
# ortho = TRUE applies Cholesky identification.

var_structural <- vars::irf(
  var_model,
  impulse = "trans_FEDFUNDS",
  response = variables,
  n.ahead = H,
  ortho = TRUE,
  boot = TRUE,
  ci = 0.95,
  runs = B
)

# Normalize the structural VAR response to a unit
# contemporaneous change in transformed FEDFUNDS
impact <- var_structural$irf$trans_FEDFUNDS[
  1, "trans_FEDFUNDS"
]
# ----------------------------------------------------------------
# 14.5 Prepare results for plotting
# ----------------------------------------------------------------
# Reduced-form VAR results from Section 11
rf <- irf_ff_reduced

reduced_data <- data.frame(
  horizon = rep(0:H, times = K),
  variable = rep(variables, each = H + 1),

  response = as.vector(
    rf$irf$trans_FEDFUNDS[, variables]
  ),

  lower = as.vector(
    rf$Lower$trans_FEDFUNDS[, variables]
  ),

  upper = as.vector(
    rf$Upper$trans_FEDFUNDS[, variables]
  ),

  method = "Reduced-form VAR"
)

# Structural VAR results
sv <- var_structural

svar_data <- data.frame(
  horizon = rep(0:H, times = K),
  variable = rep(variables, each = H + 1),

  response = as.vector(
    sv$irf$trans_FEDFUNDS[, variables]
  ) / impact,

  lower = as.vector(
    sv$Lower$trans_FEDFUNDS[, variables]
  ) / impact,

  upper = as.vector(
    sv$Upper$trans_FEDFUNDS[, variables]
  ) / impact,

  method = "Structural VAR"
)

# Structural LP results with bootstrap intervals
lp_data <- data.frame(
  horizon = rep(0:H, times = K),
  variable = rep(variables, each = H + 1),

  response = as.vector(t(lp_original)),
  lower = as.vector(t(lp_lower)),
  upper = as.vector(t(lp_upper)),

  method = "Structural LP"
)

# Combine results
comparison_structural <- bind_rows(
  reduced_data,
  svar_data,
  lp_data
)

comparison_structural$variable <- factor(
  comparison_structural$variable,
  levels = variables,
  labels = labels
)
# ----------------------------------------------------------------
# 14.6 Plot comparison
# ----------------------------------------------------------------
structural_plot <- ggplot(
  comparison_structural,
  aes(
    x = horizon,
    y = response,
    colour = method,
    linetype = method,
    fill = method
  )
) +

  geom_hline(
    yintercept = 0,
    linetype = "dotted",
    colour = "grey60"
  ) +

  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.08,
    colour = NA
  ) +

  geom_line(linewidth = 0.9) +

  facet_wrap(
    ~variable,
    ncol = 3,
    scales = "free_y"
  ) +

  scale_colour_manual(
    values = c(
      "Reduced-form VAR" = "#174A7E",
      "Structural VAR" = "#2A9D8F",
      "Structural LP" = "#D97732"
    )
  ) +

  scale_fill_manual(
    values = c(
      "Reduced-form VAR" = "#174A7E",
      "Structural VAR" = "#2A9D8F",
      "Structural LP" = "#D97732"
    )
  ) +

  scale_linetype_manual(
    values = c(
      "Reduced-form VAR" = "solid",
      "Structural VAR" = "dashed",
      "Structural LP" = "solid"
    )
  ) +

  scale_x_continuous(
    breaks = seq(0, H, 6)
  ) +

  labs(
    title = "Reduced-form VAR vs. Structural VAR and LP",
    subtitle = "FEDFUNDS impulse; Cholesky ordering: INDPRO, CPI, FEDFUNDS",
    x = "Months after impulse",
    y = "Response (transformed units)",
    colour = NULL,
    linetype = NULL,
    fill = NULL,
    caption = paste(
      "95% bootstrap CIs:",
      "bootstrap"
    )
  ) +

  theme_minimal(base_size = 13) +

  theme(
    plot.title = element_text(face = "bold", size = 16),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    panel.spacing = grid::unit(1.2, "lines"),
    plot.background = element_rect(
      fill = "white",
      colour = NA
    )
  )

structural_plot

ggsave(
  "VAR_vs_Structural_LP.png",
  plot = structural_plot,
  width = 13,
  height = 5,
  dpi = 300,
  bg = "white"
)

# ================================================================
# PART III: EXTERNAL MONETARY POLICY SHOCK PROXIES
# TIME SERIES ANALYTICS - SHOCK PROXIES
# Proxy-SVAR using the US Monetary Policy Event Study Database (USMPD)
#
# Variables:
#   INDPRO   : first difference of log industrial production
#   CPIAUCSL : second difference of log CPI
#   FEDFUNDS : first difference of the federal funds rate
#
# Sample: 1994-2019, subject to available observations
# Identification: MP1 and MP2 from FOMC Statements
# IRF horizon: 36 months
# Normalization: +0.25 percentage points in FEDFUNDS on impact
#
# NOTE:
# This produces point-estimate proxy-SVAR IRFs.
# Confidence intervals require a separate bootstrap procedure.

# ================================================================
# ================================================================
# 1. PACKAGES

# ================================================================
packages <- c(
  "readxl", "dplyr", "tidyr",
  "lubridate", "quantmod", "vars"
)

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(quantmod)
library(vars)

# Prevent conflicts with other packages
select <- dplyr::select

# ================================================================
# 2. FILE PATH AND SETTINGS

# ================================================================
file_path <- paste0(
  "C:/Users/koent/OneDrive/Documenten/School/",
  "Master of Econometrics & Operations Research/",
  "Time Series Analytics/Time Series Analytics Project/",
  "USMPD.xlsx"
)

stopifnot(file.exists(file_path))

start_date <- as.Date("1994-01-01")
end_date   <- as.Date("2019-12-01")

horizon <- 36
shock_size <- 0.25

# We use Statements as our main event type.
event_sheet <- "Statements"

# ================================================================
# 3. IMPORT MONETARY POLICY SURPRISES

# ================================================================
usmpd <- read_excel(
  file_path,
  sheet = event_sheet
)

cat("\nUSMPD columns:\n")
print(names(usmpd))

cat("\nFirst observations:\n")
print(head(usmpd[, c("Date", "MP1", "MP2")]))

# Aggregate all statement surprises within a month.
# NA is kept if any event in that month lacks the relevant measure.

monthly_shocks <- usmpd %>%
  mutate(
    Date = as.Date(Date),
    month = floor_date(Date, "month")
  ) %>%
  filter(
    month >= start_date,
    month <= end_date
  ) %>%
  group_by(month) %>%
  summarise(
    MP1 = if (any(is.na(MP1))) NA_real_
    else sum(MP1),
    MP2 = if (any(is.na(MP2))) NA_real_
    else sum(MP2),
    events = n(),
    .groups = "drop"
  )

# Create a complete monthly timeline.
# Months without announcements receive zero surprise.

monthly_shocks <- tibble(
  month = seq(
    start_date,
    end_date,
    by = "month"
  )
) %>%
  left_join(monthly_shocks, by = "month") %>%
  mutate(
    no_event = is.na(events),
    MP1 = if_else(no_event, 0, MP1),
    MP2 = if_else(no_event, 0, MP2),
    events = replace_na(events, 0L)
  ) %>%
  dplyr::select(-no_event)

cat("\nMonthly proxy summary:\n")
print(summary(monthly_shocks[, c("MP1", "MP2")]))

cat("\nNumber of months with announcements:\n")
print(sum(monthly_shocks$events > 0))

# ================================================================
# 4. PLOT THE TWO PROXIES

# ================================================================
par(mfrow = c(1,2))

plot(
  monthly_shocks$month,
  monthly_shocks$MP1,
  type = "h",
  main = "MP1: Current Target Surprise",
  xlab = "Year",
  ylab = "Percentage points"
)
abline(h = 0, lty = 2)

plot(
  monthly_shocks$month,
  monthly_shocks$MP2,
  type = "h",
  main = "MP2: Next-Meeting Target Surprise",
  xlab = "Year",
  ylab = "Percentage points"
)
abline(h = 0, lty = 2)

par(mfrow = c(1, 1))

# ================================================================
# 5. DOWNLOAD FRED DATA

# ================================================================
fred_env <- new.env()

getSymbols(
  c("INDPRO", "CPIAUCSL", "FEDFUNDS"),
  src = "FRED",
  env = fred_env,
  auto.assign = TRUE
)

fred_data <- merge(
  fred_env$INDPRO,
  fred_env$CPIAUCSL,
  fred_env$FEDFUNDS
)

fred_df <- data.frame(
  month = as.Date(index(fred_data)),
  INDPRO = as.numeric(fred_data$INDPRO),
  CPIAUCSL = as.numeric(fred_data$CPIAUCSL),
  FEDFUNDS = as.numeric(fred_data$FEDFUNDS)
) %>%
  arrange(month) %>%
  filter(month <= end_date)

# ================================================================
# 6. APPLY OUR VAR TRANSFORMATIONS

# ================================================================
fred_df <- fred_df %>%
  mutate(
    trans_INDPRO = c(
      NA,
      diff(log(INDPRO))
    ),
    trans_CPIAUCSL = c(
      NA, NA,
      diff(log(CPIAUCSL), differences = 2)
    ),
    trans_FEDFUNDS = c(
      NA,
      diff(FEDFUNDS)
    )
  )

# Merge with external instruments.

combined <- fred_df %>%
  inner_join(monthly_shocks, by = "month") %>%
  select(
    month,
    trans_INDPRO,
    trans_CPIAUCSL,
    trans_FEDFUNDS,
    MP1,
    MP2
  ) %>%
  arrange(month)

# Keep complete macro observations.
# Proxy missingness is handled separately below.

combined <- combined %>%
  filter(
    if_all(
      c(
        trans_INDPRO,
        trans_CPIAUCSL,
        trans_FEDFUNDS
      ),
      ~ !is.na(.x)
    )
  )

cat("\nCombined data:\n")
print(head(combined))
print(tail(combined))

# ================================================================
# 7. ESTIMATE VAR ON THE OVERLAPPING SAMPLE

# ================================================================
var_data <- combined %>%
  select(
    trans_INDPRO,
    trans_CPIAUCSL,
    trans_FEDFUNDS
  )

# Select lag length using BIC.
# We also report AIC and HQ.

lag_selection <- VARselect(
  var_data,
  lag.max = 14,
  type = "const"
)

cat("\nVAR lag selection:\n")
print(lag_selection$selection)

p <- as.integer(
  lag_selection$selection["SC(n)"]
)

cat("\nSelected lag order (BIC):", p, "\n")

var_model_proxy <- VAR(
  var_data,
  p = p,
  type = "const"
)

cat("\nVAR summary:\n")
print(summary(var_model_proxy))

# ================================================================
# 8. EXTRACT VAR RESIDUALS

# ================================================================
u <- residuals(var_model_proxy)

# A VAR(p) loses its first p observations.
# Align residuals and proxy observations by month.

residual_dates <- tail(
  combined$month,
  nrow(u)
)

residual_df <- data.frame(
  month = residual_dates,
  u_INDPRO = u[, 1],
  u_CPI = u[, 2],
  u_FEDFUNDS = u[, 3]
)

proxy_data <- residual_df %>%
  left_join(
    monthly_shocks %>%
      select(month, MP1, MP2),
    by = "month"
  )

cat("\nAligned residuals and proxies:\n")
print(head(proxy_data))

# ================================================================
# 9. TEST PROXY RELEVANCE

# ================================================================
# A valid instrument must be correlated with the
# structural monetary policy shock.
#
# Regressing the FEDFUNDS residual on the proxy gives
# a useful initial diagnostic, but it is NOT a formal
# test of all proxy-SVAR identification assumptions.

test_proxy <- function(data, proxy_name) {

  d <- data %>%
    select(
      u_FEDFUNDS,
      all_of(proxy_name)
    ) %>%
    na.omit()

  names(d)[2] <- "z"

  model <- lm(
    u_FEDFUNDS ~ z,
    data = d
  )

  model_summary <- summary(model)

  f_value <- unname(
    model_summary$fstatistic[1]
  )

  result <- data.frame(
    Proxy = proxy_name,
    Observations = nrow(d),
    Correlation = cor(
      d$u_FEDFUNDS,
      d$z
    ),
    R_squared = model_summary$r.squared,
    F_statistic = f_value,
    P_value = pf(
      f_value,
      df1 = 1,
      df2 = nrow(d) - 2,
      lower.tail = FALSE
    )
  )

  return(result)
}

proxy_tests <- bind_rows(
  test_proxy(proxy_data, "MP1"),
  test_proxy(proxy_data, "MP2")
)

cat("\n====================================\n")
cat("PROXY RELEVANCE DIAGNOSTICS\n")
cat("====================================\n")
print(proxy_tests)

# Do not automatically select the proxy with the
# largest F-statistic. Economic validity matters too.

# ================================================================
# 10. IDENTIFY THE MONETARY POLICY IMPACT VECTOR

# ================================================================
# Structural representation:
#
# u_t = B * epsilon_t
#
# Valid external instrument z_t:
#
# Cov(u_t, z_t) = c * b_MP
#
# Hence the covariance vector identifies the
# contemporaneous impact vector up to scale.
#
# Normalize the impact on the first difference of
# FEDFUNDS to +0.25 percentage points.

identify_proxy_impact <- function(
    proxy_name,
    data,
    shock_size = 0.25
) {

  d <- data %>%
    select(
      u_INDPRO,
      u_CPI,
      u_FEDFUNDS,
      all_of(proxy_name)
    ) %>%
    na.omit()

  u_matrix <- as.matrix(d[, 1:3])
  z <- d[[4]]

  covariance_vector <- as.numeric(
    cov(u_matrix, z)
  )

  names(covariance_vector) <- c(
    "INDPRO",
    "CPI",
    "FEDFUNDS"
  )

  # The FEDFUNDS covariance must not be near zero.
  # Check instrument strength separately.

  if (
    abs(covariance_vector[3]) <
    1e-10
  ) {
    stop("FEDFUNDS covariance is near zero.")
  }

  impact <- (
    covariance_vector /
      covariance_vector[3]
  ) * shock_size

  return(impact)
}

impact_MP1 <- identify_proxy_impact(
  "MP1",
  proxy_data,
  shock_size
)

impact_MP2 <- identify_proxy_impact(
  "MP2",
  proxy_data,
  shock_size
)

cat("\nMP1 impact vector:\n")
print(impact_MP1)

cat("\nMP2 impact vector:\n")
print(impact_MP2)

# ================================================================
# 11. CALCULATE PROXY-SVAR IRFs

# ================================================================
# Reduced-form VAR moving-average representation:
#
# y_t = ... + Phi_0 u_t + Phi_1 u_(t-1) + ...
#
# Structural IRF:
#
# IRF(h) = Phi_h * b_MP

calculate_proxy_irf <- function(
    model,
    impact,
    horizon = 36
) {

  phi_array <- Phi(
    model,
    nstep = horizon
  )

  k <- length(impact)

  irf_matrix <- matrix(
    NA_real_,
    nrow = horizon + 1,
    ncol = k
  )

  for (h in 0:horizon) {

    phi_h <- matrix(
      phi_array[, , h + 1],
      nrow = k,
      ncol = k
    )

    irf_matrix[h + 1, ] <-
      as.numeric(phi_h %*% impact)
  }

  colnames(irf_matrix) <- c(
    "INDPRO",
    "CPI",
    "FEDFUNDS"
  )

  return(irf_matrix)
}

irf_MP1 <- calculate_proxy_irf(
  var_model_proxy,
  impact_MP1,
  horizon
)

irf_MP2 <- calculate_proxy_irf(
  var_model_proxy,
  impact_MP2,
  horizon
)

# ================================================================
# 12. CONVERT RESPONSES BACK TO LEVELS

# ================================================================
# INDPRO:   first difference log -> cumulative sum
# CPI:      second difference log -> double cumulative sum
# FEDFUNDS: first difference level -> cumulative sum
#
# Multiply log responses by 100 to obtain approximate
# percentage deviations from baseline.

to_level_irf <- function(irf_matrix) {

  level_irf <- irf_matrix

  level_irf[, "INDPRO"] <-
    100 * cumsum(
      irf_matrix[, "INDPRO"]
    )

  level_irf[, "CPI"] <-
    100 * cumsum(
      cumsum(
        irf_matrix[, "CPI"]
      )
    )

  level_irf[, "FEDFUNDS"] <-
    cumsum(
      irf_matrix[, "FEDFUNDS"]
    )

  return(level_irf)
}

level_MP1 <- to_level_irf(irf_MP1)
level_MP2 <- to_level_irf(irf_MP2)

# ================================================================
# 13. PLOT IRFs: MP1 VERSUS MP2

# ================================================================
plot_proxy_irfs <- function(
    irf1,
    irf2,
    title_text
) {

  h <- 0:(nrow(irf1) - 1)

  variable_names <- c(
    "INDPRO",
    "CPI",
    "FEDFUNDS"
  )

  y_labels <- c(
    "Industrial production (%)",
    "CPI (%)",
    "FEDFUNDS (percentage points)"
  )

  old_par <- par(mfrow = c(1, 3))

  for (j in 1:3) {

    y_range <- range(
      irf1[, j],
      irf2[, j],
      0,
      finite = TRUE
    )

    plot(
      h,
      irf1[, j],
      type = "l",
      lwd = 2,
      col = "blue",
      ylim = y_range,
      xlab = "Months after shock",
      ylab = y_labels[j],
      main = paste(
        title_text,
        "-",
        variable_names[j]
      )
    )

    lines(
      h,
      irf2[, j],
      lwd = 2,
      col = "red",
      lty = 2
    )

    abline(h = 0, lty = 3)

    legend(
      "topright",
      legend = c("MP1", "MP2"),
      col = c("blue", "red"),
      lty = c(1, 2),
      bty = "n"
    )
  }

  par(old_par)
}

# Responses in transformed variables
plot_proxy_irfs(
  irf_MP1,
  irf_MP2,
  "Transformed IRF"
)

# Responses in levels
plot_proxy_irfs(
  level_MP1,
  level_MP2,
  "Level IRF"
)

# ================================================================
# 14. EXPORT RESULTS

# ================================================================
output_dir <- dirname(file_path)

irf_results <- data.frame(
  Month = 0:horizon,

  MP1_INDPRO = level_MP1[, "INDPRO"],
  MP1_CPI = level_MP1[, "CPI"],
  MP1_FEDFUNDS = level_MP1[, "FEDFUNDS"],

  MP2_INDPRO = level_MP2[, "INDPRO"],
  MP2_CPI = level_MP2[, "CPI"],
  MP2_FEDFUNDS = level_MP2[, "FEDFUNDS"]
)

write.csv(
  proxy_tests,
  file.path(
    output_dir,
    "proxy_relevance_tests.csv"
  ),
  row.names = FALSE
)

write.csv(
  irf_results,
  file.path(
    output_dir,
    "proxy_svar_irfs.csv"
  ),
  row.names = FALSE
)

cat("\n====================================\n")
cat("PROXY-SVAR ANALYSIS COMPLETE\n")
cat("====================================\n")

cat("\nSelected VAR lag:", p, "\n")
cat("Shock normalization:", shock_size, "pp\n")

cat("\nRelevance diagnostics:\n")
print(proxy_tests)

cat("\nContemporaneous impact vectors:\n")
print(
  rbind(
    MP1 = impact_MP1,
    MP2 = impact_MP2
  )
)

cat("\nResults exported to:\n")
cat(output_dir, "\n")

# ================================================================
# 15. INTERNAL-INSTRUMENT SVAR
# Plagborg-Moller and Wolf (2021)

# ================================================================
library(dplyr)
library(vars)
# ----------------------------------------------------------------
# 15.1 Prepare augmented VAR data
# ----------------------------------------------------------------
# 'combined' comes from Section 6 of our existing script.
# It contains the transformed macro variables and monthly MP1/MP2.

run_internal_svar <- function(instrument_name,
                              horizon = 36,
                              shock_size = 0.25,
                              runs = 500) {

  variables <- c(
    instrument_name,
    "trans_INDPRO",
    "trans_CPIAUCSL",
    "trans_FEDFUNDS"
  )

  # The instrument must be FIRST.
  data_internal <- combined |>
    dplyr::select(dplyr::all_of(variables)) |>
    tidyr::drop_na()

  data_internal <- as.data.frame(data_internal)
# ----------------------------------------------------------------
  # 15.2 Select lag length and estimate augmented VAR
# ----------------------------------------------------------------
  lag_selection <- vars::VARselect(
    data_internal,
    lag.max = 12,
    type = "const"
  )

  p <- as.integer(lag_selection$selection["SC(n)"])

  model <- vars::VAR(
    data_internal,
    p = p,
    type = "const"
  )
# ----------------------------------------------------------------
  # 15.3 Cholesky IRFs: shock to instrument
# ----------------------------------------------------------------
  set.seed(123)

  irf_result <- vars::irf(
    model,
    impulse = instrument_name,
    response = c(
      "trans_INDPRO",
      "trans_CPIAUCSL",
      "trans_FEDFUNDS"
    ),
    n.ahead = horizon,
    ortho = TRUE,
    boot = TRUE,
    ci = 0.95,
    runs = runs
  )
# ----------------------------------------------------------------
  # 15.4 Normalize to +0.25 percentage points in FEDFUNDS
# ----------------------------------------------------------------
  raw <- irf_result$irf[[instrument_name]]
  lower <- irf_result$Lower[[instrument_name]]
  upper <- irf_result$Upper[[instrument_name]]

  # FEDFUNDS is modeled in first differences.
  # Its impact response must equal +0.25.
  impact <- raw[1, "trans_FEDFUNDS"]

  if (!is.finite(impact) || abs(impact) < 1e-10) {
    stop("Instrument has nearly zero FEDFUNDS impact.")
  }

  scale_factor <- shock_size / impact

  scaled <- raw * scale_factor

  # Negative scaling reverses confidence interval endpoints.
  scaled_lower <- pmin(
    lower * scale_factor,
    upper * scale_factor
  )

  scaled_upper <- pmax(
    lower * scale_factor,
    upper * scale_factor
  )
# ----------------------------------------------------------------
  # 15.5 Convert transformed IRFs to level responses
# ----------------------------------------------------------------
  to_levels <- function(x) {
    cbind(
      INDPRO = 100 * cumsum(x[, "trans_INDPRO"]),
      CPI = 100 * cumsum(cumsum(x[, "trans_CPIAUCSL"])),
      FEDFUNDS = cumsum(x[, "trans_FEDFUNDS"])
    )
  }

  level_irf <- to_levels(scaled)

  # The following bands are transformations of pointwise
  # transformed-response bands, NOT valid level-response
  # confidence intervals. We therefore do not plot them.
# ----------------------------------------------------------------
  # 15.6 Plot level IRFs
# ----------------------------------------------------------------
  old_par <- par(mfrow = c(1, 3))

  on.exit(par(old_par), add = TRUE)

  horizons <- 0:horizon

  titles <- c(
    "Industrial Production",
    "Consumer Price Index",
    "Federal Funds Rate"
  )

  units <- c(
    "Approximate % change",
    "Approximate % change",
    "Percentage points"
  )

  for (j in 1:3) {

    plot(
      horizons,
      level_irf[, j],
      type = "l",
      lwd = 2,
      col = "blue",
      xlab = "Months",
      ylab = units[j],
      main = paste(instrument_name, "-", titles[j])
    )

    abline(h = 0, lty = 2, col = "gray")
  }

  cat(
    "\nInstrument:", instrument_name,
    "\nSelected lags:", p,
    "\nObservations:", nrow(data_internal),
    "\nInitial FEDFUNDS response:", scaled[1, "trans_FEDFUNDS"],
    "\n"
  )

  return(list(
    model = model,
    lag_order = p,
    irf_transformed = scaled,
    lower_transformed = scaled_lower,
    upper_transformed = scaled_upper,
    irf_levels = level_irf
  ))
}

# ================================================================
# 15.7 Run for MP1 and MP2

# ================================================================
internal_MP1 <- run_internal_svar("MP1")

internal_MP2 <- run_internal_svar("MP2")

# ================================================================
# 15.8 Export results

# ================================================================
internal_results <- data.frame(
  horizon = 0:horizon,

  MP1_INDPRO = internal_MP1$irf_levels[, "INDPRO"],
  MP1_CPI = internal_MP1$irf_levels[, "CPI"],
  MP1_FEDFUNDS = internal_MP1$irf_levels[, "FEDFUNDS"],

  MP2_INDPRO = internal_MP2$irf_levels[, "INDPRO"],
  MP2_CPI = internal_MP2$irf_levels[, "CPI"],
  MP2_FEDFUNDS = internal_MP2$irf_levels[, "FEDFUNDS"]
)

write.csv(
  internal_results,
  "internal_instrument_svar_irfs.csv",
  row.names = FALSE
)

print(proxy_tests)

# ================================================================
# 16. COMPARISON OF IRFs WITH 95% CONFIDENCE BANDS
# Three figures, each containing Cholesky, MP1 and MP2

# ================================================================
library(vars)

horizon <- 36
shock_size <- 0.25
n_boot <- 500
# ----------------------------------------------------------------
# 16.1 Convert transformed IRFs to levels
# ----------------------------------------------------------------
to_levels <- function(x) {
  cbind(
    INDPRO = 100 * cumsum(x[, "trans_INDPRO"]),
    CPI = 100 * cumsum(cumsum(x[, "trans_CPIAUCSL"])),
    FEDFUNDS = cumsum(x[, "trans_FEDFUNDS"])
  )
}
# ----------------------------------------------------------------
# 16.2 Calculate normalized IRFs from a fitted VAR
# ----------------------------------------------------------------
get_irf_levels <- function(model, impulse, shock_size = 0.25) {

  raw <- vars::irf(
    model,
    impulse = impulse,
    response = c(
      "trans_INDPRO",
      "trans_CPIAUCSL",
      "trans_FEDFUNDS"
    ),
    n.ahead = horizon,
    ortho = TRUE,
    boot = FALSE
  )$irf[[impulse]]

  impact <- raw[1, "trans_FEDFUNDS"]

  if (!is.finite(impact) || abs(impact) < 1e-8) {
    stop("FEDFUNDS impact is too small for normalization.")
  }

  scaled <- raw * (shock_size / impact)

  to_levels(scaled)
}
# ----------------------------------------------------------------
# 16.3 Bootstrap confidence intervals
# ----------------------------------------------------------------
bootstrap_irfs <- function(model, impulse, B = 500) {

  point <- get_irf_levels(model, impulse)

  p <- model$p
  Y <- as.matrix(model$y)
  n <- nrow(Y)
  K <- ncol(Y)

  # Original VAR residuals, centered
  U <- as.matrix(residuals(model))
  U <- sweep(U, 2, colMeans(U), "-")

  # Fitted values for observations p+1,...,n
  fitted_Y <- Y[(p + 1):n, , drop = FALSE] - U

  # Coefficient matrices for recursive simulation
  A <- vars::Acoef(model)
  intercept <- sapply(model$varresult, function(eq) {
    cf <- coef(eq)
    if ("const" %in% names(cf)) cf["const"] else 0
  })

  boot_results <- array(
    NA_real_,
    dim = c(horizon + 1, 3, B),
    dimnames = list(
      NULL,
      c("INDPRO", "CPI", "FEDFUNDS"),
      NULL
    )
  )

  set.seed(123)

  for (b in 1:B) {

    # Resample complete residual vectors to preserve
    # contemporaneous correlation across equations
    idx <- sample(seq_len(nrow(U)), nrow(U), replace = TRUE)
    U_boot <- U[idx, , drop = FALSE]

    Y_boot <- Y
    Y_boot[1:p, ] <- Y[1:p, ]

    for (t in (p + 1):n) {

      prediction <- as.numeric(intercept)

      for (lag in 1:p) {
        prediction <- prediction +
          as.numeric(A[[lag]] %*% Y_boot[t - lag, ])
      }

      Y_boot[t, ] <- prediction + U_boot[t - p, ]
    }

    boot_model <- tryCatch(
      vars::VAR(
        as.data.frame(Y_boot),
        p = p,
        type = "const"
      ),
      error = function(e) NULL
    )

    if (is.null(boot_model)) next

    boot_results[, , b] <- tryCatch(
      get_irf_levels(boot_model, impulse),
      error = function(e) matrix(
        NA_real_, horizon + 1, 3
      )
    )
  }

  lower <- upper <- point

  for (j in 1:3) {
    for (h in 1:(horizon + 1)) {

      values <- boot_results[h, j, ]
      values <- values[is.finite(values)]

      if (length(values) >= B * 0.8) {
        lower[h, j] <- quantile(values, 0.025)
        upper[h, j] <- quantile(values, 0.975)
      } else {
        lower[h, j] <- NA_real_
        upper[h, j] <- NA_real_
      }
    }
  }

  list(
    point = point,
    lower = lower,
    upper = upper
  )
}
# ----------------------------------------------------------------
# 16.4 Calculate Cholesky, MP1 and MP2 confidence intervals
# ----------------------------------------------------------------
cat("Bootstrapping Cholesky...\n")

result_chol <- bootstrap_irfs(
  var_model,
  "trans_FEDFUNDS",
  B = n_boot
)

cat("Bootstrapping MP1...\n")

result_MP1 <- bootstrap_irfs(
  internal_MP1$model,
  "MP1",
  B = n_boot
)

cat("Bootstrapping MP2...\n")

result_MP2 <- bootstrap_irfs(
  internal_MP2$model,
  "MP2",
  B = n_boot
)
# ----------------------------------------------------------------
# 16.5 Plot three methods side by side for one variable
# ----------------------------------------------------------------
plot_comparison <- function(variable, title, ylab) {

  results <- list(
    Cholesky = result_chol,
    MP1 = result_MP1,
    MP2 = result_MP2
  )

  colors <- c("black", "blue", "red")

  old_par <- par(mfrow = c(1, 3))
  on.exit(par(old_par), add = TRUE)

  all_values <- unlist(lapply(results, function(x) {
    c(
      x$point[, variable],
      x$lower[, variable],
      x$upper[, variable]
    )
  }))

  ylim <- range(c(0, all_values), na.rm = TRUE)

  for (i in 1:3) {

    res <- results[[i]]

    months <- 0:horizon

    point <- res$point[, variable]
    lower <- res$lower[, variable]
    upper <- res$upper[, variable]

    plot(
      months,
      point,
      type = "n",
      ylim = ylim,
      xlab = "Months",
      ylab = ylab,
      main = paste(title, "-", names(results)[i])
    )

    polygon(
      c(months, rev(months)),
      c(lower, rev(upper)),
      col = adjustcolor(colors[i], alpha.f = 0.18),
      border = NA
    )

    abline(h = 0, lty = 2, col = "gray")

    lines(
      months,
      point,
      col = colors[i],
      lwd = 2
    )
  }
}

# ================================================================
# Figure 1: Industrial Production

# ================================================================
plot_comparison(
  "INDPRO",
  "Industrial Production",
  "Approximate % change"
)

# ================================================================
# Figure 2: Consumer Price Index

# ================================================================
plot_comparison(
  "CPI",
  "Consumer Price Index",
  "Approximate % change"
)

# ================================================================
# Figure 3: Federal Funds Rate

# ================================================================
plot_comparison(
  "FEDFUNDS",
  "Federal Funds Rate",
  "Percentage points"
)
