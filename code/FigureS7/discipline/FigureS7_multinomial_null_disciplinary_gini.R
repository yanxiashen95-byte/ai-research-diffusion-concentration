library(tidyverse)
library(grid)
library(scales)

# ============================================================
# Supplementary Figure S11
# Finite-sample sensitivity of the six-field disciplinary Gini
# ============================================================

input_file <- "D:/Python/WOS_AI/OECD_Natural_Sciences_1990_2025/annual_natural_field_country_counts.csv"
output_dir <- "D:/博后/论文/3/需要上传的数据和代码/code/Figure3"

start_year <- 2000L
end_year <- 2025L
study_years <- start_year:end_year
B <- 1000L
set.seed(20260902)

field_levels <- c(
  "Mathematics",
  "Computer and information sciences",
  "Physical sciences",
  "Chemical sciences",
  "Earth and related environmental sciences",
  "Biological sciences"
)

# Raw Gini over six fields; theoretical maximum = 5/6.
gini_fun <- function(x) {
  x <- as.numeric(x)
  if (length(x) != 6L || any(!is.finite(x)) || any(x < 0) || sum(x) <= 0) {
    return(NA_real_)
  }
  x <- sort(x)
  n <- length(x)
  sum((2 * seq_along(x) - n - 1) * x) / (n * sum(x))
}

# Fast Gini calculation for a 6 x B simulation matrix.
gini_matrix <- function(x) {
  x_sorted <- apply(x, 2, sort)
  coeff <- 2 * seq_len(nrow(x_sorted)) - nrow(x_sorted) - 1
  colSums(x_sorted * coeff) / (nrow(x_sorted) * colSums(x_sorted))
}

sen_slope <- function(year, value) {
  ok <- is.finite(year) & is.finite(value)
  year <- year[ok]
  value <- value[ok]
  if (length(value) < 2L) return(NA_real_)
  slopes <- c()
  for (i in seq_len(length(value) - 1L)) {
    slopes <- c(slopes, (value[(i + 1L):length(value)] - value[i]) /
      (year[(i + 1L):length(year)] - year[i]))
  }
  median(slopes)
}

# -------------------------
# 1. Build complete six-field vectors
# Missing fields are true zero counts, not deletion criteria.
# -------------------------
field_long <- read_csv(input_file, show_col_types = FALSE) %>%
  transmute(
    year = as.integer(year),
    country = str_squish(country),
    field = oecd_field,
    publications = as.numeric(publication_count)
  ) %>%
  filter(
    between(year, start_year, end_year),
    !is.na(country), country != "", country != "Unknown",
    field %in% field_levels,
    !is.na(publications), publications >= 0
  ) %>%
  group_by(country, year, field) %>%
  summarise(publications = sum(publications), .groups = "drop")

all_countries <- sort(unique(field_long$country))

complete_long <- expand_grid(
  country = all_countries,
  year = study_years,
  field = field_levels
) %>%
  left_join(field_long, by = c("country", "year", "field")) %>%
  mutate(publications = replace_na(publications, 0))

country_year <- complete_long %>%
  pivot_wider(names_from = field, values_from = publications) %>%
  arrange(country, year)

field_matrix <- as.matrix(country_year[, field_levels])
storage.mode(field_matrix) <- "numeric"
country_year$total_publications <- rowSums(field_matrix)
country_year$observed_gini <- apply(field_matrix, 1, gini_fun)

# Only active country-years enter the annual comparison.
country_year <- country_year %>%
  filter(total_publications > 0, !is.na(observed_gini))

field_matrix <- as.matrix(country_year[, field_levels])
storage.mode(field_matrix) <- "numeric"

# -------------------------
# 2. Estimate fixed country-specific field propensities
# from pooled counts over the complete study window.
# -------------------------
country_propensity <- complete_long %>%
  group_by(country, field) %>%
  summarise(pooled_publications = sum(publications), .groups = "drop") %>%
  group_by(country) %>%
  mutate(
    pooled_total = sum(pooled_publications),
    fixed_propensity = pooled_publications / pooled_total
  ) %>%
  ungroup() %>%
  filter(pooled_total > 0) %>%
  select(country, field, pooled_publications, pooled_total, fixed_propensity)

propensity_wide <- country_propensity %>%
  select(country, field, fixed_propensity) %>%
  pivot_wider(names_from = field, values_from = fixed_propensity) %>%
  arrange(country)

propensity_matrix <- as.matrix(propensity_wide[, field_levels])
storage.mode(propensity_matrix) <- "numeric"
rownames(propensity_matrix) <- propensity_wide$country

# -------------------------
# 3. Country-year multinomial null simulation
# X_cy ~ Multinomial(N_cy, p_c), with N_cy observed and p_c fixed.
# The annual statistic exactly follows Figure 3a: publication-weighted
# mean of country-level Gini coefficients.
# -------------------------
n_years <- length(study_years)
annual_null_weighted_sum <- matrix(0, nrow = n_years, ncol = B)
annual_total_weight <- numeric(n_years)

country_year$null_expected_gini <- NA_real_
country_year$null_lwr_95 <- NA_real_
country_year$null_upr_95 <- NA_real_

for (i in seq_len(nrow(country_year))) {
  ctry <- country_year$country[i]
  yr <- country_year$year[i]
  n_pub <- as.integer(round(country_year$total_publications[i]))
  p_fixed <- propensity_matrix[ctry, ]

  simulated_counts <- rmultinom(B, size = n_pub, prob = p_fixed)
  simulated_gini <- gini_matrix(simulated_counts)

  country_year$null_expected_gini[i] <- mean(simulated_gini)
  country_year$null_lwr_95[i] <- quantile(simulated_gini, 0.025, names = FALSE)
  country_year$null_upr_95[i] <- quantile(simulated_gini, 0.975, names = FALSE)

  year_index <- match(yr, study_years)
  annual_null_weighted_sum[year_index, ] <-
    annual_null_weighted_sum[year_index, ] + simulated_gini * n_pub
  annual_total_weight[year_index] <- annual_total_weight[year_index] + n_pub
}

annual_null_matrix <- sweep(
  annual_null_weighted_sum,
  MARGIN = 1,
  STATS = annual_total_weight,
  FUN = "/"
)

# -------------------------
# 4. Observed and null annual series
# -------------------------
annual_observed <- country_year %>%
  group_by(year) %>%
  summarise(
    observed_gini = weighted.mean(observed_gini, total_publications),
    n_countries = n_distinct(country),
    median_country_year_volume = median(total_publications),
    total_publications = sum(total_publications),
    .groups = "drop"
  ) %>%
  arrange(year)

annual_null <- tibble(
  year = study_years,
  null_expected_gini = rowMeans(annual_null_matrix),
  null_lwr_95 = apply(annual_null_matrix, 1, quantile, probs = 0.025),
  null_upr_95 = apply(annual_null_matrix, 1, quantile, probs = 0.975)
)

annual_null_comparison <- annual_observed %>%
  left_join(annual_null, by = "year") %>%
  mutate(
    adjusted_gini = observed_gini - null_expected_gini,
    adjusted_lwr_95 = observed_gini - null_upr_95,
    adjusted_upr_95 = observed_gini - null_lwr_95
  )

# Add country-year null-adjusted values for audit and reuse.
country_year <- country_year %>%
  mutate(
    adjusted_gini = observed_gini - null_expected_gini
  ) %>%
  arrange(country, year)

# -------------------------
# 5. Minimum-volume threshold sensitivity
# -------------------------
thresholds <- c(1L, 20L, 50L)

threshold_series <- map_dfr(thresholds, function(threshold) {
  country_year %>%
    filter(total_publications >= threshold) %>%
    group_by(year) %>%
    summarise(
      gini = weighted.mean(observed_gini, total_publications),
      n_countries = n_distinct(country),
      total_publications = sum(total_publications),
      .groups = "drop"
    ) %>%
    mutate(
      threshold = threshold,
      threshold_label = case_when(
        threshold == 1L ~ "All active country-years",
        TRUE ~ paste0("At least ", threshold, " publications")
      )
    )
}) %>%
  mutate(
    threshold_label = factor(
      threshold_label,
      levels = c(
        "All active country-years",
        "At least 20 publications",
        "At least 50 publications"
      )
    )
  )

# -------------------------
# 6. Trend summary
# -------------------------
trend_summary <- bind_rows(
  tibble(
    series = c("Observed", "Null expected", "Null adjusted"),
    sen_slope_gini_per_year = c(
      sen_slope(annual_null_comparison$year, annual_null_comparison$observed_gini),
      sen_slope(annual_null_comparison$year, annual_null_comparison$null_expected_gini),
      sen_slope(annual_null_comparison$year, annual_null_comparison$adjusted_gini)
    )
  ),
  threshold_series %>%
    group_by(threshold_label) %>%
    summarise(
      series = paste0("Threshold: ", as.character(first(threshold_label))),
      sen_slope_gini_per_year = sen_slope(year, gini),
      .groups = "drop"
    ) %>%
    select(series, sen_slope_gini_per_year)
) %>%
  mutate(sen_slope_gini_points_per_year = sen_slope_gini_per_year * 100)

# -------------------------
# 7. Save all analysis outputs
# -------------------------
write_csv(
  annual_null_comparison,
  file.path(output_dir, "FigureS11a_b_multinomial_null_annual_results.csv")
)

write_csv(
  threshold_series,
  file.path(output_dir, "FigureS11c_volume_threshold_sensitivity.csv")
)

write_csv(
  trend_summary,
  file.path(output_dir, "FigureS11_sen_slope_summary.csv")
)

write_csv(
  country_year,
  file.path(output_dir, "FigureS11_country_year_null_model_results.csv")
)

write_csv(
  country_propensity,
  file.path(output_dir, "FigureS11_fixed_country_field_propensities.csv")
)

# -------------------------
# 8. Plot three panels
# -------------------------
observed_col <- "#FF8081"
null_col <- "#53A8E1"
adjusted_col <- "#7A68A6"
threshold_cols <- c(
  "All active country-years" = "#FF8081",
  "At least 20 publications" = "#53A8E1",
  "At least 50 publications" = "#F2C14E"
)

theme_s11 <- theme_classic(base_size = 15) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.title = element_text(size = 15, color = "black"),
    axis.text = element_text(size = 13, color = "black"),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(size = 15, face = "bold", hjust = 0),
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.position = "bottom",
    plot.margin = margin(8, 10, 8, 8)
  )

p_a <- ggplot(annual_null_comparison, aes(x = year)) +
  geom_ribbon(
    aes(ymin = null_lwr_95, ymax = null_upr_95),
    fill = null_col, alpha = 0.20
  ) +
  geom_line(aes(y = null_expected_gini, color = "Null expectation"), linewidth = 1.2) +
  geom_line(aes(y = observed_gini, color = "Observed"), linewidth = 1.35) +
  geom_point(aes(y = observed_gini, color = "Observed"), size = 2.1) +
  scale_color_manual(values = c("Observed" = observed_col, "Null expectation" = null_col)) +
  scale_x_continuous(breaks = seq(2000, 2025, 5), limits = c(2000, 2025)) +
  labs(
    x = "Year", y = "Publication-weighted Gini",
    title = "a  Observed and multinomial-null Gini"
  ) +
  theme_s11

p_b <- ggplot(annual_null_comparison, aes(x = year, y = adjusted_gini)) +
  geom_ribbon(
    aes(ymin = adjusted_lwr_95, ymax = adjusted_upr_95),
    fill = adjusted_col, alpha = 0.20
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.7) +
  geom_line(linewidth = 1.3, color = adjusted_col) +
  geom_point(size = 2.1, color = adjusted_col) +
  scale_x_continuous(breaks = seq(2000, 2025, 5), limits = c(2000, 2025)) +
  labs(
    x = "Year", y = "Observed - expected Gini",
    title = "b  Null-adjusted disciplinary inequality"
  ) +
  theme_s11 +
  theme(legend.position = "none")

p_c <- ggplot(
  threshold_series,
  aes(x = year, y = gini, color = threshold_label, group = threshold_label)
) +
  geom_line(linewidth = 1.15) +
  geom_point(size = 1.9) +
  scale_color_manual(
    values = threshold_cols,
    labels = c("All active", "N >= 20", "N >= 50")
  ) +
  scale_x_continuous(breaks = seq(2000, 2025, 5), limits = c(2000, 2025)) +
  labs(
    x = "Year", y = "Publication-weighted Gini",
    title = "c  Minimum-volume sensitivity"
  ) +
  theme_s11

draw_combined <- function() {
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(nrow = 1, ncol = 3)))
  print(p_a, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(p_b, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
  print(p_c, vp = viewport(layout.pos.row = 1, layout.pos.col = 3))
}

png(
  file.path(output_dir, "FigureS11_multinomial_null_disciplinary_gini.png"),
  width = 15.5, height = 5.3, units = "in", res = 600
)
draw_combined()
dev.off()

pdf(
  file.path(output_dir, "FigureS11_multinomial_null_disciplinary_gini.pdf"),
  width = 15.5, height = 5.3, useDingbats = FALSE
)
draw_combined()
dev.off()

message("Multinomial simulations: ", B)
message("Country-year observations: ", nrow(country_year))
message("Countries: ", n_distinct(country_year$country))
message("Completed Figure S11 and all audit tables.")
