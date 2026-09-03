library(tidyverse)
library(scales)
library(grid)

# ============================================================
# Supplementary Figure S10
# Sensitivity of cross-country publication Gini to country-set definition
# Study window: 2000-2024
# ============================================================

input_file <- "D:/博后/论文/3/需要上传的数据和代码/code/Figure4/annual_all_natural_country_publication_counts_with_income_sorted.csv"
output_dir <- "D:/博后/论文/3/需要上传的数据和代码/code/Figure4"
start_year <- 2000L
end_year <- 2024L
study_years <- start_year:end_year

# -------------------------
# Gini function with zeros retained
# -------------------------
gini_fun <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x) & x >= 0]
  if (length(x) == 0L || sum(x) <= 0) return(NA_real_)
  x <- sort(x)
  n <- length(x)
  sum((2 * seq_along(x) - n - 1) * x) / (n * sum(x))
}

# -------------------------
# 1. Prepare one row per country-year
# -------------------------
df_count <- read_csv(input_file, show_col_types = FALSE) %>%
  transmute(
    Year = as.integer(Year),
    Country = str_squish(Country),
    Total_Paper_Count = as.numeric(Total_Paper_Count)
  ) %>%
  filter(
    between(Year, start_year, end_year),
    !is.na(Country), Country != "",
    !is.na(Total_Paper_Count), Total_Paper_Count >= 0
  ) %>%
  group_by(Country, Year) %>%
  summarise(Total_Paper_Count = sum(Total_Paper_Count), .groups = "drop")

# Country sets
all_observed_countries <- sort(unique(df_count$Country))

country_frequency <- df_count %>%
  filter(Total_Paper_Count > 0) %>%
  count(Country, name = "n_active_years")

balanced_countries <- country_frequency %>%
  filter(n_active_years == length(study_years)) %>%
  pull(Country) %>%
  sort()

# -------------------------
# 2a. Dynamic active-country specification
# -------------------------
gini_active <- df_count %>%
  filter(Total_Paper_Count > 0) %>%
  group_by(Year) %>%
  summarise(
    Gini_active = gini_fun(Total_Paper_Count),
    n_active_countries = n_distinct(Country),
    total_publications = sum(Total_Paper_Count),
    .groups = "drop"
  )

# -------------------------
# 2b. Fixed union panel; missing country-years set to zero
# -------------------------
fixed_zero_panel <- expand_grid(
  Country = all_observed_countries,
  Year = study_years
) %>%
  left_join(df_count, by = c("Country", "Year")) %>%
  mutate(Total_Paper_Count = replace_na(Total_Paper_Count, 0))

gini_fixed_zero <- fixed_zero_panel %>%
  group_by(Year) %>%
  summarise(
    Gini_fixed_zero = gini_fun(Total_Paper_Count),
    n_fixed_countries = n_distinct(Country),
    n_active_in_fixed_panel = sum(Total_Paper_Count > 0),
    .groups = "drop"
  )

# -------------------------
# 2c. Balanced panel; positive publication count in every year
# -------------------------
balanced_panel <- df_count %>%
  filter(Country %in% balanced_countries, Total_Paper_Count > 0)

gini_balanced <- balanced_panel %>%
  group_by(Year) %>%
  summarise(
    Gini_balanced = gini_fun(Total_Paper_Count),
    n_balanced_countries = n_distinct(Country),
    .groups = "drop"
  )

# -------------------------
# 3. Merge and save auditable results
# -------------------------
sensitivity_results <- gini_active %>%
  left_join(gini_fixed_zero, by = "Year") %>%
  left_join(gini_balanced, by = "Year") %>%
  arrange(Year)

panel_membership <- tibble(Country = all_observed_countries) %>%
  left_join(country_frequency, by = "Country") %>%
  mutate(
    n_active_years = replace_na(n_active_years, 0L),
    in_fixed_union_panel = TRUE,
    in_balanced_panel = Country %in% balanced_countries
  ) %>%
  arrange(desc(in_balanced_panel), Country)

write_csv(
  sensitivity_results,
  file.path(output_dir, "FigureS10_cross_country_gini_sensitivity_data.csv")
)

write_csv(
  panel_membership,
  file.path(output_dir, "FigureS10_country_panel_membership.csv")
)

# -------------------------
# 4. Common theme and scales
# -------------------------
gini_col <- "#FF8081"
country_col <- "#53A8E1"
fixed_col <- "#99D096"
balanced_col <- "#F2C14E"
common_gini_limits <- c(0.55, 0.93)

theme_s10 <- theme_classic(base_size = 15) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.title = element_text(size = 15, color = "black"),
    axis.text = element_text(size = 13, color = "black"),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.position = "bottom",
    plot.margin = margin(8, 12, 8, 8)
  )

# Panel a uses a secondary axis only to display the active-country count.
# The linear transformation is stated explicitly and is used for visualization only.
n_axis_min <- 40
n_axis_max <- 150
to_gini_axis <- function(n) {
  common_gini_limits[1] +
    (n - n_axis_min) /
    (n_axis_max - n_axis_min) *
    diff(common_gini_limits)
}
from_gini_axis <- function(y) {
  n_axis_min +
    (y - common_gini_limits[1]) /
    diff(common_gini_limits) *
    (n_axis_max - n_axis_min)
}

p_a <- ggplot(sensitivity_results, aes(x = Year)) +
  geom_line(aes(y = Gini_active, color = "Active-country Gini"), linewidth = 1.25) +
  geom_point(aes(y = Gini_active, color = "Active-country Gini"), size = 2.2) +
  geom_line(
    aes(y = to_gini_axis(n_active_countries), color = "Active countries"),
    linewidth = 1.15,
    linetype = "dashed"
  ) +
  geom_point(
    aes(y = to_gini_axis(n_active_countries), color = "Active countries"),
    size = 2.0,
    shape = 1
  ) +
  scale_color_manual(values = c(
    "Active-country Gini" = gini_col,
    "Active countries" = country_col
  )) +
  scale_x_continuous(breaks = seq(2000, 2025, 5), limits = c(2000, 2024)) +
  scale_y_continuous(
    name = "Gini coefficient",
    limits = common_gini_limits,
    breaks = seq(0.6, 0.9, 0.1),
    sec.axis = sec_axis(
      transform = from_gini_axis,
      name = "Number of active countries",
      breaks = seq(50, 150, 25)
    )
  ) +
  labs(x = "Year", title = "a  Dynamic active-country panel") +
  theme_s10 +
  theme(
    axis.title.y = element_text(color = gini_col),
    axis.title.y.right = element_text(color = country_col),
    legend.position = "none"
  )

p_b <- ggplot(sensitivity_results, aes(x = Year, y = Gini_fixed_zero)) +
  geom_line(linewidth = 1.25, color = fixed_col) +
  geom_point(size = 2.2, color = fixed_col) +
  scale_x_continuous(breaks = seq(2000, 2025, 5), limits = c(2000, 2024)) +
  scale_y_continuous(
    limits = common_gini_limits,
    breaks = seq(0.6, 0.9, 0.1)
  ) +
  labs(
    x = "Year",
    y = "Gini coefficient",
    title = paste0("b  Fixed panel with zeros (n = ", length(all_observed_countries), ")")
  ) +
  theme_s10 +
  theme(legend.position = "none")

p_c <- ggplot(sensitivity_results, aes(x = Year, y = Gini_balanced)) +
  geom_line(linewidth = 1.25, color = balanced_col) +
  geom_point(size = 2.2, color = balanced_col) +
  scale_x_continuous(breaks = seq(2000, 2025, 5), limits = c(2000, 2024)) +
  scale_y_continuous(
    limits = common_gini_limits,
    breaks = seq(0.6, 0.9, 0.1)
  ) +
  labs(
    x = "Year",
    y = "Gini coefficient",
    title = paste0("c  Balanced panel (n = ", length(balanced_countries), ")")
  ) +
  theme_s10 +
  theme(legend.position = "none")

# -------------------------
# 5. Combine the three ggplots without additional packages
# -------------------------
draw_combined <- function() {
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(nrow = 1, ncol = 3)))
  print(p_a, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(p_b, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
  print(p_c, vp = viewport(layout.pos.row = 1, layout.pos.col = 3))
}

png(
  file.path(output_dir, "FigureS10_cross_country_gini_sensitivity.png"),
  width = 15,
  height = 5.2,
  units = "in",
  res = 600
)
draw_combined()
dev.off()

pdf(
  file.path(output_dir, "FigureS10_cross_country_gini_sensitivity.pdf"),
  width = 15,
  height = 5.2,
  useDingbats = FALSE
)
draw_combined()
dev.off()

message("All observed countries in fixed union panel: ", length(all_observed_countries))
message("Countries active in every year: ", length(balanced_countries))
message("Completed Figure S10 and sensitivity-analysis CSV files.")
