library(readr)
library(dplyr)
library(ggplot2)
library(purrr)
library(scales)

# ============================================================
# Figure S7: sensitivity analysis of country-level Gini trends
# Different minimum numbers of valid country-years: 5, 10, 20
# ============================================================

# =========================
# 1) Read country-level Sen slope results
# =========================
df <- read_csv(
  "country_gini_sen_slope.csv",
  show_col_types = FALSE
) %>%
  transmute(
    Country = as.character(Country),
    Income_Group = as.character(Income_Group),
    valid_years = as.integer(valid_years),
    first_year = as.integer(first_year),
    last_year = as.integer(last_year),
    median_gini = as.numeric(median_gini),
    sen_slope = as.numeric(sen_slope),

    # Absolute change in Gini, expressed as percentage points per year.
    # Example: sen_slope = -0.01 becomes -1 percentage point/year.
    sen_slope_pct = as.numeric(sen_slope) * 100
  ) %>%
  filter(
    !is.na(Country),
    !is.na(valid_years),
    !is.na(sen_slope)
  )

# =========================
# 2) Minimum valid-year thresholds
# =========================
thresholds <- c(5, 10, 20)

# =========================
# 3) Apply each threshold
# =========================
df_long <- map_dfr(thresholds, function(th) {
  df %>%
    filter(valid_years >= th) %>%
    mutate(
      threshold = factor(
        paste0("At least ", th, " years"),
        levels = paste0("At least ", thresholds, " years")
      ),
      trend = case_when(
        sen_slope < 0 ~ "Declining",
        sen_slope > 0 ~ "Increasing",
        TRUE ~ "Stable"
      ),
      trend = factor(
        trend,
        levels = c("Declining", "Increasing", "Stable")
      )
    )
})

# Save all threshold-specific country classifications for audit.
write_csv(
  df_long,
  "Figure_S7_Gini_sensitivity_country_classification.csv"
)

# =========================
# 4) Panel a data
# Share of countries with declining/increasing Gini
# Stable countries are reported in the audit but excluded here so that
# Declining + Increasing = 100% within every threshold.
# =========================
plot_a_df <- df_long %>%
  filter(trend %in% c("Declining", "Increasing")) %>%
  group_by(threshold, trend) %>%
  summarise(n = n_distinct(Country), .groups = "drop") %>%
  group_by(threshold) %>%
  mutate(
    total_n = sum(n),
    prop = n / total_n,
    label = percent(prop, accuracy = 1)
  ) %>%
  ungroup()

plot_a_n <- df_long %>%
  filter(trend %in% c("Declining", "Increasing")) %>%
  group_by(threshold) %>%
  summarise(
    total_n = n_distinct(Country),
    .groups = "drop"
  )

write_csv(
  plot_a_df,
  "Figure_S7_Gini_sensitivity_proportions.csv"
)

# =========================
# 5) Panel a: stacked proportions
# =========================
p_a <- ggplot(
  plot_a_df,
  aes(x = threshold, y = prop, fill = trend)
) +
  geom_col(
    width = 0.65,
    colour = "black",
    linewidth = 0.3
  ) +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 6
  ) +
  geom_text(
    data = plot_a_n,
    aes(
      x = threshold,
      y = 1.04,
      label = paste0("n = ", total_n)
    ),
    inherit.aes = FALSE,
    size = 5
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1.10),
    breaks = seq(0, 1, by = 0.2),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_fill_manual(
    values = c(
      "Declining" = "#74add1",
      "Increasing" = "#f46d43"
    ),
    drop = FALSE
  ) +
  labs(
    x = NULL,
    y = "Share of countries",
    fill = NULL
  ) +
  theme_classic(base_size = 16) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.8
    ),
    axis.line = element_blank()
  )

print(p_a)

# =========================
# 6) Panel b: Sen slope distributions
# =========================
p_b <- ggplot(
  df_long %>%
    filter(trend %in% c("Declining", "Increasing")),
  aes(x = threshold, y = sen_slope_pct)
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.6
  ) +
  geom_boxplot(
    width = 0.60,
    outlier.shape = NA,
    fill = "#74add1",
    colour = "black",
    alpha = 0.85
  ) +
  geom_jitter(
    width = 0.12,
    height = 0,
    alpha = 0.70,
    size = 2,
    colour = "grey35"
  ) +
  # Range covers all observed slopes in the supplied file.
  coord_cartesian(ylim = c(-4, 2.5)) +
  labs(
    x = NULL,
    y = expression("Change in Gini" ~ "(percentage points " * yr^{-1} * ")")
  ) +
  theme_classic(base_size = 16) +
  theme(
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.8
    ),
    axis.line = element_blank()
  )

print(p_b)

# =========================
# 7) Save figures
# =========================
ggsave(
  "Figure_S7a_Gini_trend_proportions.png",
  plot = p_a,
  width = 6,
  height = 5,
  dpi = 600,
  bg = "white"
)

ggsave(
  "Figure_S7b_Gini_slope_sensitivity.png",
  plot = p_b,
  width = 6.5,
  height = 5,
  dpi = 600,
  bg = "white"
)

ggsave(
  "Figure_S7a_Gini_trend_proportions.pdf",
  plot = p_a,
  width = 6,
  height = 5,
  device = cairo_pdf,
  bg = "white"
)

ggsave(
  "Figure_S7b_Gini_slope_sensitivity.pdf",
  plot = p_b,
  width = 6.5,
  height = 5,
  device = cairo_pdf,
  bg = "white"
)
