library(tidyverse)
library(scales)

# ============================================================
# Figure S2b: AI vs non-AI disciplinary inequality
#
# Both series are calculated from the same WoS aggregate source,
# for the same country-year observations and the same six OECD fields.
# This prevents changes in the country sample or classification source
# from driving the comparison.
# ============================================================

source_file <- "D:/Python/WOS_AI/OECD_Natural_Sciences_1990_2025/annual_field_country_ai_non_ai_wos_aggregate.csv"
output_dir <- "D:/博后/论文/3/需要上传的数据和代码/code/FigureS2/FigureS2b"

start_year <- 1996L
end_year <- 2025L

field_levels <- c(
  "Mathematics",
  "Computer and information sciences",
  "Physical sciences",
  "Chemical sciences",
  "Earth and related environmental sciences",
  "Biological sciences"
)

field_column_names <- c(
  "Mathematics" = "mathematics",
  "Computer and information sciences" = "computer_information_sciences",
  "Physical sciences" = "physical_sciences",
  "Chemical sciences" = "chemical_sciences",
  "Earth and related environmental sciences" = "earth_environmental_sciences",
  "Biological sciences" = "biological_sciences"
)

# Raw six-component Gini. Its theoretical maximum is 5/6.
gini_coefficient <- function(x) {
  x <- as.numeric(x)
  if (length(x) != 6L || any(!is.finite(x)) || any(x < 0) || sum(x) <= 0) {
    return(NA_real_)
  }

  x <- sort(x)
  n <- length(x)
  i <- seq_len(n)
  sum((2 * i - n - 1) * x) / (n * sum(x))
}

# Harmonize the country labels used by WoS.
normalize_country <- function(x) {
  x <- str_squish(str_to_title(x))
  recode(
    x,
    "Peoples R China" = "China",
    "Usa" = "United States",
    "England" = "United Kingdom",
    "U Arab Emirates" = "United Arab Emirates",
    "Turkey" = "Turkiye",
    .default = x
  )
}

# 1. Read AI and non-AI counts from the same source.
field_long <- read_csv(source_file, show_col_types = FALSE) %>%
  transmute(
    country = normalize_country(country_region),
    year = as.integer(year),
    field = oecd_field,
    ai_publications = as.numeric(ai_publications),
    non_ai_publications = as.numeric(non_ai_publications)
  ) %>%
  filter(
    between(year, start_year, end_year),
    !is.na(country), country != "",
    field %in% field_levels,
    !is.na(ai_publications), ai_publications >= 0,
    !is.na(non_ai_publications), non_ai_publications >= 0
  ) %>%
  group_by(country, year, field) %>%
  summarise(
    ai_publications = max(ai_publications),
    non_ai_publications = max(non_ai_publications),
    .groups = "drop"
  )

# 2. Retain only country-years represented in all six fields.
# AI and non-AI are therefore calculated on exactly the same rows.
paired_complete <- field_long %>%
  group_by(country, year) %>%
  filter(n_distinct(field) == length(field_levels)) %>%
  ungroup() %>%
  mutate(field_column = unname(field_column_names[field])) %>%
  select(country, year, field_column, ai_publications, non_ai_publications) %>%
  pivot_wider(
    names_from = field_column,
    values_from = c(ai_publications, non_ai_publications),
    names_glue = "{.value}__{field_column}"
  )

ai_columns <- paste0("ai_publications__", unname(field_column_names))
non_ai_columns <- paste0("non_ai_publications__", unname(field_column_names))

# 3. Calculate paired country-year Gini coefficients.
country_year_paired <- paired_complete %>%
  rowwise() %>%
  mutate(
    total_ai = sum(c_across(all_of(ai_columns))),
    total_non_ai = sum(c_across(all_of(non_ai_columns))),
    gini_ai = gini_coefficient(c_across(all_of(ai_columns))),
    gini_non_ai = gini_coefficient(c_across(all_of(non_ai_columns))),
    gini_ai_normalized_0_1 = gini_ai * 6 / 5,
    gini_non_ai_normalized_0_1 = gini_non_ai * 6 / 5
  ) %>%
  ungroup() %>%
  filter(
    total_ai > 0,
    total_non_ai > 0,
    !is.na(gini_ai),
    !is.na(gini_non_ai)
  ) %>%
  arrange(country, year)

# 4. Annual paired medians and IQRs.
annual_paired <- country_year_paired %>%
  group_by(year) %>%
  summarise(
    median_AI = median(gini_ai),
    q25_AI = quantile(gini_ai, 0.25, names = FALSE),
    q75_AI = quantile(gini_ai, 0.75, names = FALSE),
    median_NonAI = median(gini_non_ai),
    q25_NonAI = quantile(gini_non_ai, 0.25, names = FALSE),
    q75_NonAI = quantile(gini_non_ai, 0.75, names = FALSE),
    n_matched_countries = n_distinct(country),
    .groups = "drop"
  ) %>%
  arrange(year)

annual_long <- bind_rows(
  annual_paired %>%
    transmute(
      year,
      median_gini = median_AI,
      q25_gini = q25_AI,
      q75_gini = q75_AI,
      n_countries = n_matched_countries,
      Type = "AI"
    ),
  annual_paired %>%
    transmute(
      year,
      median_gini = median_NonAI,
      q25_gini = q25_NonAI,
      q75_gini = q75_NonAI,
      n_countries = n_matched_countries,
      Type = "Non-AI"
    )
) %>%
  mutate(Type = factor(Type, levels = c("AI", "Non-AI"))) %>%
  arrange(Type, year)

# 5. Save auditable data tables.
write_csv(
  country_year_paired,
  file.path(output_dir, "country_year_six_fields_gini_AI_NonAI_matched.csv")
)

write_csv(
  annual_paired,
  file.path(output_dir, "annual_median_six_field_Gini_AI_NonAI_matched_wide.csv")
)

write_csv(
  annual_long,
  file.path(output_dir, "annual_median_six_field_Gini_AI_NonAI_matched.csv")
)

# 6. Plot.
line_colors <- c(
  "AI" = "#FF8081",
  "Non-AI" = "#53A8E1"
)

p <- ggplot(
  annual_long,
  aes(x = year, y = median_gini, color = Type, group = Type)
) +
  geom_errorbar(
    aes(ymin = q25_gini, ymax = q75_gini),
    width = 0.12,
    linewidth = 0.55,
    alpha = 0.45,
    position = position_dodge(width = 0.20)
  ) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 3.4) +
  scale_color_manual(values = line_colors) +
  scale_x_continuous(
    breaks = seq(1995, 2025, 5),
    limits = c(start_year, end_year),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    limits = c(0.2, 0.85),
    breaks = seq(0.2, 0.8, 0.1),
    labels = label_number(accuracy = 0.1),
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  labs(
    x = "Year",
    y = "Disciplinary Gini coefficient",
    color = NULL
  ) +
  theme_classic(base_size = 20) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.0),
    axis.title = element_text(size = 24, color = "black"),
    axis.text = element_text(size = 24, color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.position = "right",
    legend.text = element_text(size = 18),
    legend.key.width = unit(1.2, "cm"),
    plot.margin = margin(10, 15, 10, 10)
  )

print(p)

ggsave(
  file.path(output_dir, "AI_vs_NonAI_annual_median_six_field_Gini_matched.png"),
  p,
  width = 10.2,
  height = 5.8,
  dpi = 600
)

message("Completed: paired AI/non-AI country-year Gini tables and figure were written to ", output_dir)
