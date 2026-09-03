library(readr)
library(dplyr)
library(ggplot2)
library(ineq)

# ============================================================
# Figure S8: robustness to country-attribution method
# 1) First-author country: each paper is assigned to one country
# 2) Full-country counting: each distinct country on a paper gets one count
#
# Gini definition follows Figure4b.R exactly:
# - calculated annually across countries with positive publication counts;
# - no zero-filling for countries inactive in a given year;
# - bootstrap SE = SD of 1,000 bootstrap Gini estimates;
# - displayed interval = Gini +/- 1 bootstrap SE.
# ============================================================

input_dir <- "D:/博后/论文/3/需要上传的数据和代码/code/FigureS8/不同国家计数方法"

full_count_file <- file.path(
  input_dir,
  "annual_all_natural_country_publication_counts_full_country.csv"
)

first_author_gini_file <- paste0(
  "D:/博后/论文/3/需要上传的数据和代码/code/Figure4/",
  "annual_Gini_global_income_groups_2000_2025.csv"
)

output_csv <- file.path(
  input_dir,
  "annual_Gini_first_author_vs_full_country_2000_2025.csv"
)

output_png <- file.path(
  input_dir,
  "Gini_first_author_vs_full_country.png"
)

output_pdf <- file.path(
  input_dir,
  "Gini_first_author_vs_full_country.pdf"
)

set.seed(20260903)
B <- 1000

# ============================================================
# 1. First-author country: use the already calculated Global series
# ============================================================

gini_first <- read_csv(
  first_author_gini_file,
  show_col_types = FALSE
) %>%
  filter(
    Group == "Global",
    Year >= 2000,
    Year <= 2025
  ) %>%
  transmute(
    Year = as.integer(Year),
    Gini = as.numeric(Gini),
    Gini_se = as.numeric(Gini_se),
    Gini_lwr = as.numeric(Gini_lwr),
    Gini_upr = as.numeric(Gini_upr),
    n_countries = NA_integer_,
    total_country_counts = NA_real_,
    Method = "First-author country"
  )

# ============================================================
# 2. Full-country counting: calculate annual cross-country Gini
# ============================================================

full_counts <- read_csv(
  full_count_file,
  show_col_types = FALSE
) %>%
  transmute(
    Year = as.integer(year),
    Country = trimws(country),
    publication_count = as.numeric(publication_count)
  ) %>%
  filter(
    Year >= 2000,
    Year <= 2025,
    !is.na(Country),
    Country != "",
    Country != "Unknown",
    !is.na(publication_count),
    publication_count > 0
  ) %>%
  group_by(Year, Country) %>%
  summarise(
    publication_count = sum(publication_count),
    .groups = "drop"
  )

gini_full <- full_counts %>%
  group_by(Year) %>%
  summarise(
    Gini = ineq::Gini(publication_count, na.rm = TRUE),
    Gini_se = sd(
      replicate(
        B,
        ineq::Gini(
          sample(publication_count, replace = TRUE),
          na.rm = TRUE
        )
      ),
      na.rm = TRUE
    ),
    n_countries = n(),
    total_country_counts = sum(publication_count),
    .groups = "drop"
  ) %>%
  mutate(
    Gini_lwr = pmax(Gini - Gini_se, 0),
    Gini_upr = pmin(Gini + Gini_se, 1),
    Method = "Full-country counting"
  ) %>%
  select(
    Year, Gini, Gini_se, Gini_lwr, Gini_upr,
    n_countries, total_country_counts, Method
  )

# ============================================================
# 3. Combine and save annual results
# ============================================================

plot_data <- bind_rows(gini_first, gini_full) %>%
  mutate(
    Method = factor(
      Method,
      levels = c(
        "First-author country",
        "Full-country counting"
      )
    )
  ) %>%
  arrange(Method, Year)

write_csv(plot_data, output_csv)

# ============================================================
# 4. Plot the two Global curves
# ============================================================

method_colors <- c(
  "First-author country" = "#FF8081",
  "Full-country counting" = "#53A8E1"
)

p <- ggplot(
  plot_data,
  aes(
    x = Year,
    y = Gini,
    group = Method
  )
) +
  geom_ribbon(
    aes(ymin = Gini_lwr, ymax = Gini_upr, fill = Method),
    alpha = 0.13,
    color = NA,
    show.legend = FALSE
  ) +
  geom_line(aes(color = Method), linewidth = 2.5) +
  geom_point(aes(color = Method), size = 4) +
  scale_color_manual(values = method_colors) +
  scale_fill_manual(values = method_colors) +
  guides(fill = "none") +
  scale_x_continuous(
    breaks = seq(2000, 2025, by = 5),
    limits = c(2000, 2025),
    expand = expansion(mult = c(0.01, 0.015))
  ) +
  scale_y_continuous(
    breaks = seq(0.60, 0.95, by = 0.1),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  coord_cartesian(ylim = c(0.60, 0.95)) +
  labs(
    x = "Year",
    y = "Cross-country Gini",
    color = NULL
  ) +
  theme_classic(base_size = 20) +
  theme(
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 1.2
    ),
    axis.title = element_text(size = 22, color = "black"),
    axis.text = element_text(size = 22, color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 17),
    legend.key.width = grid::unit(1.5, "cm")
  )

print(p)

ggsave(
  output_png,
  plot = p,
  width = 11,
  height = 5,
  dpi = 600,
  bg = "white"
)

ggsave(
  output_pdf,
  plot = p,
  width = 10,
  height = 6,
  device = cairo_pdf,
  bg = "white"
)

cat("\nSaved outputs:\n", output_csv, "\n", output_png, "\n", output_pdf, "\n")
cat("\nSelected annual values:\n")
print(plot_data %>% filter(Year %in% c(2000, 2015, 2024, 2025)))
