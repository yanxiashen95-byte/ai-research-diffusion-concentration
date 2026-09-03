# ============================================================
# Fixed journal subset: annual AI-related publications
# Six OECD natural-science fields, 1990-2025
# Input: 5 fixed journals per field × year
# Output: stacked bars by field + grey annual-total line
# ============================================================

rm(list = ls())

# ---- 1. Packages ----
required_packages <- c("readr", "dplyr", "ggplot2", "scales")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Missing R packages: ", paste(missing_packages, collapse = ", "),
    ". Please install them first with install.packages()."
  )
}

library(readr)
library(dplyr)
library(ggplot2)
library(scales)

# ---- 2. File paths ----
data_dir <- "D:/博后/论文/3/Figure/Figure 1-SI/特定期刊的变化"
input_file <- file.path(data_dir, "annual_fixed_journal_subset_counts.csv")
output_png <- file.path(data_dir, "fixed_journal_subset_six_fields.png")
output_pdf <- file.path(data_dir, "fixed_journal_subset_six_fields.pdf")

# ---- 3. Read and validate data ----
raw_data <- read_csv(input_file, show_col_types = FALSE)

required_columns <- c(
  "oecd_code", "oecd_field", "journal", "year", "publication_count"
)

if (!all(required_columns %in% names(raw_data))) {
  stop(
    "The input file is missing required columns: ",
    paste(setdiff(required_columns, names(raw_data)), collapse = ", ")
  )
}

# ---- 4. Labels, stacking order, and colors ----
# Factor order controls both the legend and the order of the stacked segments.
field_labels <- c(
  "1.1" = "Mathematics",
  "1.2" = "Computer and information sciences",
  "1.3" = "Physical sciences",
  "1.4" = "Chemical sciences",
  "1.5" = "Earth and environmental sciences",
  "1.6" = "Biological sciences"
)

# Six-color publication palette. Change only these hex codes if desired.
field_colors <- c(
  "Mathematics"                         = "#bcbddc",
  "Computer and information sciences"  = "#53A8E1",
  "Physical sciences"                  = "#99D096",
  "Chemical sciences"                  = "#F2C14E",
  "Earth and environmental sciences"   = "#FF8081",
  "Biological sciences"                = "#C9CED6"
)

# ---- 5. Aggregate the five journals within each field and year ----
plot_data <- raw_data %>%
  mutate(
    year = as.integer(year),
    publication_count = as.numeric(publication_count),
    discipline = unname(field_labels[as.character(oecd_code)])
  ) %>%
  filter(!is.na(discipline), year >= 1990, year <= 2025) %>%
  group_by(year, discipline) %>%
  summarise(publication_count = sum(publication_count), .groups = "drop") %>%
  mutate(
    discipline = factor(discipline, levels = unname(field_labels))
  ) %>%
  arrange(year, discipline)

# The CSV already contains zero rows, but this check protects against future edits.
expected_rows <- length(1990:2025) * length(field_labels)
if (nrow(plot_data) != expected_rows) {
  stop(
    "The aggregated panel is incomplete: expected ", expected_rows,
    " year-field rows, but found ", nrow(plot_data), "."
  )
}

annual_total <- plot_data %>%
  group_by(year) %>%
  summarise(total_publications = sum(publication_count), .groups = "drop")

# ---- 6. Draw figure ----
p <- ggplot(plot_data, aes(x = year, y = publication_count, fill = discipline)) +
  geom_col(width = 0.78, color = NA) +
  
  geom_line(
    data = annual_total,
    aes(x = year, y = total_publications),
    inherit.aes = FALSE,
    color = "#A9A9A9",
    linewidth = 1.25,
    lineend = "round"
  ) +
  
  geom_point(
    data = annual_total,
    aes(x = year, y = total_publications),
    inherit.aes = FALSE,
    color = "#A9A9A9",
    size = 2.6
  ) +
  
  scale_fill_manual(
    values = field_colors,
    breaks = names(field_colors),
    drop = FALSE
  ) +
  
  scale_x_continuous(
    breaks = seq(1990, 2025, by = 5),
    limits = c(1989.3, 2025.7),
    expand = expansion(mult = c(0, 0))
  ) +
  
  scale_y_continuous(
    labels = label_comma(big.mark = ","),
    expand = expansion(mult = c(0, 0.06))
  ) +
  
  labs(
    x = "Year",
    y = "Publication count",
    fill = "Discipline"
  ) +
  
  guides(
    fill = guide_legend(
      title.position = "top",
      keyheight = grid::unit(0.65, "cm"),
      keywidth = grid::unit(0.65, "cm")
    )
  ) +
  
  theme_classic(base_size = 18) +
  
  theme(
    
    # 四周完整黑色边框
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.8
    ),
    
    axis.title = element_text(
      size = 22,
      color = "black"
    ),
    
    axis.text = element_text(
      size = 20,
      color = "black"
    ),
    
    axis.line = element_line(
      color = "black",
      linewidth = 0.8
    ),
    
    axis.ticks = element_line(
      color = "black",
      linewidth = 0.8
    ),
    
    axis.ticks.length = grid::unit(
      0.18,
      "cm"
    ),
    
    legend.position = "right",
    
    legend.title = element_text(
      size = 19,
      face = "plain"
    ),
    
    legend.text = element_text(
      size = 14
    ),
    
    legend.key = element_blank(),
    
    plot.margin = margin(
      12,
      20,
      12,
      12
    )
  )

# ---- 7. Save publication-ready outputs ----
ggsave(
  filename = output_png,
  plot = p,
  width = 13,
  height = 7.8,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = output_pdf,
  plot = p,
  width = 13,
  height = 7.8,
  units = "in",
  device = cairo_pdf,
  bg = "white"
)

print(p)
message("PNG saved to: ", output_png)
message("PDF saved to: ", output_pdf)
