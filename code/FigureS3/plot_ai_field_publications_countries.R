# AI-related publications and participating countries across OECD natural-science fields
# Data period: 1990-2025

options(stringsAsFactors = FALSE)

input_dir <- "D:/博后/论文/3/需要上传的数据和代码/code/FigureS3"
output_dir <- input_dir

required_packages <- c("ggplot2", "scales")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop("Missing R packages: ", paste(missing_packages, collapse = ", "))
}

library(ggplot2)
library(scales)

field_levels <- c(
  "Mathematics",
  "Computer and information sciences",
  "Physical sciences",
  "Chemical sciences",
  "Earth and environmental sciences",
  "Biological sciences"
)

field_colors <- c(
  "Mathematics"                        = "#bcbddc",
  "Computer and information sciences" = "#53A8E1",
  "Physical sciences"                 = "#99D096",
  "Chemical sciences"                 = "#F2C14E",
  "Earth and environmental sciences"  = "#FF8081",
  "Biological sciences"               = "#C9CED6"
)

natural_codes <- sprintf("1.%d", 1:6)
years <- 1990:2025

publication_raw <- read.csv(
  file.path(input_dir, "annual_field_counts.csv"),
  check.names = FALSE,
  fileEncoding = "UTF-8-BOM"
)

country_raw <- read.csv(
  file.path(input_dir, "annual_natural_field_country_counts.csv"),
  check.names = FALSE,
  fileEncoding = "UTF-8-BOM"
)

rename_earth_field <- function(x) {
  x[x == "Earth and related environmental sciences"] <-
    "Earth and environmental sciences"
  x
}

# Complete all 36 years x 6 fields so absent combinations are explicitly zero.
complete_grid <- expand.grid(
  year = years,
  field = field_levels,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

publication_data <- publication_raw[
  publication_raw$oecd_code %in% natural_codes,
  c("year", "oecd_field", "publication_count")
]
names(publication_data) <- c("year", "field", "publication_count")
publication_data$field <- rename_earth_field(publication_data$field)
publication_data$year <- as.integer(publication_data$year)
publication_data$publication_count <- as.numeric(publication_data$publication_count)

publication_data <- aggregate(
  publication_count ~ year + field,
  data = publication_data,
  FUN = sum,
  na.rm = TRUE
)
publication_data <- merge(
  complete_grid,
  publication_data,
  by = c("year", "field"),
  all.x = TRUE,
  sort = FALSE
)
publication_data$publication_count[is.na(publication_data$publication_count)] <- 0
publication_data$field <- factor(publication_data$field, levels = field_levels)
publication_data <- publication_data[order(publication_data$field, publication_data$year), ]

# A participating country is counted once within each field-year.
# Unknown or blank country labels are excluded.
country_data <- country_raw[
  country_raw$oecd_code %in% natural_codes &
    !is.na(country_raw$country) &
    trimws(country_raw$country) != "" &
    trimws(country_raw$country) != "Unknown" &
    suppressWarnings(as.numeric(country_raw$publication_count)) > 0,
  c("year", "oecd_field", "country")
]
country_data$field <- rename_earth_field(country_data$oecd_field)
country_data$year <- as.integer(country_data$year)
country_data <- unique(country_data[c("year", "field", "country")])

country_counts <- aggregate(
  country ~ year + field,
  data = country_data,
  FUN = length
)
names(country_counts)[names(country_counts) == "country"] <- "country_count"
country_counts <- merge(
  complete_grid,
  country_counts,
  by = c("year", "field"),
  all.x = TRUE,
  sort = FALSE
)
country_counts$country_count[is.na(country_counts$country_count)] <- 0
country_counts$field <- factor(country_counts$field, levels = field_levels)
country_counts <- country_counts[order(country_counts$field, country_counts$year), ]

write.csv(
  publication_data,
  file.path(output_dir, "plot_data_annual_ai_publications_by_field.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
write.csv(
  country_counts,
  file.path(output_dir, "plot_data_annual_ai_participating_countries_by_field.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

base_theme <- theme_classic(base_size = 15) +
  theme(
    text = element_text(family = "sans", colour = "black"),
    axis.title = element_text(size = 25),
    axis.text = element_text(size = 25, colour = "#444444"),
    axis.line = element_line(linewidth = 0.7, colour = "black"),
    axis.ticks = element_line(linewidth = 0.6, colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 12),
    legend.key.height = grid::unit(0.65, "lines"),
    legend.position = "right",
    plot.margin = margin(12, 18, 10, 10)
  )

p_publications <- ggplot(
  publication_data,
  aes(x = year, y = publication_count, colour = field, group = field)
) +
  geom_line(linewidth = 2.5, lineend = "round") +
  geom_point(size = 4) +
  scale_colour_manual(values = field_colors, drop = FALSE) +
  scale_x_continuous(
    breaks = seq(1990, 2025, by = 5),
    limits = c(1990, 2025),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    labels = label_comma(),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  labs(x = "Year", y = "Publication count", colour = "Discipline") +
  base_theme

print(p_publications)

p_countries <- ggplot(
  country_counts,
  aes(x = year, y = country_count, colour = field, group = field)
) +
  geom_line(linewidth = 2.5, lineend = "round") +
  geom_point(size = 4) +
  scale_colour_manual(values = field_colors, drop = FALSE) +
  scale_x_continuous(
    breaks = seq(1990, 2025, by = 5),
    limits = c(1990, 2025),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    breaks = pretty_breaks(n = 7),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  labs(x = "Year", y = "Participating countries", colour = "Discipline") +
  base_theme

print(p_countries)

ggsave(
  file.path(output_dir, "annual_ai_publications_by_field.png"),
  p_publications,
  width = 12.5,
  height = 6.8,
  dpi = 300,
  bg = "white"
)

ggsave(
  file.path(output_dir, "annual_ai_participating_countries_by_field.png"),
  p_countries,
  width = 12,
  height = 6.8,
  dpi = 300,
  bg = "white"
)

message("Saved plots and plotting data to: ", output_dir)
