# Gini coefficient of AI-related publication counts across participating countries
# for six OECD natural-science fields, 1990-2025.

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

country_raw <- read.csv(
  file.path(input_dir, "annual_natural_field_country_counts.csv"),
  check.names = FALSE,
  fileEncoding = "UTF-8-BOM"
)

country_data <- country_raw[
  country_raw$oecd_code %in% natural_codes &
    !is.na(country_raw$country) &
    trimws(country_raw$country) != "" &
    trimws(country_raw$country) != "Unknown",
  c("year", "oecd_code", "oecd_field", "country", "publication_count")
]

country_data$year <- as.integer(country_data$year)
country_data$publication_count <- suppressWarnings(
  as.numeric(country_data$publication_count)
)
country_data <- country_data[
  is.finite(country_data$publication_count) & country_data$publication_count > 0,
]

# Guard against duplicate country rows by aggregating before calculating Gini.
country_data <- aggregate(
  publication_count ~ year + oecd_code + oecd_field + country,
  data = country_data,
  FUN = sum,
  na.rm = TRUE
)

gini_standard <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x) & x >= 0]
  n <- length(x)
  total <- sum(x)
  if (n < 2L || total <= 0) return(NA_real_)
  x <- sort(x)
  sum((2 * seq_len(n) - n - 1) * x) / (n * total)
}

group_key <- interaction(
  country_data$year,
  country_data$oecd_code,
  drop = TRUE,
  lex.order = TRUE
)
groups <- split(country_data, group_key)

gini_rows <- lapply(groups, function(d) {
  data.frame(
    year = d$year[1],
    oecd_code = d$oecd_code[1],
    field = d$oecd_field[1],
    participating_countries = length(unique(d$country)),
    total_publications = sum(d$publication_count),
    gini = gini_standard(d$publication_count),
    stringsAsFactors = FALSE
  )
})
gini_data <- do.call(rbind, gini_rows)

gini_data$field[gini_data$field == "Earth and related environmental sciences"] <-
  "Earth and environmental sciences"
gini_data$field <- factor(gini_data$field, levels = field_levels)
gini_data <- gini_data[order(gini_data$field, gini_data$year), ]
row.names(gini_data) <- NULL

write.csv(
  gini_data,
  file.path(output_dir, "annual_ai_field_country_publication_gini.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

p_gini <- ggplot(
  gini_data[is.finite(gini_data$gini), ],
  aes(x = year, y = gini, colour = field, group = field)
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
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2),
    labels = label_number(accuracy = 0.1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    x = "Year",
    y = "Gini coefficient of publication count",
    colour = "Discipline"
  ) +
  theme_classic(base_size = 15) +
  theme(
    text = element_text(family = "sans", colour = "black"),
    axis.title = element_text(size = 25),
    axis.text = element_text(size = 25, colour = "#444444"),
    axis.line = element_line(linewidth = 0.7, colour = "black"),
    axis.ticks = element_line(linewidth = 0.6, colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 20),
    legend.key.height = grid::unit(0.65, "lines"),
    legend.position = "right",
    plot.margin = margin(12, 18, 10, 10)
  )

print(p_gini)

ggsave(
  file.path(output_dir, "annual_ai_field_country_publication_gini.png"),
  p_gini,
  width = 12,
  height = 6.8,
  dpi = 300,
  bg = "white"
)

message("Saved Gini data and plots to: ", output_dir)
