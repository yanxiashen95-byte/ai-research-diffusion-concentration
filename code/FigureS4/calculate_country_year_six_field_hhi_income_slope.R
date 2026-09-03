# Country-year disciplinary HHI across six OECD natural-science fields.
# Produces country-level HHI, income-group/global annual summaries,
# Sen slopes, and trend figures using both raw and normalized HHI.

options(stringsAsFactors = FALSE)

input_dir <- "D:/博后/论文/3/需要上传的数据和代码/code/FigureS4"
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

field_codes <- sprintf("1.%d", 1:6)
field_names <- c(
  "Mathematics",
  "Computer_and_information_sciences",
  "Physical_sciences",
  "Chemical_sciences",
  "Earth_and_environmental_sciences",
  "Biological_sciences"
)
names(field_names) <- field_codes
k <- length(field_codes)

income_levels <- c(
  "Global",
  "Low income",
  "Lower middle income",
  "Upper middle income",
  "High income"
)

income_colors <- c(
  "Global"              = "grey75",
  "Low income"          = "#F2C14E",
  "Lower middle income" = "#99D096",
  "Upper middle income" = "#53A8E1",
  "High income"         = "#FF8081"
)

field_raw <- read.csv(
  file.path(input_dir, "annual_natural_field_country_counts.csv"),
  check.names = FALSE,
  fileEncoding = "UTF-8-BOM"
)

income_raw <- read.csv(
  file.path(
    input_dir,
    "annual_all_natural_country_publication_counts_with_income_sorted.csv"
  ),
  check.names = FALSE,
  fileEncoding = "UTF-8-BOM"
)

field_data <- field_raw[
  field_raw$oecd_code %in% field_codes &
    !is.na(field_raw$country) &
    trimws(field_raw$country) != "" &
    trimws(field_raw$country) != "Unknown",
  c("year", "country", "oecd_code", "publication_count")
]
field_data$year <- as.integer(field_data$year)
field_data$country <- trimws(field_data$country)
field_data$publication_count <- suppressWarnings(
  as.numeric(field_data$publication_count)
)
field_data <- field_data[
  is.finite(field_data$year) &
    is.finite(field_data$publication_count) &
    field_data$publication_count > 0,
]

field_data <- aggregate(
  publication_count ~ country + year + oecd_code,
  data = field_data,
  FUN = sum,
  na.rm = TRUE
)

# Align all country-year observations to the same six fields; absent fields = 0.
country_years <- unique(field_data[c("country", "year")])
complete_grid <- merge(
  country_years,
  data.frame(oecd_code = field_codes, stringsAsFactors = FALSE),
  all = TRUE
)
aligned_long <- merge(
  complete_grid,
  field_data,
  by = c("country", "year", "oecd_code"),
  all.x = TRUE,
  sort = FALSE
)
aligned_long$publication_count[is.na(aligned_long$publication_count)] <- 0

country_wide <- reshape(
  aligned_long,
  idvar = c("country", "year"),
  timevar = "oecd_code",
  direction = "wide"
)

raw_count_cols <- paste0("publication_count.", field_codes)
missing_count_cols <- setdiff(raw_count_cols, names(country_wide))
for (col in missing_count_cols) country_wide[[col]] <- 0
country_wide <- country_wide[c("country", "year", raw_count_cols)]
names(country_wide)[match(raw_count_cols, names(country_wide))] <- unname(field_names)

discipline_cols <- unname(field_names)
count_matrix <- as.matrix(country_wide[discipline_cols])
storage.mode(count_matrix) <- "numeric"

country_wide$Total <- rowSums(count_matrix)
country_wide$n_active_fields <- rowSums(count_matrix > 0)
shares <- count_matrix / country_wide$Total
country_wide$HHI <- rowSums(shares^2)

# Six fields are fixed for every country-year.
# HHI_norm: 0 = equal allocation across all 6 fields; 1 = one-field concentration.
country_wide$HHI_norm <- (country_wide$HHI - 1 / k) / (1 - 1 / k)
country_wide$balance_index <- 1 - country_wide$HHI_norm

# Income mapping is constant by country in the supplied classification.
income_map <- unique(income_raw[c("country", "Income_Group")])
income_map$country <- trimws(income_map$country)
income_map$Income_Group <- trimws(income_map$Income_Group)
income_map <- income_map[!duplicated(income_map$country), ]

country_hhi <- merge(
  country_wide,
  income_map,
  by = "country",
  all.x = TRUE,
  sort = FALSE
)
country_hhi$Income_Group[
  is.na(country_hhi$Income_Group) | country_hhi$Income_Group == ""
] <- "Unmatched"
country_hhi <- country_hhi[order(country_hhi$country, country_hhi$year), ]
row.names(country_hhi) <- NULL

write.csv(
  country_hhi,
  file.path(output_dir, "country_year_six_fields_hhi_with_income.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

income_audit <- aggregate(
  year ~ country + Income_Group,
  data = country_hhi[
    !country_hhi$Income_Group %in% income_levels[-1],
  ],
  FUN = length
)
names(income_audit)[names(income_audit) == "year"] <- "n_country_years"
write.csv(
  income_audit,
  file.path(output_dir, "country_hhi_income_group_exclusion_audit.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

summarize_hhi <- function(d, group_name) {
  years <- sort(unique(d$year))
  do.call(rbind, lapply(years, function(y) {
    z <- d[d$year == y, ]
    data.frame(
      year = y,
      Group = group_name,
      mean_HHI = mean(z$HHI, na.rm = TRUE),
      sd_HHI = sd(z$HHI, na.rm = TRUE),
      mean_HHI_norm = mean(z$HHI_norm, na.rm = TRUE),
      sd_HHI_norm = sd(z$HHI_norm, na.rm = TRUE),
      mean_balance_index = mean(z$balance_index, na.rm = TRUE),
      n_countries = sum(is.finite(z$HHI)),
      stringsAsFactors = FALSE
    )
  }))
}

summary_rows <- list(summarize_hhi(country_hhi, "Global"))
for (income_group in income_levels[-1]) {
  d <- country_hhi[country_hhi$Income_Group == income_group, ]
  if (nrow(d) > 0L) {
    summary_rows[[length(summary_rows) + 1L]] <-
      summarize_hhi(d, income_group)
  }
}
annual_summary <- do.call(rbind, summary_rows)
annual_summary$Group <- factor(annual_summary$Group, levels = income_levels)
annual_summary <- annual_summary[order(annual_summary$Group, annual_summary$year), ]
row.names(annual_summary) <- NULL

write.csv(
  annual_summary,
  file.path(output_dir, "annual_country_hhi_global_and_income_groups.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

sen_slope <- function(year, value) {
  keep <- is.finite(year) & is.finite(value)
  year <- as.numeric(year[keep])
  value <- as.numeric(value[keep])
  n <- length(value)
  if (n < 2L) return(NA_real_)

  slopes <- numeric(0)
  for (i in seq_len(n - 1L)) {
    j <- (i + 1L):n
    valid <- year[j] != year[i]
    if (any(valid)) {
      slopes <- c(
        slopes,
        (value[j[valid]] - value[i]) / (year[j[valid]] - year[i])
      )
    }
  }
  if (length(slopes) == 0L) NA_real_ else median(slopes)
}

calculate_country_slope <- function(d) {
  d <- d[order(d$year), ]
  n <- nrow(d)
  slope_raw <- sen_slope(d$year, d$HHI)
  slope_norm <- sen_slope(d$year, d$HHI_norm)

  kendall_test <- if (n >= 3L) {
    suppressWarnings(cor.test(d$year, d$HHI_norm, method = "kendall", exact = FALSE))
  } else {
    NULL
  }

  data.frame(
    Income_Group = d$Income_Group[1],
    n_years = n,
    start_year = min(d$year),
    end_year = max(d$year),
    start_HHI = d$HHI[1],
    end_HHI = d$HHI[n],
    start_HHI_norm = d$HHI_norm[1],
    end_HHI_norm = d$HHI_norm[n],
    sen_slope_HHI_per_year = slope_raw,
    sen_slope_HHI_percentage_points_per_year = slope_raw * 100,
    sen_slope_HHI_norm_per_year = slope_norm,
    sen_slope_HHI_norm_percentage_points_per_year = slope_norm * 100,
    sen_slope_balance_per_year = -slope_norm,
    trend_concentration = ifelse(
      is.na(slope_norm),
      NA_character_,
      ifelse(slope_norm > 0, "increasing concentration",
             ifelse(slope_norm < 0, "decreasing concentration", "stable"))
    ),
    kendall_tau = if (is.null(kendall_test)) NA_real_ else
      unname(kendall_test$estimate),
    kendall_p_value = if (is.null(kendall_test)) NA_real_ else
      kendall_test$p.value,
    stringsAsFactors = FALSE
  )
}

country_groups <- split(country_hhi, country_hhi$country)
slope_rows <- lapply(names(country_groups), function(country_name) {
  cbind(
    data.frame(country = country_name, stringsAsFactors = FALSE),
    calculate_country_slope(country_groups[[country_name]])
  )
})
country_slopes <- do.call(rbind, slope_rows)
country_slopes <- country_slopes[
  order(-country_slopes$sen_slope_HHI_norm_per_year),
]
row.names(country_slopes) <- NULL

write.csv(
  country_slopes,
  file.path(output_dir, "country_six_field_hhi_sen_slope.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

base_theme <- theme_minimal(base_size = 15) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    axis.title = element_text(size = 22, colour = "black"),
    axis.text = element_text(size = 20, colour = "#444444"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.position = "bottom",
    plot.margin = margin(12, 18, 10, 10)
  )

make_trend_plot <- function(y_column, y_label) {
  plot_data <- annual_summary[is.finite(annual_summary[[y_column]]), ]
  ggplot(
    plot_data,
    aes(x = year, y = .data[[y_column]], colour = Group, group = Group)
  ) +
    annotate(
      "rect",
      xmin = 2016,
      xmax = 2025,
      ymin = -Inf,
      ymax = Inf,
      fill = "grey85",
      alpha = 0.4
    ) +
    geom_line(linewidth = 2, lineend = "round") +
    geom_point(size = 3.5) +
    scale_colour_manual(values = income_colors, drop = FALSE) +
    scale_x_continuous(
      breaks = seq(1990, 2025, by = 5),
      limits = c(1990, 2025),
      expand = expansion(mult = c(0.01, 0.02))
    ) +
    scale_y_continuous(
      breaks = pretty_breaks(n = 5),
      labels = label_number(accuracy = 0.01),
      expand = expansion(mult = c(0.04, 0.08))
    ) +
    labs(x = "Year", y = y_label) +
    base_theme
}

p_raw <- make_trend_plot("mean_HHI", "HHI coefficient")
p_norm <- make_trend_plot("mean_HHI_norm", "HHI coefficient")

ggsave(
  file.path(output_dir, "annual_country_raw_hhi_by_income_group.png"),
  p_raw,
  width = 10.5,
  height = 5.5,
  dpi = 300,
  bg = "white"
)

ggsave(
  file.path(output_dir, "annual_country_normalized_hhi_by_income_group.png"),
  p_norm,
  width = 10.5,
  height = 5.5,
  dpi = 300,
  bg = "white"
)

message("Saved country HHI, income summaries, Sen slopes, and plots to: ", output_dir)


#========================= 和世界地图的叠加 ========================
#========================= 和世界地图的叠加 ========================
library(readr)
library(dplyr)
library(stringr)
library(sf)
library(ggplot2)
library(scales)

# =========================
# 1. 读取数据
# =========================

slope_data <- read_csv(
  "country_six_field_hhi_sen_slope.csv",
  show_col_types = FALSE
)

world <- st_read(
  "D:/博后/论文/3/导出文献/世界地图.shp",
  quiet = TRUE
)

# 检查地图国家字段
print(names(world))

# =========================
# 2. 国家名称标准化函数
# =========================

clean_country <- function(x) {
  x <- x %>%
    str_squish() %>%
    str_to_lower()
  
  case_when(
    x %in% c(
      "usa",
      "united states of america",
      "united states"
    ) ~ "united states",
    
    x %in% c(
      "uk",
      "great britain",
      "united kingdom"
    ) ~ "united kingdom",
    
    x %in% c(
      "russian federation",
      "russia"
    ) ~ "russia",
    
    x %in% c(
      "south korea",
      "republic of korea",
      "korea south"
    ) ~ "south korea",
    
    x %in% c(
      "iran",
      "iran islamic republic of"
    ) ~ "iran",
    
    x %in% c(
      "viet nam",
      "vietnam"
    ) ~ "vietnam",
    
    x %in% c(
      "turkiye",
      "turkey"
    ) ~ "turkey",
    
    x %in% c(
      "czechia",
      "czech republic"
    ) ~ "czech republic",
    
    x %in% c(
      "china mainland",
      "people's republic of china",
      "china"
    ) ~ "china",
    
    TRUE ~ x
  )
}

# =========================
# 3. 整理Sen slope数据
# =========================

slope_data <- slope_data %>%
  transmute(
    Country = str_squish(Country),
    n_years = as.integer(n_years),
    
    # 这一列已经是“百分点/年”，不要再乘100
    sen_slope_pct =
      as.numeric(
        sen_slope_HHI_norm_percentage_points_per_year
      ),
    
    join_key = clean_country(Country)
  ) %>%
  filter(
    n_years >= 5,
    !is.na(sen_slope_pct)
  )

# 如果希望更严格，可以改成：
# slope_data <- slope_data %>%
#   filter(n_years >= 10)

# =========================
# 4. 整理地图数据
# =========================

# 假设地图国家字段为COUNTRY
world <- world %>%
  mutate(
    join_key = clean_country(COUNTRY)
  )

# =========================
# 5. 检查未匹配国家
# =========================

unmatched_countries <- slope_data %>%
  anti_join(
    world %>%
      st_drop_geometry() %>%
      distinct(join_key),
    by = "join_key"
  )

print(unmatched_countries)

write_csv(
  unmatched_countries,
  "country_sen_slope_unmatched_map.csv"
)

# =========================
# 6. 合并地图和Sen slope
# =========================

map_data <- world %>%
  left_join(
    slope_data,
    by = "join_key"
  )

# 检查成功匹配的国家数量
cat(
  "Countries with slope on map:",
  sum(!is.na(map_data$sen_slope_pct)),
  "\n"
)

# =========================
# 7. 经纬度标签函数
# =========================

longitude_labels <- function(x) {
  ifelse(
    x < 0,
    paste0(abs(x), "°W"),
    ifelse(x > 0, paste0(x, "°E"), "0°")
  )
}

latitude_labels <- function(x) {
  ifelse(
    x < 0,
    paste0(abs(x), "°S"),
    ifelse(x > 0, paste0(x, "°N"), "0°")
  )
}

# =========================
# 8. 绘制世界地图
# =========================

p <- ggplot(map_data) +
  geom_sf(
    aes(fill = sen_slope_pct),
    color = "gray60",
    linewidth = 0.25
  ) +
  scale_fill_gradient2(
    name = "Sen’s slope\n(% points / year)",
    low = "#1f78b4",
    mid = "white",
    high = "#FF8081",
    midpoint = 0,
    
    # 超出范围的数值压缩到颜色两端
    limits = c(-1, 1),
    oob = squish,
    
    # 没有有效Sen slope的国家
    na.value = "grey80"
  ) +
  coord_sf(
    expand = FALSE,
    xlim = c(-180, 180),
    ylim = c(-60, 90)
  ) +
  scale_x_continuous(
    breaks = seq(-180, 180, by = 60),
    labels = longitude_labels
  ) +
  scale_y_continuous(
    breaks = seq(-60, 90, by = 30),
    labels = latitude_labels
  ) +
  theme_minimal(base_size = 18) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.8
    ),
    
    axis.title = element_blank(),
    axis.text.x = element_text(
      size = 18,
      color = "black"
    ),
    axis.text.y = element_text(
      size = 18,
      color = "black"
    ),
    axis.ticks = element_line(color = "black"),
    
    legend.title = element_text(
      size = 13,
      face = "bold"
    ),
    legend.text = element_text(size = 11)
  )

print(p)

# === 6️⃣ 保存图像 ===
ggsave(
  "global_HHI_norm_sen_slope_map.png",
  plot = p,
  width = 12,
  height = 6,
  dpi = 600
)
