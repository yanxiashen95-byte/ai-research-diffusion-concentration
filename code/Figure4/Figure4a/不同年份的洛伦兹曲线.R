library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

df_total <- read_csv("annual_all_natural_country_publication_counts_with_income_sorted.csv",
                     show_col_types = FALSE)

df_sel <- df_total %>%
  filter(Year %in% c(2014, 2022, 2024))

lorenz_year <- df_sel %>%
  group_by(Year) %>%
  arrange(Total_Paper_Count, .by_group = TRUE) %>%
  mutate(
    n = n(),
    x = row_number() / n,
    y = cumsum(Total_Paper_Count) / sum(Total_Paper_Count)
  ) %>%
  reframe(
    x = c(0, x),
    y = c(0, y)
  )

p <- ggplot(lorenz_year, aes(x = x, y = y, color = factor(Year))) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 1.5) +
  geom_line(linewidth = 3) +
  scale_color_manual(values = c(
    "2014" = "#74add1",
    "2022" = "#fdae61",
    "2024" = "#d73027"
  )) +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    expand = c(0, 0)
  ) +
  labs(
    x = "Cumulative share of countries",
    y = "Cumulative share of publications",
    color = "Year"
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5),
    axis.title = element_text(size = 21),
    axis.text  = element_text(size = 22),
    legend.position = "none",
    plot.margin = margin(t = 10, r = 30, b = 10, l = 15)
  )

print(p)

ggsave("20260826_lorenz_curves_year.png", p, width = 8.8, height = 5, dpi = 600)


#============ 计算与45°的面积
library(dplyr)

# 计算每个年份的洛伦兹曲线下面积
area_df <- lorenz_year %>%
  group_by(Year) %>%
  arrange(x, .by_group = TRUE) %>%
  mutate(
    dx = x - lag(x),
    avg_y = (y + lag(y)) / 2,
    trapezoid = dx * avg_y
  ) %>%
  summarise(
    Lorenz_area = sum(trapezoid, na.rm = TRUE),
    Gini_from_area = 1 - 2 * Lorenz_area,
    Inequality_area = 0.5 - Lorenz_area,
    .groups = "drop"
  )

print(area_df)

#============== 给图画图例 ================================
# ============================================================
# 自定义 Year-Gini 图例
# ============================================================

rm(list = ls())
gc()

library(ggplot2)
library(dplyr)


# ============================================================
# 1. 数据
# ============================================================

legend_data <- data.frame(
  Year = factor(
    c("2014", "2022", "2025"),
    levels = c("2014", "2022", "2025")
  ),
  Gini = c(
    0.78,
    0.88,
    0.90
  ),
  y = c(
    2,
    1.5,
    1
  )
)


# ============================================================
# 2. 颜色
# ============================================================

year_colors <- c(
  "2014" = "#74add1",
  "2022" = "#fdae61",
  "2025" = "#d73027"
)


# ============================================================
# 3. 绘图
# ============================================================

p <- ggplot() +
  
  # ----------------------------------------------------------
# 左侧色块
# ----------------------------------------------------------

geom_segment(
  data = legend_data,
  aes(
    x = 0.25,
    xend = 0.45,
    y = y,
    yend = y,
    color = Year
  ),
  linewidth = 7,
  lineend = "butt",
  show.legend = FALSE
) +
  
  
  # ----------------------------------------------------------
# Year
# ----------------------------------------------------------

geom_text(
  data = legend_data,
  aes(
    x = 0.7,
    y = y,
    label = Year
  ),
  size = 8,
  hjust = 0.5
) +
  
  
  # ----------------------------------------------------------
# Gini
# ----------------------------------------------------------

geom_text(
  data = legend_data,
  aes(
    x = 1.05,
    y = y,
    label = sprintf("%.2f", Gini)
  ),
  size = 8,
  hjust = 0.5
) +
  
  
  # ----------------------------------------------------------
# 表头
# ----------------------------------------------------------

annotate(
  "text",
  x = 0.35,
  y = 2.5,
  label = "Line",
  size = 8,
  fontface = "bold"
) +
  
  annotate(
    "text",
    x = 0.7,
    y = 2.5,
    label = "Year",
    size = 8,
    fontface = "bold"
  ) +
  
  annotate(
    "text",
    x = 1.05,
    y = 2.5,
    label = "Gini",
    size = 8,
    fontface = "bold"
  ) +
  
  
  # ----------------------------------------------------------
# 手动颜色
# ----------------------------------------------------------

scale_color_manual(
  values = year_colors
) +
  
  
  # ----------------------------------------------------------
# 控制画布范围
# ----------------------------------------------------------

coord_cartesian(
  xlim = c(0, 3),
  ylim = c(0.5, 4.35),
  expand = FALSE,
  clip = "off"
) +
  
  
  # ----------------------------------------------------------
# 去除所有坐标轴和背景
# ----------------------------------------------------------

theme_void() +
  
  theme(
    plot.margin = margin(
      5,
      5,
      5,
      5
    )
  )


# ============================================================
# 4. 显示
# ============================================================

print(p)