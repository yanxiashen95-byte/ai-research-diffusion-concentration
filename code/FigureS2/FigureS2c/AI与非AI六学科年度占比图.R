library(tidyverse)
library(scales)

# ============================================================
# AI与Non-AI六个OECD自然科学领域的年度发文占比
# 数据来自最新WoS汇总结果。
# ============================================================

source_file <- "D:/Python/WOS_AI/OECD_Natural_Sciences_1990_2025/annual_field_country_ai_non_ai_wos_aggregate.csv"
output_dir <- "D:/博后/论文/3/需要上传的数据和代码/code/FigureS2/FigureS2c"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

start_year <- 1996L
end_year <- 2025L
break_year <- 2012L  # 可根据论文分期修改

field_levels <- c(
  "Earth and environmental sciences",
  "Chemical sciences",
  "Biological sciences",
  "Physical sciences",
  "Computer and information sciences",
  "Mathematics"
)

field_colors <- c(
  "Mathematics" = "#bcbddc",
  "Computer and information sciences" = "#53A8E1",
  "Physical sciences" = "#99D096",
  "Chemical sciences" = "#F2C14E",
  "Earth and environmental sciences" = "#FF8081",
  "Biological sciences" = "#C9CED6"
)

# =========================
# 1) 提取唯一的“学科－年份”总量
# =========================
# field_year_*在每个国家行中重复，因此必须先按学科－年份取一次，
# 绝不能对国家行求和。
field_year <- read_csv(source_file, show_col_types = FALSE) %>%
  transmute(
    Field_original = oecd_field,
    Year = as.integer(year),
    All_publications = as.numeric(field_year_all_records),
    AI_publications = as.numeric(field_year_ai_records)
  ) %>%
  mutate(
    Field = recode(
      Field_original,
      "Earth and related environmental sciences" = "Earth and environmental sciences"
    )
  ) %>%
  filter(
    between(Year, start_year, end_year),
    Field %in% field_levels
  ) %>%
  group_by(Field, Year) %>%
  summarise(
    All_publications = first(na.omit(All_publications)),
    AI_publications = first(na.omit(AI_publications)),
    .groups = "drop"
  ) %>%
  mutate(
    Non_AI_publications = pmax(All_publications - AI_publications, 0)
  )

# 检查每一年是否六个学科齐全
coverage_check <- field_year %>%
  count(Year, name = "n_fields")

if (any(coverage_check$n_fields != 6)) {
  stop("存在学科不齐全的年份，请检查coverage_check。")
}

# =========================
# 2) 转成长表并计算年度内部占比
# =========================
plot_data <- field_year %>%
  select(Year, Field, AI = AI_publications, `Non-AI` = Non_AI_publications) %>%
  pivot_longer(
    cols = c(AI, `Non-AI`),
    names_to = "Publication_type",
    values_to = "Publication_count"
  ) %>%
  group_by(Year, Publication_type) %>%
  mutate(
    Total_in_year = sum(Publication_count, na.rm = TRUE),
    Share = if_else(Total_in_year > 0, Publication_count / Total_in_year, NA_real_)
  ) %>%
  ungroup() %>%
  mutate(
    Field = factor(Field, levels = field_levels),
    Publication_type = factor(
      Publication_type,
      levels = c("AI", "Non-AI"),
      labels = c("AI-related publications", "Non-AI publications")
    )
  ) %>%
  arrange(Publication_type, Year, Field)

write_csv(
  plot_data,
  file.path(output_dir, "annual_AI_NonAI_six_field_publication_shares.csv")
)

# 每个年份、每种类型的占比之和必须等于1
share_check <- plot_data %>%
  group_by(Year, Publication_type) %>%
  summarise(sum_share = sum(Share), .groups = "drop")

if (any(abs(share_check$sum_share - 1) > 1e-10)) {
  stop("年度学科占比之和不等于1。")
}

# =========================
# 3) 绘制上下两幅100%堆积面积图
# =========================
panel_labels <- tibble(
  Publication_type = factor(
    c("AI-related publications", "Non-AI publications"),
    levels = c("AI-related publications", "Non-AI publications")
  ),
  Year = start_year + 0.8,
  Share = 0.965,
  label = c("AI-related publications", "Non-AI publications")
)

p <- ggplot(
  plot_data,
  aes(x = Year, y = Share, fill = Field)
) +
  geom_area(
    position = "stack",
    color = "white",
    linewidth = 0.25,
    alpha = 0.96
  ) +
  geom_text(
    data = panel_labels,
    aes(x = Year, y = Share, label = label),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 1,
    size = 8,
    color = "black"
  ) +
  facet_grid(
    rows = vars(Publication_type),
    scales = "fixed",
    switch = "y"
  ) +
  scale_fill_manual(
    values = field_colors,
    breaks = field_levels,
    labels = c(
      "Earth and environmental sciences",
      "Chemical sciences",
      "Biological sciences",
      "Physical sciences",
      "Computer and information sciences",
      "Mathematics"
    )
  ) +
  scale_x_continuous(
    breaks = seq(1995, 2025, 5),
    limits = c(start_year, end_year),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.25),
    labels = percent_format(accuracy = 1),
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  labs(
    x = "Year",
    y = "Share of publications",
    fill = NULL
  ) +
  theme_classic(base_size = 18) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.0),
    panel.spacing.y = unit(0.20, "cm"),
    axis.title = element_text(size = 24, color = "black"),
    axis.text = element_text(size = 24, color = "black"),
    strip.background = element_blank(),
    strip.text = element_blank(),
    legend.position = "right",
    legend.text = element_text(size = 14),
    legend.key.height = unit(0.65, "cm"),
    legend.key.width = unit(0.75, "cm"),
    plot.margin = margin(8, 10, 8, 8)
  )

print(p)

ggsave(
  file.path(output_dir, "AI_vs_NonAI_six_field_annual_shares.png"),
  p, width = 12, height = 8.0, dpi = 600
)

ggsave(
  file.path(output_dir, "AI_vs_NonAI_six_field_annual_shares.pdf"),
  p, width = 11.5, height = 8.0
)

message("完成：AI与Non-AI六学科年度占比图及底表已输出。")
