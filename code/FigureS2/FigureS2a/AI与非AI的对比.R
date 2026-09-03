library(tidyverse)
library(scales)

# ============================================================
# AI 与 Non-AI 年度发文量对比
#
# 输出两个版本：
# 1) consistent_WoS（推荐）：AI 与 Non-AI 均使用 WoS 汇总结果，统计口径一致；
# 2) requested_mixed（敏感性分析）：AI 使用唯一论文/第一作者国家数据，
#    Non-AI 使用 WoS 学科汇总结果。两条曲线口径不同，不宜比较绝对数量。
# ============================================================

ai_file <- "D:/博后/论文/3/需要上传的数据和代码/code/FigureS2/FigureS2a/annual_all_natural_country_publication_counts_with_income_sorted.csv"
wos_file <- "D:/博后/论文/3/需要上传的数据和代码/code/FigureS2/FigureS2a/annual_field_country_ai_non_ai_wos_aggregate.csv"

output_dir <- "D:/博后/论文/3/需要上传的数据和代码/code/FigureS2/FigureS2a"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# 1990—1995年的WoS ALL结果存在明显不完整和1996年结构性跳变，
# 因此主图与CAGR均从1996年开始；不要用不完整早期值计算增长率。
analysis_start_year <- 1996L
analysis_end_year <- 2025L

# =========================
# 1) 读取并整理唯一AI论文数据
# =========================
# 该文件按“年份－第一作者国家”排列；同一篇论文只归入一个国家。
ai_unique <- read_csv(ai_file, show_col_types = FALSE) %>%
  transmute(
    Year = as.integer(Year),
    AI_unique = as.numeric(Total_Paper_Count)
  ) %>%
  filter(!is.na(Year), !is.na(AI_unique)) %>%
  group_by(Year) %>%
  summarise(AI_unique = sum(AI_unique, na.rm = TRUE), .groups = "drop")

# =========================
# 2) 读取WoS汇总数据
# =========================
# field_year_* 在每一个国家行中重复出现，因此必须先按“学科－年份”去重，
# 绝对不能直接对所有国家行求和。
wos_field_year <- read_csv(wos_file, show_col_types = FALSE) %>%
  transmute(
    Field = oecd_field,
    Year = as.integer(year),
    all_records = as.numeric(field_year_all_records),
    ai_records = as.numeric(field_year_ai_records)
  ) %>%
  filter(!is.na(Field), !is.na(Year)) %>%
  group_by(Field, Year) %>%
  summarise(
    all_records = first(na.omit(all_records)),
    ai_records = first(na.omit(ai_records)),
    .groups = "drop"
  ) %>%
  mutate(non_ai_records = pmax(all_records - ai_records, 0))

# 六个领域相加。由于WoS Categories可以重叠，这里是field-count（学科归属次数），
# 不是跨领域去重后的唯一论文数。
wos_annual <- wos_field_year %>%
  group_by(Year) %>%
  summarise(
    AI = sum(ai_records, na.rm = TRUE),
    `Non-AI` = sum(non_ai_records, na.rm = TRUE),
    .groups = "drop"
  )

# 推荐版本：相同的WoS field-count口径
df_consistent <- wos_annual %>%
  filter(between(Year, analysis_start_year, analysis_end_year))

# 用户指定版本：AI为唯一论文；Non-AI为WoS field-count
df_mixed <- wos_annual %>%
  select(Year, `Non-AI`) %>%
  inner_join(ai_unique, by = "Year") %>%
  transmute(Year, AI = AI_unique, `Non-AI`) %>%
  filter(between(Year, analysis_start_year, analysis_end_year))

# =========================
# 3) CAGR及指数趋势线
# =========================
calc_cagr_curve <- function(data, value_col) {
  z <- data %>%
    select(Year, value = all_of(value_col)) %>%
    filter(!is.na(Year), !is.na(value), value > 0) %>%
    arrange(Year)

  if (nrow(z) < 2) stop(paste("CAGR至少需要两个正值：", value_col))

  start_year <- first(z$Year)
  end_year <- last(z$Year)
  start_value <- first(z$value)
  end_value <- last(z$value)
  n_intervals <- end_year - start_year
  cagr <- (end_value / start_value)^(1 / n_intervals) - 1

  list(
    cagr = cagr,
    trend = tibble(
      Year = seq(start_year, end_year),
      trend = start_value * (1 + cagr)^(seq(start_year, end_year) - start_year)
    )
  )
}

# =========================
# 4) 双轴绘图函数
# =========================
plot_ai_non_ai <- function(df, suffix) {
  df <- df %>% arrange(Year)
  ai_fit <- calc_cagr_curve(df, "AI")
  nonai_fit <- calc_cagr_curve(df, "Non-AI")

  # 将Non-AI线性映射到左轴；右轴按同一系数还原。
  scale_factor <- max(df$`Non-AI`, na.rm = TRUE) / max(df$AI, na.rm = TRUE)
  if (!is.finite(scale_factor) || scale_factor <= 0) stop("双轴缩放系数无效")

  plot_df <- df %>% mutate(NonAI_scaled = `Non-AI` / scale_factor)
  nonai_trend <- nonai_fit$trend %>% mutate(trend_scaled = trend / scale_factor)

  ai_col <- "#FF8081"
  nonai_col <- "#53A8E1"
  label_ai <- sprintf("AI: %.2f%%/yr", 100 * ai_fit$cagr)
  label_nonai <- sprintf("Non-AI: %.2f%%/yr", 100 * nonai_fit$cagr)

  p <- ggplot(plot_df, aes(x = Year)) +
    geom_line(aes(y = AI), color = ai_col, linewidth = 1.8) +
    geom_point(aes(y = AI), color = ai_col, size = 3.2) +
    geom_line(aes(y = NonAI_scaled), color = nonai_col, linewidth = 1.8) +
    geom_point(aes(y = NonAI_scaled), color = nonai_col, size = 3.2) +
    geom_line(
      data = ai_fit$trend,
      aes(x = Year, y = trend),
      inherit.aes = FALSE,
      color = ai_col, linewidth = 1.3, linetype = "dashed"
    ) +
    geom_line(
      data = nonai_trend,
      aes(x = Year, y = trend_scaled),
      inherit.aes = FALSE,
      color = nonai_col, linewidth = 1.3, linetype = "dashed"
    ) +
    annotate(
      "text", x = min(plot_df$Year) + 2,
      y = max(plot_df$AI, na.rm = TRUE) * 0.93,
      label = label_ai, hjust = 0, size = 7, color = ai_col
    ) +
    annotate(
      "text", x = min(plot_df$Year) + 2,
      y = max(plot_df$AI, na.rm = TRUE) * 0.79,
      label = label_nonai, hjust = 0, size = 7, color = nonai_col
    ) +
    scale_y_continuous(
      name = expression("AI publications (" * 10^3 * ")"),
      labels = label_number(scale = 1e-3, accuracy = 1),
      sec.axis = sec_axis(
        ~ . * scale_factor,
        name = expression("Non-AI publications (" * 10^5 * ")"),
        labels = label_number(scale = 1e-5, accuracy = 0.1)
      ),
      expand = expansion(mult = c(0.02, 0.06))
    ) +
    scale_x_continuous(
      breaks = seq(1995, 2025, 5),
      limits = range(plot_df$Year),
      expand = expansion(mult = c(0.01, 0.02))
    ) +
    labs(x = "Year") +
    theme_classic(base_size = 20) +
    theme(
      axis.title.y.left = element_text(size = 24, color = ai_col),
      axis.title.y.right = element_text(size = 24, color = nonai_col),
      axis.title.x = element_text(size = 24, color = "black"),
      axis.text = element_text(size = 24, color = "black"),
      axis.ticks = element_line(color = "black"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1.1),
      legend.position = "none",
      plot.margin = margin(10, 18, 10, 10)
    )

  ggsave(
    file.path(output_dir, paste0("AI_vs_NonAI_", suffix, ".png")),
    p, width = 10, height = 6.3, dpi = 600
  )

  write_csv(df, file.path(output_dir, paste0("annual_AI_NonAI_", suffix, ".csv")))
  write_csv(
    tibble(
      Series = c("AI", "Non-AI"),
      CAGR = c(ai_fit$cagr, nonai_fit$cagr),
      CAGR_percent_per_year = 100 * c(ai_fit$cagr, nonai_fit$cagr)
    ),
    file.path(output_dir, paste0("CAGR_AI_NonAI_", suffix, ".csv"))
  )

  print(p)
  invisible(p)
}

# =========================
# 5) 生成两个版本
# =========================
p_consistent <- plot_ai_non_ai(df_consistent, "consistent_WoS")
p_mixed <- plot_ai_non_ai(df_mixed, "requested_mixed")

message("完成。论文主分析建议使用：AI_vs_NonAI_consistent_WoS.png")
message("requested_mixed仅用于敏感性分析，因为AI与Non-AI统计口径不同。")
