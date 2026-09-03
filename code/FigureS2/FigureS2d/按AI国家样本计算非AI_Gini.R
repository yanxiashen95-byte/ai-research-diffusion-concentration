library(tidyverse)
library(ineq)

# ============================================================
# 严格按照既有AI国家样本计算Non-AI跨国Gini
#
# 核心规则：
# - 每一年Non-AI只使用AI国家表在该年出现的国家；
# - 收入分组直接继承AI国家表；
# - AI曲线直接使用既有annual_Gini文件中的Global结果；
# - Non-AI国家数来自WoS六学科field-count汇总。
# ============================================================

ai_country_file <- "D:/博后/论文/3/需要上传的数据和代码/code/FigureS2/FigureS2d/annual_all_natural_country_publication_counts_with_income_sorted.csv"
ai_gini_file <- "D:/博后/论文/3/需要上传的数据和代码/code/FigureS2/FigureS2d/annual_Gini_global_income_groups_2000_2025.csv"
non_ai_wos_file <- "D:/Python/WOS_AI/OECD_Natural_Sciences_1990_2025/annual_field_country_ai_non_ai_wos_aggregate.csv"
output_dir <- "D:/博后/论文/3/需要上传的数据和代码/code/FigureS2/FigureS2d"

start_year <- 2000L
end_year <- 2025L
B <- 1000L
set.seed(20260901)

clean_key <- function(x) {
  x %>%
    str_squish() %>%
    str_to_lower()
}

# =========================
# 1) AI国家－年份样本（决定Non-AI每年的国家集合）
# =========================
ai_country_sample <- read_csv(ai_country_file, show_col_types = FALSE) %>%
  transmute(
    Year = as.integer(Year),
    Country = str_squish(Country),
    country_key = clean_key(Country),
    AI_Count = as.numeric(Total_Paper_Count),
    `Income group` = str_squish(`Income group`)
  ) %>%
  filter(
    between(Year, start_year, end_year),
    !is.na(Country), Country != "",
    !is.na(AI_Count)
  ) %>%
  group_by(Year, Country, country_key, `Income group`) %>%
  summarise(AI_Count = sum(AI_Count, na.rm = TRUE), .groups = "drop")

# =========================
# 2) WoS Non-AI国家名称统一
# =========================
# WoS将英国拆分为England/Scotland/Wales/North Ireland，
# 而AI国家表使用United Kingdom，因此这里先合并。
non_ai_country_year <- read_csv(non_ai_wos_file, show_col_types = FALSE) %>%
  transmute(
    Year = as.integer(year),
    Country_WoS = str_squish(str_to_upper(country_region)),
    Field = oecd_field,
    Non_AI_Count = as.numeric(non_ai_publications)
  ) %>%
  filter(
    between(Year, start_year, end_year),
    !is.na(Country_WoS), Country_WoS != "",
    !is.na(Non_AI_Count), Non_AI_Count >= 0
  ) %>%
  mutate(
    Country_mapped = case_when(
      Country_WoS == "PEOPLES R CHINA" ~ "China",
      Country_WoS %in% c("ENGLAND", "SCOTLAND", "WALES", "NORTH IRELAND") ~ "United Kingdom",
      Country_WoS %in% c("BOSNIA HERCEG", "BOSNIA HERZEG") ~ "Bosnia & Herceg",
      TRUE ~ str_to_title(Country_WoS)
    ),
    country_key = clean_key(Country_mapped)
  ) %>%
  # 先去除完全重复的国家－年份－学科记录
  group_by(Year, country_key, Country_mapped, Field) %>%
  summarise(Non_AI_Count = max(Non_AI_Count, na.rm = TRUE), .groups = "drop") %>%
  # 六个学科相加；英国各组成地区也在这里合并
  group_by(Year, country_key) %>%
  summarise(Non_AI_Count = sum(Non_AI_Count, na.rm = TRUE), .groups = "drop")

# =========================
# 3) 严格套用AI国家－年份名单
# =========================
analysis_panel <- ai_country_sample %>%
  left_join(non_ai_country_year, by = c("Year", "country_key")) %>%
  mutate(
    # 国家确实在AI样本中、但WoS Non-AI没有对应记录时按0处理
    Non_AI_Count = replace_na(Non_AI_Count, 0)
  ) %>%
  arrange(Year, Country)

write_csv(
  analysis_panel,
  file.path(output_dir, "annual_country_AI_NonAI_same_AI_country_sample.csv")
)

# 名称匹配检查
match_check <- analysis_panel %>%
  group_by(Year) %>%
  summarise(
    n_AI_sample_countries = n_distinct(Country),
    n_nonAI_positive = sum(Non_AI_Count > 0),
    n_nonAI_zero_or_unmatched = sum(Non_AI_Count == 0),
    .groups = "drop"
  )

write_csv(
  match_check,
  file.path(output_dir, "country_matching_check_nonAI_same_AI_sample.csv")
)

# =========================
# 4) 与原代码完全相同的Gini＋bootstrap SE算法
# =========================
summarise_gini <- function(data, group_vars, group_label = NULL) {
  out <- data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      Gini = ineq::Gini(Non_AI_Count, na.rm = TRUE),
      Gini_se = sd(
        replicate(
          B,
          ineq::Gini(sample(Non_AI_Count, replace = TRUE), na.rm = TRUE)
        ),
        na.rm = TRUE
      ),
      Gini_lwr = Gini - Gini_se,
      Gini_upr = Gini + Gini_se,
      n_countries = n_distinct(Country),
      .groups = "drop"
    )

  if (!is.null(group_label)) out <- out %>% mutate(Group = group_label)
  out
}

non_ai_global <- summarise_gini(analysis_panel, "Year", "Global")

non_ai_income <- analysis_panel %>%
  filter(
    !is.na(`Income group`),
    `Income group` %in% c(
      "Low income",
      "Lower middle income",
      "Upper middle income",
      "High income"
    )
  ) %>%
  summarise_gini(c("Year", "Income group")) %>%
  rename(Group = `Income group`)

non_ai_gini <- bind_rows(non_ai_global, non_ai_income) %>%
  mutate(
    Gini = ifelse(Group == "Low income" & Gini <= 1e-8, NA, Gini),
    Gini_se = ifelse(Group == "Low income" & is.na(Gini), NA, Gini_se),
    Gini_lwr = pmax(Gini_lwr, 0, na.rm = FALSE),
    Gini_upr = pmin(Gini_upr, 1, na.rm = FALSE),
    Group = factor(
      Group,
      levels = c(
        "Global", "Low income", "Lower middle income",
        "Upper middle income", "High income"
      )
    )
  ) %>%
  arrange(Group, Year)

write_csv(
  non_ai_gini,
  file.path(output_dir, "annual_Gini_nonAI_same_AI_countries_global_income_2000_2025.csv")
)

# =========================
# 5) 使用用户既有AI Gini Global结果进行对比
# =========================
ai_global <- read_csv(ai_gini_file, show_col_types = FALSE) %>%
  filter(Group == "Global", between(as.integer(Year), start_year, end_year)) %>%
  transmute(Year = as.integer(Year), AI = as.numeric(Gini))

comparison <- non_ai_global %>%
  transmute(Year, `Non-AI` = Gini) %>%
  inner_join(ai_global, by = "Year") %>%
  select(Year, AI, `Non-AI`) %>%
  arrange(Year)

write_csv(
  comparison,
  file.path(output_dir, "annual_Gini_AI_vs_nonAI_same_AI_countries_2000_2025.csv")
)

plot_data <- comparison %>%
  pivot_longer(c(AI, `Non-AI`), names_to = "Type", values_to = "Gini") %>%
  mutate(Type = factor(Type, levels = c("AI", "Non-AI")))

line_colors <- c("AI" = "#FF8081", "Non-AI" = "#53A8E1")

p <- ggplot(plot_data, aes(Year, Gini, color = Type, group = Type)) +
  geom_line(linewidth = 2.1) +
  geom_point(size = 4.1) +
  scale_color_manual(values = line_colors) +
  scale_x_continuous(
    breaks = seq(2000, 2025, 5),
    limits = c(2000, 2025),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    limits = c(0.6, 1),
    breaks = seq(0.6, 1, 0.1),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  labs(x = "Year", y = "Cross-country Gini coefficient", color = NULL) +
  theme_classic(base_size = 20) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.1),
    axis.title = element_text(size = 26, color = "black"),
    axis.text = element_text(size = 26, color = "black"),
    legend.position = "right",
    legend.text = element_text(size = 19),
    legend.key.width = unit(1.4, "cm")
  )

print(p)

ggsave(
  file.path(output_dir, "AI_vs_nonAI_Gini_same_AI_countries_2000_2025.png"),
  p, width = 12, height = 6, dpi = 600
)

ggsave(
  file.path(output_dir, "AI_vs_nonAI_Gini_same_AI_countries_2000_2025.pdf"),
  p, width = 10.5, height = 6.2
)

message("完成：Non-AI已严格按照AI国家表中每年的国家样本重新计算。")
