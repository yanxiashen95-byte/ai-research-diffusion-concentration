library(readxl)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

#========================
# 1 读入数据
#========================

univ_file <- "top_university.xlsx"              # 修改成你的大学文件名
pub_file  <- "annual_all_natural_country_publication_counts_with_income_sorted.csv"  # AI 出版物数量文件

univ_raw <- read_excel(univ_file, sheet = 1, .name_repair = "unique")
pub_raw  <- read_csv(pub_file, show_col_types = FALSE)

# 查看大学文件列名
print(names(univ_raw))

#========================
# 2 按大学文件中的 Country 列统计每个国家的 top university 数量
# 你的截图中完整国家名在第 5 列，也就是 E 列
#========================
print(names(univ_raw))

country_col <- names(univ_raw)[5]

univ_country <- univ_raw %>%
  mutate(
    Country = trimws(as.character(.data[[country_col]])),
    Country = ifelse(Country == "China (Mainland)", "China", Country)
  ) %>%
  filter(
    !is.na(Country),
    Country != ""
  ) %>%
  group_by(Country) %>%
  summarise(
    top_university_count = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(top_university_count))

write_csv(univ_country, "top_university_country_count.csv")

#========================
# 3 AI 发文量按国家汇总
# 建议用 2015–2024，对应现代 AI 快速发展阶段
#========================

year_start <- 2015
year_end   <- 2024

pub_country <- pub_raw %>%
  mutate(
    Country = trimws(as.character(Country)),
    Year = as.integer(Year),
    Count = as.numeric(Count)
  ) %>%
  filter(
    !is.na(Country),
    Country != "",
    !is.na(Count),
    Year >= year_start,
    Year <= year_end
  ) %>%
  group_by(Country) %>%
  summarise(
    total_pub = sum(Count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_pub))

#========================
# 4 按国家合并
# 没有 top university 的国家记为 0
#========================

plot_df_country <- pub_country %>%
  left_join(univ_country, by = "Country") %>%
  mutate(
    top_university_count = replace_na(top_university_count, 0),
    log_pub = log10(total_pub + 1),
    log_university = log10(top_university_count + 1)
  ) %>%
  filter(
    is.finite(log_pub),
    is.finite(log_university)
  )

cat("Countries used in plot:", nrow(plot_df_country), "\n")

write_csv(plot_df_country, "AI_publications_top_university_country_matched.csv")

# 检查没有匹配上的大学国家名
unmatched_univ <- anti_join(univ_country, pub_country, by = "Country")
write_csv(unmatched_univ, "unmatched_countries_in_top_university_file.csv")

#========================
# 5 Pearson 相关
#========================

cor_test <- cor.test(
  plot_df_country$log_university,
  plot_df_country$log_pub,
  method = "pearson"
)

label_text <- paste0(
  "r = ", sprintf("%.3f", unname(cor_test$estimate)), "\n",
  "p  ", ifelse(cor_test$p.value < 0.001, "< 0.001", sprintf("%.3f", cor_test$p.value))
)

#========================
# 6 作图
#========================

p_univ_pub <- ggplot(plot_df_country, aes(x = log_university, y = log_pub)) +
  geom_point(
    size = 3.2,
    alpha = 0.7,
    color = "#4575b4"
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    linewidth = 1.2,
    color = "#d73027"
  ) +
  annotate(
    "text",
    x = min(plot_df_country$log_university, na.rm = TRUE),
    y = max(plot_df_country$log_pub, na.rm = TRUE),
    label = label_text,
    hjust = 0,
    vjust = 1,
    size = 5,
    color = "black"
  ) +
  labs(
    x = "Top university count(log)",
    y = "Total AI publications(log)"
  ) +
  theme_classic(base_size = 17) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.1),
    axis.title = element_text(size = 17, color = "black"),
    axis.text = element_text(size = 14, color = "black")
  )

print(p_univ_pub)

#========================
# 7 保存图片
#========================

ggsave(
  "Top_university_vs_AI_publications_2015_2024.png",
  plot = p_univ_pub,
  width = 6.5,
  height = 5.2,
  dpi = 600
)
