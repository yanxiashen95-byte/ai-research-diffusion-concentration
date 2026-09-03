library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

#========================
# 1 读入数据
#========================

rd_file  <- "R&D expenditure.csv"          # 修改为你的 R&D expenditure 文件名
pub_file <- "annual_all_natural_country_publication_counts_with_income_sorted.csv"       # 修改为你的出版物数量文件名

rd_raw  <- read_csv(rd_file, show_col_types = FALSE)
pub_raw <- read_csv(pub_file, show_col_types = FALSE)

#========================
# 2 整理 R&D expenditure 宽表
#========================

rd_long <- rd_raw %>%
  rename(Country = 1) %>%
  pivot_longer(
    cols = matches("^\\d{4}$"),
    names_to = "Year",
    values_to = "rd_expenditure"
  ) %>%
  mutate(
    Country = trimws(Country),
    Year = as.integer(Year),
    rd_expenditure = as.numeric(rd_expenditure)
  ) %>%
  filter(
    !is.na(Country),
    !is.na(Year),
    !is.na(rd_expenditure)
  )

#========================
# 3 整理出版物数量
#========================

pub_df <- pub_raw %>%
  mutate(
    Country = trimws(Country),
    Year = as.integer(Year),
    Count = as.numeric(Count)
  ) %>%
  filter(
    !is.na(Country),
    !is.na(Year),
    !is.na(Count)
  ) %>%
  group_by(Country, Year) %>%
  summarise(
    Count = sum(Count, na.rm = TRUE),
    .groups = "drop"
  )

#========================
# 4 设置分析年份
#========================

year_start <- 2015
year_end   <- 2024

#========================
# 5 按国家汇总，一个国家一个点
#========================

rd_country <- rd_long %>%
  filter(Year >= year_start, Year <= year_end) %>%
  group_by(Country) %>%
  summarise(
    mean_rd = mean(rd_expenditure, na.rm = TRUE),
    n_rd_years = sum(!is.na(rd_expenditure)),
    .groups = "drop"
  ) %>%
  filter(!is.na(mean_rd))

pub_country <- pub_df %>%
  filter(Year >= year_start, Year <= year_end) %>%
  group_by(Country) %>%
  summarise(
    total_pub = sum(Count, na.rm = TRUE),
    mean_pub = mean(Count, na.rm = TRUE),
    n_pub_years = n_distinct(Year),
    .groups = "drop"
  )

plot_df_country <- pub_country %>%
  inner_join(rd_country, by = "Country") %>%
  filter(
    !is.na(total_pub),
    !is.na(mean_rd),
    total_pub >= 0,
    mean_rd >= 0
  ) %>%
  mutate(
    log_pub = log10(total_pub + 1),
    log_rd  = log10(mean_rd + 1)
  )

cat("Matched countries:", nrow(plot_df_country), "\n")

#========================
# 6 Pearson 相关
#========================

cor_test <- cor.test(
  plot_df_country$log_rd,
  plot_df_country$log_pub,
  method = "pearson"
)

label_text <- paste0(
  "r = ", sprintf("%.3f", unname(cor_test$estimate)), "\n",
  "p  ", ifelse(cor_test$p.value < 0.001, "< 0.001", sprintf("%.3f", cor_test$p.value))
)

#========================
# 7 作图
#========================

p_rd_country <- ggplot(plot_df_country, aes(x = log_rd, y = log_pub)) +
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
    x = min(plot_df_country$log_rd, na.rm = TRUE),
    y = max(plot_df_country$log_pub, na.rm = TRUE),
    label = label_text,
    hjust = 0,
    vjust = 1,
    size = 5,
    color = "black"
  ) +
  labs(
    x = "R&D expenditure(log)",
    y = "Total AI publications(log)"
  ) +
  theme_classic(base_size = 17) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.1),
    axis.title = element_text(size = 17, color = "black"),
    axis.text = element_text(size = 14, color = "black")
  )

print(p_rd_country)

#========================
# 7 保存图片
#========================

ggsave(
  "R&D_expenditure_vs_AI_publications.png",
  plot = p_rd_country,
  width = 6.5,
  height = 5.2,
  dpi = 600
)
