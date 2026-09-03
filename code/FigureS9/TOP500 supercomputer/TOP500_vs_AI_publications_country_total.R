library(readxl)
library(readr)
library(dplyr)
library(ggplot2)

#========================
# 1 读入数据
#========================

top500_file <- "TOP500 supercomputer.xlsx"
pub_file    <- "annual_all_natural_country_publication_counts_with_income_sorted.csv"

top500_raw <- read_excel(top500_file, sheet = 1, .name_repair = "unique")
pub_raw    <- read_csv(pub_file, show_col_types = FALSE)

# 查看列名，确认 Country 列
print(names(top500_raw))

#========================
# 2 直接按照 TOP500 文件中的 Country 列统计数量
#========================

top500_country <- top500_raw %>%
  mutate(
    Country = trimws(as.character(Country))
  ) %>%
  filter(
    !is.na(Country),
    Country != ""
  ) %>%
  group_by(Country) %>%
  summarise(
    top500_count = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(top500_count))

print(top500_country, n = Inf)

# 保存 TOP500 国家统计结果
write_csv(top500_country, "TOP500_country_count.csv")

#========================
# 3 AI 发文量按国家汇总
#========================

pub_country <- pub_raw %>%
  mutate(
    Country = trimws(as.character(Country)),
    Count = as.numeric(Count)
  ) %>%
  filter(
    !is.na(Country),
    Country != "",
    !is.na(Count)
  ) %>%
  group_by(Country) %>%
  summarise(
    total_pub = sum(Count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_pub))

#========================
# 4 按国家合并
#========================

plot_df_country <- pub_country %>%
  inner_join(top500_country, by = "Country") %>%
  mutate(
    log_pub = log10(total_pub + 1),
    log_top500 = log10(top500_count + 1)
  )

cat("Matched countries:", nrow(plot_df_country), "\n")

print(plot_df_country %>% arrange(desc(top500_count)), n = Inf)

# 保存合并后的数据
write_csv(plot_df_country, "AI_publications_TOP500_country_matched.csv")

# 输出没有匹配上的国家，方便检查国家名
unmatched_pub <- anti_join(pub_country, top500_country, by = "Country")
unmatched_top500 <- anti_join(top500_country, pub_country, by = "Country")

write_csv(unmatched_pub, "unmatched_countries_in_publication_file.csv")
write_csv(unmatched_top500, "unmatched_countries_in_TOP500_file.csv")

#========================
# 5 Pearson 相关
#========================

cor_test <- cor.test(
  plot_df_country$log_top500,
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

p_top500_pub <- ggplot(plot_df_country, aes(x = log_top500, y = log_pub)) +
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
    x = min(plot_df_country$log_top500, na.rm = TRUE),
    y = max(plot_df_country$log_pub, na.rm = TRUE),
    label = label_text,
    hjust = 0,
    vjust = 1,
    size = 5,
    color = "black"
  ) +
  labs(
    x = "Top500 supercomputer(log)",
    y = "Total AI publications(log)"
  ) +
  theme_classic(base_size = 17) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.1),
    axis.title = element_text(size = 17, color = "black"),
    axis.text = element_text(size = 14, color = "black")
  )

print(p_top500_pub)

ggsave(
  "TOP500_vs_AI_publications_country_total.png",
  plot = p_top500_pub,
  width = 6.5,
  height = 5.2,
  dpi = 600
)