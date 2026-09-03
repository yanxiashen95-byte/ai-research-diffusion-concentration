library(readr)
library(dplyr)
library(ggplot2)

#========================
# 1 读入数据
#========================

collab_file <- "country_partner_counts.csv"       # 合作伙伴数量文件，包含 Country, num_partners
pub_file    <- "annual_all_natural_country_publication_counts_with_income_sorted.csv"  # AI 出版物数量文件，包含 Country, Year, Count

collab_raw <- read_csv(collab_file, show_col_types = FALSE)
pub_raw    <- read_csv(pub_file, show_col_types = FALSE)

#========================
# 2 整理合作伙伴数量
#========================

collab_country <- collab_raw %>%
  mutate(
    Country = trimws(as.character(Country)),
    num_partners = as.numeric(num_partners)
  ) %>%
  filter(
    !is.na(Country),
    Country != "",
    !is.na(num_partners)
  ) %>%
  group_by(Country) %>%
  summarise(
    num_partners = sum(num_partners, na.rm = TRUE),
    .groups = "drop"
  )

#========================
# 3 AI 发文量按国家汇总
# 建议用 2015–2024，与前面结构性因素分析保持一致
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
  )

#========================
# 4 按国家合并
# 只保留成功匹配的国家
#========================

plot_df_country <- pub_country %>%
  inner_join(collab_country, by = "Country") %>%
  mutate(
    log_pub = log10(total_pub + 1),
    log_collab = log10(num_partners + 1)
  ) %>%
  filter(
    is.finite(log_pub),
    is.finite(log_collab)
  )

cat("Countries used in plot:", nrow(plot_df_country), "\n")

# 保存匹配后的数据
write_csv(plot_df_country, "AI_publications_collaboration_country_matched.csv")

# 输出未匹配国家，方便检查国家名
unmatched_pub <- anti_join(pub_country, collab_country, by = "Country")
unmatched_collab <- anti_join(collab_country, pub_country, by = "Country")

write_csv(unmatched_pub, "unmatched_countries_in_publication_file.csv")
write_csv(unmatched_collab, "unmatched_countries_in_collaboration_file.csv")

#========================
# 5 Pearson 相关
#========================

cor_test <- cor.test(
  plot_df_country$log_collab,
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

p_collab_pub <- ggplot(plot_df_country, aes(x = log_collab, y = log_pub)) +
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
    x = min(plot_df_country$log_collab, na.rm = TRUE),
    y = max(plot_df_country$log_pub, na.rm = TRUE),
    label = label_text,
    hjust = 0,
    vjust = 1,
    size = 5,
    color = "black"
  ) +
  labs(
    x =  "Partners(log)",
    y = "Total AI publications(log)"
  ) +
  theme_classic(base_size = 17) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.1),
    axis.title = element_text(size = 17, color = "black"),
    axis.text = element_text(size = 14, color = "black")
  )

print(p_collab_pub)

#========================
# 7 保存图片
#========================

ggsave(
  "Collaboration_vs_AI_publications_2015_2024.png",
  plot = p_collab_pub,
  width = 6.5,
  height = 5.2,
  dpi = 600
)

ggsave(
  "Collaboration_vs_AI_publications_2015_2024.pdf",
  plot = p_collab_pub,
  width = 6.5,
  height = 5.2
)