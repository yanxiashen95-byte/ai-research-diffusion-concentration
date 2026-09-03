library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

#========================
# 1 读入数据
#========================

df_raw <- read_csv("annual_all_natural_country_publication_counts_with_income_sorted.csv", show_col_types = FALSE)

# 你的文件应包含 Country, Year, Count 三列

df <- df_raw %>%
  mutate(
    Country = trimws(Country),
    Year = as.integer(Year),
    Count = as.numeric(Count)
  ) %>%
  filter(!is.na(Country), !is.na(Year), !is.na(Count))

#========================
# 2 如果同一个国家同一年有多行，先汇总
#========================

df_count <- df %>%
  group_by(Country, Year) %>%
  summarise(
    Count = sum(Count, na.rm = TRUE),
    .groups = "drop"
  )

#========================
# 3 构建固定国家 年份面板，并补 0
#========================

all_countries <- sort(unique(df_count$Country))
all_years <- seq(min(df_count$Year, na.rm = TRUE),
                 max(df_count$Year, na.rm = TRUE),
                 by = 1)

df_panel <- expand_grid(
  Country = all_countries,
  Year = all_years
) %>%
  left_join(df_count, by = c("Country", "Year")) %>%
  mutate(
    Count = ifelse(is.na(Count), 0, Count)
  )

#========================
# 4 逐年计算 Top 1 / Top 5 / Top 10 / Long-tail share
#========================

top_share_df <- df_panel %>%
  group_by(Year) %>%
  arrange(desc(Count), .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  summarise(
    total_pub = sum(Count, na.rm = TRUE),
    top1_pub = sum(Count[rank <= 1], na.rm = TRUE),
    top5_pub = sum(Count[rank <= 5], na.rm = TRUE),
    top10_pub = sum(Count[rank <= 10], na.rm = TRUE),
    long_tail_pub = total_pub - top10_pub,
    n_country = n_distinct(Country),
    n_active_country = sum(Count > 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    top1_share = top1_pub / total_pub,
    top5_share = top5_pub / total_pub,
    top10_share = top10_pub / total_pub,
    long_tail_share = long_tail_pub / total_pub
  )

# 保存计算结果
write_csv(top_share_df, "top_country_publication_shares.csv")

#========================
# 5 转成长格式用于画图
#========================

plot_top_share <- top_share_df %>%
  select(Year, top1_share, top5_share, top10_share, long_tail_share) %>%
  pivot_longer(
    cols = c(top1_share, top5_share, top10_share, long_tail_share),
    names_to = "Metric",
    values_to = "Share"
  ) %>%
  mutate(
    Metric = recode(
      Metric,
      "top1_share" = "Top 1",
      "top5_share" = "Top 5",
      "top10_share" = "Top 10",
      "long_tail_share" = "Outside top 10"
    ),
    Metric = factor(
      Metric,
      levels = c("Top 1", "Top 5", "Top 10", "Outside top 10")
    )
  )

#========================
# 6 作图
#========================

my_cols <- c(
  "Top 1" = "#d73027",
  "Top 5" = "#f46d43",
  "Top 10" = "#4575b4",
  "Outside top 10" = "grey70"
)

p_top_share <- ggplot(
  plot_top_share,
  aes(x = Year, y = Share, color = Metric)
) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 2.8) +
  scale_color_manual(values = my_cols) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0.03))
  ) +
  scale_x_continuous(
    breaks = seq(2000, max(plot_top_share$Year), by = 5),
    limits = c(2000,  max(plot_top_share$Year)),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  labs(
    x = "Year",
    y = "Share of AI publications",
    color = NULL
  ) +
  theme_classic(base_size = 18) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.1),
    axis.title = element_text(size = 20, color = "black"),
    axis.text = element_text(size = 20, color = "black"),
    legend.position = "bottom",
    legend.text = element_text(size = 15)
  )

print(p_top_share)

#========================
# 7 保存图片
#========================

ggsave(
  "top_country_publication_shares.png",
  plot = p_top_share,
  width = 7.5,
  height = 5,
  dpi = 600
)
