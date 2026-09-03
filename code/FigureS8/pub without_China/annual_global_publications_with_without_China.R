library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# ============================================================
# 1. 读取数据
# ============================================================

df <- read_csv(
  "annual_all_natural_country_publication_counts.csv",
  show_col_types = FALSE
) %>%
  mutate(
    Year = as.integer(Year),
    Country = trimws(Country),
    Count = as.numeric(Count)
  )


# ============================================================
# 2. 每年全球总发文量：包含中国
# ============================================================

annual_with_china <- df %>%
  group_by(Year) %>%
  summarise(
    Publications = sum(Count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Scenario = "With China"
  )


# ============================================================
# 3. 每年全球总发文量：排除中国
# ============================================================

annual_without_china <- df %>%
  filter(
    Country != "China"
  ) %>%
  group_by(Year) %>%
  summarise(
    Publications = sum(Count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Scenario = "Without China"
  )


# ============================================================
# 4. 合并
# ============================================================

plot_data <- bind_rows(
  annual_with_china,
  annual_without_china
) %>%
  mutate(
    Scenario = factor(
      Scenario,
      levels = c(
        "With China",
        "Without China"
      )
    )
  )


# 查看结果
print(plot_data, n = Inf)


# ============================================================
# 5. 绘图
# ============================================================

p <- ggplot(
  plot_data,
  aes(
    x = Year,
    y = Publications,
    color = Scenario,
    linetype = Scenario
  )
) +
  
  geom_line(
    linewidth = 2
  ) +
  
  geom_point(
    size = 3.5
  ) +
  
  scale_color_manual(
    values = c(
      "With China" = "#FF8081",
      "Without China" = "#53A8E1"
    )
  ) +
  
  scale_linetype_manual(
    values = c(
      "With China" = "solid",
      "Without China" = "dashed"
    )
  ) +
  
  scale_x_continuous(
    breaks = seq(
      1990,
      2025,
      by = 5
    ),
    limits = c(
      1990,
      2025
    ),
    expand = c(
      0,
      0
    )
  ) +
  
  scale_y_continuous(
    breaks = c(25000, 50000, 75000),
    labels = c("25", "50", "75"),
    expand = expansion(
      mult = c(0, 0.05)
    )
  ) +
  
  labs(
    x = "Year",
    y = "Number of publications (10³)",
    color = NULL,
    linetype = NULL
  ) +
  
  theme_minimal(
    base_size = 18
  ) +
  
  theme(
    panel.grid = element_blank(),
    
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 1.2
    ),
    
    axis.title = element_text(
      size = 22,
      color = "black"
    ),
    
    axis.text = element_text(
      size = 22,
      color = "black"
    ),
    
    legend.position = "right",
    
    legend.text = element_text(
      size = 15
    )
  )

print(p)


# ============================================================
# 6. 保存年度数据
# ============================================================

write_csv(
  plot_data,
  "annual_global_publications_with_without_China.csv"
)


# ============================================================
# 7. 保存图片
# ============================================================

ggsave(
  "annual_global_publications_with_without_China.png",
  plot = p,
  width = 10,
  height = 5,
  dpi = 600
)