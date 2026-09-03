#=============== without China =======================
library(readr)
library(dplyr)
library(ggplot2)
library(ineq)

# ============================================================
# 1. 读取完整数据，并在代码中排除 China
# ============================================================

df_raw <- read_csv(
  "annual_all_natural_country_publication_counts_with_income_sorted.csv",
  show_col_types = FALSE
) %>%
  mutate(
    Country = trimws(Country),
    Year = as.integer(Year),
    Total_Paper_Count = as.numeric(Total_Paper_Count),
    `Income group` = trimws(`Income group`)
  ) %>%
  filter(
    Country != "China"
  )


# 检查 China 是否已经完全删除
cat(
  "删除 China 后剩余 China 行数：",
  sum(df_raw$Country == "China", na.rm = TRUE),
  "\n"
)


B <- 1000


# ============================================================
# 2. Global Gini
# ============================================================

df_global <- df_raw %>%
  filter(
    !is.na(Year),
    !is.na(Total_Paper_Count)
  ) %>%
  group_by(Year) %>%
  summarise(
    Gini = ineq::Gini(
      Total_Paper_Count,
      na.rm = TRUE
    ),
    
    Gini_se = sd(
      replicate(
        B,
        ineq::Gini(
          sample(
            Total_Paper_Count,
            replace = TRUE
          ),
          na.rm = TRUE
        )
      ),
      na.rm = TRUE
    ),
    
    Gini_lwr = Gini - Gini_se,
    Gini_upr = Gini + Gini_se,
    
    Group = "Global",
    
    .groups = "drop"
  )


# ============================================================
# 3. Income-group Gini
# ============================================================

df_income <- df_raw %>%
  filter(
    !is.na(Year),
    !is.na(`Income group`),
    !is.na(Total_Paper_Count)
  ) %>%
  filter(
    `Income group` %in% c(
      "Low income",
      "Lower middle income",
      "Upper middle income",
      "High income"
    )
  ) %>%
  group_by(
    Year,
    `Income group`
  ) %>%
  summarise(
    Gini = ineq::Gini(
      Total_Paper_Count,
      na.rm = TRUE
    ),
    
    Gini_se = sd(
      replicate(
        B,
        ineq::Gini(
          sample(
            Total_Paper_Count,
            replace = TRUE
          ),
          na.rm = TRUE
        )
      ),
      na.rm = TRUE
    ),
    
    Gini_lwr = Gini - Gini_se,
    Gini_upr = Gini + Gini_se,
    
    .groups = "drop"
  ) %>%
  rename(
    Group = `Income group`
  )


# ============================================================
# 4. 合并
#    Low income 中 Gini = 0 的年份设为 NA
# ============================================================

df_all <- bind_rows(
  df_global,
  df_income
) %>%
  mutate(
    
    # 只针对 Low income，把 0 视为缺失，不画
    Gini = ifelse(
      Group == "Low income" &
        Gini <= 1e-8,
      NA,
      Gini
    ),
    
    Gini_se = ifelse(
      Group == "Low income" &
        is.na(Gini),
      NA,
      Gini_se
    ),
    
    Gini_lwr = ifelse(
      Group == "Low income" &
        is.na(Gini),
      NA,
      Gini_lwr
    ),
    
    Gini_upr = ifelse(
      Group == "Low income" &
        is.na(Gini),
      NA,
      Gini_upr
    )
  ) %>%
  mutate(
    Gini_lwr = pmax(
      Gini_lwr,
      0,
      na.rm = FALSE
    ),
    
    Gini_upr = pmin(
      Gini_upr,
      1,
      na.rm = FALSE
    ),
    
    Group = factor(
      Group,
      levels = c(
        "Global",
        "Low income",
        "Lower middle income",
        "Upper middle income",
        "High income"
      )
    )
  )


# ============================================================
# 5. 只绘制 2000–2025
# ============================================================

df_plot <- df_all %>%
  filter(
    Year >= 2000,
    Year <= 2025
  ) %>%
  mutate(
    line_w = 2.5
  )


# ============================================================
# 6. 保存计算结果
# ============================================================

write_csv(
  df_plot,
  "annual_Gini_global_income_groups_2000_2025_without_China.csv"
)


# ============================================================
# 7. 坐标轴
# ============================================================

x_breaks <- seq(
  2000,
  2025,
  by = 5
)

y_breaks <- seq(
  0,
  1,
  by = 0.2
)


# ============================================================
# 8. 绘图
# ============================================================

p <- ggplot(
  df_plot,
  aes(
    x = Year,
    y = Gini,
    group = Group
  )
) +
  
  geom_line(
    aes(
      color = Group,
      linewidth = line_w
    ),
    show.legend = FALSE,
    na.rm = TRUE
  ) +
  
  geom_point(
    aes(
      color = Group
    ),
    size = 4,
    na.rm = TRUE
  ) +
  
  scale_color_manual(
    values = c(
      "Global" = "grey80",
      "Low income" = "#F2C14E",
      "Lower middle income" = "#99D096",
      "Upper middle income" = "#53A8E1",
      "High income" = "#FF8081"
    )
  ) +
  
  scale_linewidth_identity() +
  
  scale_x_continuous(
    breaks = x_breaks,
    limits = c(
      2000,
      2025
    ),
    expand = c(
      0,
      0
    )
  ) +
  
  scale_y_continuous(
    breaks = y_breaks,
    limits = c(
      0,
      1
    ),
    expand = c(
      0,
      0
    )
  ) +
  
  labs(
    x = "Year",
    y = "Cross-country Gini without China",
    color = NULL
  ) +
  
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 1.5
    ),
    
    axis.title = element_text(
      size = 21, color = "black",
    ),
    
    axis.text = element_text(
      size = 21, color = "black",
    ),
    
    legend.title = element_blank(),
    
    legend.text = element_text(
      size = 16
    ),
    
    legend.position = "right"
  )


print(p)


# ============================================================
# 9. 保存图
# ============================================================

ggsave(
  "20260826_Gini_global_four_income_without_China.png",
  plot = p,
  width = 10,
  height = 4.8,
  dpi = 600
)