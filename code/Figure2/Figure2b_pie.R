# ============================================================
# 10个国家 × 3个时期：每个国家单独绘制一个饼图
# ============================================================

rm(list = ls())
gc()
cat("\014")

library(tidyverse)


# ============================================================
# 1. 数据路径
# ============================================================

data_path <- "country_cleaned_natural_sciences_three_periods.csv"


# ============================================================
# 2. 输出文件夹
# ============================================================

out_dir <- "pie_by_country"

dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 3. 三个时期
# ============================================================

period_levels <- c(
  "1990-2014",
  "2015-2022",
  "2023-2025"
)


# ============================================================
# 4. 三个时期颜色
# ============================================================

color_map <- c(
  "1990-2014" = "#99D096",
  "2015-2022" = "#53A8E1",
  "2023-2025" = "#FF8081"
)


# ============================================================
# 5. 需要绘制的10个国家
# ============================================================

countries_keep <- c(
  "Argentina",
  "Australia",
  "Brazil",
  "India",
  "China",
  "Portugal",
  "South Africa",
  "Sweden",
  "United Kingdom",
  "United States"
)


# ============================================================
# 6. 读取数据
# ============================================================

df_raw <- read_csv(
  data_path,
  show_col_types = FALSE
)


# ============================================================
# 7. 清洗时期名称
#
# 原CSV为：
# 1990–2014
# 2015–2022
# 2023–2025
#
# 将长横线统一替换为 "-"
# ============================================================

df <- df_raw %>%
  mutate(
    
    period = str_squish(period),
    
    period = str_replace_all(
      period,
      "[\u2012\u2013\u2014\u2212]",
      "-"
    ),
    
    Country = str_squish(Country)
  )


# ============================================================
# 8. 只保留10个国家和3个时期
# ============================================================

df_pie <- df %>%
  
  filter(
    Country %in% countries_keep,
    period %in% period_levels
  ) %>%
  
  group_by(
    Country,
    period
  ) %>%
  
  summarise(
    publications = sum(
      publications,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  
  complete(
    Country = countries_keep,
    period = period_levels,
    fill = list(
      publications = 0
    )
  ) %>%
  
  group_by(
    Country
  ) %>%
  
  mutate(
    
    total = sum(
      publications
    ),
    
    share = publications / total,
    
    percent = share * 100
  ) %>%
  
  ungroup() %>%
  
  mutate(
    
    period = factor(
      period,
      levels = period_levels
    ),
    
    Country = factor(
      Country,
      levels = countries_keep
    )
  )


# ============================================================
# 9. 检查数据
# ============================================================

print(df_pie)


# ============================================================
# 10. 每个国家单独画一个饼图
# ============================================================

for (cty in countries_keep) {
  
  
  # ----------------------------------------------------------
  # 当前国家数据
  # ----------------------------------------------------------
  
  d1 <- df_pie %>%
    filter(
      as.character(Country) == cty
    )
  
  
  # ----------------------------------------------------------
  # 绘制饼图
  # ----------------------------------------------------------
  
  p <- ggplot(
    d1,
    aes(
      x = "",
      y = publications,
      fill = period
    )
  ) +
    
    geom_col(
      width = 1,
      color = "white",
      linewidth = 0.7
    ) +
    
    coord_polar(
      theta = "y",
      start = 0
    ) +
    
    scale_fill_manual(
      values = color_map,
      breaks = period_levels,
      drop = FALSE
    ) +
    
    labs(
      title = cty,
      fill = NULL
    ) +
    
    theme_void() +
    
    theme(
      
      plot.title = element_text(
        size = 20,
        face = "bold",
        hjust = 0.5
      ),
      
      legend.position = "right",
      
      legend.text = element_text(
        size = 13
      )
    )
  
  
  # ----------------------------------------------------------
  # 在RStudio Plot窗口显示当前国家饼图
  # ----------------------------------------------------------
  
  print(p)
  
  
  # ----------------------------------------------------------
  # 文件名
  # ----------------------------------------------------------
  
  file_name <- paste0(
    str_replace_all(
      cty,
      "\\s+",
      "_"
    ),
    "_pie.png"
  )
  
  
  # ----------------------------------------------------------
  # 保存PNG
  # ----------------------------------------------------------
  
  ggsave(
    filename = file.path(
      out_dir,
      file_name
    ),
    plot = p,
    width = 5.5,
    height = 5.0,
    dpi = 600,
    bg = "white"
  )
  
  
  message(
    "Saved: ",
    file_name
  )
}


# ============================================================
# 11. 完成
# ============================================================

cat(
  "\n====================================\n"
)

cat(
  "10个国家的饼图全部绘制完成！\n"
)

cat(
  "保存位置：",
  normalizePath(out_dir),
  "\n"
)

cat(
  "====================================\n"
)