#========================== 搞csv文件======================

library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)

# ===== 1) 把这里改成你5个学科CSV的真实路径 =====
files <- c(
  Chemistry = "化学-人口.csv",
  Geoscience = "地球科学-人口.csv",
  Interdisciplinary = "交叉学科-人口.csv",
  LifeScience = "生命科学-人口.csv",
  Physics = "物理科学-人口.csv"
)

# ===== 2) 读入 + 标准化列名 + 同一国家可能重复行则先求和 =====
df_list <- imap(files, function(fp, discipline){
  read_csv(fp, show_col_types = FALSE) %>%
    # 只保留需要的两列（如果你列名不完全一致，下面两行能兜底）
    rename(
      Country = any_of(c("Country", "country", "COUNTRY")),
      Total_Paper_Count = any_of(c("Total_Paper_Count", "Total_Paper", "Total_PaperCount", "count", "Count"))
    ) %>%
    select(Country, Total_Paper_Count) %>%
    mutate(
      Country = str_squish(Country)
    ) %>%
    group_by(Country) %>%                          # 防止同一国家在同一学科文件里重复
    summarise(!!discipline := sum(Total_Paper_Count, na.rm = TRUE), .groups = "drop")
})

# ===== 3) 按 Country 全连接合并成宽表 =====
lorenz_wide <- reduce(df_list, full_join, by = "Country") %>%
  # 缺失学科填0（用于Lorenz更合理：表示该国在该学科发文为0）
  mutate(across(-Country, ~replace_na(.x, 0))) %>%
  arrange(Country)

# ===== 4) 输出宽表CSV（这就是你要的“Lorenz输入文件”）=====
out_path <- "lorenz_input_wide.csv"
write_csv(lorenz_wide, out_path)
cat("Saved:", out_path, "\n")



#======================== 绘图 ============================
#======================== 绘图 ============================
#======================== 绘图 ============================

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

# ===== 1. 读入示例（换成你的文件路径即可）=====
df <- read_csv("lorenz_input_wide.csv", show_col_types = FALSE)

# ===== 2. 计算 Lorenz 曲线点（每学科一条）=====
lorenz_df <- df %>%
  pivot_longer(cols = -Country, names_to = "Discipline", values_to = "Count") %>%
  group_by(Discipline) %>%
  arrange(Count, .by_group = TRUE) %>%
  mutate(
    n = n(),
    x = row_number() / n,                     # 累计国家比例
    y = cumsum(Count) / sum(Count)            # 累计发文量比例
  ) %>%
  # 补上(0,0)起点，画线更标准
  summarise(
    x = c(0, x),
    y = c(0, y),
    .groups = "drop"
  ) %>%
  unnest(c(x, y))

# ===== 3. 画图：对角线 + 各学科Lorenz曲线 =====
disc_cols <- c(
  "Geoscience" = "#FF8081",
  "LifeScience" = "#99D096",
  "Chemistry"     = "#53A8E1",
  "Interdisciplinary" = "#e0e0e0",
  "Physics"     = "#F2C14E"
)

p <- ggplot(lorenz_df, aes(x = x, y = y, color = Discipline)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 1) +
  geom_line(linewidth = 2.2) +
  scale_color_manual(values = disc_cols, breaks = names(disc_cols)) +
  # === expand：给 x/y 两端留白 ===
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(
    x = "Cumulative share of countries",
    y = "Cumulative share of publications",
    color = NULL
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.position = "right",
    
    # 四周边框粗细（面板边框）
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.2),
    
    
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x  = element_text(size = 20),
    axis.text.y  = element_text(size = 20),
    
    legend.title = element_text(size = 18),
    legend.text  = element_text(size = 16)
  )

print(p)

ggsave("lorenz_curves.png", plot = p, width = 8.5, height = 5, units = "in", dpi = 600)
