library(tidyverse)
library(sf)

# === 1️⃣ 读取数据 ===
pub_data <- read_csv("country_year_six_fields_gini_with_income_group.csv")   # Gini文件
world <- st_read("D:/博后/论文/3/导出文献/世界地图.shp")  # 世界地图

# === 2️⃣ 数据清洗 ===
pub_data <- pub_data %>%
  mutate(
    Country = str_trim(Country),
    Country = str_to_title(Country)
  ) %>%
  rename(COUNTRY = Country)

# === 3️⃣ 筛选年份：2000 年 ===
pub_2024 <- pub_data %>%
  filter(Year == 2024)

# === 4️⃣ 合并地图与数据 ===
map_data <- world %>%
  left_join(pub_2024, by = "COUNTRY")

# === 5️⃣ 绘制地图 ===
p <- ggplot(map_data) +
  geom_sf(aes(fill = gini_pub), color = "gray60", linewidth = 0.3) +
  scale_fill_stepsn(
    name = "Gini Coefficient",
    breaks = c(0.1, 0.2, 0.3, 0.4, 0.5),
    colors = c( "#c6dbef","#9ecae1", "#6baed6", "#3182bd","#08519c"),
    limits = c(0, 1),
    na.value = "white"
  ) +
  labs(
    title = "Global Distribution of Disciplinary Inequality (Gini, 2024)",
    subtitle = "Calculated from AI-related publications across five disciplines"
  ) +
  coord_sf(expand = FALSE) +
  # ✅ 新增：经纬度间隔
  scale_x_continuous(breaks = seq(-180, 180, by = 60)) +
  scale_y_continuous(breaks = seq(-90, 90, by = 30)) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5, color = "gray30"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.ticks = element_line(color = "black")
  )

print (p)

# === 6️⃣ 保存高分辨率图像 ===
ggsave("Gini_Global_2024.png", p, width = 10, height = 6, dpi = 600)
