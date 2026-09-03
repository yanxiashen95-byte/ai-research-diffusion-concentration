
#===================然后叠加世界地图================

slope_data <- read_csv("country_gini_sen_slope.csv")  # 含 sen_slope_% 的文件
world <- st_read("D:/博后/论文/3/导出文献/世界地图.shp")  # 世界地图 Shapefile

# === 2️⃣ 数据清洗 ===
slope_data <- slope_data %>%
  mutate(
    Country = str_trim(Country),
    Country = str_to_title(Country)
  ) %>%
  rename(COUNTRY = Country)

# === 3️⃣ 合并地图和数据 ===
map_data <- world %>%
  left_join(slope_data, by = "COUNTRY")

# === 4️⃣ 绘制地图 ===
p <- ggplot(map_data) +
  geom_sf(aes(fill = sen_slope_pct), color = "gray60", linewidth = 0.3) +
  scale_fill_gradient2(
    name = "Sen’s slope (% / year)",
    low = "#1f78b4",   # 蓝色：不平衡下降
    mid = "white",     
    high = "#FF8081",  # 红色：不平衡上升
    midpoint = 0,      # 0为中点
    limits = c(-1, 1), # 你可以调整范围，比如 ±2%
    oob = scales::squish,
    na.value = "grey90"
  ) +
  labs(
    title = "Global Trends in Disciplinary Inequality (Sen’s slope % per year)",
    subtitle = "Positive: inequality increasing | Negative: inequality decreasing"
  ) +
  coord_sf(expand = FALSE) +
  scale_x_continuous(breaks = seq(-180, 180, by = 60)) +
  scale_y_continuous(breaks = seq(-90, 90, by = 30)) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5, color = "gray40"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.text.x = element_text(size = 13, color = "black"),
    axis.text.y = element_text(size = 13, color = "black"),
    axis.ticks = element_line(color = "black")
  )

print(p)

# === 5️⃣ 保存图像 ===
ggsave("20260826_global_gini_trend_map.png",
       plot = p, width = 12, height = 6, dpi = 600)
