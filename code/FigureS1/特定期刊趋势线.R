df_raw <- read_csv("journal.csv", show_col_types = FALSE)

# 2) 统一年份列名：PY -> Year（如果本来就叫 Year，不影响）
df_raw <- df_raw %>%
  rename(Year = any_of(c("PY", "year", "YEAR", "Year")))

# 3) 确保四个学科列为数值；并确保 sum 存在（若不存在就计算）
disc_cols <- c("Chemistry", "Geoscience", "Life science", "Physics")

df_raw <- df_raw %>%
  mutate(across(all_of(disc_cols), as.numeric))

if (!("sum" %in% names(df_raw))) {
  df_raw <- df_raw %>% mutate(sum = rowSums(across(all_of(disc_cols)), na.rm = TRUE))
} else {
  df_raw <- df_raw %>% mutate(sum = as.numeric(sum))
}

df_raw <- df_raw %>% arrange(Year)

# 4) 长表：用于堆积柱状图
df_long <- df_raw %>%
  select(Year, all_of(disc_cols)) %>%
  pivot_longer(cols = all_of(disc_cols),
               names_to = "Discipline",
               values_to = "Count") %>%
  mutate(
    Discipline = factor(Discipline, levels = disc_cols)  # 控制堆积顺序（从下到上）
  )

# 5) 总量折线数据（趋势线）
df_total <- df_raw %>%
  select(Year, Global_Total = sum)

# 6) 颜色（你也可以按自己想要的改）
my_colors <- c(
  "Chemistry" = "#53A8E1",
  "Geoscience"       = "#FF8081",
  "Life science"      = "#99D096",
  "Physics"   = "#F2C14E"
)

ymax <- max(df_total$Global_Total, na.rm = TRUE)

# 7) 画图：堆积柱状 + 总量趋势线
p <- ggplot() +
  geom_bar(
    data = df_long,
    aes(x = Year, y = Count, fill = Discipline),
    stat = "identity",
    position = position_stack(reverse = TRUE),  # 第一个level在最底部
    width = 0.8
  ) +
  geom_line(
    data = df_total,
    aes(x = Year, y = Global_Total),
    color = "grey70",
    linewidth = 1.5
  ) +
  geom_point(
    data = df_total,
    aes(x = Year, y = Global_Total),
    color = "grey70",
    size = 2.5
  ) +
  scale_fill_manual(values = my_colors) +
  scale_y_continuous(
    breaks = seq(0, ymax, by = 200),   # 这里的 200 你自己改
    labels = scales::label_comma(accuracy = 1),
    expand = c(0, 0)
    # 你如果想固定 breaks，比如每 50（千）一格，可以加：
    # breaks = seq(0, max(df_total$Global_Total, na.rm = TRUE), by = 50)
  ) +
  labs(
    x = "Year",
    y = "Publication Count",
    fill = "Discipline"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 20),
    axis.text  = element_text(size = 20),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.2),
    legend.title = element_text(size = 22),
    legend.text  = element_text(size = 22)
  )

print(p)

ggsave("specific_journal.png", p, width = 10, height = 6, dpi = 600)
