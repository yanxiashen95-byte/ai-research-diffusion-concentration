
#==================叠加世界地图===============================
suppressPackageStartupMessages({
  library(sf); library(tidyverse); library(stringr); library(scales)
})

# 1) 读入与清洗
first_year <- read_csv("first_year_by_country_natural_sciences.csv", show_col_types = FALSE) %>%
  rename(Country = country, First_Year = first_year) %>%
  mutate(
    Country = tools::toTitleCase(str_squish(tolower(Country))),
    Country = recode(Country,
                     "United States Of America"="United States",
                     "Usa"="United States",
                     "Peoples R China"="China","Hong Kong"="China","Taiwan"="China",
                     "England"="United Kingdom","Scotland"="United Kingdom",
                     "Wales"="United Kingdom","Northern Ireland"="United Kingdom",
                     "Republic Of Korea"="South Korea","Korea"="South Korea",
                     .default = Country
    )
  )

# 2) 读取世界地图数据
world_map <- st_read("D:/博后/论文/3/导出文献/世界地图.shp", quiet = TRUE) %>%
  rename(Country = COUNTRY) %>%
  mutate(Country = tools::toTitleCase(str_squish(tolower(Country))))

# 3) 合并数据框，确保有geometry列
world_yr <- first_year %>%
  mutate(
    First_Year = suppressWarnings(as.integer(First_Year)),
    First_Year_Bin = case_when(
      !is.na(First_Year) & First_Year <= 2000 ~ "≤2000",
      !is.na(First_Year) & First_Year >= 2001 & First_Year <= 2005 ~ "2001–2005",
      !is.na(First_Year) & First_Year >= 2006 & First_Year <= 2010 ~ "2006–2010",
      !is.na(First_Year) & First_Year >= 2011 & First_Year <= 2015 ~ "2011–2015",
      !is.na(First_Year) & First_Year >= 2016 & First_Year <= 2020 ~ "2016–2020",
      !is.na(First_Year) & First_Year >= 2021              ~ "≥2021",
      TRUE ~ NA_character_
    ),
    First_Year_Bin = factor(
      First_Year_Bin,
      levels = c("≤2000", "2001–2005", "2006–2010", "2011–2015", "2016–2020", "≥2021")
    )
  )

# Merge the world map with world_yr based on the Country column to include geometry
world_yr_sf <- world_map %>%
  left_join(world_yr, by = "Country")

# 4) 离散配色
bin_colors <- c(
  "≤2000"     = "#4292c6",
  "2001–2005" = "#9ECAE9",
  "2006–2010" = "#7FC97F",
  "2011–2015" = "#C2E699",
  "2016–2020" = "#FDCB82",
  "≥2021"     = "#D96B5F",
  "NA"        = "#D9D9D9"
)

# 5) 绘图（离散填色）
p <- ggplot(world_yr_sf) +
  geom_sf(aes(fill = First_Year_Bin), color = "gray70", linewidth = 0.2) +
  scale_fill_manual(
    values = bin_colors,
    na.value = "gray90",
    name = "First year",
    na.translate = FALSE
  ) +
  coord_sf(expand = FALSE) +
  scale_x_continuous(breaks = seq(-180, 180, 60)) +
  scale_y_continuous(breaks = seq(-90, 90, 30)) +
  theme(
    legend.position   = "right",
    legend.title      = element_text(size = 20),
    legend.text       = element_text(size = 18),
    legend.key.height = unit(0.4, "cm"),
    legend.key.width  = unit(0.6, "cm"),
    legend.key.spacing.y = unit(0.1, "cm"),
    axis.text         = element_text(size = 18, colour = "black"),
    axis.title        = element_text(size = 20),
    axis.ticks        = element_line(colour = "black"),
    axis.line         = element_line(colour = "black"),
    panel.border      = element_rect(colour = "black", fill = NA, linewidth = 1),
    panel.grid        = element_blank(),
    panel.background  = element_blank()
  ) +
  guides(fill = guide_legend(reverse = TRUE))

# Print the plot
print(p)

ggsave("20260826_first_year_world_map.png", p, width = 18, height = 6, dpi = 600)