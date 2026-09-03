library(tidyverse)

pub_file <- "D:/博后/论文/3/需要上传的数据和代码/code/FigureS3/plot_data_annual_ai_publications_by_field.csv"
country_file <- "D:/博后/论文/3/需要上传的数据和代码/code/FigureS3/plot_data_annual_ai_participating_countries_by_field.csv"
output_dir <- "D:/博后/论文/3/需要上传的数据和代码/code/FigureS3"

pub <- read_csv(pub_file, show_col_types = FALSE) %>%
  transmute(Year = as.integer(year), Field = field,
            Publication_Count = as.numeric(publication_count))

countries <- read_csv(country_file, show_col_types = FALSE) %>%
  transmute(Year = as.integer(year), Field = field,
            Country_Count = as.numeric(country_count))

lm_slope <- function(year, value) {
  unname(coef(lm(value ~ year))["year"])
}

publication_results <- pub %>%
  group_by(Field) %>%
  summarise(
    Total_1990_2025 = sum(Publication_Count),
    Slope_1990_2025 = lm_slope(Year, Publication_Count),
    Slope_1996_2025 = lm_slope(Year[Year >= 1996], Publication_Count[Year >= 1996]),
    Slope_2015_2025 = lm_slope(Year[Year >= 2015], Publication_Count[Year >= 2015]),
    Slope_2016_2025 = lm_slope(Year[Year >= 2016], Publication_Count[Year >= 2016]),
    Count_2025 = Publication_Count[Year == 2025][1],
    .groups = "drop"
  ) %>%
  mutate(
    Share_percent = 100 * Total_1990_2025 / sum(Total_1990_2025)
  ) %>%
  arrange(desc(Total_1990_2025))

country_results <- countries %>%
  group_by(Field) %>%
  summarise(
    Slope_1990_2025 = lm_slope(Year, Country_Count),
    Slope_1996_2025 = lm_slope(Year[Year >= 1996], Country_Count[Year >= 1996]),
    Slope_2015_2025 = lm_slope(Year[Year >= 2015], Country_Count[Year >= 2015]),
    Countries_2025 = Country_Count[Year == 2025][1],
    Maximum_countries = max(Country_Count),
    Year_of_maximum = Year[which.max(Country_Count)][1],
    .groups = "drop"
  ) %>%
  arrange(desc(Countries_2025))

print(publication_results, width = Inf)
print(country_results, width = Inf)

write_csv(
  publication_results,
  file.path(output_dir, "six_fields_publication_growth_metrics.csv")
)

write_csv(
  country_results,
  file.path(output_dir, "six_fields_participating_country_growth_metrics.csv")
)
