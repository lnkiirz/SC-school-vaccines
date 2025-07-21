library(tidyverse)
library(readxl)

data_source <- 
  "https://dph.sc.gov/sites/scdph/files/media/document/Religious_Exemption_Rates_Last_5_School_Years_as_of_22-23.pdf"

vax_data <-
  read_excel(
    "Religious exemptions through 2023.xlsx",
    sheet = 2
  )

vax_clean <-
  vax_data |> 
  select(
    County,
    `2018-2019_pct`,
    `2019-2020_pct`,
    `2020-2021_pct`,
    `2021-2022_pct`,
    `2022-2023_pct`
  ) |> 
  rename(
    "Year_2018" = `2018-2019_pct`,
    "Year_2019" = `2019-2020_pct`,
    "Year_2020" = `2020-2021_pct`,
    "Year_2021" = `2021-2022_pct`,
    "Year_2022" = `2022-2023_pct`
  ) |> 
  pivot_longer(
    2:6,
    names_to = "Year",
    values_to = "% exempted"
  ) |> 
  mutate(
    `% exempted` = `% exempted` * 100
  ) |> 
  group_by(
    County
  ) |> 
  ungroup() |> 
  arrange(Year, County)

theme_set(theme_minimal())

ggplot(
  data = vax_clean |> filter(County != "Statewide"),
  aes(x = Year, y = `% exempted`, group = County, fill = "dark blue")
) +
  scale_fill_manual(values = "dark blue") +
  geom_col(position = position_dodge()) +
  coord_flip() +
  theme(
    legend.position = "none"
  )

vax_overall_increase <-
  vax_clean |> 
  filter(
    Year %in% c("Year_2018", "Year_2022")
  ) |> 
  pivot_wider(
    names_from = "Year",
    values_from = "% exempted"
  ) |> 
  mutate(
    pct_change = Year_2022 - Year_2018
  )

state_avg <-
  vax_overall_increase |> 
  filter(County == "Statewide") |> 
  pull(pct_change)

ggplot(
  data = vax_overall_increase |> 
    filter(County != "Statewide"),
  aes(x = reorder(County, pct_change), y = pct_change, fill = if_else(pct_change > state_avg, "darkorange", "dark green"))
) +
  geom_col(position = position_dodge()) +
  scale_fill_manual(values = c("darkorange" = "darkorange", "dark green" = "dark green")) +
  coord_flip() +
  theme(
    legend.position = "none"
  ) +
  geom_hline(
    yintercept = state_avg,
    color = "red",
    linewidth = 1
  )
