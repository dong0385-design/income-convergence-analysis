# Income Convergence Analysis
# Undergraduate Economics Course Project
# Reconstructed from my original project report

# Load required packages
library(pwt10)
library(ggplot2)
library(dplyr)
library(tidyr)
library(knitr)
library(WDI)
library(kableExtra)

# Load Penn World Table data
data("pwt10.0")

# Check the dataset
head(pwt10.0)

# Download World Bank data

natural_resources <- WDI(
  indicator = "NY.GDP.TOTL.RT.ZS",
  start = 1960,
  end = 2020,
  extra = TRUE
)

# Check the downloaded data
head(natural_resources)


# Merge the two datasets

merged_data <- pwt10.0 %>%
  left_join(
    natural_resources,
    by = c("country", "year")
  )

# Check the merged data
head(merged_data)

# Check the number of rows and columns
dim(merged_data)

# Clean the data

pwt_clean <- merged_data %>%
  mutate(
    decade = case_when(
      year >= 1960 & year < 1970 ~ "1960-1970",
      year >= 1970 & year < 1980 ~ "1970-1980",
      year >= 1980 & year < 1990 ~ "1980-1990",
      year >= 1990 & year < 2000 ~ "1990-2000",
      year >= 2000 & year < 2007 ~ "2000-2010",
      year >= 2007 & year <= 2017 ~ "2007-2017",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(pop), !is.na(rgdpna)) %>%
  group_by(country, decade) %>%
  mutate(
    NY.GDP.TOTL.RT.ZS = ifelse(
      is.na(NY.GDP.TOTL.RT.ZS),
      0,
      NY.GDP.TOTL.RT.ZS
    ),
    max_pop = max(pop, na.rm = TRUE),
    max_nature = max(NY.GDP.TOTL.RT.ZS, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(max_pop >= 0.2) %>%
  filter(max_nature < 75) %>%
  select(country, year, rgdpna, pop, hc) %>%
  mutate(
    gdp_pc = rgdpna / pop,
    log_gdp_pc = log(gdp_pc)
  )

# Check the cleaned data
head(pwt_clean)
dim(pwt_clean)

# Calculate economic growth rates

decades <- c(1960, 1970, 1980, 1990, 2000, 2007)

decade_data_list <- list()

for (i in decades) {
  
  decade_data <- pwt_clean %>%
    filter(year %in% c(i, i + 10)) %>%
    select(country, year, log_gdp_pc, hc) %>%
    pivot_wider(
      names_from = year,
      values_from = c(log_gdp_pc, hc)
    ) %>%
    mutate(
      growth_rate = 100 * (
        (.[[paste0("log_gdp_pc_", i + 10)]] -
           .[[paste0("log_gdp_pc_", i)]]) / 10
      ),
      initial_gdp = .[[paste0("log_gdp_pc_", i)]],
      initial_hc = .[[paste0("hc_", i)]]
    ) %>%
    filter(!is.na(growth_rate), !is.na(initial_gdp)) %>%
    mutate(decade = as.character(i))
  
  decade_data_list[[as.character(i)]] <- decade_data
}

# Combine all six periods
decade_data <- bind_rows(decade_data_list)

# Check the result
head(decade_data)

# Number of observations in each period
table(decade_data$decade)

# Create income convergence plots

convergence_plot <- ggplot(
  decade_data,
  aes(x = initial_gdp, y = growth_rate)
) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "blue",
    linewidth = 0.8
  ) +
  facet_wrap(~ decade, ncol = 3) +
  labs(
    title = "Income Convergence by Decade",
    x = "Log GDP per capita (initial)",
    y = "Annualized Growth Rate (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.background = element_rect(fill = "lightgray")
  )

# Display the plot
print(convergence_plot)

# Save the figure as a PNG file

ggsave(
  filename = "income_convergence_plot.png",
  plot = convergence_plot,
  width = 12,
  height = 8,
  dpi = 300
)

# Run convergence regressions

regression_data_list <- list()

for (i in decades) {
  
  df <- decade_data %>%
    filter(decade == as.character(i))
  
  model <- lm(
    growth_rate ~ initial_gdp + initial_hc,
    data = df
  )
  
  result <- data.frame(
    decade = i,
    beta_gdp = coef(model)[2],
    beta_hc = coef(model)[3],
    se_gdp = summary(model)$coefficients[2, 2],
    se_hc = summary(model)$coefficients[3, 2],
    n_obs = nobs(model),
    r_squared = summary(model)$r.squared
  )
  
  regression_data_list[[as.character(i)]] <- result
}

# Combine regression results
regression_results <- bind_rows(regression_data_list)

rownames(regression_results) <- NULL

# Display regression results
print(regression_results)

# Export regression results

write.csv(
  regression_results,
  "regression_results.csv",
  row.names = FALSE
)

# Export the convergence figure
ggsave(
  "income_convergence_plot.png",
  plot = convergence_plot,
  width = 12,
  height = 8,
  dpi = 300
)
