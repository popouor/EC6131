# XXXII. Creation of one observation of exposure per estate

estate_summary <- resale %>%
  group_by(Estate) %>%
  summarise(
    exposure_gis = first(exposure_gis),
    
    avg_price = mean(resale_price),
    avg_log_price = mean(log_price),
    
    avg_floor_area = mean(floor_area_sqm),
    avg_remaining_lease = mean(remaining_lease),
    
    n_transactions = n(),
    
    share_1room = mean(flat_type == "1 ROOM"),
    share_2room = mean(flat_type == "2 ROOM"),
    share_3room = mean(flat_type == "3 ROOM"),
    share_4room = mean(flat_type == "4 ROOM"),
    share_5room = mean(flat_type == "5 ROOM")
  )

# XXXIII. What correlates most with Exposure? Defining High Exposure variables

estate_summary <- estate_summary %>%
  mutate(
    high_exposure = exposure_gis > median(exposure_gis, na.rm = TRUE)
  )

estate_summary %>%
  group_by(high_exposure) %>%
  summarise(
    avg_price = mean(avg_price),
    avg_floor_area = mean(avg_floor_area),
    avg_remaining_lease = mean(avg_remaining_lease),
    share_2room = mean(share_2room),
    share_3room = mean(share_3room),
    share_4room = mean(share_4room),
    share_5room = mean(share_5room)
  )

# XXXIV. Are the differences between low and high exposure significant?

t.test(avg_price ~ high_exposure, data = estate_summary)

t.test(avg_floor_area ~ high_exposure, data = estate_summary)

t.test(avg_remaining_lease ~ high_exposure, data = estate_summary)

t.test(share_2room ~ high_exposure, data = estate_summary)

t.test(share_3room ~ high_exposure, data = estate_summary)

t.test(share_4room ~ high_exposure, data = estate_summary)

t.test(share_5room ~ high_exposure, data = estate_summary)

# Figure 1: 

# Merge exposure group back into transaction data

price_trends <- resale %>%
  left_join(
    estate_summary %>%
      transmute(
        Estate,
        exposure_group = if_else(
          high_exposure,
          "High Exposure",
          "Low Exposure"
        )
      ),
    by = "Estate"
  ) %>%
  group_by(month, exposure_group) %>%
  summarise(
    mean_price = mean(resale_price),
    .groups = "drop"
  )

base_prices <- price_trends %>%
  filter(month == as.Date("2017-01-01")) %>%
  select(exposure_group, base = mean_price)

price_trends <- price_trends %>%
  left_join(base_prices,
            by = "exposure_group") %>%
  mutate(
    price_index = 100 * mean_price / base
  )

library(ggplot2)

ggplot(
  price_trends,
  aes(
    x = month,
    y = price_index,
    colour = exposure_group
  )
) +
  
  geom_line(linewidth = 1.25) +
  
  geom_point(size = 1) +
  
  geom_vline(
    xintercept = as.Date("2024-08-01"),
    linetype = "dashed",
    linewidth = 0.8
  ) +
  
  annotate(
    "text",
    x = as.Date("2024-08-01"),
    y = max(price_trends$price_index) + 2,
    label = "EHG Enhancement",
    angle = 90,
    vjust = -0.4,
    size = 4
  ) +
  
  labs(
    title = "Figure 1: HDB resale price index by policy exposure",
    subtitle = "January 2017 = 100",
    x = NULL,
    y = "Price Index",
    colour = NULL
  ) +
  
  theme_classic(base_size = 14)

# Figure 7:

estate_exposure_gis2 <-
  estate_weights %>%
  filter(census_area != "OTHERS") %>%
  left_join(
    planning_exposure,
    by = "census_area"
  ) %>%
  group_by(Estate) %>%
  summarise(
    exposure_gis = sum(weight * exposure)
  )

resale2 <-
  resale %>%
  select(-exposure_gis) %>%
  left_join(
    estate_exposure_gis2,
    by = "Estate"
  )

resale2 <- resale2 %>%
  mutate(
    exposure_z = as.numeric(scale(exposure_gis)),
    treat = Post * exposure_z
  )

model_noOthers <- feols(
  log_price ~
    treat +
    floor_area_sqm +
    remaining_lease +
    i(flat_type) +
    i(flat_model) +
    i(storey_range)
  |
    Estate +
    month,
  cluster = "Estate",
  data = resale2
)

# Table

library(dplyr)

robustness_table <- tibble(
  `Robustness Check` = c(
    "Baseline specification",
    "Wild cluster bootstrap",
    "GIS sensitivity check"
  ),
  Estimate = c(
    coeftable(model)["treat","Estimate"],
    boot$point_estimate,
    coeftable(model_noOthers)["treat","Estimate"]
  ),
  `Std. Error` = c(
    coeftable(model)["treat","Std. Error"],
    NA,
    coeftable(model_noOthers)["treat","Std. Error"]
  ),
  `P-value` = c(
    coeftable(model)["treat","Pr(>|t|)"],
    boot$p_val,
    coeftable(model_noOthers)["treat","Pr(>|t|)"]
  )
)

robustness_table