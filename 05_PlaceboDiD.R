# XXX. Creation of Placebo Variable

resale <- resale %>%
  mutate(
    placebo_post = if_else(month >= as.Date("2024-03-01"), 1, 0)
  )

# XXXI. Placebo DiD

model_placebo <- feols(
  log_price ~
    placebo_post:exposure_gis +
    floor_area_sqm +
    remaining_lease +
    i(flat_type) +
    i(flat_model) +
    i(storey_range)
  | Estate + month,
  data = resale,
  cluster = ~Estate
)

summary(model_placebo)

coeftable(model_placebo)["placebo_post:exposure_gis", ]

# Placebo Date Generation

placebo_dates <- seq(
  as.Date("2023-01-01"),
  as.Date("2024-07-01"),
  by = "month"
)

# Placebo Table

results <- data.frame(
  date = placebo_dates,
  estimate = NA_real_,
  se = NA_real_,
  t = NA_real_,
  p = NA_real_
)

for(i in seq_along(placebo_dates)){
  
  fake_date <- placebo_dates[i]
  
  resale$placebo_post <- ifelse(
    resale$month >= fake_date,
    1,
    0
  )
  
  model <- feols(
    log_price ~
      placebo_post:exposure_gis +
      floor_area_sqm +
      remaining_lease +
      i(flat_type) +
      i(flat_model) +
      i(storey_range)
    | Estate + month,
    data = resale,
    cluster = ~Estate
  )
  
  ct <- coeftable(model)
  
  results$estimate[i] <- ct["placebo_post:exposure_gis","Estimate"]
  results$se[i]       <- ct["placebo_post:exposure_gis","Std. Error"]
  results$t[i]        <- ct["placebo_post:exposure_gis","t value"]
  results$p[i]        <- ct["placebo_post:exposure_gis","Pr(>|t|)"]
}

