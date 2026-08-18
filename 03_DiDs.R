# XXVII. DiD

library(fixest)

resale <- resale %>%
  mutate(
    exposure_z = as.numeric(scale(exposure_gis)),
    treat = Post * exposure_z
  )


# XXVII.I DiD (Normal)

model <- feols(
  log_price~
    treat+
    floor_area_sqm+
    remaining_lease+
    i(flat_type)+
    i(flat_model)+
    i(storey_range)
  |
    Estate+
    month,
  cluster="Estate",
  data=resale
)

summary(model)
etable(model)

# XXVII.II Separated (Pooled) DiD

model_het <- feols(
  log_price ~
    i(flat_type, treat) +
    floor_area_sqm +
    remaining_lease +
    i(flat_model) +
    i(storey_range)
  | Estate + month,
  data = resale,
  cluster = ~Estate
)

summary(model_het)
etable(model_het)

# XXVII.III Bootstrap DiD

resale$Estate <- as.factor(resale$Estate)

boot <- boottest(
  model,
  param = ~ treat,
  clustid = ~ Estate,
  B = 9999
)

boot$point_estimate
boot$p_val
length(boot$t_boot)

# XXVII.IV Alternative Clustering

model_twoway <- feols(
  log_price ~
    treat +
    floor_area_sqm +
    remaining_lease +
    i(flat_type) +
    i(flat_model) +
    i(storey_range)
  | Estate + month,
  data = resale,
  cluster = ~Estate + month
)

etable(model, model_twoway)

# XXVII.V Sepaarate DiDs

two <- feols(
  log_price ~
    treat +
    floor_area_sqm +
    remaining_lease +
    i(flat_model) +
    i(storey_range)
  | Estate + month,
  data = filter(resale, flat_type == "2 ROOM"),
  cluster = "Estate"
)

three <- feols(
  log_price ~
    treat +
    floor_area_sqm +
    remaining_lease +
    i(flat_model) +
    i(storey_range)
  | Estate + month,
  data = filter(resale, flat_type == "3 ROOM"),
  cluster = "Estate"
)

four <- feols(
  log_price ~
    treat +
    floor_area_sqm +
    remaining_lease +
    i(flat_model) +
    i(storey_range)
  | Estate + month,
  data = filter(resale, flat_type == "4 ROOM"),
  cluster = "Estate"
)

five <- feols(
  log_price ~
    treat +
    floor_area_sqm +
    remaining_lease +
    i(flat_model) +
    i(storey_range)
  | Estate + month,
  data = filter(resale, flat_type == "5 ROOM"),
  cluster = "Estate"
)
