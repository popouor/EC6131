# Core: Event Study - Parallel Trends Assumption

# XXVIII. Date-setting

library(zoo)

resale <- resale %>%
  mutate(
    event_time = round(12 * (as.yearmon(month) - as.yearmon("2024-08")))
  )


# XXIX. Event Study & Figure 2
model_es <- feols(
  log_price ~
    i(event_time, exposure_gis, ref = -1) +
    floor_area_sqm +
    remaining_lease +
    i(flat_type) +
    i(flat_model) +
    i(storey_range)
  | Estate + month,
  data = resale,
  cluster = "Estate"
)

library(broom)
library(dplyr)
library(stringr)

event_df <- tidy(model_es, conf.int = TRUE) |>
  filter(str_detect(term, "^event_time::")) |>
  mutate(
    event_time = as.numeric(str_extract(term, "-?[0-9]+"))
  ) |>
  filter(event_time >= -24,
         event_time <= 20)

ggplot(event_df, aes(event_time, estimate)) +
  geom_point(size = 2) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    colour = "grey50",
    alpha = 0.6,
    width = 0.15
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Months relative to August 2024",
    y = "Estimated treatment effect",
    title = "Figure 2: Dynamic treatment effects relative to the 
              August 2024 EHG enhancement"
  ) +
  theme_classic(base_size = 14)

mean(abs(event_df$estimate[event_df$event_time < 0]))

# XXX. Wald Test

pre_terms24 <- names(coef(model_es))
pre_terms12 <- names(coef(model_es))
pre_terms6 <- names(coef(model_es))

pre_terms24 <- pre_terms24[
  grepl("^event_time::-([2-9]|1[0-9]|2[0-4]):exposure_gis$", pre_terms24)
]

pre_terms12 <- pre_terms12[
  grepl("^event_time::-([2-9]|1[0-2]):exposure_gis$", pre_terms12)
]

pre_terms6 <- pre_terms6[
  grepl("^event_time::-([2-6]):exposure_gis$", pre_terms6)
]

wald(model_es, keep = pre_terms24)
wald(model_es, keep = pre_terms12)
wald(model_es, keep = pre_terms6)
