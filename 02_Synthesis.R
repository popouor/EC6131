# XII. Spatial Mapping

install.packages("sf")
library(sf)

sf_use_s2(FALSE) # We turn off Google S2, as govt data may not work well with this stricter system

hdb <- st_read("HDBExistingBuilding.geojson")
ura <- st_read("MasterPlan2019PlanningAreaBoundaryNoSea.geojson")

# XIII. Checking CRS (Coordinate Reference System) match & Visual match

st_crs(hdb)
st_crs(ura)

# XIII.I Checking geometry type

table(st_geometry_type(hdb))
table(st_geometry_type(ura))

# XIII.II Conversion of buildings to centroids

hdb_centroids <- st_centroid(hdb)
table(st_geometry_type(hdb_centroids))

# XIV. Assignment of all HDBs to Planning Area

hdb_pa <- st_join(
  hdb_centroids,
  ura[, "PLN_AREA_N"],
  join = st_within
)

# Installation of Packages

install.packages(c("httr2", "jsonlite", "readr"))
api_token <- "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1c2VyX2lkIjoxODQxNCwiZm9yZXZlciI6ZmFsc2UsImlzcyI6Ik9uZU1hcCIsImlhdCI6MTc4MzM5NDI3OSwibmJmIjoxNzgzMzk0Mjc5LCJleHAiOjE3ODM2NTM0NzksImp0aSI6ImJlZTMyOWI5LThlNjItNGEyNi1hNmRlLTg3Njc2NWZlZTFhNCJ9.TwwJUlI17SmR_-73OU_ZoGtQB3avhKHgXugUtt8dkKv7M3YHo_A9PtLNp4yu_0AiDfk4RiT0LIGAURsroXrb55EV8Pjm6FRiysLOoquCW6GwxGB2Oxrwtx__WQhHQJhaE-0E_-iGu3glFAsSM3aSX9mfhE_lJjBG3vuAZqDYbY9zqxmPcfGmq9YuErvDj8yEJNoE5sMOlY1CC4wCokIZhPQZHEiLyFN-BBwj8KConb3gA1KpWDrqMlVXXolC5j-K3i0KcG6pEQVTnpbiaPJs8iD672NMYBmf2Vi1tVapS1ruF0AJ1XEKA9M6_3nreBbr9v9P3ncaK8sw7eSss7JdAw"

# Saving Progress

library(readr)

if (!file.exists("address_lookup.csv")) {
  write_csv(
    data.frame(
      block = character(),
      street_name = character(),
      postal = character(),
      latitude = numeric(),
      longitude = numeric(),
      found = logical()
    ),
    "address_lookup.csv"
  )
}

lookup <- read_csv("address_lookup.csv", show_col_types = FALSE)

addresses <- resale %>%
  distinct(block, street_name)

todo <- anti_join(
  addresses,
  lookup,
  by = c("block", "street_name")
)

nrow(todo)

# XV. Actual Querying

library(httr2)
library(jsonlite)

library(httr2)
library(jsonlite)

clean_string <- function(x) {
  x |>
    toupper() |>
    trimws()
}

lookup_address <- function(block, street){
  
  query <- paste(block, street, "Singapore")
  
  url <- paste0(
    "https://www.onemap.gov.sg/api/common/elastic/search?",
    "searchVal=", URLencode(query),
    "&returnGeom=Y",
    "&getAddrDetails=Y",
    "&pageNum=1"
  )
  
  resp <-
    request(url) |>
    req_headers(
      Authorization = api_token
    ) |>
    req_perform()
  
  js <- fromJSON(resp_body_string(resp))
  
  if(js$found == 0){
    
    return(data.frame(
      block = block,
      street_name = street,
      postal = NA_character_,
      latitude = NA_real_,
      longitude = NA_real_,
      found = FALSE,
      verified = FALSE
    ))
  }
  
  results <- js$results
  
  block_clean  <- clean_string(block)
  street_clean <- clean_string(street)
  
  match <- results[
    clean_string(results$BLK_NO) == block_clean &
      clean_string(results$ROAD_NAME) == street_clean,
  ]
  
  if(nrow(match) == 0){
    
    match <- results[1,]
    
    verified <- FALSE
    
  } else{
    
    match <- match[1,]
    
    verified <- TRUE
  }
  
  data.frame(
    block = block,
    street_name = street,
    postal = match$POSTAL,
    latitude = as.numeric(match$LATITUDE),
    longitude = as.numeric(match$LONGITUDE),
    found = TRUE,
    verified = verified
  )
  
}

# Standardisation of Nomenclature

clean_string <- function(x){
  
  x <- toupper(x)
  
  x <- gsub("\\bAVE\\b", "AVENUE", x)
  x <- gsub("\\bST\\b", "STREET", x)
  x <- gsub("\\bRD\\b", "ROAD", x)
  x <- gsub("\\bDR\\b", "DRIVE", x)
  x <- gsub("\\bCRES\\b", "CRESCENT", x)
  x <- gsub("\\bPL\\b", "PLACE", x)
  x <- gsub("\\bCL\\b", "CLOSE", x)
  x <- gsub("\\bCTRL\\b", "CENTRAL", x)
  x <- gsub("\\bNTH\\b", "NORTH", x)
  x <- gsub("\\bSTH\\b", "SOUTH", x)
  
  trimws(x)
}

# XVI. Rerun the query until all HDBs are found

library(readr)

for(i in seq_len(nrow(todo))) {
  
  out <- lookup_address(
    todo$block[i],
    todo$street_name[i]
  )
  
  write_csv(
    out,
    "address_lookup.csv",
    append = TRUE
  )
  
  if(i %% 100 == 0) {
    cat(i, "of", nrow(todo), "completed\n")
  }
  
  Sys.sleep(0.15)
}



# XVII. Merger of Postal Codes

lookup <- read_csv("address_lookup.csv", show_col_types = FALSE)

resale <- resale %>%
  left_join(
    lookup %>%
      select(block, street_name, postal),
    by = c("block", "street_name")
  )

# XVIII. Merger of Planning Areas

lookup_pa <- hdb_pa %>%
  st_drop_geometry() %>%
  select(POSTAL_COD, PLN_AREA_N) %>%
  distinct()

resale <- resale %>%
  left_join(
    lookup_pa,
    by = c("postal" = "POSTAL_COD")
  )

# XIX. Creation of Estate Weights for Exposure

# XIX.I Creation of Estate Weights

estate_weights <-
  resale %>%
  count(Estate, PLN_AREA_N) %>%
  group_by(Estate) %>%
  mutate(weight = n / sum(n))

# XIX.II Removal of NA Planning Areas

estate_weights <- resale %>%
  filter(!is.na(PLN_AREA_N)) %>%
  count(Estate, PLN_AREA_N) %>%
  group_by(Estate) %>%
  mutate(weight = n / sum(n))

# Check:

estate_weights %>%
  group_by(Estate) %>%
  summarise(total = sum(weight))

# XIX.III Standardisation

income_data <- income_data %>%
  mutate(Estate = toupper(Estate))

estate_weights <- estate_weights %>%
  mutate(PLN_AREA_N = toupper(PLN_AREA_N))

# XX. Aggregation of "OTHERS"

others_pa <- c(
  "ROCHOR",
  "MUSEUM",
  "NEWTON",
  "ORCHARD",
  "SINGAPORE RIVER",
  "STRAITS VIEW",
  "CHANGI",
  "WESTERN WATER CATCHMENT"
)

estate_weights <- estate_weights %>%
  mutate(
    census_area = if_else(
      PLN_AREA_N %in% others_pa,
      "OTHERS",
      PLN_AREA_N
    )
  )

# XXI. Joining weights

planning_weights <- estate_weights %>%
  left_join(
    income_data,
    by = c("census_area" = "Estate")
  )

# XXII. Recreation of Exposure

planning_long <- planning_weights %>%
  tidyr::pivot_longer(
    cols = NoEmployedPerson:`20_000andOver`,
    names_to = "income_band",
    values_to = "count"
  )

# XXIII. Creation of Midpoints

planning_long <- planning_long %>%
  mutate(
    grant_increase = case_when(
      income_band == "NoEmployedPerson" ~ 0,
      income_band == "Below_1_000"      ~ 40000,
      income_band == "1_000_1_999"      ~ 37500,
      income_band == "2_000_2_999"      ~ 32500,
      income_band == "3_000_3_999"      ~ 27500,
      income_band == "4_000_4_999"      ~ 20000,
      income_band == "5_000_5_999"      ~ 15000,
      income_band == "6_000_6_999"      ~ 7500,
      income_band == "7_000_7_999"      ~ 5000,
      
      TRUE ~ 0
    )
  )


glimpse(planning_long)

# XXIV. Creation of planning_exposure

planning_exposure <- planning_long %>%
  group_by(census_area) %>%
  summarise(
    exposure = sum(count * grant_increase) / sum(count),
    .groups = "drop"
  )


# XXV. Application of GIS Weights

estate_exposure_gis <-
  estate_weights %>%
  left_join(
    planning_exposure,
    by = "census_area"
  ) %>%
  group_by(Estate) %>%
  summarise(
    exposure_gis = sum(weight * exposure)
  )


# XXVI. Merger into resale

resale <- resale %>%
  left_join(
    estate_exposure_gis,
    by = "Estate"
  )