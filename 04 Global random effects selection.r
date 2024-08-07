# set directory path
remove(list = ls())
my_dir <- paste(getwd(), "/datasets/", sep = "")

# import library
require(INLA)
inla.setOption(scale.model.default = FALSE)
require(spdep)
require(abind)
require(ggplot2)
require(sf)

# import shape file
global_map <- st_read(
  paste(my_dir, "Global suicides/countries181/countries181.shp",
  sep = ""))

# build adj matrix from shape file
glb_adj <- nb2mat(
  poly2nb(global_map),
  style = "B",
  zero.policy = TRUE)

# add path between China and South Korea
glb_adj[35, 131] <- 1
glb_adj[131, 35] <- 1

# construct spatiotemporal model
# import spatiotemporal data
data <- read.csv(
  paste(my_dir, "Global suicides/age_adjusted_both_melt_181.csv",
  sep = ""))

# log normalized + gitter
data$log_y <- log(data$y + 0.01)

# convert every column to numeric
data$country <- as.numeric(data$country) # id of country 1-181
data$year <- as.numeric(data$year) # id of year 1-20

data$country_year <- seq(1, length(data[, 1])) # id of country-year interaction 1-3620

# interaction id
country_int <- data$country
year_int <- data$year

# risk factors
risk_factors <- read.csv(
  paste(my_dir, "Global suicides/0-cleaned_filled_risk_factor_37.csv",
  sep = ""))

# merge data with fixed effect
data <- cbind(data, risk_factors)
View(data)

## model selection ##
# full model + random effects

# interaction type 1
# combination 1
formula_1_bym_rw1 <- log_y ~ 1 +
  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
  x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 +
  x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 +
  x31 + x32 + x33 + x34 + x35 + x36 + x37 +
  f(country, model = "bym", graph = glb_adj) +
  f(year, model = "rw1") +
  f(country_year, model = "iid")

# combination 2
formula_1_besag_rw1 <- log_y ~ 1 +
  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
  x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 +
  x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 +
  x31 + x32 + x33 + x34 + x35 + x36 + x37 +
  f(country, model = "besag", graph = glb_adj) +
  f(year, model = "rw1") +
  f(country_year, model = "iid")

# combination 3
formula_1_iid_rw1 <- log_y ~ 1 +
  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
  x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 +
  x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 +
  x31 + x32 + x33 + x34 + x35 + x36 + x37 +
  f(country, model = "iid") +
  f(year, model = "rw1") +
  f(country_year, model = "iid")

# combination 4
formula_1_bym_rw2 <- log_y ~ 1 +
  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
  x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 +
  x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 +
  x31 + x32 + x33 + x34 + x35 + x36 + x37 +
  f(country, model = "bym", graph = glb_adj) +
  f(year, model = "rw2") +
  f(country_year, model = "iid")

# combination 5
formula_1_besag_rw2 <- log_y ~ 1 +
  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
  x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 +
  x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 +
  x31 + x32 + x33 + x34 + x35 + x36 + x37 +
  f(country, model = "besag", graph = glb_adj) +
  f(year, model = "rw2") +
  f(country_year, model = "iid")

# combination 6
formula_1_iid_rw2 <- log_y ~ 1 +
  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
  x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 +
  x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 +
  x31 + x32 + x33 + x34 + x35 + x36 + x37 +
  f(country, model = "iid", graph = glb_adj) +
  f(year, model = "rw2") +
  f(country_year, model = "iid")

# combination 7
formula_1_bym_iid <- log_y ~ 1 +
  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
  x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 +
  x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 +
  x31 + x32 + x33 + x34 + x35 + x36 + x37 +
  f(country, model = "bym", graph = glb_adj) +
  f(year, model = "iid") +
  f(country_year, model = "iid")

# combination 8
formula_1_besag_iid <- log_y ~ 1 +
  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
  x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 +
  x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 +
  x31 + x32 + x33 + x34 + x35 + x36 + x37 +
  f(country, model = "besag", graph = glb_adj) +
  f(year, model = "iid") +
  f(country_year, model = "iid")

# combination 9
formula_1_iid_iid <- log_y ~ 1 +
  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
  x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 +
  x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 +
  x31 + x32 + x33 + x34 + x35 + x36 + x37 +
  f(country, model = "iid", graph = glb_adj) +
  f(year, model = "iid") +
  f(country_year, model = "iid")

# interaction type 2
# combination 1
formula_2_bym_rw1 <- log_y ~ 1 +
  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
  x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 +
  x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 +
  x31 + x32 + x33 + x34 + x35 + x36 + x37 +
  f(country, model = "bym", graph = glb_adj) +
  f(year, model = "rw1") +
  f(country_int, model = "iid",
   group = year_int,
   control.group = list(model = "rw1"))

# combination 2
formula_2_besag_rw1 <- log_y ~ 1 +
  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
  x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 +
  x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 +
  x31 + x32 + x33 + x34 + x35 + x36 + x37 +
  f(country, model = "besag", graph = glb_adj) +
  f(year, model = "rw1") +
  f(country_int, model = "iid",
   group = year_int,
   control.group = list(model = "rw1"))

# combination 3
formula_2_iid_rw1 <- log_y ~ 1 +
  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
  x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 +
  x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 +
  x31 + x32 + x33 + x34 + x35 + x36 + x37 +
  f(country, model = "iid") +
  f(year, model = "rw1") +
  f(country_int, model = "iid",
   group = year_int,
   control.group = list(model = "rw1"))

# combination 4
formula_2_bym_rw2 <- log_y ~ 1 +
  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
  x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 +
  x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 +
  x31 + x32 + x33 + x34 + x35 + x36 + x37 +
  f(country, model = "bym", graph = glb_adj) +
  f(year, model = "rw2") +
  f(country_int, model = "iid",
   group = year_int,
   control.group = list(model = "rw1"))

# combination 5
formula_2_besag_rw2 <- log_y ~ 1 +
  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
  x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 +
  x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 +
  x31 + x32 + x33 + x34 + x35 + x36 + x37 +
  f(country, model = "besag", graph = glb_adj) +
  f(year, model = "rw2") +
  f(country_int, model = "iid",
   group = year_int,
   control.group = list(model = "rw1"))

# combination 6
formula_2_iid_rw2 <- log_y ~ 1 +
  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
  x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 +
  x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 +
  x31 + x32 + x33 + x34 + x35 + x36 + x37 +
  f(country, model = "iid") +
  f(year, model = "rw2") +
  f(country_int, model = "iid",
   group = year_int,
   control.group = list(model = "rw1"))

# combination 7
formula_2_bym_iid <- log_y ~ 1 +
  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
  x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 +
  x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 +
  x31 + x32 + x33 + x34 + x35 + x36 + x37 +
  f(country, model = "bym", graph = glb_adj) +
  f(year, model = "iid") +
  f(country_int, model = "iid",
   group = year_int,
   control.group = list(model = "rw1"))

# combination 8
formula_2_besag_iid <- log_y ~ 1 +
  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
  x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 +
  x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 +
  x31 + x32 + x33 + x34 + x35 + x36 + x37 +
  f(country, model = "besag", graph = glb_adj) +
  f(year, model = "iid") +
  f(country_int, model = "iid",
   group = year_int,
   control.group = list(model = "rw1"))

# combination 9
formula_2_iid_iid <- log_y ~ 1 +
  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
  x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 +
  x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 +
  x31 + x32 + x33 + x34 + x35 + x36 + x37 +
  f(country, model = "iid") +
  f(year, model = "iid") +
  f(country_int, model = "iid",
   group = year_int,
   control.group = list(model = "rw1"))

# interaction type 3
# combination 1
formula_3_bym_rw1 <- log_y ~ 1 +
  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
  x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 +
  x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 +
  x31 + x32 + x33 + x34 + x35 + x36 + x37 +
  f(country, model = "bym", graph = glb_adj) +
  f(year, model = "rw1") +
  f(year_int, model = "iid",
   group = country_int,
   control.group = list(model = "besag", graph = glb_adj))

# combination 2
formula_3_besag_rw1 <- log_y ~ 1 +
  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
  x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 +
  x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 +
  x31 + x32 + x33 + x34 + x35 + x36 + x37 +
  f(country, model = "besag", graph = glb_adj) +
  f(year, model = "rw1") +
  f(year_int, model = "iid",
   group = country_int,
   control.group = list(model = "besag", graph = glb_adj))

# combination 3
formula_3_iid_rw1 <- log_y ~ 1 +
  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
  x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 +
  x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 +
  x31 + x32 + x33 + x34 + x35 + x36 + x37 +
  f(country, model = "iid") +
  f(year, model = "rw1") +
  f(year_int, model = "iid",
   group = country_int,
   control.group = list(model = "besag", graph = glb_adj))

# combination 4
formula_3_bym_rw2 <- log_y ~ 1 +
  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
  x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 +
  x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 +
  x31 + x32 + x33 + x34 + x35 + x36 + x37 +
  f(country, model = "bym", graph = glb_adj) +
  f(year, model = "rw2") +
  f(year_int, model = "iid",
   group = country_int,
   control.group = list(model = "besag", graph = glb_adj))

# combination 5
formula_3_besag_rw2 <- log_y ~ 1 +
  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
  x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 +
  x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 +
  x31 + x32 + x33 + x34 + x35 + x36 + x37 +
  f(country, model = "besag", graph = glb_adj) +
  f(year, model = "rw2") +
  f(year_int, model = "iid",
   group = country_int,
   control.group = list(model = "besag", graph = glb_adj))

# combination 6
formula_3_iid_rw2 <- log_y ~ 1 +
  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
  x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 +
  x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 +
  x31 + x32 + x33 + x34 + x35 + x36 + x37 +
  f(country, model = "iid") +
  f(year, model = "rw2") +
  f(year_int, model = "iid",
   group = country_int,
   control.group = list(model = "besag", graph = glb_adj))

# combination 7
formula_3_bym_iid <- log_y ~ 1 +
  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
  x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 +
  x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 +
  x31 + x32 + x33 + x34 + x35 + x36 + x37 +
  f(country, model = "bym", graph = glb_adj) +
  f(year, model = "iid") +
  f(year_int, model = "iid",
   group = country_int,
   control.group = list(model = "besag", graph = glb_adj))

# combination 8
formula_3_besag_iid <- log_y ~ 1 +
  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
  x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 +
  x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 +
  x31 + x32 + x33 + x34 + x35 + x36 + x37 +
  f(country, model = "besag", graph = glb_adj) +
  f(year, model = "iid") +
  f(year_int, model = "iid",
   group = country_int,
   control.group = list(model = "besag", graph = glb_adj))

# combination 9
formula_3_iid_iid <- log_y ~ 1 +
  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
  x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 +
  x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 +
  x31 + x32 + x33 + x34 + x35 + x36 + x37 +
  f(country, model = "iid") +
  f(year, model = "iid") +
  f(year_int, model = "iid",
   group = country_int,
   control.group = list(model = "besag", graph = glb_adj))

# interaction type 4
# combination 1
formula_4_bym_rw1 <- log_y ~ 1 +
  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
  x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 +
  x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 +
  x31 + x32 + x33 + x34 + x35 + x36 + x37 +
  f(country, model = "bym", graph = glb_adj) +
  f(year, model = "rw1") +
  f(country_int, model = "bym", graph = glb_adj,
   group = year_int,
   control.group = list(model = "rw1"))

# combination 2
formula_4_besag_rw1 <- log_y ~ 1 +
  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
  x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 +
  x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 +
  x31 + x32 + x33 + x34 + x35 + x36 + x37 +
  f(country, model = "besag", graph = glb_adj) +
  f(year, model = "rw1") +
  f(country_int, model = "bym", graph = glb_adj,
   group = year_int,
   control.group = list(model = "rw1"))

# combination 3
formula_4_iid_rw1 <- log_y ~ 1 +
  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
  x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 +
  x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 +
  x31 + x32 + x33 + x34 + x35 + x36 + x37 +
  f(country, model = "iid") +
  f(year, model = "rw1") +
  f(country_int, model = "bym", graph = glb_adj,
   group = year_int,
   control.group = list(model = "rw1"))

# combination 4
formula_4_bym_rw2 <- log_y ~ 1 +
  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
  x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 +
  x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 +
  x31 + x32 + x33 + x34 + x35 + x36 + x37 +
  f(country, model = "bym", graph = glb_adj) +
  f(year, model = "rw2") +
  f(country_int, model = "bym", graph = glb_adj,
   group = year_int,
   control.group = list(model = "rw1"))

# combination 5
formula_4_besag_rw2 <- log_y ~ 1 +
  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
  x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 +
  x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 +
  x31 + x32 + x33 + x34 + x35 + x36 + x37 +
  f(country, model = "besag", graph = glb_adj) +
  f(year, model = "rw2") +
  f(country_int, model = "bym", graph = glb_adj,
   group = year_int,
   control.group = list(model = "rw1"))

# combination 6
formula_4_iid_rw2 <- log_y ~ 1 +
  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
  x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 +
  x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 +
  x31 + x32 + x33 + x34 + x35 + x36 + x37 +
  f(country, model = "iid") +
  f(year, model = "rw2") +
  f(country_int, model = "bym", graph = glb_adj,
   group = year_int,
   control.group = list(model = "rw1"))

# combination 7
formula_4_bym_iid <- log_y ~ 1 +
  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
  x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 +
  x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 +
  x31 + x32 + x33 + x34 + x35 + x36 + x37 +
  f(country, model = "bym", graph = glb_adj) +
  f(year, model = "iid") +
  f(country_int, model = "bym", graph = glb_adj,
   group = year_int,
   control.group = list(model = "rw1"))

# combination 8
formula_4_besag_iid <- log_y ~ 1 +
  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
  x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 +
  x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 +
  x31 + x32 + x33 + x34 + x35 + x36 + x37 +
  f(country, model = "besag", graph = glb_adj) +
  f(year, model = "iid") +
  f(country_int, model = "bym", graph = glb_adj,
   group = year_int,
   control.group = list(model = "rw1"))

# combination 9
formula_4_iid_iid <- log_y ~ 1 +
  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
  x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 +
  x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 +
  x31 + x32 + x33 + x34 + x35 + x36 + x37 +
  f(country, model = "iid") +
  f(year, model = "iid") +
  f(country_int, model = "bym", graph = glb_adj,
   group = year_int,
   control.group = list(model = "rw1"))

# function for spatiotemporal modeling
spt_model <- function(model_name, formula, data) {
  # computing part
  start_time <- Sys.time()

  model <- inla(
  formula,
  family = "gaussian",
  data = data,
  control.predictor = list(compute = TRUE),
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE))

  end_time <- Sys.time()
  time_taken <- end_time - start_time

  bias <- sum(values - data$log_y, na.rm = TRUE) / 3620
  mse <- sum((values - data$log_y)**2, na.rm = TRUE) / 3620

  # calculating evaluation metric
  pearson_corr <- cor(
    values, data$log_y,
    method = "pearson",
    use = "complete.obs")

  spearman_corr <- cor(
    values, data$log_y,
    method = "spearman",
    use = "complete.obs")

  cpo <- sum(-log(model$cpo$cpo))

  results <- list("model"         = model,
                  "bias"          = bias,
                  "dic"           = model$dic$dic,
                  "pdic"          = model$dic$p.eff,
                  "waic"          = model$waic$waic,
                  "pwaic"         = model$waic$p.eff,
                  "mse"           = mse,
                  "pearson_corr"  = pearson_corr,
                  "spearman_corr" = spearman_corr,
                  "cpo"           = cpo,
                  "run_time"      = time_taken)

  return(results)
}

model_1_bym_rw1   <- spt_model("model_1_bym_rw1",   formula_1_bym_rw1, data)
model_1_besag_rw1 <- spt_model("model_1_besag_rw1", formula_1_besag_rw1, data)
model_1_iid_rw1   <- spt_model("model_1_iid_rw1",   formula_1_iid_rw1, data)
model_1_bym_rw2   <- spt_model("model_1_bym_rw2",   formula_1_bym_rw2, data)
model_1_besag_rw2 <- spt_model("model_1_besag_rw2", formula_1_besag_rw2, data)
model_1_iid_rw2   <- spt_model("model_1_iid_rw2",   formula_1_iid_rw2, data)
model_1_bym_iid   <- spt_model("model_1_bym_iid",   formula_1_bym_iid, data)
model_1_besag_iid <- spt_model("model_1_besag_iid", formula_1_besag_iid, data)
model_1_iid_iid   <- spt_model("model_1_iid_iid",   formula_1_iid_iid, data)

model_2_bym_rw1   <- spt_model("model_2_bym_rw1",   formula_2_bym_rw1, data)
model_2_besag_rw1 <- spt_model("model_2_besag_rw1", formula_2_besag_rw1, data)
model_2_iid_rw1   <- spt_model("model_2_iid_rw1",   formula_2_iid_rw1, data)
model_2_bym_rw2   <- spt_model("model_2_bym_rw2",   formula_2_bym_rw2, data)
model_2_besag_rw2 <- spt_model("model_2_besag_rw2", formula_2_besag_rw2, data)
model_2_iid_rw2   <- spt_model("model_2_iid_rw2",   formula_2_iid_rw2, data)
model_2_bym_iid   <- spt_model("model_2_bym_iid",   formula_2_bym_iid, data)
model_2_besag_iid <- spt_model("model_2_besag_iid", formula_2_besag_iid, data)
model_2_iid_iid   <- spt_model("model_2_iid_iid",   formula_2_iid_iid, data)

model_3_bym_rw1   <- spt_model("model_3_bym_rw1",   formula_3_bym_rw1, data)
model_3_besag_rw1 <- spt_model("model_3_besag_rw1", formula_3_besag_rw1, data)
model_3_iid_rw1   <- spt_model("model_3_iid_rw1",   formula_3_iid_rw1, data)
model_3_bym_rw2   <- spt_model("model_3_bym_rw2",   formula_3_bym_rw2, data)
model_3_besag_rw2 <- spt_model("model_3_besag_rw2", formula_3_besag_rw2, data)
model_3_iid_rw2   <- spt_model("model_3_iid_rw2",   formula_3_iid_rw2, data)
model_3_bym_iid   <- spt_model("model_3_bym_iid",   formula_3_bym_iid, data)
model_3_besag_iid <- spt_model("model_3_besag_iid", formula_3_besag_iid, data)
model_3_iid_iid   <- spt_model("model_3_iid_iid",   formula_3_iid_iid, data)

model_4_bym_rw1   <- spt_model("model_4_bym_rw1",   formula_4_bym_rw1, data)
model_4_besag_rw1 <- spt_model("model_4_besag_rw1", formula_4_besag_rw1, data)
model_4_iid_rw1   <- spt_model("model_4_iid_rw1",   formula_4_iid_rw1, data)
model_4_bym_rw2   <- spt_model("model_4_bym_rw2",   formula_4_bym_rw2, data)
model_4_besag_rw2 <- spt_model("model_4_besag_rw2", formula_4_besag_rw2, data)
model_4_iid_rw2   <- spt_model("model_4_iid_rw2",   formula_4_iid_rw2, data)
model_4_bym_iid   <- spt_model("model_4_bym_iid",   formula_4_bym_iid, data)
model_4_besag_iid <- spt_model("model_4_besag_iid", formula_4_besag_iid, data)
model_4_iid_iid   <- spt_model("model_4_iid_iid",   formula_4_iid_iid, data)

## create dataframe
model_name <- c("model_1_bym_rw1", "model_1_besag_rw1", "model_1_iid_rw1",
                "model_1_bym_rw2", "model_1_besag_rw2", "model_1_iid_rw2",
                "model_1_bym_iid", "model_1_besag_iid", "model_1_iid_iid",
                "model_2_bym_rw1", "model_2_besag_rw1", "model_2_iid_rw1",
                "model_2_bym_rw2", "model_2_besag_rw2", "model_2_iid_rw2",
                "model_2_bym_iid", "model_2_besag_iid", "model_2_iid_iid",
                "model_3_bym_rw1", "model_3_besag_rw1", "model_3_iid_rw1",
                "model_3_bym_rw2", "model_3_besag_rw2", "model_3_iid_rw2",
                "model_3_bym_iid", "model_3_besag_iid", "model_3_iid_iid",
                "model_4_bym_rw1", "model_4_besag_rw1", "model_4_iid_rw1",
                "model_4_bym_rw2", "model_4_besag_rw2", "model_4_iid_rw2",
                "model_4_bym_iid", "model_4_besag_iid", "model_4_iid_iid")

dic <- c(model_1_bym_rw1$dic, model_1_besag_rw1$dic, model_1_iid_rw1$dic,
         model_1_bym_rw2$dic, model_1_besag_rw2$dic, model_1_iid_rw2$dic,
         model_1_bym_iid$dic, model_1_besag_iid$dic, model_1_iid_iid$dic,
         model_2_bym_rw1$dic, model_2_besag_rw1$dic, model_2_iid_rw1$dic,
         model_2_bym_rw2$dic, model_2_besag_rw2$dic, model_2_iid_rw2$dic,
         model_2_bym_iid$dic, model_2_besag_iid$dic, model_2_iid_iid$dic,
         model_3_bym_rw1$dic, model_3_besag_rw1$dic, model_3_iid_rw1$dic,
         model_3_bym_rw2$dic, model_3_besag_rw2$dic, model_3_iid_rw2$dic,
         model_3_bym_iid$dic, model_3_besag_iid$dic, model_3_iid_iid$dic,
         model_4_bym_rw1$dic, model_4_besag_rw1$dic, model_4_iid_rw1$dic,
         model_4_bym_rw2$dic, model_4_besag_rw2$dic, model_4_iid_rw2$dic,
         model_4_bym_iid$dic, model_4_besag_iid$dic, model_4_iid_iid$dic)

pdic <- c(model_1_bym_rw1$pdic, model_1_besag_rw1$pdic, model_1_iid_rw1$pdic,
         model_1_bym_rw2$pdic, model_1_besag_rw2$pdic, model_1_iid_rw2$pdic,
         model_1_bym_iid$pdic, model_1_besag_iid$pdic, model_1_iid_iid$pdic,
         model_2_bym_rw1$pdic, model_2_besag_rw1$pdic, model_2_iid_rw1$pdic,
         model_2_bym_rw2$pdic, model_2_besag_rw2$pdic, model_2_iid_rw2$pdic,
         model_2_bym_iid$pdic, model_2_besag_iid$pdic, model_2_iid_iid$pdic,
         model_3_bym_rw1$pdic, model_3_besag_rw1$pdic, model_3_iid_rw1$pdic,
         model_3_bym_rw2$pdic, model_3_besag_rw2$pdic, model_3_iid_rw2$pdic,
         model_3_bym_iid$pdic, model_3_besag_iid$pdic, model_3_iid_iid$pdic,
         model_4_bym_rw1$pdic, model_4_besag_rw1$pdic, model_4_iid_rw1$pdic,
         model_4_bym_rw2$pdic, model_4_besag_rw2$pdic, model_4_iid_rw2$pdic,
         model_4_bym_iid$pdic, model_4_besag_iid$pdic, model_4_iid_iid$pdic)

waic <- c(model_1_bym_rw1$waic, model_1_besag_rw1$waic, model_1_iid_rw1$waic,
         model_1_bym_rw2$waic, model_1_besag_rw2$waic, model_1_iid_rw2$waic,
         model_1_bym_iid$waic, model_1_besag_iid$waic, model_1_iid_iid$waic,
         model_2_bym_rw1$waic, model_2_besag_rw1$waic, model_2_iid_rw1$waic,
         model_2_bym_rw2$waic, model_2_besag_rw2$waic, model_2_iid_rw2$waic,
         model_2_bym_iid$waic, model_2_besag_iid$waic, model_2_iid_iid$waic,
         model_3_bym_rw1$waic, model_3_besag_rw1$waic, model_3_iid_rw1$waic,
         model_3_bym_rw2$waic, model_3_besag_rw2$waic, model_3_iid_rw2$waic,
         model_3_bym_iid$waic, model_3_besag_iid$waic, model_3_iid_iid$waic,
         model_4_bym_rw1$waic, model_4_besag_rw1$waic, model_4_iid_rw1$waic,
         model_4_bym_rw2$waic, model_4_besag_rw2$waic, model_4_iid_rw2$waic,
         model_4_bym_iid$waic, model_4_besag_iid$waic, model_4_iid_iid$waic)

pwaic <- c(model_1_bym_rw1$pwaic, model_1_besag_rw1$pwaic, model_1_iid_rw1$pwaic,
         model_1_bym_rw2$pwaic, model_1_besag_rw2$pwaic, model_1_iid_rw2$pwaic,
         model_1_bym_iid$pwaic, model_1_besag_iid$pwaic, model_1_iid_iid$pwaic,
         model_2_bym_rw1$pwaic, model_2_besag_rw1$pwaic, model_2_iid_rw1$pwaic,
         model_2_bym_rw2$pwaic, model_2_besag_rw2$pwaic, model_2_iid_rw2$pwaic,
         model_2_bym_iid$pwaic, model_2_besag_iid$pwaic, model_2_iid_iid$pwaic,
         model_3_bym_rw1$pwaic, model_3_besag_rw1$pwaic, model_3_iid_rw1$pwaic,
         model_3_bym_rw2$pwaic, model_3_besag_rw2$pwaic, model_3_iid_rw2$pwaic,
         model_3_bym_iid$pwaic, model_3_besag_iid$pwaic, model_3_iid_iid$pwaic,
         model_4_bym_rw1$pwaic, model_4_besag_rw1$pwaic, model_4_iid_rw1$pwaic,
         model_4_bym_rw2$pwaic, model_4_besag_rw2$pwaic, model_4_iid_rw2$pwaic,
         model_4_bym_iid$pwaic, model_4_besag_iid$pwaic, model_4_iid_iid$pwaic)

cpo <- c(model_1_bym_rw1$cpo, model_1_besag_rw1$cpo, model_1_iid_rw1$cpo,
         model_1_bym_rw2$cpo, model_1_besag_rw2$cpo, model_1_iid_rw2$cpo,
         model_1_bym_iid$cpo, model_1_besag_iid$cpo, model_1_iid_iid$cpo,
         model_2_bym_rw1$cpo, model_2_besag_rw1$cpo, model_2_iid_rw1$cpo,
         model_2_bym_rw2$cpo, model_2_besag_rw2$cpo, model_2_iid_rw2$cpo,
         model_2_bym_iid$cpo, model_2_besag_iid$cpo, model_2_iid_iid$cpo,
         model_3_bym_rw1$cpo, model_3_besag_rw1$cpo, model_3_iid_rw1$cpo,
         model_3_bym_rw2$cpo, model_3_besag_rw2$cpo, model_3_iid_rw2$cpo,
         model_3_bym_iid$cpo, model_3_besag_iid$cpo, model_3_iid_iid$cpo,
         model_4_bym_rw1$cpo, model_4_besag_rw1$cpo, model_4_iid_rw1$cpo,
         model_4_bym_rw2$cpo, model_4_besag_rw2$cpo, model_4_iid_rw2$cpo,
         model_4_bym_iid$cpo, model_4_besag_iid$cpo, model_4_iid_iid$cpo)

bias <- c(model_1_bym_rw1$bias, model_1_besag_rw1$bias, model_1_iid_rw1$bias,
         model_1_bym_rw2$bias, model_1_besag_rw2$bias, model_1_iid_rw2$bias,
         model_1_bym_iid$bias, model_1_besag_iid$bias, model_1_iid_iid$bias,
         model_2_bym_rw1$bias, model_2_besag_rw1$bias, model_2_iid_rw1$bias,
         model_2_bym_rw2$bias, model_2_besag_rw2$bias, model_2_iid_rw2$bias,
         model_2_bym_iid$bias, model_2_besag_iid$bias, model_2_iid_iid$bias,
         model_3_bym_rw1$bias, model_3_besag_rw1$bias, model_3_iid_rw1$bias,
         model_3_bym_rw2$bias, model_3_besag_rw2$bias, model_3_iid_rw2$bias,
         model_3_bym_iid$bias, model_3_besag_iid$bias, model_3_iid_iid$bias,
         model_4_bym_rw1$bias, model_4_besag_rw1$bias, model_4_iid_rw1$bias,
         model_4_bym_rw2$bias, model_4_besag_rw2$bias, model_4_iid_rw2$bias,
         model_4_bym_iid$bias, model_4_besag_iid$bias, model_4_iid_iid$bias)

mse <- c(model_1_bym_rw1$mse, model_1_besag_rw1$mse, model_1_iid_rw1$mse,
         model_1_bym_rw2$mse, model_1_besag_rw2$mse, model_1_iid_rw2$mse,
         model_1_bym_iid$mse, model_1_besag_iid$mse, model_1_iid_iid$mse,
         model_2_bym_rw1$mse, model_2_besag_rw1$mse, model_2_iid_rw1$mse,
         model_2_bym_rw2$mse, model_2_besag_rw2$mse, model_2_iid_rw2$mse,
         model_2_bym_iid$mse, model_2_besag_iid$mse, model_2_iid_iid$mse,
         model_3_bym_rw1$mse, model_3_besag_rw1$mse, model_3_iid_rw1$mse,
         model_3_bym_rw2$mse, model_3_besag_rw2$mse, model_3_iid_rw2$mse,
         model_3_bym_iid$mse, model_3_besag_iid$mse, model_3_iid_iid$mse,
         model_4_bym_rw1$mse, model_4_besag_rw1$mse, model_4_iid_rw1$mse,
         model_4_bym_rw2$mse, model_4_besag_rw2$mse, model_4_iid_rw2$mse,
         model_4_bym_iid$mse, model_4_besag_iid$mse, model_4_iid_iid$mse)

run_time <- c(model_1_bym_rw1$run_time, model_1_besag_rw1$run_time, model_1_iid_rw1$run_time,
         model_1_bym_rw2$run_time, model_1_besag_rw2$run_time, model_1_iid_rw2$run_time,
         model_1_bym_iid$run_time, model_1_besag_iid$run_time, model_1_iid_iid$run_time,
         model_2_bym_rw1$run_time, model_2_besag_rw1$run_time, model_2_iid_rw1$run_time,
         model_2_bym_rw2$run_time, model_2_besag_rw2$run_time, model_2_iid_rw2$run_time,
         model_2_bym_iid$run_time, model_2_besag_iid$run_time, model_2_iid_iid$run_time,
         model_3_bym_rw1$run_time, model_3_besag_rw1$run_time, model_3_iid_rw1$run_time,
         model_3_bym_rw2$run_time, model_3_besag_rw2$run_time, model_3_iid_rw2$run_time,
         model_3_bym_iid$run_time, model_3_besag_iid$run_time, model_3_iid_iid$run_time,
         model_4_bym_rw1$run_time, model_4_besag_rw1$run_time, model_4_iid_rw1$run_time,
         model_4_bym_rw2$run_time, model_4_besag_rw2$run_time, model_4_iid_rw2$run_time,
         model_4_bym_iid$run_time, model_4_besag_iid$run_time, model_4_iid_iid$run_time)

pearson_corr <- c(model_1_bym_rw1$pearson_corr, model_1_besag_rw1$pearson_corr, model_1_iid_rw1$pearson_corr,
         model_1_bym_rw2$pearson_corr, model_1_besag_rw2$pearson_corr, model_1_iid_rw2$pearson_corr,
         model_1_bym_iid$pearson_corr, model_1_besag_iid$pearson_corr, model_1_iid_iid$pearson_corr,
         model_2_bym_rw1$pearson_corr, model_2_besag_rw1$pearson_corr, model_2_iid_rw1$pearson_corr,
         model_2_bym_rw2$pearson_corr, model_2_besag_rw2$pearson_corr, model_2_iid_rw2$pearson_corr,
         model_2_bym_iid$pearson_corr, model_2_besag_iid$pearson_corr, model_2_iid_iid$pearson_corr,
         model_3_bym_rw1$pearson_corr, model_3_besag_rw1$pearson_corr, model_3_iid_rw1$pearson_corr,
         model_3_bym_rw2$pearson_corr, model_3_besag_rw2$pearson_corr, model_3_iid_rw2$pearson_corr,
         model_3_bym_iid$pearson_corr, model_3_besag_iid$pearson_corr, model_3_iid_iid$pearson_corr,
         model_4_bym_rw1$pearson_corr, model_4_besag_rw1$pearson_corr, model_4_iid_rw1$pearson_corr,
         model_4_bym_rw2$pearson_corr, model_4_besag_rw2$pearson_corr, model_4_iid_rw2$pearson_corr,
         model_4_bym_iid$pearson_corr, model_4_besag_iid$pearson_corr, model_4_iid_iid$pearson_corr)

spearman_corr <- c(model_1_bym_rw1$spearman_corr, model_1_besag_rw1$spearman_corr, model_1_iid_rw1$spearman_corr,
         model_1_bym_rw2$spearman_corr, model_1_besag_rw2$spearman_corr, model_1_iid_rw2$spearman_corr,
         model_1_bym_iid$spearman_corr, model_1_besag_iid$spearman_corr, model_1_iid_iid$spearman_corr,
         model_2_bym_rw1$spearman_corr, model_2_besag_rw1$spearman_corr, model_2_iid_rw1$spearman_corr,
         model_2_bym_rw2$spearman_corr, model_2_besag_rw2$spearman_corr, model_2_iid_rw2$spearman_corr,
         model_2_bym_iid$spearman_corr, model_2_besag_iid$spearman_corr, model_2_iid_iid$spearman_corr,
         model_3_bym_rw1$spearman_corr, model_3_besag_rw1$spearman_corr, model_3_iid_rw1$spearman_corr,
         model_3_bym_rw2$spearman_corr, model_3_besag_rw2$spearman_corr, model_3_iid_rw2$spearman_corr,
         model_3_bym_iid$spearman_corr, model_3_besag_iid$spearman_corr, model_3_iid_iid$spearman_corr,
         model_4_bym_rw1$spearman_corr, model_4_besag_rw1$spearman_corr, model_4_iid_rw1$spearman_corr,
         model_4_bym_rw2$spearman_corr, model_4_besag_rw2$spearman_corr, model_4_iid_rw2$spearman_corr,
         model_4_bym_iid$spearman_corr, model_4_besag_iid$spearman_corr, model_4_iid_iid$spearman_corr)

evaluate_df <- data.frame(
  model_name,
  bias,
  mse,
  dic,
  waic,
  cpo,
  pearson_corr,
  spearman_corr,
  run_time,
  pdic,
  pwaic)

View(evaluate_df)
write.csv(evaluate_df, "Gb_model_evaluation_both.csv", row.names = FALSE)