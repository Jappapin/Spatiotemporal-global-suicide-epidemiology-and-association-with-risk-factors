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
  paste(my_dir, "Global suicides/age_adjusted_male_melt_181.csv",
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

## model selection ##

results <- data.frame(mean = numeric(0), lower_bound = numeric(0), upper_bound = numeric(0))

for (x in 1:37) {
x1 <- risk_factors[, x]

formula <- log_y ~ 1 + x1 +
  f(country, model = "besag", graph = glb_adj) +
  f(year, model = "rw2") +
  f(country_year, model = "iid")

model <- inla(
formula,
family = "gaussian",
data = data,
control.predictor = list(compute = TRUE),
control.compute = list(
  dic = TRUE,
  waic = TRUE,
  cpo = TRUE),
  verbose = TRUE)

results[nrow(results) + 1, ] <- c(model$summary.fixed[2, 1], model$summary.fixed[2, 3], model$summary.fixed[2, 5])
print("one round passed!")
}
write.csv(results, "gb_fixed_effects_male.csv", row.names = FALSE)

model$summary.fixed