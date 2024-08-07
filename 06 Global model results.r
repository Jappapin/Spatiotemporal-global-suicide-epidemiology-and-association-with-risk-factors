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

# risk factors
risk_factors <- read.csv(
  paste(my_dir, "Global suicides/0-cleaned_filled_risk_factor_37.csv",
  sep = ""))

# import data
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
# id for association each country
data$x1_id <- data$country
data$x3_id <- data$country
data$x6_id <- data$country
data$x7_id <- data$country
data$x13_id <- data$country
data$x16_id <- data$country
data$x18_id <- data$country
data$x19_id <- data$country
data$x26_id <- data$country
data$x33_id <- data$country
# merge data with fixed effect
gb_both <- cbind(data, risk_factors)

# import data
data <- read.csv(
  paste(my_dir, "Global suicides/age_adjusted_female_melt_181.csv",
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
# id for association each country
data$x1_id <- data$country
data$x3_id <- data$country
data$x6_id <- data$country
data$x7_id <- data$country
data$x13_id <- data$country
data$x16_id <- data$country
data$x18_id <- data$country
data$x19_id <- data$country
data$x26_id <- data$country
data$x33_id <- data$country
# merge data with fixed effect
gb_female <- cbind(data, risk_factors)

# import data
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
# id for association each country
data$x1_id <- data$country
data$x3_id <- data$country
data$x6_id <- data$country
data$x7_id <- data$country
data$x13_id <- data$country
data$x16_id <- data$country
data$x18_id <- data$country
data$x19_id <- data$country
data$x26_id <- data$country
data$x33_id <- data$country
# merge data with fixed effect
gb_male <- cbind(data, risk_factors)

# selected multivariable analysis

# function for spatiotemporal modeling
association <- function(name, data, formula) {
  # computing part
  model <- inla(
  formula,
  family = "gaussian",
  data = data,
  control.predictor = list(compute = TRUE))

  print("the data's association has been computed 1 dataset!")
  
  inc_df <- data.frame(
    c(exp(model$summary.random$x1_id$mean)),
    c(exp(model$summary.random$x3_id$mean)),
    c(exp(model$summary.random$x6_id$mean)),
    c(exp(model$summary.random$x7_id$mean)),
    c(exp(model$summary.random$x13_id$mean)),
    c(exp(model$summary.random$x16_id$mean)),
    c(exp(model$summary.random$x18_id$mean)),
    c(exp(model$summary.random$x19_id$mean)),
    c(exp(model$summary.random$x26_id$mean)),
    c(exp(model$summary.random$x33_id$mean)))

  colnames(inc_df) <- c(
    'NCD mortality', 'health expenditure', 'CHE domestic health expenditure', 'GGE domestic health expenditure',
    'Domestic private health expenditure', 'external health expenditure', 'number of people living with HIV', 'number of road traffic deaths',
    'population using basic drinking water', 'TB incidence' )
  inc_df <- (inc_df - 1)*100
  write.csv(inc_df, sprintf("%s_inc.csv", name), row.names = FALSE)

  x1 <- ifelse(model$summary.random$x1_id[,4] > 0 | model$summary.random$x1_id[,6] < 0, "Significant", "Not Significant")
  x2 <- ifelse(model$summary.random$x3_id[,4] > 0 | model$summary.random$x3_id[,6] < 0, "Significant", "Not Significant")
  x3 <- ifelse(model$summary.random$x6_id[,4] > 0 | model$summary.random$x6_id[,6] < 0, "Significant", "Not Significant")
  x4 <- ifelse(model$summary.random$x7_id[,4] > 0 | model$summary.random$x7_id[,6] < 0, "Significant", "Not Significant")
  x5 <- ifelse(model$summary.random$x13_id[,4] > 0 | model$summary.random$x13_id[,6] < 0, "Significant", "Not Significant")
  x6 <- ifelse(model$summary.random$x16_id[,4] > 0 | model$summary.random$x16_id[,6] < 0, "Significant", "Not Significant")
  x7 <- ifelse(model$summary.random$x18_id[,4] > 0 | model$summary.random$x18_id[,6] < 0, "Significant", "Not Significant")
  x8 <- ifelse(model$summary.random$x19_id[,4] > 0 | model$summary.random$x19_id[,6] < 0, "Significant", "Not Significant")
  x9 <- ifelse(model$summary.random$x26_id[,4] > 0 | model$summary.random$x26_id[,6] < 0, "Significant", "Not Significant")
  x10 <- ifelse(model$summary.random$x33_id[,4] > 0 | model$summary.random$x33_id[,6] < 0, "Significant", "Not Significant")

  sig_df <- data.frame(
    c(x1),
    c(x2),
    c(x3),
    c(x4),
    c(x5),
    c(x6),
    c(x7),
    c(x8),
    c(x9),
    c(x10))

  colnames(sig_df) <- c(
    'NCD mortality', 'health expenditure', 'CHE domestic health expenditure', 'GGE domestic health expenditure',
    'Domestic private health expenditure', 'external health expenditure', 'number of people living with HIV', 'number of road traffic deaths',
    'population using basic drinking water', 'TB incidence' )
  write.csv(sig_df, sprintf("%s_sig.csv", name), row.names = FALSE)
}

# construct model
# best random for both
# best random for female
formula_1_bym_rw2 <- log_y ~ 1 +
  f(x1_id, x1, model = "iid") +
  f(x3_id, x3, model = "iid") +
  f(x6_id, x6, model = "iid") +
  f(x7_id, x7, model = "iid") +
  f(x13_id, x13, model = "iid") +
  f(x16_id, x16, model = "iid") +
  f(x18_id, x18, model = "iid") +
  f(x19_id, x19, model = "iid") +
  f(x26_id, x26, model = "iid") +
  f(x33_id, x33, model = "iid") +
  f(country, model = "bym", graph = glb_adj) +
  f(year, model = "rw2") +
  f(country_year, model = "iid")

# best random for male
formula_1_besag_rw2 <- log_y ~ 1 +
  f(x1_id, x1, model = "iid") +
  f(x3_id, x3, model = "iid") +
  f(x6_id, x6, model = "iid") +
  f(x7_id, x7, model = "iid") +
  f(x13_id, x13, model = "iid") +
  f(x16_id, x16, model = "iid") +
  f(x18_id, x18, model = "iid") +
  f(x19_id, x19, model = "iid") +
  f(x26_id, x26, model = "iid") +
  f(x33_id, x33, model = "iid") +
  f(country, model = "besag", graph = glb_adj) +
  f(year, model = "rw2") +
  f(country_year, model = "iid")

association("gb_both_association", gb_both, formula_1_bym_rw2)
association("gb_female_association", gb_female, formula_1_bym_rw2)
association("gb_male_association", gb_male, formula_1_besag_rw2)

# function for spatiotemporal modeling
cluster_detection <- function(name, data, formula) {

  # computing part
  model <- inla(
  formula,
  family = "gaussian",
  data = data,
  control.predictor = list(compute = TRUE),
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE,
    return.marginals.predictor = TRUE))

  print("the data's cluster detection has been computed 1 dataset!")

  start <- 1
  end <- 181
  years <- nrow(data) / 181
  for (x in 1:years) {

    median <- log(median(data$y[start:end], na.rm = FALSE))
    exceedance_prob <- sapply(
      model$marginals.fitted.values[start:end],
      FUN = function(marg) {
        1 - inla.pmarginal(q = median, marginal = marg) })

    cluster_df <- data[start:end, 1:3]
    cluster_df[, "hotspot"] <- exceedance_prob > 0.95
    cluster_df[, "hotspot"] <- ifelse(exceedance_prob > 0.95, "hotspot", "non-hotspot")

    #cluster_df <- cbind(global_map, cluster_df)
    write.csv(cluster_df, sprintf("%s%s.csv", name, x + 1999), row.names = FALSE)

    start <- start + 181
    end <- end + 181
  }
}

# construct model
# best random for both
# best random for female
formula_1_bym_rw2 <- log_y ~ 1 +
  f(country, model = "bym", graph = glb_adj) +
  f(year, model = "rw2") +
  f(country_year, model = "iid")

# best random for male
formula_1_besag_rw2 <- log_y ~ 1 +
  f(country, model = "besag", graph = glb_adj) +
  f(year, model = "rw2") +
  f(country_year, model = "iid")

cluster_gb_both <- cluster_detection("gb_both_cluster_", gb_both, formula_1_bym_rw2)
cluster_gb_female <- cluster_detection("gb_female_cluster_", gb_female, formula_1_bym_rw2)
cluster_gb_male <- cluster_detection("gb_male_cluster_", gb_male, formula_1_besag_rw2)



formula_1_bym_rw2 <- log_y ~ 1 +
  f(country, model = "bym", graph = glb_adj) +
  f(year, model = "rw2") +
  f(country_year, model = "iid")

# computing part
model <- inla(
formula_1_bym_rw2,
family = "gaussian",
data = gb_both,
control.predictor = list(compute = TRUE),
control.compute = list(
  dic = TRUE,
  waic = TRUE,
  cpo = TRUE,
  return.marginals.predictor = TRUE))

median <- log(median(gb_both$y, na.rm = FALSE))
exceedance_prob <- sapply(
  model$marginals.fitted.values[1:181],
  FUN = function(marg) {
    1 - inla.pmarginal(q = median, marginal = marg) })

cluster_df <- gb_both[1:181, 1:3]
cluster_df[, "hotspot"] <- exceedance_prob > 0.95
cluster_df[, "hotspot"] <- ifelse(exceedance_prob > 0.95, "hotspot", "non-hotspot")

cluster_df <- cbind(global_map, cluster_df)

# plot cluster detection using exceedance probaility
ggplot(cluster_df) + geom_sf(aes(fill = hotspot))

# plot association
ggplot(global_map) +
scale_fill_gradient(low = "white", high = "red") +
geom_sf(aes(fill = hotspot)) +

labs(fill = "coefficient") +

theme(legend.position = "bottom",
legend.key.size = unit(1, "cm"), #change legend key size
legend.key.height = unit(0.5, "cm"), #change legend key height
legend.key.width = unit(3.85, "cm"), #change legend key width
legend.title = element_text(size = 14, vjust = 0.9),
legend.text = element_text(size = 10), #change legend text font size

plot.title = element_text(size = 16, hjust = 0.5, vjust = 0.5),
plot.subtitle = element_text(size = 12, hjust = 0.5, vjust = 0.5)) +

ggtitle(
  label = "Association with NCD mortality rate 2000-2019",
  subtitle = "Both sexes Age-adjusted suicide rates per 100k")
ggsave(
  filename = "x1.png",
  device = "png",
  path = "picture/association_plot/both_sexes/",
  width = 10,
  height = 5,
  dpi = 300,
  limitsize = FALSE)