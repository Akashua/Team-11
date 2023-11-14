
# Perform linear modeling for each covariate separately for the North region
model_north_pop15 <- lm(y ~ Pop15, data = north_theft_locations_df)
model_north_apart <- lm(y ~ Apart, data = north_theft_locations_df)
model_north_eco <- lm(y ~ Eco, data = north_theft_locations_df)
model_north_employ <- lm(y ~ Employ, data = north_theft_locations_df)
model_north_inBorn <- lm(y ~ inBorn, data = north_theft_locations_df)
model_north_health <- lm(y ~ Health, data = north_theft_locations_df)
model_north_scholar <- lm(y ~ Scholar, data = north_theft_locations_df)
model_north_extor <- lm(y ~ Extor, data = north_theft_locations_df)
model_north_murder <- lm(y ~ Murder, data = north_theft_locations_df)
model_north_burg <- lm(y ~ Burg, data = north_theft_locations_df)
model_north_shop <- lm(y ~ Shop, data = north_theft_locations_df)
model_north_public <- lm(y ~ Public, data = north_theft_locations_df)
model_north_street <- lm(y ~ Street, data = north_theft_locations_df)
model_north_kidnap <- lm(y ~ Kidnap, data = north_theft_locations_df)

# Summarize the model results for the North region
summary(model_north_pop15)
summary(model_north_apart)
summary(model_north_eco)
summary(model_north_employ)
summary(model_north_inBorn)
summary(model_north_health)
summary(model_north_scholar)
summary(model_north_extor)
summary(model_north_murder)
summary(model_north_burg)
summary(model_north_shop)
summary(model_north_public)
summary(model_north_street)
summary(model_north_kidnap)



# Perform linear modeling for each covariate separately for the South region
model_south_pop15 <- lm(y ~ Pop15, data = south_theft_locations_df)
model_south_apart <- lm(y ~ Apart, data = south_theft_locations_df)
model_south_eco <- lm(y ~ Eco, data = south_theft_locations_df)
model_south_employ <- lm(y ~ Employ, data = south_theft_locations_df)
model_south_inBorn <- lm(y ~ inBorn, data = south_theft_locations_df)
model_south_health <- lm(y ~ Health, data = south_theft_locations_df)
model_south_scholar <- lm(y ~ Scholar, data = south_theft_locations_df)
model_south_extor <- lm(y ~ Extor, data = south_theft_locations_df)
model_south_murder <- lm(y ~ Murder, data = south_theft_locations_df)
model_south_burg <- lm(y ~ Burg, data = south_theft_locations_df)
model_south_shop <- lm(y ~ Shop, data = south_theft_locations_df)
model_south_public <- lm(y ~ Public, data = south_theft_locations_df)
model_south_street <- lm(y ~ Street, data = south_theft_locations_df)
model_south_kidnap <- lm(y ~ Kidnap, data = south_theft_locations_df)

# Summarize the model results for the South region
summary(model_south_pop15)
summary(model_south_apart)
summary(model_south_eco)
summary(model_south_employ)
summary(model_south_inBorn)
summary(model_south_health)
summary(model_south_scholar)
summary(model_south_extor)
summary(model_south_murder)
summary(model_south_burg)
summary(model_south_shop)
summary(model_south_public)
summary(model_south_street)
summary(model_south_kidnap)



# Function to identify significant covariates from a list of linear models
identify_significant_covariates <- function(model_list, significance_level = 0.05) {
  significant_covariates <- list()
  
  for (model_name in names(model_list)) {
    model <- model_list[[model_name]]
    p_values <- summary(model)$coefficients[, "Pr(>|t|)"]
    
    # Exclude the intercept
    p_values <- p_values[names(p_values) != "(Intercept)"]
    
    # Find significant covariates
    significant_covariates[[model_name]] <- names(p_values[p_values < significance_level])
  }
  
  return(significant_covariates)
}

# Create a list of models for the North region
models_north <- list(
  model_north_pop15 = model_north_pop15,
  model_north_apart = model_north_apart,
  model_north_eco = model_north_eco,
  model_north_employ = model_north_employ,
  model_north_inBorn = model_north_inBorn,
  model_north_health = model_north_health,
  model_north_scholar = model_north_scholar,
  model_north_extor = model_north_extor,
  model_north_murder = model_north_murder,
  model_north_burg = model_north_burg,
  model_north_shop = model_north_shop,
  model_north_public = model_north_public,
  model_north_street = model_north_street,
  model_north_kidnap = model_north_kidnap
)

# Create a list of models for the South region
models_south <- list(
  model_south_pop15 = model_south_pop15,
  model_south_apart = model_south_apart,
  model_south_eco = model_south_eco,
  model_south_employ = model_south_employ,
  model_south_inBorn = model_south_inBorn,
  model_south_health = model_south_health,
  model_south_scholar = model_south_scholar,
  model_south_extor = model_south_extor,
  model_south_murder = model_south_murder,
  model_south_burg = model_south_burg,
  model_south_shop = model_south_shop,
  model_south_public = model_south_public,
  model_south_street = model_south_street,
  model_south_kidnap = model_south_kidnap
)


# Identify significant covariates for the North region
significant_north <- identify_significant_covariates(models_north)

# Identify significant covariates for the South region
significant_south <- identify_significant_covariates(models_south)

# Print significant covariates
print("Significant Covariates - North:")
print(significant_north)

print("Significant Covariates - South:")
print(significant_south)





# Function to perform stepwise selection based on BIC
perform_stepwise_selection <- function(initial_model) {
  step(initial_model, direction = "both", k = log(nrow(initial_model$model)))
}

# Applying stepwise selection for the North region
north_models_stepwise <- lapply(models_north, perform_stepwise_selection)

# Applying stepwise selection for the South region
south_models_stepwise <- lapply(models_south, perform_stepwise_selection)

# Function to extract model formula
extract_model_formula <- function(model_list) {
  sapply(model_list, function(model) as.character(formula(model)))
}

# Extracting final model formulas after stepwise selection
final_formulas_north <- extract_model_formula(north_models_stepwise)
final_formulas_south <- extract_model_formula(south_models_stepwise)

# Print final model formulas
print("Final Model Formulas - North:")
print(final_formulas_north)

print("Final Model Formulas - South:")
print(final_formulas_south)

