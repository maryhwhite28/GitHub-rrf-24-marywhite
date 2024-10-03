# Reproducible Research Fundamentals 
# 02. Data construction
# RRF - 2024 - Construction
#library(WDI)

# Preliminary - Load Data ----
#data_path <- "C:/Users/wb612884/OneDrive - WBG/Documents/RRF/Course Materials/DataWork/Data"

# Load household-level data (HH)
hh_data <- read_dta(file.path(data_path, "Intermediate/TZA_CCT_HH.dta"))

# Load HH-member data
mem_data <- read_dta(file.path(data_path, "Intermediate/TZA_CCT_HH_mem.dta"))

# Load secondary data
secondary_data <- read_dta(file.path(data_path, "Intermediate/TZA_amenity_tidy.dta"))

# Exercise 1: Plan construction outputs ----
# Plan the following outputs:
# 1. Area in acres. - hh_data: Divide hectare by 1000, create area_acres variable
# 2. Household consumption (food and nonfood) in USD. hh_data: cons = food_cons and nonfood_cons
# 3. Any HH member sick. mem_data - sick, collapse to HH level
# 4. Any HH member can read or write. mem_data - read, collapse to HH level
# 5. Average sick days. mem-data: days_sick, collapse HH level
# 6. Total treatment cost in USD. - mem_data: treat_cost, collapse HH level
# 7. Total medical facilities. secondary_data: n_hospital + n_clinic

# Exercise 2: Standardize conversion values ----
# Define standardized conversion values:
# 1. Conversion factor for acres.
acre_conversion = 2.47
# 2. USD conversion factor.
usd = 0.0037

# Data construction: Household (HH) ----
# Instructions:
# 1. Convert farming area to acres where necessary.
hh_data = hh_data %>%
    mutate(area_acre = case_when(
        ar_unit == 2 ~ ar_farm,
        ar_unit == 3 ~ ar_farm * acre_conversion)) %>%
    mutate(area_acre = ifelse(is.na(area_acre),0,area_acre)) %>%
    set_variable_labels(area_acre = "Area farmed in acres") %>%
# 2. Convert household consumption for food and nonfood into USD.
    mutate(across(c(food_cons, nonfood_cons),
                  ~ .x * usd,
                  .names = "{.col}_usd"))

# Exercise 3: Handle outliers ----
# you can use custom Winsorization function to handle outliers.
winsor_function <- function(dataset, var, min = 0.00, max = 0.95) {
    var_sym <- sym(var)
    
    percentiles <- quantile(
        dataset %>% pull(!!var_sym), probs = c(min, max), na.rm = TRUE
    )
    
    min_percentile <- percentiles[1]
    max_percentile <- percentiles[2]
    
    dataset %>%
        mutate(
            !!paste0(var, "_w") := case_when(
                is.na(!!var_sym) ~ NA_real_,
                !!var_sym <= min_percentile ~ percentiles[1],
                !!var_sym >= max_percentile ~ percentiles[2],
                TRUE ~ !!var_sym
            )
        )
}

# Tips: Apply the Winsorization function to the relevant variables.
# Create a list of variables that require Winsorization and apply the function to each.
win_vars = c("area_acre","food_cons_usd","nonfood_cons_usd")

for (var in win_vars) {
    hh_data = winsor_function(hh_data,var)
}

#update label
hh_data = hh_data %>%
    mutate(across(ends_with("w"),
                  ~ labelled(.x, label = paste0(attr(.x, "label"), 
                                                     " (winsorized 0.05)"))))

# Exercise 4.1: Create indicators at household level ----
# Instructions:
# Collapse HH-member level data to HH level.
hh_mem_collapsed = mem_data %>%
    group_by(hhid) %>%
# Plan to create the following indicators:
# 1. Any member was sick.
    summarise(
        sick = max(sick, na.rm=T),
        # 2. Any member can read/write.
        read = max(read, na.rm=T),
        # 3. Average sick days.
        days_sick = ifelse(all(is.na(days_sick)), NA_real_,
                           mean(days_sick,na.rm=T)),
        #If all values of treat_cost are NA, return NA
        treat_cost_usd = if_else(all(is.na(treat_cost)),NA_real_, sum(treat_cost,na.rm=T)*usd))%>%
    ungroup() %>%
    mutate(treat_cost_usd = if_else(is.na(treat_cost_usd),
                                    mean(treat_cost_usd,na.rm=T),
                                    treat_cost_usd)) %>%
        # 4. Total treatment cost in USD.
    set_variable_labels(sick = "Any HH member sick",
                        read = "Any HH member read",
                        days_sick = "Average days HH members sick",
                        treat_cost_usd = "Total treatment cost in USD")


# Exercise 4.2: Data construction: Secondary data ----
# Instructions:
# Calculate the total number of medical facilities by summing relevant columns.
secondary_data = secondary_data %>%
    mutate(n_medical = rowSums(select(.,n_clinic,n_hospital),
                               na.rm=T)) %>%
# Apply appropriate labels to the new variables created.
    set_variable_labels(n_medical = "Number of medical facilities",
                        adm2_en = "District")

# Exercise 5: Merge HH and HH-member data ----
# Instructions:
# Merge the household-level data with the HH-member level indicators.
HH_merge = hh_data %>%
    left_join(hh_mem_collapsed,by=c('hhid'))
# After merging, ensure the treatment status is included in the final dataset.
treat_status = read_dta(file.path(data_path, "Raw/treat_status.dta"))

final_hh_data = HH_merge %>%
    left_join(treat_status, by=c('vid'))

# Exercise 6: Save final dataset ----
# Instructions:
# Only keep the variables you will use for analysis.
# Save the final dataset for further analysis.
write_dta(final_hh_data,file.path(data_path, "Final/TZA_CCT_analysis.dta"))
# Save both the HH dataset and the secondary data.
write_dta(secondary_data,file.path(data_path, "Final/TZA_amenity_analysis.dta"))

# Tip: Ensure all variables are correctly labeled 

