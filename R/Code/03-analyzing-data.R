# Reproducible Research Fundamentals 
# 03. Data Analysis

# Libraries
 library(haven)
 library(dplyr)
 library(modelsummary)
 library(stargazer)
 library(ggplot2)
 library(tidyr)
 library(ggthem)

# Load data 
#household level data
data_path <- "C:/Users/wb612884/OneDrive - WBG/Documents/RRF/Course Materials/DataWork/Data"
hh_data   <- read_dta(file.path(data_path, "Final/TZA_CCT_analysis.dta"))

# secondary data 
secondary_data <- read_dta(file.path(data_path, "Final/TZA_amenity_analysis.dta")) %>%
    mutate(district = as_factor(adm2_en))

# Summary statistics ----

# Create summary statistics by district and export to CSV
data_renamed = hh_data %>%
    select(
        `HH Size` = hh_size,
        `N children <= 5` = n_child_5,
        `N of elderly (> 60)` = n_elder,
        `Any member can read` = read,
        `Any member was sick in last 4 days` = sick,
        `District` = district)


summary_table <- datasummary(
    All (data_renamed) ~ as_factor(District) * (Mean + SD) * Arguments(na.rm = TRUE), 
    data = data_renamed,
    title = "Summary Statistics by District",
    output = file.path("Outputs", "summary_table.csv")  # Change to CSV
)


# Balance table ----
data_by_treatment = hh_data %>%
    mutate(treatment = as.factor(treatment)) %>%
    select(
        `Treatment` = treatment,
        `HH Size` = hh_size,
        `N children <= 5` = n_child_5,
        `N of elderly (> 60)` = n_elder,
        `Any member can read` = read,
        `Any member was sick in last 4 days` = sick,
        `District` = district)
    
    balance_table <- datasummary_balance(
    sumvars ~ Treatment,
    data = data_by_treatment,
    stars = TRUE,
    title = "Balance by Treatment Status",
    note = "Includes HHS with observations for baseline and endline",
    output = file.path("Outputs", "balance_table.csv")  # Change to CSV
)

# Regressions ----
   
# Model 1: Food consumption regressed on treatment
model1 <- lm(food_cons_usd_w ~ treatment, data = hh_data)
summary(model1)

# Model 2: Add controls (crop_damage, drought_flood)
model2 <- lm(food_cons_usd_w ~ treatment + crop_damage + drought_flood, data = hh_data)
summary(model2)

# Model 3: Add FE by district
model3 <- lm(food_cons_usd_w ~ treatment + crop_damage + drought_flood + factor(district), data = hh_data)
summary(model3)

# Create regression table using stargazer
stargazer(
    model1, model2, model3,
    title = "Food Consumption Effects",
    keep = c("treatment", "crop_damage", "drought_flood"),
    covariate.labels = c("Treatment",
                         "Crop Damage",
                         "Drought/Flood"),
    dep.var.labels = c("Food Consumption (USD)"),
    dep.var.caption = "",
    add.lines = list(c("District Fixed Effects", "No", "No", "Yes")),
    header = FALSE,
    keep.stat = c("n", "adj.rsq"),
    notes = "Standard errors in parentheses",
    out = file.path("Outputs","regression_table.tex")
)

# Graphs: Area cultivated by treatment assignment across districts ----
ggthemr("fresh")
# Bar graph by treatment for all districts
# Ensure treatment is a factor for proper labeling
hh_data_plot <- hh_data %>%
    mutate(treatment = factor(treatment, labels = c("Control", "Treatment")), 
           district = as_factor(district)) %>%
    group_by(district,treatment)%>%
    summarise(mean_area_acre = mean(area_acre,na.rm=T))

# Create the bar plot
# Create the bar plot
ggplot(hh_data_plot, aes(x = treatment, y = mean_area_acre, fill = treatment)) +
    geom_bar(stat = "identity", position = 'dodge') +
    geom_text(aes(label = round(mean_area_acre, 1)),
              vjust = -0.5) +  # Add text labels
    facet_wrap(~district) +  # Facet by district
    labs(title = "Area cultivated by treatment assignment across districts",
         x = NULL, y = "Average area cultivated (Acre)") +  # Remove x-axis title
    theme_minimal()

ggsave(file.path("Outputs", "fig1.png"), width = 10, height = 6)


# Graphs: Distribution of non-food consumption by female-headed households ----

# Calculate mean non-food consumption for female and male-headed households
mean_female <- hh_data %>% 
    filter(female_head == 1) %>% 
    summarise(mean = mean(nonfood_cons_usd_w, na.rm = TRUE)) %>% 
    pull(mean)

mean_male <- hh_data %>% 
    filter(female_head == 0) %>% 
    summarise(mean = mean(nonfood_cons_usd_w, na.rm = TRUE)) %>% 
    pull(mean)

# Create the density plot
ggplot(hh_data, 
       aes(x = nonfood_cons_usd_w, color = as.factor(female_head))) +
    geom_density(linewidth = 1) +  # Density plot
    geom_vline(xintercept = mean_female, color = "purple", linetype = "dashed", size = 1) +  # Vertical line for female mean
    geom_vline(xintercept = mean_male, color = "grey", linetype = "dashed", size = 1) +  # Vertical line for male mean
    labs(title = "Distribution of Non-Food Consumption",
         x = "Non-food consumption value (USD)", 
         y = "Density",
         color = "Household Head:") +  # Custom labels
    geom_text(aes(x = mean_female, y = 0.0015, label = paste("Mean (Female):", round(mean_female, 1))),
              color = "purple", vjust = -0.5) +  # Label for female mean
    geom_text(aes(x = mean_male, y = 0.0005, label = paste("Mean (Male):", round(mean_male, 1))),
              color = "grey", vjust = -0.5) +
    theme_minimal()+ # Add other customization if needed
    scale_color_manual(values = c("grey", "purple"), 
                       labels = c("Male-headed", "Female-headed"))

ggsave(file.path("Outputs", "fig2.png"), width = 10, height = 6)

# Graphs: Secondary data ----

long_data <- secondary_data %>%
    ungroup() %>% 
    select(-c(n_hospital, n_clinic)) %>% 
    pivot_longer(cols = c(n_school, n_medical), names_to = "amenity", values_to = "count") %>%
    mutate(amenity = recode(amenity, n_school = "Number of Schools", n_medical = "Number of Medical Facilities"),
           in_sample = if_else(district %in% c("Kibaha", "Chamwino", "Bagamoyo"), "In Sample", "Not in Sample"))

long_data <- long_data %>%
    mutate(district = factor(district, 
                             levels = long_data %>%
                                 group_by(district) %>%
                                 summarise(total_count = sum(count)) %>%
                                 arrange(-total_count) %>%
                                 pull(district)))

# Create the facet-wrapped bar plot
ggplot(long_data, 
       aes(x = district, y = count, fill = in_sample)) +
    geom_bar(stat = "identity", position = "dodge") +  # Use position dodge for side-by-side bars
    coord_flip() +  # Flip coordinates for better visibility
    facet_wrap(~amenity) +  # Create facets for schools and medical facilities
    labs(title = "Access to Amenities: By Districts",
         x = "District", 
         y = NULL, 
         fill = "In Sample Status:") +  # Update legend title
    scale_fill_brewer(palette = "PuRd") +  # Use a color palette
    theme_minimal() +
    theme(legend.position = "top") 

ggsave(file.path("Outputs", "fig3.png"), width = 10, height = 6)
