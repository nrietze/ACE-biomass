
library(stats)
library(ggplot2)
library(ggthemes)
library(gridExtra)

#data <- read.csv("C:/Users/Vitalii/Desktop/Arctic_century_analysis/biomass_cover_reformated.csv")

data <- read.csv(file.choose())

# Filter out the specified sites without drone data
data <- subset(data, !(Site %in% c("GrahamBell", "OctRevCentre", "Bolshevik", "Komsomolets")))

###################################################################################
# 0. LM and GLM #
###################################################################################
# lm cover-biomass
lm_biomass_cover <- lm(formula = Live_biomass ~ Cover_live, data = data)
summary(lm_biomass_cover)

# lm stdev Sentinel2
lm_biomass_NDVI_sentinel <- lm(formula = Live_biomass ~ Sentinel2_stdev, data = data)
summary(lm_biomass_NDVI_sentinel)

# lm stdev drone
lm_biomass_NDVI_drone <- lm(formula = Live_biomass ~ drone_stdev, data = data)
summary(lm_biomass_NDVI_drone)

# lm mean Sentinel2
lm_biomass_NDVI_sentinel_mean <- lm(formula = Live_biomass ~ Sentinel2_mean, data = data)
summary(lm_biomass_NDVI_sentinel_mean)

# lm mean drone
lm_biomass_NDVI_drone_mean <- lm(formula = Live_biomass ~ drone_mean, data = data)
summary(lm_biomass_NDVI_drone_mean)

###start script###
###################################################################################

#linear models to check influence of cover and site on biomass including cover^2 - trial

data$Site

# backwards selection starting with full model
lm3_biomass_cover <- lm(Live_biomass ~ Cover_live + I(Cover_live^2) + Site + 
                          Cover_live:Site + I(Cover_live^2):Site, data = data)
summary(lm3_biomass_cover)

# (- I(Cover_live^2):Site)
lm4_biomass_cover <- lm(Live_biomass ~ Cover_live + I(Cover_live^2) + Site + 
                          Cover_live:Site , data = data)
summary(lm4_biomass_cover)

# compare models
anova(lm3_biomass_cover, lm4_biomass_cover) # no significant effect of I(Cover_live^2):Site

# (- Cover_live:Site)
lm5_biomass_cover <- lm(Live_biomass ~ Cover_live + I(Cover_live^2) + Site 
                          , data = data)
summary(lm5_biomass_cover)

# compare models
anova(lm4_biomass_cover, lm5_biomass_cover) # no significant effect of Cover_live:Site 

# (- Site)
lm6_biomass_cover <- lm(Live_biomass ~ Cover_live + I(Cover_live^2)  
                        , data = data)
summary(lm6_biomass_cover)

# compare models
anova(lm5_biomass_cover, lm6_biomass_cover) # no significant effect of Site  

# (- I(Cover_live^2) )
lm7_biomass_cover <- lm(Live_biomass ~ Cover_live  
                        , data = data)
summary(lm7_biomass_cover)

# compare models
anova(lm6_biomass_cover, lm7_biomass_cover) # no significant effect of I(Cover_live^2)  

# (- Cover_live  )
lm8_biomass_cover <- lm(Live_biomass ~ 1 
                        , data = data)
summary(lm8_biomass_cover)

# compare models
anova(lm7_biomass_cover, lm8_biomass_cover) # significant effect of cover!!! 

#check model assumptions
library(DHARMa)

# Use DHARMa to simulate residuals and assess model diagnostics
residuals <- simulateResiduals(lm7_biomass_cover, plot = TRUE) #ok



################# final models (no quadratic term, to avoid over fitting)

lm1_biomass_cover <- lm(Live_biomass ~ Cover_live + Site + 
                          Cover_live:Site, data = data)
summary(lm1_biomass_cover)

# Use DHARMa to simulate residuals and assess model diagnostics
residuals <- simulateResiduals(lm1_biomass_cover, plot = TRUE) #ok

# remove interaction and compare to full model, - Cover_live:Site
lm2_biomass_cover <- lm(Live_biomass ~ Cover_live + Site
                          , data = data)
summary(lm2_biomass_cover) 
anova(lm1_biomass_cover, lm2_biomass_cover) # F = 0.31, p = 0.74

# remove site and compare to model withoput interaction term, - Site
lm3_biomass_cover <- lm(Live_biomass ~ Cover_live 
                        , data = data)
summary(lm3_biomass_cover) 
anova(lm2_biomass_cover, lm3_biomass_cover) # F = 1.02, p = 0.37

# remove cover and compare to model withoput interaction term, - Cover_live
lm4_biomass_cover <- lm(Live_biomass ~ Site
                        , data = data)
summary(lm4_biomass_cover) 
anova(lm2_biomass_cover, lm4_biomass_cover) # F = 49.33, p = < 0.001 ***



###############################################################################

#glm
data$Site <- factor(data$Site)
form.glmBIOMASS <- as.formula(Live_biomass ~ Cover_live + I(Cover_live^2) + Site)
glm.BIOMASS.full <- glm(form.glmBIOMASS, data = data)
summary(glm.BIOMASS.full)

# interaction terms
form.glmBIOMASS <- as.formula(Live_biomass ~ Cover_live + I(Cover_live^2) + Site + Cover_live:Site + I(Cover_live^2):Site)
glm.BIOMASS.full <- glm(form.glmBIOMASS, data = data)
summary(glm.BIOMASS.full)

###################################################################################
# 1. Create the scatter plot with linear regression lines for each site separately#
###################################################################################
# 1st plot just to have a look
plot <- ggplot(data, aes(x=Cover_live, y=Live_biomass, color=Site)) +
  geom_point(size=1.5) +
  geom_smooth(method='lm', se=TRUE, aes(fill=Site)) +
  labs(
    x="Cover live biomass (%)", 
    y="Live biomass (g)") +
  theme_minimal() +
  theme(legend.position="bottom") +
  scale_color_brewer(palette="Set1") +
  scale_fill_brewer(palette="Pastel1") +
  coord_cartesian(xlim=c(0, max(data$Cover_live)), ylim=c(0, max(data$Live_biomass)))  # Set axis limits

print(plot)

# Calculate R^2 and p-values for each site
site_levels <- unique(data$Site)
site_labels <- c()


for (site in site_levels) {
  site_data <- subset(data, Site == site)
  lm_model <- lm(Live_biomass ~ Cover_live, data=site_data)
  summary_model <- summary(lm_model)
  r2 <- summary_model$r.squared
  
  # Check if the p-value for the slope (Cover_live) exists
  if (ncol(summary_model$coefficients) >= 4 && nrow(summary_model$coefficients) >= 2) {
    p_value <- summary_model$coefficients[2, 4]
    label <- sprintf("%s, R² = %.2f, p = %.4f", site, r2, p_value)
  } else {
    # Handle cases where the p-value cannot be computed
    label <- sprintf("%s, R² = %.2f, p-value not available", site, r2)
  }
  
  site_labels <- append(site_labels, label)
}


##########################################################################################

# Create a new factor variable for site labels
data$Site_label <- factor(data$Site, labels=site_labels)

# Create the scatter plot with linear regression lines for each site
plot <- ggplot(data, aes(x=Cover_live, y=Live_biomass, color=Site_label)) +
  geom_point(size=1.5) +
  geom_smooth(method='lm', se=TRUE, aes(fill=Site_label)) +  # Removed confidence intervals (se=FALSE)
  labs(
    x="Cover live biomass (%)", 
    y="Live biomass (g)") +
  theme_minimal() +
  theme(legend.position="bottom") +
  scale_color_brewer(name="Site", palette="Set1") +
  scale_fill_brewer(name="Site", palette="Pastel1") +
  coord_cartesian(xlim=c(0, max(data$Cover_live)), ylim=c(0, NA)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), text = element_text(size=18))

print(plot)

#######################################
# all sites with low p-value combined##
#######################################
data <- read.csv("C:/Users/Vitalii/Desktop/Arctic_century_analysis/biomass_cover_reformated.csv")
# Filter to include only the sites with drone data
filtered_data <- subset(data, Site %in% c("Vize", "Pioneer", "Uedinieniya", "OctRevCentre"))

# Combine "Vize", "Pioneer", "Uedinieniya" into one group 
filtered_data$Site_combined <- ifelse(filtered_data$Site %in% c("Vize", "Pioneer", "Uedinieniya"), "Vize, Pioneer and Uedinieniya combined", "October Revolution inland")

# Calculate R^2 and p-values
combined_data <- subset(filtered_data, Site_combined == "Vize, Pioneer and Uedinieniya combined")
octRevCentre_data <- subset(filtered_data, Site_combined == "October Revolution inland")

combined_model <- lm(Live_biomass ~ Cover_live, data=combined_data)
octRevCentre_model <- lm(Live_biomass ~ Cover_live, data=octRevCentre_data)

combined_r2 <- summary(combined_model)$r.squared
combined_p_value <- summary(combined_model)$coefficients[2,4]

octRevCentre_r2 <- summary(octRevCentre_model)$r.squared
octRevCentre_p_value <- summary(octRevCentre_model)$coefficients[2,4]

# Create labels for the legend
combined_label <- sprintf("Vize, Pioneer and Uedinieniya combined\nR^2 = %.2f, p = %.6f", combined_r2, combined_p_value)
octRevCentre_label <- sprintf("October Revolution inland\nR^2 = %.2f, p = %.4f", octRevCentre_r2, octRevCentre_p_value)

# Customize colors and transparency
trend_line_color_combined <- "darkblue"  
trend_line_color_octRevCentre <- "#d62728"  
confidence_interval_all <- 0.07 
confidence_interval_oct <- 0.01  
legend_color_combined <- "darkblue"
legend_color_octRevCentre <- "#d62728"

# plot
plot <- ggplot(filtered_data, aes(x=Cover_live, y=Live_biomass)) +
  geom_point(aes(color=Site_combined), size=1.5) +
  geom_smooth(data=combined_data, aes(fill=combined_label, color=combined_label), method='lm', se=TRUE, 
              color=trend_line_color_combined, fill=trend_line_color_combined, alpha=confidence_interval_all) +
  geom_smooth(data=octRevCentre_data, aes(fill=octRevCentre_label, color=octRevCentre_label), method='lm', linetype = "dotted", se=TRUE, 
              color=trend_line_color_octRevCentre, fill=trend_line_color_octRevCentre, alpha=confidence_interval_oct) +
  scale_color_manual(values=c("Vize, Pioneer and Uedinieniya combined"=legend_color_combined, "October Revolution inland"=legend_color_octRevCentre), 
                     labels = c(octRevCentre_label, combined_label)) +
  scale_fill_manual(values=c(legend_color_octRevCentre, legend_color_combined), 
                    labels = c(octRevCentre_label, combined_label)) +
  labs(x="Cover live biomass (%)", y="Live biomass (g)", color=NULL, fill=NULL) +
  theme_minimal() + 
  theme(legend.position="bottom", panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  coord_cartesian(xlim=c(0, max(filtered_data$Cover_live)), ylim=c(0, max(filtered_data$Live_biomass)))

print(plot)


# Save the models
save(combined_model, file="combined_model.RData")
save(octRevCentre_model, file="octRevCentre_model.RData")



