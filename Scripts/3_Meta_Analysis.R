################################################################################
# North Carolina State University. Mali Lab 
# Spotted Turtle (Clemmys guttata) Home Range Liturature Review
# Abigale Conklin, Eliza Foli, Will Harrod, Courtney Grubbs, Alex Acker, 
# Charlette Whorton, & Ivana Malli 
# Script 4 of 4: Meta Analysis
################################################################################

################################################################################
# 1: Add data and packages #####################################################
################################################################################

# 1.1: Clear environments ------------------------------------------------------
rm(list = ls())

# 1.2: Add Packages ------------------------------------------------------------

# Install packages (comment back in if you haven't installed these before)
# install.packages(c("tidyverse", "meta", "AICcmodavg", "flextable")) 

# Load packages
library(tidyverse)
library(metafor)
library(flextable)
library(broom)

# Add the data
hr <- read.csv("Data\\C_guttata_ MetaReg_Data.csv")

# View
glimpse(hr)

# 1.3: Clean Data  ------------------------------------------------------------

# View a histogram of the SR area sizes
hr %>% 
  ggplot() + 
  geom_histogram(aes(x = SR.Area)) +
  theme_classic()

# View a histogram of the tracking frequencies
hr %>% 
  ggplot() + 
  geom_histogram(aes(x = Track.Freq)) +
  theme_classic()

# Pivot to a longer format
hr_lng <- hr%>% 
  pivot_longer(
    cols = contains(c("mean", "sd", "SampSize")), 
    names_to = c(".value", "Sex"), 
    names_pattern = "(.*)\\.(.*)") 

# View
glimpse(hr_lng)

# View distinct Ecoregions
hr_lng %>% 
  count(EcoReg.II)

# View distinct Home Range Estimators
hr_lng %>% 
  count(HR.Estimator)

# Final data cleaning
hr_final <- hr_lng %>% 
  # remove rows that are missing a mean value
  filter(!is.na(mean)) %>% 
  # Convert to factors, log-transform area, and scale numeric covariates
  mutate(ln.SR.Area = log(SR.Area)) %>% 
  mutate(ln.SR.Area.scl = scale(ln.SR.Area)[,1]) %>% 
  mutate(Track.Freq.scl = scale(Track.Freq)[,1]) %>% 
  mutate(Year.scl = scale(Year.Start)[,1]) %>% 
  mutate(across(.cols = c("Paper", "Treatment"), ~ as.factor(.x))) %>% 
  # Impute missing covariate values using the mean
  mutate(across(c(ln.SR.Area.scl, Track.Freq.scl), ~replace_na(.x, 0))) %>%
  # Impute missing sd values using the mean (sd is generally equal to mean in the papers we read)
  mutate(sd = case_when(is.na(sd) ~ mean, TRUE ~ sd)) %>% 
  # Calculate sampling varience. Formula: (SD / Mean)^2 / n
  mutate(vi = (sd / exp(mean))^2 / SampSize) %>%
  # rename response variable
  rename(mean.HR = mean) %>% 
  # Name categorical variables  
  mutate(Sex = case_when(Sex == "C" ~ "Combined",
                         Sex == "F" ~ "Female",
                         Sex == "M" ~ "Male",
                         TRUE ~ NA),
         EcoReg.II = case_when(EcoReg.II == "5_2" ~ "5.2 Mixed Woood Shield",
                               EcoReg.II == "5_3" ~ "5.3 Atlantic Highlands",
                               EcoReg.II == "8_1" ~ "8.1 Mixed Wood Plain",
                               EcoReg.II == "8_2" ~ "8.2 Central USA Plains",
                               EcoReg.II == "8_3" ~ "8.3 SE USA Plains",
                               EcoReg.II == "8_4" ~ "8.4 Ozarks",
                               EcoReg.II == "8_5" ~ "8.5 SE Costal Plain",
                               TRUE ~ NA),
         HR.Estimator = case_when(HR.Estimator == "kde.095" ~ "95% KDE",
                                  HR.Estimator == "mcp.095" ~ "95% MCP",
                                  HR.Estimator == "mcp.100" ~ "100% MCP",
                                  HR.Estimator == "tlocoh" ~ "T-LoCoH",
                                  TRUE ~ NA)) %>% 
  # Convert them to factors
  mutate(Sex = factor(Sex, levels = c("Combined", "Female", "Male")),
         EcoReg.II = factor(EcoReg.II, levels = c("5.2 Mixed Woood Shield", 
                                                  "5.3 Atlantic Highlands", 
                                                  "8.1 Mixed Wood Plain", 
                                                  "8.2 Central USA Plains", 
                                                  "8.3 SE USA Plains", 
                                                  "8.4 Ozarks", 
                                                  "8.5 SE Costal Plain")),
         HR.Estimator = factor(HR.Estimator, levels = c("95% KDE", "95% MCP", "100% MCP", "T-LoCoH")))

# View
glimpse(hr_final)

# View NA's (shouldn't be any)
hr_final %>% 
  select(Paper, Sex, mean.HR, vi, SampSize, EcoReg.II, HR.Estimator, ln.SR.Area.scl, Track.Freq.scl) %>% 
  filter(is.na(mean.HR) | 
           is.na(vi) | 
           is.na(SampSize) | 
           is.na(EcoReg.II) | 
           is.na(HR.Estimator) | 
           is.na(ln.SR.Area.scl) |
           is.na(Track.Freq.scl) )

# View the number of observations by paper and sex
hr_final %>% 
  count(Paper, Sex) %>% 
  print(n = Inf)


################################################################################
# 2: Explore Data ##############################################################
################################################################################

# 2.1: prep --------------------------------------------------------------------

# Define a color pallete (can you guess the theme?)
clemmys_pal <- c("#F7E57C", "#C0361C", "#422E28")

# 2.2: Exploratory Plots ------------------------------------------------------- 

# HR by male and female
hr_final %>% 
  ggplot() +
  geom_boxplot(aes(x = Sex, y = mean.HR, col = Sex, fill = Sex)) +
  scale_color_manual(values = clemmys_pal) +
  scale_fill_manual(values = clemmys_pal) +
  theme_classic() +
  labs(y = "Average Home Range Size") +
  theme(
    axis.title.x = element_blank(),
    plot.title = element_text(size = 22, family = "Open Sans"),
    axis.title.y = element_text(size = 18, family = "Open Sans"),
    axis.text = element_text(size = 18, family = "Open Sans"),
    legend.text = element_text(size = 18, family = "Open Sans"),
    legend.position = "none",
    legend.title = element_blank()
  )

# HR by Ecoregion and sex
hr_final %>% 
  ggplot() +
  geom_boxplot(aes(x = EcoReg.II, y = mean.HR, col = Sex, fill = Sex)) +
  scale_color_manual(values = clemmys_pal) +
  scale_fill_manual(values = clemmys_pal) +
  theme_classic()

# HR by Study area 
hr_final %>% 
  ggplot() +
  geom_smooth(aes(x = ln.SR.Area, y = mean.HR, col = Sex,  fill = Sex), alpha = 0.3, method = "lm") +
  scale_color_manual(values = clemmys_pal) +
  scale_fill_manual(values = clemmys_pal) +
  theme_classic() +
  labs(y = "Average Home Range Size (ha)",
       x = "log of Study Area Size (ha)") +
  theme(
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    plot.title = element_text(size = 22, family = "Open Sans"),
    axis.text = element_text(size = 18, family = "Open Sans"),
    legend.text = element_text(size = 18, family = "Open Sans"),
    legend.position = "right",
    legend.title = element_blank()
  )

# HR by Year
hr_final %>% 
  ggplot() +
  geom_smooth(aes(x = Year.scl, y = mean.HR, col = Sex,  fill = Sex), alpha = 0.3, method = "lm") +
  scale_color_manual(values = clemmys_pal) +
  scale_fill_manual(values = clemmys_pal) +
  theme_classic()

# HR by Tracking frequency
hr_final %>% 
  ggplot() +
  geom_smooth(aes(x = Track.Freq.scl, y = mean.HR, col = Sex,  fill = Sex), alpha = 0.3, method = "lm") +
  scale_color_manual(values = clemmys_pal) +
  scale_fill_manual(values = clemmys_pal) +
  theme_classic()

################################################################################
# 3: Meta Regression ###########################################################
################################################################################

# 3.1: Prep --------------------------------------------------------------------

# View the data again
glimpse(hr_final)

# Calculate the effect size (yi) and sampling variance (vi)
dat_eff <- metafor::escalc(measure = "MN", 
                           mi = mean.HR, 
                           sdi = sd, 
                           ni = SampSize, 
                           data = hr_final)

# View the new columns: 'yi' (effect size) and 'vi' (variance)
slice_head(dat_eff, n = 30)

# 3.2: Candidate models --------------------------------------------------------

# null model
out_null <- metafor::rma(yi= mean.HR,
                         vi = vi, 
                         data = hr_final,
                         method = "REML") 
# View the results
sum_out_null <- summary(out_null)
sum_out_null

# Fit the meta-regression model with the full set of covariates
out1 <- metafor::rma(yi= mean.HR,
                    vi = vi, 
                    mods = ~ Sex + EcoReg.II + HR.Estimator + ln.SR.Area.scl + Track.Freq.scl, 
                    data = hr_final,
                    method = "REML") 

# View the results
sum_out1 <- summary(out1)
sum_out1

# Drop Tracking Frequency 
out2 <- metafor::rma(yi= mean.HR,
                      vi = vi, 
                      mods = ~ Sex + EcoReg.II + HR.Estimator + ln.SR.Area.scl, 
                      data = hr_final,
                      method = "REML") 

# View the results
sum_out2 <- summary(out2)
sum_out2

# Drop Sex
out3 <- metafor::rma(yi= mean.HR,
                     vi = vi, 
                     mods = ~ EcoReg.II + HR.Estimator + ln.SR.Area.scl, 
                     data = hr_final,
                     method = "REML") 

# View the results
sum_out3 <- summary(out3)
sum_out3

# Drop Ecoregion
out4 <- metafor::rma(yi= mean.HR,
                     vi = vi, 
                     mods = ~ HR.Estimator + ln.SR.Area.scl, 
                     data = hr_final,
                     method = "REML") 

# View the results
sum_out4 <- summary(out4)
sum_out4

# Drop HR estimator
out5 <- metafor::rma(yi= mean.HR,
                     vi = vi, 
                     mods = ~ ln.SR.Area.scl, 
                     data = hr_final,
                     method = "REML") 

# View the results
sum_out5 <- summary(out5)
sum_out5

# Add sex back in
out6 <- metafor::rma(yi= mean.HR,
                     vi = vi, 
                     mods = ~ Sex + ln.SR.Area.scl, 
                     data = hr_final,
                     method = "REML") 

# View the results
sum_out6 <- summary(out6)
sum_out6

# 3.3: Model comparisons--------------------------------------------------------

# List the parameter in each model
mod_params <- c(
  "Intercept Only (Null)",
  "Sex + EcoReg + HR.Est + ln.SR.Area + Track.Freq",
  "Sex + EcoReg + HR.Est + ln.SR.Area",
  "EcoReg + HR.Est + ln.SR.Area",
  "HR.Est + ln.SR.Area",
  "ln.SR.Area",
  "Sex + ln.SR.Area"
)

# Create a table of model AIC scores 
aic_table <- AIC(out_null, out1, out2, out3, out4, out5, out6) %>% 
  tibble() %>% 
  # Calculate delta AIC
  mutate(Dela.AIC = AIC - min(AIC)) %>% 
  # Calculate Parameters
  mutate(Parameters = mod_params) %>% 
  # Sort by AIC Scores
  arrange(AIC) %>% 
  # Change column order
  select(Parameters, AIC, Dela.AIC, df)
  
# View model AIC table 
aic_table

# Convert to a flextable
aic_flx_tbl <- aic_table %>% 
  flextable::flextable() %>% 
  flextable::width(j = "Parameters", width = 100, unit = "mm") 

# View the flextable
aic_flx_tbl

# 3.4: Save model outputs ------------------------------------------------------

# Save the AIC flextable
flextable::save_as_image(aic_flx_tbl, path = "Figures\\MetaReg_AIC_table.png")

# Define the "Best" model
best_mod <- out2

# Save the "best" model
save(best_mod, file = "Data\\top_MetaReg_mode.RData")

# View the "best" model as a flextable
mod_flx_tbl <- tidy(best_mod) %>% 
  mutate(across(where(is.numeric), ~ signif(.x, digits = 2))) %>% 
  mutate(p.value = signif(p.value, 2)) %>% 
  mutate(term = str_remove(term, "EcoReg.II")) %>% 
  mutate(term = str_remove(term, "HR.Estimator")) %>% 
  flextable()

# View the best model flextable
mod_flx_tbl

# Save the best model flextable
save(mod_flx_tbl, path = "Figures\\Best_Mod_table.png")

