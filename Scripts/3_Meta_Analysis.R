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

# Install packages: (comment back in if you haven't installed these before)
# install.packages(c("tidyverse", "meta", "AICcmodavg", "flextable")) 

# Load packages
library(tidyverse)
library(metafor)
library(flextable)
library(ggpubr)
library(gridExtra)
library(broom)
library(scales)

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
  # Create a new colun for coastal vs inland
  mutate(Coastal = case_when(EcoReg.II %in% c("8_1", "8_5") ~ "Coastal",
                            TRUE ~ "Inland")) %>% 
  mutate(Coastal = factor(Coastal, levels = c("Coastal", "Inland"))) %>% 
  # rename response variable
  rename(mean.HR = mean) %>% 
  # Name categorical variables  
  mutate(Sex = case_when(Sex == "C" ~ "Combined",
                         Sex == "F" ~ "Female",
                         Sex == "M" ~ "Male",
                         TRUE ~ NA),
         EcoReg.II = case_when(EcoReg.II == "5_2" ~ "5.2 Mixed Wood Shield",
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
                                  TRUE ~ NA)
         ) %>%
  # Convert them to factors
  mutate(Sex = factor(Sex, levels = c("Female", "Male", "Combined")),
         EcoReg.II = factor(EcoReg.II, levels = c("5.2 Mixed Wood Shield", 
                                                  "5.3 Atlantic Highlands", 
                                                  "8.1 Mixed Wood Plain", 
                                                  "8.2 Central USA Plains", 
                                                  "8.3 SE USA Plains", 
                                                  "8.4 Ozarks", 
                                                  "8.5 SE Costal Plain")),
         HR.Estimator = factor(HR.Estimator, levels = c("100% MCP", "95% MCP", "95% KDE", "T-LoCoH"))) %>% 
 # Try removing O'Bryan (2015)
  filter(Paper != "OBryan-et-al.-2015")
  
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
  geom_boxplot(aes(x = Sex, y = mean.HR, col = Sex)) +
  scale_color_manual(values = clemmys_pal) +
  scale_fill_manual(values = clemmys_pal) +
  theme_classic() +
  labs(y = "Average Home Range Size (ha)") +
  theme(
    axis.title.x = element_blank(),
    plot.title = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.text = element_text(size = 18),
    legend.position = "none",
    legend.title = element_blank()
  )

# HR by Ecoregion and sex
hr_final %>% 
  ggplot() +
  geom_boxplot(aes(x = Coastal, y = mean.HR, col = Sex, fill = Sex)) +
  scale_color_manual(values = clemmys_pal) +
  scale_fill_manual(values = clemmys_pal) +
  theme_classic() +
  labs(y = "Average Home Range Size (ha)") +
  theme(
    axis.title.x = element_blank(),
    plot.title = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.text = element_text(size = 18),
    legend.position = "right",
    legend.title = element_blank()
  )

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
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 18),
    plot.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.text = element_text(size = 18),
    legend.position = "right",
    legend.title = element_blank()
  )

# HR by Year
hr_final %>% 
  ggplot() +
  geom_smooth(aes(x = Year.Start, y = mean.HR, col = Sex,  fill = Sex), alpha = 0.3, method = "lm") +
  scale_color_manual(values = clemmys_pal) +
  scale_fill_manual(values = clemmys_pal) +
  theme_classic() +
  labs(y = "Average Home Range Size (ha)",
       x = "Year") +
  theme(
    axis.title.x = element_text(size = 16),
    plot.title = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.text = element_text(size = 18),
    legend.position = "none",
    legend.title = element_blank()
  )

# HR by Tracking frequency
hr_final %>% 
  ggplot() +
  geom_smooth(aes(x = Track.Freq, y = mean.HR, col = Sex,  fill = Sex), alpha = 0.3, method = "lm") +
  scale_color_manual(values = clemmys_pal) +
  scale_fill_manual(values = clemmys_pal) +
  theme_classic() +
  labs(y = "Average Home Range Size (ha)",
       x = "Tracking Frequency (days/month)") +
  theme(
    axis.title.x = element_text(size = 16),
    plot.title = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.text = element_text(size = 18),
    legend.position = "none",
    legend.title = element_blank()
  )

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
                    mods = ~ Sex + Coastal + HR.Estimator + ln.SR.Area.scl + Track.Freq.scl, 
                    data = hr_final,
                    method = "REML") 

# View the results
sum_out1 <- summary(out1)
sum_out1

# Drop Tracking Frequency 
out2 <- metafor::rma(yi= mean.HR,
                      vi = vi, 
                      mods = ~ Sex + Coastal + HR.Estimator + ln.SR.Area.scl, 
                      data = hr_final,
                      method = "REML") 

# View the results
sum_out2 <- summary(out2)
sum_out2

# Drop Sex
out3 <- metafor::rma(yi= mean.HR,
                     vi = vi, 
                     mods = ~ Coastal + HR.Estimator + ln.SR.Area.scl, 
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
  "Sex + Coastal + HR.Est + ln(study area) + Track.Freq",
  "Sex + Coastal + HR.Est + ln(study area)",
  "Coastal + HR.Est + ln(study area)",
  "HR.Est + ln(study area)",
  "ln(study area)",
  "Sex + ln(study area)"
)

# Create a table of model AIC scores 
aic_table <- AIC(out_null, out1, out2, out3, out4, out5, out6) %>% 
  tibble() %>% 
  # Calculate delta AIC
  mutate(Delta.AIC = AIC - min(AIC)) %>% 
  # Calculate Parameters
  mutate(Parameters = mod_params) %>% 
  # Sort by AIC Scores
  arrange(AIC) %>% 
  # Change column order
  select(Parameters, AIC, Delta.AIC, df) %>% 
  # Round
  mutate(across(.cols = c("AIC", "Delta.AIC"), .fns = ~ round(.x, 2)))
  
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
mod_flx_tbl <- tibble(Parameter = tidy(best_mod)$term,
                      Estimate = best_mod$beta[,1],
                      std.error = best_mod$se,
                      z.value = best_mod$zval,
                      p.value = best_mod$pval
                      ) %>% 
  # Round
  mutate(across(where(is.numeric), ~ round(.x, digits = 2))) %>% 
  # Format as p-values
  mutate(p.value = pvalue(p.value, accuracy = 0.01)) %>%
  # Significance level
  mutate(Signif = case_when(p.value <= 0.01 ~ "***",
                            p.value > 0.01 & p.value <= 0.05 ~ "**",
                            p.value > 0.05 & p.value <= 0.15 ~ "*",
                            TRUE ~ NA)) %>% 
  # Clear excess text
  mutate(Parameter = str_remove(Parameter, "Coastal")) %>% 
  mutate(Parameter = str_remove(Parameter, "HR.Estimator")) %>% 
  mutate(Parameter = str_replace(Parameter, "Sex", "Sex ")) %>% 
  mutate(Parameter = str_replace(Parameter, "ln.SR.Area.scl", "ln(study area size)")) %>% 
  flextable() %>% 
  # Change width
  flextable::width(j = "Parameter", width = 40, unit = "mm") 

# View the best model flextable
mod_flx_tbl

# Save the best model flextable
flextable::save_as_image(mod_flx_tbl, path = "Figures\\Best_Mod_table.png")

################################################################################
# 4: Data Visualization ########################################################
################################################################################

# 4.1: prepare model output ----------------------------------------------------

# Define the length of the scaled output
lgth_sim <- 1000

# calculate the mean and sd of study area size
mu_sa_size <- mean(hr_final$ln.SR.Area, na.rm = TRUE)
sd_sa_size <- sd(hr_final$ln.SR.Area, na.rm = TRUE)

# View these
mu_sa_size
sd_sa_size

# Build a tibble of the best model's output
best_mod_long <- tibble(
  Parameter = tidy(best_mod)$term,
  mean = best_mod$beta[,1],
  lb = best_mod$ci.lb,
  ub = best_mod$ci.ub
) %>%  
  # Round
  mutate(across(where(is.numeric), ~ round(.x, digits = 2))) %>% 
  # Clear excess text
  mutate(Parameter = str_remove(Parameter, "Coastal")) %>% 
  mutate(Parameter = str_remove(Parameter, "HR.Estimator")) %>% 
  mutate(Parameter = str_remove(Parameter, "%")) %>% 
  mutate(Parameter = str_replace(Parameter, "Sex", "Sex ")) %>%
  mutate(Parameter = str_replace_all(Parameter, " ", ".")) %>%
  mutate(Parameter = str_replace_all(Parameter, "_", "."))

# Widen
best_mod_tbl <- best_mod_long %>% 
  pivot_wider(names_from = Parameter, values_from = c("mean", "lb", "ub"), names_sep = ".") %>% 
  # Repeat
  slice(rep(1:n(), each = lgth_sim)) %>% 
  # Add scaled values 
  mutate(x = seq(from = -2, to = 2, length.out = lgth_sim)) %>% 
  # Unscale study area size
  mutate(x.naive = x * sd_sa_size + mu_sa_size)

# View
glimpse(best_mod_tbl)
best_mod_tbl

# 4.2: Make the initial plots --------------------------------------------------

# View the data again
glimpse(best_mod_tbl)

# 100% MCP coastal
mcp_100_coast_plot <- best_mod_tbl %>% 
  ggplot() +
  # Trend line and error ribbon for coastal females
  geom_line(aes(x = exp(x.naive), 
                y = mean.intercept + mean.ln.SR.Area.scl * x), 
            color = "#C0361C", linetype = "solid", lwd = 1.6, alpha = 0.8) +
  geom_ribbon(aes(x = exp(x.naive), 
                  ymin = lb.intercept + lb.ln.SR.Area.scl * x,
                  ymax = ub.intercept + ub.ln.SR.Area.scl * x),
              fill = "#C0361C", alpha = 0.3) +
  # Trend line and error ribbon for coastal males
  geom_line(aes(x = exp(x.naive), 
                y = mean.intercept + mean.Sex.Male + mean.ln.SR.Area.scl * x), 
            color = "#422E28", linetype = "solid", lwd = 1.6, alpha = 0.7) +
  geom_ribbon(aes(x = exp(x.naive), 
                  ymin = lb.intercept + lb.Sex.Male + lb.ln.SR.Area.scl * x,
                  ymax = ub.intercept + ub.Sex.Male + ub.ln.SR.Area.scl * x),
              fill = "#422E28", alpha = 0.2) +
  # Trend line and error ribbon for coastal combined
  geom_line(aes(x = exp(x.naive), 
                y = mean.intercept + mean.Sex.Combined + mean.ln.SR.Area.scl * x),
            color = "#F7E57C", linetype = "solid", lwd = 1.6, alpha = 0.6) +
  geom_ribbon(aes(x = exp(x.naive), 
                  ymin = lb.intercept + lb.Sex.Combined + lb.ln.SR.Area.scl * x,
                  ymax = ub.intercept + ub.Sex.Combined + ub.ln.SR.Area.scl * x),
              fill = "#F7E57C", alpha = 0.1) +
  # Labels
  labs(
    title = "100% MCP: Costal",
       x = "",
       y = "Home Range Size"
       ) +
  scale_y_continuous(limits = c(-8, 21), breaks = c(0, 10, 20),  labels = unit_format(unit = "ha", sep = "")) +
  scale_x_continuous(labels = unit_format(unit = "ha", sep = "", big.mark = "")) +
  # Themes
  theme_classic() +
  theme(
    plot.title = element_text(size = 18),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.text = element_text(size = 18),
    legend.position = "none",
    legend.title = element_blank()
  )
# View
mcp_100_coast_plot

# 100% MCP inland
mcp_100_land_plot <- best_mod_tbl %>% 
  ggplot() +
  # Trend line and error ribbon for coastal females
  geom_line(aes(x = exp(x.naive), 
                y = mean.intercept + mean.Inland + mean.ln.SR.Area.scl * x), 
            color = "#C0361C", linetype = "solid", lwd = 1.6, alpha = 0.8) +
  geom_ribbon(aes(x = exp(x.naive), 
                  ymin = lb.intercept + lb.Inland + lb.ln.SR.Area.scl * x,
                  ymax = ub.intercept + ub.Inland + ub.ln.SR.Area.scl * x),
              fill = "#C0361C", alpha = 0.3) +
  # Trend line and error ribbon for coastal males
  geom_line(aes(x = exp(x.naive), 
                y = mean.intercept + mean.Sex.Male + mean.Inland + mean.ln.SR.Area.scl * x), 
            color = "#422E28", linetype = "solid", lwd = 1.6, alpha = 0.7) +
  geom_ribbon(aes(x = exp(x.naive), 
                  ymin = lb.intercept + lb.Sex.Male + lb.Inland + lb.ln.SR.Area.scl * x,
                  ymax = ub.intercept + ub.Sex.Male + ub.Inland + ub.ln.SR.Area.scl * x),
              fill = "#422E28", alpha = 0.2) +
  # Trend line and error ribbon for coastal combined
  geom_line(aes(x = exp(x.naive), 
                y = mean.intercept + mean.Sex.Combined + mean.Inland + mean.ln.SR.Area.scl * x),
            color = "#F7E57C", linetype = "solid", lwd = 1.6, alpha = 0.6) +
  geom_ribbon(aes(x = exp(x.naive), 
                  ymin = lb.intercept + lb.Sex.Combined + lb.Inland + lb.ln.SR.Area.scl * x,
                  ymax = ub.intercept + ub.Sex.Combined + ub.Inland + ub.ln.SR.Area.scl * x),
              fill = "#F7E57C", alpha = 0.1) +
  # Labels
  labs(
    title = "100% MCP: Inland",
    x = "",
    y = ""
  ) +
  scale_y_continuous(limits = c(-8, 21), breaks = c(0, 10, 20), labels = unit_format(unit = "ha", sep = "")) +
  scale_x_continuous(labels = unit_format(unit = "ha", sep = "", big.mark = "")) +
  # Themes
  theme_classic() +
  theme(
    plot.title = element_text(size = 18),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.text = element_text(size = 18),
    legend.position = "none",
    legend.title = element_blank()
  )
# View
mcp_100_land_plot

# 95% MCP coastal
mcp_095_coast_plot <- best_mod_tbl %>% 
  ggplot() +
  # Trend line and error ribbon for coastal females
  geom_line(aes(x = exp(x.naive), 
                y = mean.intercept + mean.95.MCP + mean.ln.SR.Area.scl * x), 
            color = "#C0361C", linetype = "solid", lwd = 1.6, alpha = 0.8) +
  geom_ribbon(aes(x = exp(x.naive), 
                  ymin = lb.intercept + lb.95.MCP + lb.ln.SR.Area.scl * x,
                  ymax = ub.intercept + ub.95.MCP + ub.ln.SR.Area.scl * x),
              fill = "#C0361C", alpha = 0.3) +
  # Trend line and error ribbon for coastal males
  geom_line(aes(x = exp(x.naive), 
                y = mean.intercept + mean.95.MCP + mean.Sex.Male + mean.ln.SR.Area.scl * x), 
            color = "#422E28", linetype = "solid", lwd = 1.6, alpha = 0.7) +
  geom_ribbon(aes(x = exp(x.naive), 
                  ymin = lb.intercept + lb.95.MCP + lb.Sex.Male + lb.ln.SR.Area.scl * x,
                  ymax = ub.intercept + ub.95.MCP+ ub.Sex.Male + ub.ln.SR.Area.scl * x),
              fill = "#422E28", alpha = 0.2) +
  # Trend line and error ribbon for coastal combined
  geom_line(aes(x = exp(x.naive), 
                y = mean.intercept + mean.95.MCP + mean.Sex.Combined + mean.ln.SR.Area.scl * x),
            color = "#F7E57C", linetype = "solid", lwd = 1.6, alpha = 0.6) +
  geom_ribbon(aes(x = exp(x.naive), 
                  ymin = lb.intercept + lb.95.MCP + lb.Sex.Combined + lb.ln.SR.Area.scl * x,
                  ymax = ub.intercept + ub.95.MCP + ub.Sex.Combined + ub.ln.SR.Area.scl * x),
              fill = "#F7E57C", alpha = 0.1) +
  # Labels
  labs(
    title = "95% MCP: Costal",
    x = "",
    y = "Home Range Size"
  ) +
  scale_y_continuous(limits = c(-8, 21), breaks = c(0, 10, 20), labels = unit_format(unit = "ha", sep = "")) +
  scale_x_continuous(labels = unit_format(unit = "ha", sep = "", big.mark = "")) +
  # Themes
  theme_classic() +
  theme(
    plot.title = element_text(size = 18),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.text = element_text(size = 18),
    legend.position = "none",
    legend.title = element_blank()
  )
# View
mcp_095_coast_plot

# 95% MCP inland
mcp_095_land_plot <- best_mod_tbl %>% 
  ggplot() +
  # Trend line and error ribbon for coastal females
  geom_line(aes(x = exp(x.naive), 
                y = mean.intercept + mean.95.MCP + mean.Inland + mean.ln.SR.Area.scl * x), 
            color = "#C0361C", linetype = "solid", lwd = 1.6, alpha = 0.8) +
  geom_ribbon(aes(x = exp(x.naive), 
                  ymin = lb.intercept + lb.95.MCP + lb.Inland + lb.ln.SR.Area.scl * x,
                  ymax = ub.intercept + ub.95.MCP + ub.Inland + ub.ln.SR.Area.scl * x),
              fill = "#C0361C", alpha = 0.3) +
  # Trend line and error ribbon for coastal males
  geom_line(aes(x = exp(x.naive), 
                y = mean.intercept + mean.95.MCP + mean.Sex.Male + mean.Inland + mean.ln.SR.Area.scl * x), 
            color = "#422E28", linetype = "solid", lwd = 1.6, alpha = 0.7) +
  geom_ribbon(aes(x = exp(x.naive), 
                  ymin = lb.intercept + lb.95.MCP + lb.Sex.Male + lb.Inland + lb.ln.SR.Area.scl * x,
                  ymax = ub.intercept + ub.95.MCP + ub.Sex.Male + ub.Inland + ub.ln.SR.Area.scl * x),
              fill = "#422E28", alpha = 0.2) +
  # Trend line and error ribbon for coastal combined
  geom_line(aes(x = exp(x.naive), 
                y = mean.intercept + mean.95.MCP + mean.Sex.Combined + mean.Inland + mean.ln.SR.Area.scl * x),
            color = "#F7E57C", linetype = "solid", lwd = 1.6, alpha = 0.6) +
  geom_ribbon(aes(x = exp(x.naive), 
                  ymin = lb.intercept + lb.95.MCP + lb.Sex.Combined + lb.Inland + lb.ln.SR.Area.scl * x,
                  ymax = ub.intercept + ub.95.MCP + ub.Sex.Combined + ub.Inland + ub.ln.SR.Area.scl * x),
              fill = "#F7E57C", alpha = 0.1) +
  # Labels
  labs(
    title = "95% MCP: Inland",
    x = "",
    y = ""
  ) +
  scale_y_continuous(limits = c(-8, 21), breaks = c(0, 10, 20), labels = unit_format(unit = "ha", sep = "")) +
  scale_x_continuous(labels = unit_format(unit = "ha", sep = "", big.mark = "")) +
  # Themes
  theme_classic() +
  theme(
    plot.title = element_text(size = 18),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.text = element_text(size = 18),
    legend.position = "none",
    legend.title = element_blank()
  )
# View
mcp_095_land_plot

# 95% KDE coastal
kde_095_coast_plot <- best_mod_tbl %>% 
  ggplot() +
  # Trend line and error ribbon for coastal females
  geom_line(aes(x = exp(x.naive), 
                y = mean.intercept + mean.95.KDE + mean.ln.SR.Area.scl * x), 
            color = "#C0361C", linetype = "solid", lwd = 1.6, alpha = 0.8) +
  geom_ribbon(aes(x = exp(x.naive), 
                  ymin = lb.intercept + lb.95.KDE + lb.ln.SR.Area.scl * x,
                  ymax = ub.intercept + ub.95.KDE + ub.ln.SR.Area.scl * x),
              fill = "#C0361C", alpha = 0.3) +
  # Trend line and error ribbon for coastal males
  geom_line(aes(x = exp(x.naive), 
                y = mean.intercept + mean.95.KDE + mean.Sex.Male + mean.ln.SR.Area.scl * x), 
            color = "#422E28", linetype = "solid", lwd = 1.6, alpha = 0.7) +
  geom_ribbon(aes(x = exp(x.naive), 
                  ymin = lb.intercept + lb.95.KDE + lb.Sex.Male + lb.ln.SR.Area.scl * x,
                  ymax = ub.intercept + ub.95.KDE+ ub.Sex.Male + ub.ln.SR.Area.scl * x),
              fill = "#422E28", alpha = 0.2) +
  # Trend line and error ribbon for coastal combined
  geom_line(aes(x = exp(x.naive), 
                y = mean.intercept + mean.95.KDE + mean.Sex.Combined + mean.ln.SR.Area.scl * x),
            color = "#F7E57C", linetype = "solid", lwd = 1.6, alpha = 0.6) +
  geom_ribbon(aes(x = exp(x.naive), 
                  ymin = lb.intercept + lb.95.KDE + lb.Sex.Combined + lb.ln.SR.Area.scl * x,
                  ymax = ub.intercept + ub.95.KDE + ub.Sex.Combined + ub.ln.SR.Area.scl * x),
              fill = "#F7E57C", alpha = 0.1) +
  # Lables 
  labs(
       x = "Study Region Size", 
       y = "Home Range Size",
       title = "95% KDE: Costal") +
  scale_y_continuous(limits = c(-8, 21), breaks = c(0, 10, 20), labels = unit_format(unit = "ha", sep = "")) +
  scale_x_continuous(labels = unit_format(unit = "ha", sep = "", big.mark = "")) +
  # Themes
  theme_classic() +
  theme(
    plot.title = element_text(size = 18),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.text = element_text(size = 18),
    legend.position = "none",
    legend.title = element_blank()
  )
# View
kde_095_coast_plot

# 95% KDE inland
kde_095_land_plot <- best_mod_tbl %>% 
  ggplot() +
  # Trend line and error ribbon for coastal females
  geom_line(aes(x = exp(x.naive), 
                y = mean.intercept + mean.95.KDE + mean.Inland + mean.ln.SR.Area.scl * x), 
            color = "#C0361C", linetype = "solid", lwd = 1.6, alpha = 0.8) +
  geom_ribbon(aes(x = exp(x.naive), 
                  ymin = lb.intercept + lb.95.KDE + lb.Inland + lb.ln.SR.Area.scl * x,
                  ymax = ub.intercept + ub.95.KDE + ub.Inland + ub.ln.SR.Area.scl * x),
              fill = "#C0361C", alpha = 0.3) +
  # Trend line and error ribbon for coastal males
  geom_line(aes(x = exp(x.naive), 
                y = mean.intercept + mean.95.KDE + mean.Sex.Male + mean.Inland + mean.ln.SR.Area.scl * x), 
            color = "#422E28", linetype = "solid", lwd = 1.6, alpha = 0.7) +
  geom_ribbon(aes(x = exp(x.naive), 
                  ymin = lb.intercept + lb.95.KDE + lb.Sex.Male + lb.Inland + lb.ln.SR.Area.scl * x,
                  ymax = ub.intercept + ub.95.KDE + ub.Sex.Male + ub.Inland + ub.ln.SR.Area.scl * x),
              fill = "#422E28", alpha = 0.2) +
  # Trend line and error ribbon for coastal combined
  geom_line(aes(x = exp(x.naive), 
                y = mean.intercept + mean.95.KDE + mean.Sex.Combined + mean.Inland + mean.ln.SR.Area.scl * x),
            color = "#F7E57C", linetype = "solid", lwd = 1.6, alpha = 0.6) +
  geom_ribbon(aes(x = exp(x.naive), 
                  ymin = lb.intercept + lb.95.KDE + lb.Sex.Combined + lb.Inland + lb.ln.SR.Area.scl * x,
                  ymax = ub.intercept + ub.95.KDE + ub.Sex.Combined + ub.Inland + ub.ln.SR.Area.scl * x),
              fill = "#F7E57C", alpha = 0.1) +
  # Lables 
  labs(x = "Study Region Size", 
       y = "",
       title = "95% KDE: Inland") +
  scale_y_continuous(limits = c(-8, 21), breaks = c(0, 10, 20), labels = unit_format(unit = "ha", sep = "")) +
  scale_x_continuous(labels = unit_format(unit = "ha", sep = "", big.mark = "")) +
  # Themes
  theme_classic() +
  theme(
    plot.title = element_text(size = 18),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.text = element_text(size = 18),
    legend.position = "none",
    legend.title = element_blank()
  )
# View
kde_095_land_plot

# 4.3: Make the legend ---------------------------------------------------------

# Define a color pallete (can you guess the theme?)
clemmys_pal <- c("#C0361C", "#422E28", "#F7E57C")

# legend data
legend_data  <- best_mod_long %>% 
  filter(Parameter %in% c("intercept", "Sex.Male", "Sex.Combined")) %>% 
  mutate(Parameter = case_when(Parameter == "intercept" ~ "Female",
                                Parameter == "Sex.Male" ~ "Male",
                                Parameter == "Sex.Combined" ~ "Combined")) %>% 
  mutate(Parameter = factor(Parameter, levels = c("Female", "Male", "Combined"))) %>% 
  mutate(x = 1:3)
         
# View
legend_data

# legend plot
legend_plot <- legend_data %>% 
  # Plot
  ggplot() +
  geom_line(aes(x = x, y = mean, color = Parameter),
            linetype = "solid", lwd = 1.6, alpha = 0.6) +
  geom_ribbon(aes(x = x, ymin = lb, ymax = ub, fill = Parameter),
              alpha = 0.15) +
  scale_color_manual(values = clemmys_pal) +
  scale_fill_manual(values = clemmys_pal) +
  scale_y_continuous(limits = c(-8, 21), breaks = c(0, 10, 20), labels = unit_format(unit = "ha", sep = "")) +
  # Themes
  theme_classic() +
  theme(
    plot.title = element_text(size = 18),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.text = element_text(size = 18),
    legend.position = "bottom",
    legend.key.size = unit(1, "cm"),
    legend.title = element_blank()
  )

# View
legend_plot

# Extract the legend 
legend <- get_legend(legend_plot)
  
# 4.4: Combine plots -----------------------------------------------------------

# Combine plots
plots_grid <- grid.arrange(mcp_095_coast_plot, mcp_095_land_plot,
                           mcp_100_coast_plot, mcp_100_land_plot,
                           kde_095_coast_plot, kde_095_land_plot,
                           nrow = 3, ncol = 2)

# View combine plots
plots_grid

# Add legend
plots_legend <- grid.arrange(plots_grid, legend, 
                             nrow = 2, ncol = 1,
                             heights = c(0.9, 0.1))

# View 
plots_legend

# 4.5: Export ------------------------------------------------------------------

# Save the plot as a png
ggsave(plot = plots_legend,
       filename = "Figures\\Clemmys_Model_Pred_Plot.png",
       width = 250,
       height = 300,
       units = "mm",
       dpi = 300)
