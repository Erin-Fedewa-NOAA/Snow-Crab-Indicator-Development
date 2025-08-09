


source("./scripts/get_crab_data.R")

#write csv for male 50% size at maturity
mat_size %>%

## Plot male size at terminal molt
ggplot(data = mat_size %>% 
         filter(DISTRICT == "ALL", !is.na(STD_ERR)) %>%
         right_join(., expand.grid(YEAR = years)),
       aes(x = YEAR, y = MAT_SIZE)) +
  geom_point() +
  geom_line() +
  geom_hline(aes(yintercept = mean(MAT_SIZE, na.rm = TRUE)), linetype = 5) +
  geom_hline(aes(yintercept = mean(MAT_SIZE, na.rm = TRUE) - sd(MAT_SIZE, na.rm = TRUE)), color = "green4") +
  geom_hline(aes(yintercept = mean(MAT_SIZE, na.rm = TRUE) + sd(MAT_SIZE, na.rm = TRUE)), color = "green4") +
  labs(x = "Year", y = "Male Tanner Crab Size\nat 50% Maturity (mm)") +   
  xlim(min(years), max(years)) +
  theme_bw() +
  theme(legend.title = element_blank())

ggsave(paste0(fig_dir, "male_SAM.png"), height = 2, width = 6)

#Calculate female size at maturity
  #i.e. mean mature female size (new hardshell only)
female_size <- calc_bioabund(crab_data = tanner,
                             species = "TANNER",
                             region = "EBS",
                             year = years,                             
                             crab_category = "mature_female",
                             shell_condition = "new_hardshell",
                             bin_1mm = TRUE)

mean_size <- female_size %>%
  group_by(YEAR) %>%
  summarize(MEAN_SIZE = weighted.mean(SIZE_1MM, w = ABUNDANCE)) %>%
  right_join(., expand.grid(YEAR = years)) %>%
  arrange(YEAR)



# Plot
ggplot(mean_size, aes(x = YEAR, y = MEAN_SIZE)) +
  geom_point() +
  geom_line() +
  labs(x = "Year", y = "Mean Mature Female\nTanner Crab Size (mm)") +
  geom_hline(aes(yintercept = mean(MEAN_SIZE, na.rm = TRUE)), linetype = 5) +
  geom_hline(aes(yintercept = mean(MEAN_SIZE, na.rm = TRUE) - sd(MEAN_SIZE, na.rm = TRUE)), color = "green4") +
  geom_hline(aes(yintercept = mean(MEAN_SIZE, na.rm = TRUE) + sd(MEAN_SIZE, na.rm = TRUE)), color = "green4") +
  xlim(min(years), max(years)) +
  theme_bw()
ggsave(paste0(fig_dir, "female_SAM.png"), height = 2, width = 6)


# Save output
mean_size %>%
  rename(year = YEAR,
         female_sam = MEAN_SIZE) %>%
  write_csv("./outputs/female_SAM.csv")