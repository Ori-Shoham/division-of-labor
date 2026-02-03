# -------------------------------------------------------------------------
# MASTER MERGE SCRIPT: UKHLS MAIN + ALL COVID WAVES
# -------------------------------------------------------------------------

library(tidyverse)
library(haven)
library(forcats)
library(scales)
rm(list = ls())
fig_path <- "figures/"

# SOC definitions
SOC <- read.csv("policies/SOC.csv")
SIC <- read.csv("policies/SIC.csv")

key_workers <- readxl::read_excel("C:/Users/USER/Dropbox/WFH_covid/UK project/division-of-labor/policies/keyworkersreferencetableupdated2.xlsx", sheet = 4)
sics <- tibble(SIC_4 = colnames(key_workers)[4:599], industry = unlist(key_workers[1,4:599]))
key_workers <- key_workers %>% filter(!is.na(Group)) %>% 
  pivot_longer(!c(Group, `SOC10M / INDC07M`, `SOC_label / SIC_label`), names_to = "SIC_4", values_to = "key") %>% 
  rename(SOC_4 = `SOC10M / INDC07M`, occ_group = Group, occupation = `SOC_label / SIC_label`  ) %>% 
  left_join(sics) %>% 
  select(occ_group,occupation, SOC_4, industry, SIC_4, , key) %>% 
  mutate(across(c(SIC_4, SOC_4, key), as.numeric))
key_inds <- key_workers %>% 
  group_by(SIC = floor(SIC_4/100), SOC = floor(SOC_4/10)) %>% 
  summarise(any_key = max(key)) %>% 
  left_join(SOC) %>% 
  left_join(SIC)



# 1. DEFINE PATHS (Edit these!)
path_main  <- "C:/Users/USER/Dropbox/WFH_covid/UK project/understanding society uk all data/UKDA-6614-stata/stata/stata13_se/ukhls"  # SN 6614
path_covid <- "C:/Users/USER/Dropbox/WFH_covid/UK project/understanding society uk all data/UKDA-8644-stata/stata/stata13_se/"  # SN 8644

# =========================================================================
# STEP 1: BUILD COMPOSITE BASELINE (Pre-March 2020)
# =========================================================================
print("--- Step 1: Building Composite Baseline ---")

# Function to load and clean baseline waves with RICH variables
clean_baseline <- function(file_path, prefix) {
  
  # Define variables to keep (Employment, Time Use, Income, Job Context)
  cols_to_keep <- c(
    "pidp", "j_hidp", # ID and Household ID
    paste0(prefix, "_sex"),              # Gender
    paste0(prefix, "_age_dv"),           # Age
    paste0(prefix, "_gor_dv"),           # Region
    paste0(prefix, "_isced11_dv"),       # Education
    
    # --- EMPLOYMENT ---
    paste0(prefix, "_jbstat"),           # Status
    paste0(prefix, "_jbsic07_cc"),       # Industry (SIC)
    paste0(prefix, "_jbsoc10_cc"),       # Occupation (SOC)
    paste0(prefix, "_jbft_dv"),          # Full-time/Part-time
    paste0(prefix, "_jbhrs"),            # Hours
    paste0(prefix, "_jbsect"),           # Public/Private
    paste0(prefix, "_jbterm1"),          # Perm/Temp
    
    # --- TIME USE ---
    paste0(prefix, "_howlng"),           # Housework Hours
    paste0(prefix, "_chcare"),           # Childcare (Binary in Baseline)
    
    # --- INCOME ---
    paste0(prefix, "_pimnu_dv"),         # Gross Personal Income (Monthly)
    
    # --- TIMING ---
    paste0(prefix, "_intdatm_dv"),       # Month
    paste0(prefix, "_intdaty_dv")        # Year
  )
  
  read_dta(file_path) %>%
    select(any_of(cols_to_keep)) %>%
    rename_with(~ str_remove(., paste0("^", prefix, "_")), -pidp) # Remove prefix
}

# 1. Load Raw J (Wave 10) and K (Wave 11)
df_j <- clean_baseline(file.path(path_main, "j_indresp.dta"), "j")
df_k <- clean_baseline(file.path(path_main, "k_indresp.dta"), "k")

# 2. Filter K for STRICT Pre-COVID (Jan 2019 - Feb 2020)
df_k_clean <- df_k %>%
  filter(intdaty_dv < 2020 | (intdaty_dv == 2020 & intdatm_dv < 3)) %>%
  mutate(source_wave = "K")

# 3. Use J for anyone NOT in clean K
df_j_clean <- df_j %>%
  anti_join(df_k_clean, by = "pidp") %>%
  mutate(source_wave = "J")

# 4. Combine
df_baseline <- bind_rows(df_k_clean, df_j_clean) %>%
  rename_with(~ paste0("base_", .), -pidp) %>%  # Prefix everything with 'base_'
  mutate(base_isced11_dv = if_else(base_isced11_dv == 8,7,base_isced11_dv),
         base_isced11_dv = if_else(base_isced11_dv <0 ,NA,base_isced11_dv)) # merge PhD with MA and merge the NAs

print(paste("Baseline Built. N =", nrow(df_baseline)))

# =========================================================================
# STEP 2: MERGE ALL COVID WAVES (The Shock)
# =========================================================================
print("--- Step 2: Merging COVID Waves ---")

# Prefixes for Waves 1-9 (April 2020 -> Sept 2021)
covid_waves <- c("ca", "cb", "cc", "cd", "ce", "cf", "cg", "ch", "ci")

load_covid_wave <- function(wave_prefix) {
  filename <- paste0(wave_prefix, "_indresp_w.dta")
  fpath <- file.path(path_covid, filename)
  
  if(!file.exists(fpath)) return(NULL)
  
  data <- read_dta(fpath)
  
  # Select variables dynamically
  vars_to_keep <- c("pidp", 
                    paste0(wave_prefix, "_furlough"),   # Furlough Status
                    paste0(wave_prefix, "_ff_furlough"),   # Furlough Status
                    
                    paste0(wave_prefix, "_hours"),      # hours worked last week
                    paste0(wave_prefix, "_blwork"),     # baseline worked in Jan-Feb 2020
                    
                    paste0(wave_prefix, "_blhours"),    # baseline hours - weekly hours in Jan-Feb 2020
                    paste0(wave_prefix, "_blwah"),      # Baseline: worked at home
                    paste0(wave_prefix, "_sempderived"),# employed (1 - employed, 2 - self-employed, 3 - both, 4 - no)
                    
                    paste0(wave_prefix, "_wah"),        # Work at home?
                    paste0(wave_prefix, "_howlng"),     # Housework Hours
                    paste0(wave_prefix, "_keyworker"),     # key worker
                    paste0(wave_prefix, "_keyworksector"),     # key worker
                    
                    paste0(wave_prefix, "timechcare")   # Childcare snd homeschooling Hours (Continuous!)
)   
  
  existing_vars <- intersect(vars_to_keep, names(data))
  data %>% select(all_of(existing_vars)) %>% 
    rename_with(\(x) str_replace(x,"ff_furlough", "fffurlough"), everything())
}

list_covid_dfs <- lapply(covid_waves, load_covid_wave)
list_covid_dfs <- list_covid_dfs[!sapply(list_covid_dfs, is.null)]

# Full Join to keep people who dipped in and out
df_covid_all <- list_covid_dfs %>% reduce(full_join, by = "pidp")

print("COVID Waves Merged.")


# # =========================================================================
# # STEP 3: POST-COVID OUTCOMES (Recovery)
# # =========================================================================
# print("--- Step 3: Adding Recovery Waves ---")
# 
# df_l <- read_dta(file.path(path_main, "l_indresp.dta")) %>%
#   select(pidp, l_jbstat_dv, l_howlng)
# df_m <- read_dta(file.path(path_main, "m_indresp.dta")) %>%
#   select(pidp, m_jbstat_dv, m_howlng, m_mastat_dv)

# =========================================================================
# STEP 4: CREATE INDIVIDUAL PANEL & DEFINE GROUPS
# =========================================================================
print("--- Step 4: Creating Individual Panel ---")

first_valid <- function(x) {
  x2 <- x[!is.na(x) & x != -8]
  if (length(x2) == 0) NA else x2[1]
}


df_ind_panel <- df_baseline %>%
  left_join(df_covid_all, by = "pidp") %>%
  # left_join(df_l, by = "pidp") %>%
  # left_join(df_m, by = "pidp") %>%
  mutate(
    # --- DEFINE SHUTDOWN SECTOR  ---
    # 55 - accommodation
    # 56 - Food and beverage service activities
    # 79 - Travel agency, tour operator and other reservation service and related activities
    # 90 - Creative, arts and entertainment activities (except ‘artistic creation’ 9003)
    # 91 - Libraries, archives, museums and other cultural activities
    # 92 - Gambling and betting activities
    # 93 - Sports activities and amusement and recreation activities
    # 96 - Other personal service activities (except ‘funeral and related activities’ 9603)
    # 97 - Activities of households as employers of domestic personnel
    # other sectors including retail mix shutdown and non shutdown sectors (e.g. food and apparel) and are not clear cut
    shutdown_sec = ifelse(base_jbsic07_cc %in% c(55, 56, 79, 90, 91, 92, 93, 96), 1, 0),
    
    # --- DEFINE KEY WORKER OCCUPATION  ---
    # Health, Education, Protective, Care
    keyworker_health_social = if_else(base_jbsic07_cc == 86 | base_jbsoc10_cc %in% c(124, 221, 222, 223, 321)|(base_jbsoc10_cc == 118 & base_jbsic07_cc %in% 86:88)|(base_jbsoc10_cc == 614 & base_jbsic07_cc %in% c(84,86:88)), 1, 0),
    keyworker_education = if_else(base_jbsic07_cc%in%c(85)|(base_jbsoc10_cc %in% c( 323,612, 623) &base_jbsic07_cc == 84)|(base_jbsoc10_cc == 231),1,0),
    keyworker_public_safety = if_else(base_jbsoc10_cc == 331, 1, 0),
    keyworker_my_def = pmax(keyworker_health_social, keyworker_education,keyworker_public_safety),
    # --- DEFINE AMBIGUOUS (To Exclude) ---
    # Retail (47) and Transport (49-51) where we can't distinguish essential/non-essential
    # is_ambiguous = ifelse(base_jbsic07_cc %in% c(47, 49, 50, 51), 1, 0)
  ) %>% 
  left_join(SOC, by = c("base_jbsoc10_cc" = "SOC"))%>%left_join(SIC, by = c("base_jbsic07_cc" = "SIC")) %>% 
  left_join(key_inds %>% select(!c(industry, occupation)), by = c("base_jbsoc10_cc" = "SOC", "base_jbsic07_cc" = "SIC"))

# filter 
df_sample <- df_ind_panel %>% 
# working at baseline
  filter(base_jbstat %in% 1:2) %>% 
  # has any covid data
  filter(rowSums(!is.na(across(starts_with("c")))) > 0)  %>% 
  pivot_longer(starts_with("c"), names_sep = "_", names_to = c("wave", "var"), values_to = "val") %>% 
  pivot_wider(names_from = var, values_from = val)%>% 
  mutate(group_self_report = case_when(shutdown_sec == 1 ~ "shutdown sector",
                           keyworker == 1 | keyworksector %in% 1:8 ~ "key worker",
                           T ~ "other"),
         group_industry_based = case_when(shutdown_sec == 1 ~ "shutdown sector",
                                  keyworker_my_def == 1 ~ "key worker",
                                  T ~ "other"),
         group_industry_based_detailed = case_when(shutdown_sec == 1 ~ "shutdown sector",
                                                   keyworker_health_social == 1 ~ "key worker - health and social services",
                                                   keyworker_education == 1 ~ "key worker - education",
                                                   keyworker_public_safety == 1 ~ "key worker - public safety",
                                                   T ~ "other"))

df_sample <- df_sample %>% 
  arrange(pidp, wave) %>% 
  group_by(pidp) %>% 
  mutate(across(starts_with("bl"), first_valid)) %>% 
  ungroup()
df_precovid <- df_sample %>% 
  group_by(pidp) %>% 
  slice_head(n=1) %>% 
  mutate(wave = "baseline",sempderived = blwork , hours = blhours, wah = blwah) %>% 
  ungroup()
df_2019 <- df_sample %>% 
  group_by(pidp) %>% 
  slice_head(n=1) %>% 
  mutate(wave = "2019",sempderived = base_jbstat , hours = base_jbhrs) %>% 
  ungroup()

df_sample <- bind_rows(df_2019, df_precovid, df_sample) %>% 
  mutate(hours = if_else(sempderived == 4 & is.na(hours), -8, hours),
         wah = if_else(sempderived == 4 & is.na(wah), -8, wah),
         workoutside = case_when(sempderived < 0 ~ NA,
                                 hours <= 0 ~ 0,
                                 wah == 1 ~ 0,
                                 wah %in% 2:4 ~ 1, 
                                 T ~NA))
# plot - working any hour last week, april 2020, by industry ----
p <- df_sample %>%
  filter(wave == "ca" & !is.na(hours) & !hours%in%c(-9,-2,-1) & sempderived>=0) %>%
  add_count(industry) %>%
  mutate(industry = if_else(n<25,"other",industry)) %>% 
  ggplot(aes(x = fct_infreq(industry), fill = factor(hours>0, levels = c(T,F), labels = c("Yes", "No") ))) +
  geom_bar() +
  coord_flip()+scale_x_discrete(limits = rev)+labs(fill = "Worked at all last week", x = element_blank(), title = "Worked last week, April 2020")+
  theme(legend.position = "bottom")
p
ggsave("worked_at_all_april20_ind.png",p,path = fig_path,width = 12, height = 8)

# in percentages
p <- df_sample %>%
  filter(wave == "ca" & !is.na(hours) & !hours %in% c(-9, -2, -1)& sempderived>=0) %>%
  add_count(industry) %>%
  mutate(industry = if_else(n < 25, "other", industry)) %>%
  group_by(industry) %>%
  mutate(
    n_ind = n(),
    share_yes = mean(hours > 0),
    industry_lab = paste0(industry, " (n=", n_ind, ")")
  ) %>%
  ungroup() %>%
  ggplot(aes(
    x = fct_reorder(industry_lab, share_yes, .desc = TRUE),
    fill = factor(hours > 0, levels = c(TRUE, FALSE), labels = c("Yes", "No"))
  )) +
  geom_bar(position = "fill") +
  coord_flip() +   scale_y_continuous(labels = percent_format()) +
  labs(fill = "Worked at all last week", x = NULL,y= NULL, title = "Worked last week, April 2020") +
  theme(legend.position = "bottom")
p
ggsave("worked_at_all_april20_ind_perc.png",p,path = fig_path,width = 12, height = 8)

# plot - work status, april 2020, by industry ----

p <- df_sample %>%
  filter(wave == "ca" & !is.na(sempderived)) %>%
  add_count(industry) %>%
  mutate(industry = if_else(n<25,"other",industry),
         status = case_when(sempderived <0 ~ "Missing",
                            sempderived == 1 ~ "Employed",
                            sempderived == 2 ~ "Self-employed",
                            sempderived == 3 ~ "Both employed and self-employed",
                            T ~ "Not employed"),
         status = factor(
           status,
           levels = c(
             "Employed",
             "Self-employed",
             "Both employed and self-employed",
             "Not employed",
             "Missing"
           )
         )) %>% 
  ggplot(aes(x = fct_infreq(industry), fill =status)) +
  geom_bar(position= position_stack(reverse = TRUE)) +
  coord_flip()+scale_x_discrete(limits = rev)+labs(fill = "Work status", x = element_blank(), title = "Work status, April 2020")+
  theme(legend.position = "bottom")
p
ggsave("work_status_april20_ind.png",p,path = fig_path,width = 14, height = 8)

# in percentages
p <- df_sample %>%
  filter(wave == "ca" & !is.na(sempderived)) %>%
  add_count(industry) %>%
  mutate(
    industry = if_else(n < 25, "other", industry),
    status = case_when(
      sempderived < 0 ~ "Missing",
      sempderived == 1 ~ "Employed",
      sempderived == 2 ~ "Self-employed",
      sempderived == 3 ~ "Both employed and self-employed",
      TRUE ~ "Not employed"
    )
  ) %>%
  group_by(industry) %>%
  mutate(
    n_ind = n(),
    share_not = mean(status == "Not employed"),
    industry_lab = paste0(industry, " (n=", n_ind, ")"),
    status = factor(
      status,
      levels = c(
        "Not employed",
        "Employed",
        "Self-employed",
        "Both employed and self-employed",
        "Missing"
      )
    )
  ) %>%
  ungroup() %>%
  ggplot(aes(
    x = fct_reorder(industry_lab, share_not, .desc = TRUE),
    fill = status
  )) +
  geom_bar(position = position_fill(reverse = TRUE)) +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  labs(fill = "Work status", x = NULL, y = NULL, title = "Work status, April 2020") +
  theme(legend.position = "bottom")

p
ggsave("work_status_april20_ind_perc.png",p,path = fig_path,width = 14, height = 8)

# plot - furloughed, april 2020, by industry ----
p <- df_sample %>%
  filter(wave == "ca" & !is.na(sempderived) & sempderived > 0 ) %>%
  add_count(industry) %>%
  mutate(industry = if_else(n<25,"other",industry),
         furlough = case_when(sempderived %in% c(2,4) ~ "Self- or not employed",
                              furlough == 1 ~ "Yes",
                              furlough == 2 ~ "No",
                              T ~ NA)) %>%
  filter(!is.na(furlough)) %>% 
  group_by(industry) %>%
  mutate(
    n_ind = n(),
    share_fur = mean(furlough == "Yes", na.rm = T),
    industry_lab = paste0(industry, " (n=", n_ind, ")"),
    furlough = factor(
      furlough,
      levels = c(
        "Yes",
        "No",
        "Self- or not employed"
      )
    )
  ) %>% ungroup() %>%
  ggplot(aes(
    x = fct_reorder(industry_lab, share_fur, .desc = TRUE),
    fill = furlough
  )) +
  geom_bar(position = position_fill(reverse = TRUE)) +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  labs(fill = "Furloughed", x = NULL, y = NULL, title = "Furlough, April 2020") +
  theme(legend.position = "bottom")
p
ggsave("furlough_april20_ind_perc.png",p,path = fig_path,width = 12, height = 8)

# plot - WFH, april 2020, by industry ----
p <- df_sample %>%
  filter(wave == "ca" & !is.na(sempderived) & sempderived > 0 ) %>%
  add_count(industry) %>%
  mutate(industry = if_else(n<25,"other",industry),
         WFH = case_when(sempderived %in% c(4) ~ "Not employed",
                              wah == 1 ~ "Always",
                              wah == 2 ~ "Often",
                         wah == 3 ~ "Sometimes",
                         wah == 4 ~ "Never",
                              T ~ NA)) %>%
  filter(!is.na(WFH)) %>% 
  group_by(industry) %>%
  mutate(
    n_ind = n(),
    share_always = mean(WFH == "Always", na.rm = T),
    industry_lab = paste0(industry, " (n=", n_ind, ")"),
    WFH = factor(
      WFH,
      levels = c(
        "Always", "Often", "Sometimes","Never", "Not employed"  )
    )
  ) %>% ungroup() %>%
  ggplot(aes(
    x = fct_reorder(industry_lab, share_always, .desc = F),
    fill = WFH
  )) +
  geom_bar(position = position_fill(reverse = TRUE)) +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  labs(fill = "Work from home", x = NULL, y = NULL, title = "Work from home, April 2020") +
  theme(legend.position = "bottom")
p
ggsave("wfh_april20_ind_perc.png",p,path = fig_path,width = 12, height = 8)

# plot - working any hour last week, May 2020, by industry ----
p <- df_sample %>%
  filter(wave == "cb" & !is.na(hours) & !hours%in%c(-9,-2,-1) & sempderived>=0) %>%
  add_count(industry) %>%
  mutate(industry = if_else(n<25,"other",industry)) %>% 
  ggplot(aes(x = fct_infreq(industry), fill = factor(hours>0, levels = c(T,F), labels = c("Yes", "No") ))) +
  geom_bar() +
  coord_flip()+scale_x_discrete(limits = rev)+labs(fill = "Worked at all last week", x = element_blank(), title = "Worked last week, May 2020")+
  theme(legend.position = "bottom")
p
ggsave("worked_at_all_may20_ind.png",p,path = fig_path,width = 12, height = 8)

# in percentages
p <- df_sample %>%
  filter(wave == "cb" & !is.na(hours) & !hours %in% c(-9, -2, -1)& sempderived>=0) %>%
  add_count(industry) %>%
  mutate(industry = if_else(n < 25, "other", industry)) %>%
  group_by(industry) %>%
  mutate(
    n_ind = n(),
    share_yes = mean(hours > 0),
    industry_lab = paste0(industry, " (n=", n_ind, ")")
  ) %>%
  ungroup() %>%
  ggplot(aes(
    x = fct_reorder(industry_lab, share_yes, .desc = TRUE),
    fill = factor(hours > 0, levels = c(TRUE, FALSE), labels = c("Yes", "No"))
  )) +
  geom_bar(position = "fill") +
  coord_flip() +   scale_y_continuous(labels = percent_format()) +
  labs(fill = "Worked at all last week", x = NULL,y= NULL, title = "Worked last week, May 2020") +
  theme(legend.position = "bottom")
p
ggsave("worked_at_all_may20_ind_perc.png",p,path = fig_path,width = 12, height = 8)

# plot - work status, May 2020, by industry ----

p <- df_sample %>%
  filter(wave == "cb" & !is.na(sempderived)) %>%
  add_count(industry) %>%
  mutate(industry = if_else(n<25,"other",industry),
         status = case_when(sempderived <0 ~ "Missing",
                            sempderived == 1 ~ "Employed",
                            sempderived == 2 ~ "Self-employed",
                            sempderived == 3 ~ "Both employed and self-employed",
                            T ~ "Not employed"),
         status = factor(
           status,
           levels = c(
             "Employed",
             "Self-employed",
             "Both employed and self-employed",
             "Not employed",
             "Missing"
           )
         )) %>% 
  ggplot(aes(x = fct_infreq(industry), fill =status)) +
  geom_bar(position= position_stack(reverse = TRUE)) +
  coord_flip()+scale_x_discrete(limits = rev)+labs(fill = "Work status", x = element_blank(), title = "Work status, May 2020")+
  theme(legend.position = "bottom")
p
ggsave("work_status_may20_ind.png",p,path = fig_path,width = 14, height = 8)

# in percentages
p <- df_sample %>%
  filter(wave == "cb" & !is.na(sempderived)) %>%
  add_count(industry) %>%
  mutate(
    industry = if_else(n < 25, "other", industry),
    status = case_when(
      sempderived < 0 ~ "Missing",
      sempderived == 1 ~ "Employed",
      sempderived == 2 ~ "Self-employed",
      sempderived == 3 ~ "Both employed and self-employed",
      TRUE ~ "Not employed"
    )
  ) %>%
  group_by(industry) %>%
  mutate(
    n_ind = n(),
    share_not = mean(status == "Not employed"),
    industry_lab = paste0(industry, " (n=", n_ind, ")"),
    status = factor(
      status,
      levels = c(
        "Not employed",
        "Employed",
        "Self-employed",
        "Both employed and self-employed",
        "Missing"
      )
    )
  ) %>%
  ungroup() %>%
  ggplot(aes(
    x = fct_reorder(industry_lab, share_not, .desc = TRUE),
    fill = status
  )) +
  geom_bar(position = position_fill(reverse = TRUE)) +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  labs(fill = "Work status", x = NULL, y = NULL, title = "Work status, May 2020") +
  theme(legend.position = "bottom")

p
ggsave("work_status_may20_ind_perc.png",p,path = fig_path,width = 14, height = 8)

# plot - WFH, May 2020, by industry ----
p <- df_sample %>%
  filter(wave == "cb" & !is.na(sempderived) & sempderived > 0 ) %>%
  add_count(industry) %>%
  mutate(industry = if_else(n<25,"other",industry),
         WFH = case_when(sempderived %in% c(4) ~ "Not employed",
                         wah == 1 ~ "Always",
                         wah == 2 ~ "Often",
                         wah == 3 ~ "Sometimes",
                         wah == 4 ~ "Never",
                         T ~ NA)) %>%
  filter(!is.na(WFH)) %>% 
  group_by(industry) %>%
  mutate(
    n_ind = n(),
    share_always = mean(WFH == "Always", na.rm = T),
    industry_lab = paste0(industry, " (n=", n_ind, ")"),
    WFH = factor(
      WFH,
      levels = c(
        "Always", "Often", "Sometimes","Never", "Not employed"  )
    )
  ) %>% ungroup() %>%
  ggplot(aes(
    x = fct_reorder(industry_lab, share_always, .desc = F),
    fill = WFH
  )) +
  geom_bar(position = position_fill(reverse = TRUE)) +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  labs(fill = "Work from home", x = NULL, y = NULL, title = "Work from home, May 2020") +
  theme(legend.position = "bottom")
p
ggsave("wfh_may20_ind_perc.png",p,path = fig_path,width = 12, height = 8)

# plot - working any hour last week, april 2020, by group  ----
p <- df_sample %>%
  filter(wave == "ca" & !is.na(hours) & !hours%in%c(-9,-2,-1) & sempderived>=0 & base_jbsic07_cc >=0) %>%
  add_count(group_industry_based) %>%
  ggplot(aes(x = fct_infreq(group_industry_based), fill = factor(hours>0, levels = c(T,F), labels = c("Yes", "No") ))) +
  geom_bar() +
  coord_flip()+scale_x_discrete(limits = rev)+labs(fill = "Worked at all last week", x = element_blank(), title = "Worked last week, April 2020")+
  theme(legend.position = "bottom")
p

ggsave("worked_at_all_april20_groups.png",p,path = fig_path,width = 12, height = 8)

# in percentages
p <- df_sample %>%
  filter(wave == "ca" & !is.na(hours) & !hours%in%c(-9,-2,-1) & sempderived>=0 & base_jbsic07_cc >=0) %>%
  add_count(group_industry_based) %>%
  group_by(group_industry_based) %>%
  mutate(
    n_ind = n(),
    share_yes = mean(hours > 0),
    industry_lab = paste0(group_industry_based, " (n=", n_ind, ")")
  ) %>%
  ungroup() %>%
  ggplot(aes(
    x = fct_reorder(industry_lab, share_yes, .desc = TRUE),
    fill = factor(hours > 0, levels = c(TRUE, FALSE), labels = c("Yes", "No"))
  )) +
  geom_bar(position = "fill") +
  coord_flip() +   scale_y_continuous(labels = percent_format()) +
  labs(fill = "Worked at all last week", x = NULL,y= NULL, title = "Worked last week, April 2020") +
  theme(legend.position = "bottom")
p
ggsave("worked_at_all_april20_groups_perc.png",p,path = fig_path,width = 12, height = 8)

# plot - working any hour last week, april 2020, by  detailed group  ----
p <- df_sample %>%
  filter(wave == "ca" & !is.na(hours) & !hours%in%c(-9,-2,-1) & sempderived>=0 & base_jbsic07_cc >=0) %>%
  add_count(group_industry_based_detailed) %>%
  ggplot(aes(x = fct_infreq(group_industry_based_detailed), fill = factor(hours>0, levels = c(T,F), labels = c("Yes", "No") ))) +
  geom_bar() +
  coord_flip()+scale_x_discrete(limits = rev)+labs(fill = "Worked at all last week", x = element_blank(), title = "Worked last week, April 2020")+
  theme(legend.position = "bottom")
p

ggsave("worked_at_all_april20_groups_detailed.png",p,path = fig_path,width = 12, height = 8)

# in percentages
p <- df_sample %>%
  filter(wave == "ca" & !is.na(hours) & !hours%in%c(-9,-2,-1) & sempderived>=0 & base_jbsic07_cc >=0) %>%
  add_count(group_industry_based_detailed) %>%
  group_by(group_industry_based_detailed) %>%
  mutate(
    n_ind = n(),
    share_yes = mean(hours > 0),
    industry_lab = paste0(group_industry_based_detailed, " (n=", n_ind, ")")
  ) %>%
  ungroup() %>%
  ggplot(aes(
    x = fct_reorder(industry_lab, share_yes, .desc = TRUE),
    fill = factor(hours > 0, levels = c(TRUE, FALSE), labels = c("Yes", "No"))
  )) +
  geom_bar(position = "fill") +
  coord_flip() +   scale_y_continuous(labels = percent_format()) +
  labs(fill = "Worked at all last week", x = NULL,y= NULL, title = "Worked last week, April 2020") +
  theme(legend.position = "bottom")
p
ggsave("worked_at_all_april20_groups_detailed_perc.png",p,path = fig_path,width = 12, height = 8)

# plot - work status, april 2020, by group ----

p <- df_sample %>%
  filter(wave == "ca" & !is.na(sempderived) & base_jbsic07_cc >=0) %>%
  add_count(group_industry_based) %>%
  mutate(industry = if_else(n<25,"other",industry),
         status = case_when(sempderived <0 ~ "Missing",
                            sempderived == 1 ~ "Employed",
                            sempderived == 2 ~ "Self-employed",
                            sempderived == 3 ~ "Both employed and self-employed",
                            T ~ "Not employed"),
         status = factor(
           status,
           levels = c(
             "Employed",
             "Self-employed",
             "Both employed and self-employed",
             "Not employed",
             "Missing"
           )
         )) %>% 
  ggplot(aes(x = fct_infreq(group_industry_based), fill =status)) +
  geom_bar(position= position_stack(reverse = TRUE)) +
  coord_flip()+scale_x_discrete(limits = rev)+labs(fill = "Work status", x = element_blank(), title = "Work status, April 2020")+
  theme(legend.position = "bottom")
p
ggsave("work_status_april20_groups.png",p,path = fig_path,width = 14, height = 8)

# in percentages
p <- df_sample %>%
  filter(wave == "ca" & !is.na(sempderived)& base_jbsic07_cc >=0) %>%
  add_count(group_industry_based) %>%
  mutate(
    industry = if_else(n < 25, "other", industry),
    status = case_when(
      sempderived < 0 ~ "Missing",
      sempderived == 1 ~ "Employed",
      sempderived == 2 ~ "Self-employed",
      sempderived == 3 ~ "Both employed and self-employed",
      TRUE ~ "Not employed"
    )
  ) %>%
  group_by(group_industry_based) %>%
  mutate(
    n_ind = n(),
    share_not = mean(status == "Not employed"),
    industry_lab = paste0(group_industry_based, " (n=", n_ind, ")"),
    status = factor(
      status,
      levels = c(
        "Not employed",
        "Employed",
        "Self-employed",
        "Both employed and self-employed",
        "Missing"
      )
    )
  ) %>%
  ungroup() %>%
  ggplot(aes(
    x = fct_reorder(industry_lab, share_not, .desc = TRUE),
    fill = status
  )) +
  geom_bar(position = position_fill(reverse = TRUE)) +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  labs(fill = "Work status", x = NULL, y = NULL, title = "Work status, April 2020") +
  theme(legend.position = "bottom")

p
ggsave("work_status_april20_groups_perc.png",p,path = fig_path,width = 14, height = 8)

# plot - furloughed, april 2020, by group status ----
p <- df_sample %>%
  filter(wave == "ca" & !is.na(sempderived) & sempderived > 0 & base_jbsic07_cc >=0 ) %>%
  add_count(group_industry_based) %>%
  mutate(
         furlough = case_when(sempderived %in% c(2,4) ~ "Self- or not employed",
                              furlough == 1 ~ "Yes",
                              furlough == 2 ~ "No",
                              T ~ NA)) %>%
  filter(!is.na(furlough)) %>% 
  group_by(group_industry_based) %>%
  mutate(
    n_ind = n(),
    share_fur = mean(furlough == "Yes", na.rm = T),
    industry_lab = paste0(group_industry_based, " (n=", n_ind, ")"),
    furlough = factor(
      furlough,
      levels = c(
        "Yes",
        "No",
        "Self- or not employed"
      )
    )
  ) %>% ungroup() %>%
  ggplot(aes(
    x = fct_reorder(industry_lab, share_fur, .desc = TRUE),
    fill = furlough
  )) +
  geom_bar(position = position_fill(reverse = TRUE)) +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  labs(fill = "Furloughed", x = NULL, y = NULL, title = "Furlough, April 2020") +
  theme(legend.position = "bottom")
p
ggsave("furlough_april20_groups_perc.png",p,path = fig_path,width = 12, height = 8)

# plot - furloughed, april 2020, by detailed group status ----
p <- df_sample %>%
  filter(wave == "ca" & !is.na(sempderived) & sempderived > 0 & base_jbsic07_cc >=0 ) %>%
  add_count(group_industry_based_detailed) %>%
  mutate(
    furlough = case_when(sempderived %in% c(2,4) ~ "Self- or not employed",
                         furlough == 1 ~ "Yes",
                         furlough == 2 ~ "No",
                         T ~ NA)) %>%
  filter(!is.na(furlough)) %>% 
  group_by(group_industry_based_detailed) %>%
  mutate(
    n_ind = n(),
    share_fur = mean(furlough == "Yes", na.rm = T),
    industry_lab = paste0(group_industry_based_detailed, " (n=", n_ind, ")"),
    furlough = factor(
      furlough,
      levels = c(
        "Yes",
        "No",
        "Self- or not employed"
      )
    )
  ) %>% ungroup() %>%
  ggplot(aes(
    x = fct_reorder(industry_lab, share_fur, .desc = TRUE),
    fill = furlough
  )) +
  geom_bar(position = position_fill(reverse = TRUE)) +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  labs(fill = "Furloughed", x = NULL, y = NULL, title = "Furlough, April 2020") +
  theme(legend.position = "bottom")
p
ggsave("furlough_april20_groups_detailed_perc.png",p,path = fig_path,width = 12, height = 8)

# plot - WFH, april 2020, by group ----
p <- df_sample %>%
  filter(wave == "ca" & !is.na(sempderived) & sempderived > 0  & base_jbsic07_cc >=0 ) %>%
  add_count(group_industry_based) %>%
  mutate(industry = if_else(n<25,"other",industry),
         WFH = case_when(sempderived %in% c(4) ~ "Not employed",
                              wah == 1 ~ "Always",
                              wah == 2 ~ "Often",
                         wah == 3 ~ "Sometimes",
                         wah == 4 ~ "Never",
                              T ~ NA)) %>%
  filter(!is.na(WFH)) %>% 
  group_by(group_industry_based) %>%
  mutate(
    n_ind = n(),
    share_always = mean(WFH == "Always", na.rm = T),
    industry_lab = paste0(group_industry_based, " (n=", n_ind, ")"),
    WFH = factor(
      WFH,
      levels = c(
        "Always", "Often", "Sometimes","Never", "Not employed"  )
    )
  ) %>% ungroup() %>%
  ggplot(aes(
    x = fct_reorder(industry_lab, share_always, .desc = F),
    fill = WFH
  )) +
  geom_bar(position = position_fill(reverse = TRUE)) +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  labs(fill = "Work from home", x = NULL, y = NULL, title = "Work from home, April 2020") +
  theme(legend.position = "bottom")
p
ggsave("wfh_april20_groups_perc.png",p,path = fig_path,width = 12, height = 8)

# plot - WFH, april 2020, by detailed group ----
p <- df_sample %>%
  filter(wave == "ca" & !is.na(sempderived) & sempderived > 0  & base_jbsic07_cc >=0 ) %>%
  add_count(group_industry_based_detailed) %>%
  mutate(industry = if_else(n<25,"other",industry),
         WFH = case_when(sempderived %in% c(4) ~ "Not employed",
                         wah == 1 ~ "Always",
                         wah == 2 ~ "Often",
                         wah == 3 ~ "Sometimes",
                         wah == 4 ~ "Never",
                         T ~ NA)) %>%
  filter(!is.na(WFH)) %>% 
  group_by(group_industry_based_detailed) %>%
  mutate(
    n_ind = n(),
    share_always = mean(WFH == "Always", na.rm = T),
    industry_lab = paste0(group_industry_based_detailed, " (n=", n_ind, ")"),
    WFH = factor(
      WFH,
      levels = c(
        "Always", "Often", "Sometimes","Never", "Not employed"  )
    )
  ) %>% ungroup() %>%
  ggplot(aes(
    x = fct_reorder(industry_lab, share_always, .desc = F),
    fill = WFH
  )) +
  geom_bar(position = position_fill(reverse = TRUE)) +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  labs(fill = "Work from home", x = NULL, y = NULL, title = "Work from home, April 2020") +
  theme(legend.position = "bottom")
p
ggsave("wfh_april20_detailed_groups_perc.png",p,path = fig_path,width = 12, height = 8)

# plot - working any hour last week, may 2020, by group  ----
p <- df_sample %>%
  filter(wave == "cb" & !is.na(hours) & !hours%in%c(-9,-2,-1) & sempderived>=0 & base_jbsic07_cc >=0) %>%
  add_count(group_industry_based) %>%
  ggplot(aes(x = fct_infreq(group_industry_based), fill = factor(hours>0, levels = c(T,F), labels = c("Yes", "No") ))) +
  geom_bar() +
  coord_flip()+scale_x_discrete(limits = rev)+labs(fill = "Worked at all last week", x = element_blank(), title = "Worked last week, May 2020")+
  theme(legend.position = "bottom")
p

ggsave("worked_at_all_may20_groups.png",p,path = fig_path,width = 12, height = 8)

# in percentages
p <- df_sample %>%
  filter(wave == "cb" & !is.na(hours) & !hours%in%c(-9,-2,-1) & sempderived>=0 & base_jbsic07_cc >=0) %>%
  add_count(group_industry_based) %>%
  group_by(group_industry_based) %>%
  mutate(
    n_ind = n(),
    share_yes = mean(hours > 0),
    industry_lab = paste0(group_industry_based, " (n=", n_ind, ")")
  ) %>%
  ungroup() %>%
  ggplot(aes(
    x = fct_reorder(industry_lab, share_yes, .desc = TRUE),
    fill = factor(hours > 0, levels = c(TRUE, FALSE), labels = c("Yes", "No"))
  )) +
  geom_bar(position = "fill") +
  coord_flip() +   scale_y_continuous(labels = percent_format()) +
  labs(fill = "Worked at all last week", x = NULL,y= NULL, title = "Worked last week, May 2020") +
  theme(legend.position = "bottom")
p
ggsave("worked_at_all_may20_groups_perc.png",p,path = fig_path,width = 12, height = 8)

# by detialed group

p <- df_sample %>%
  filter(wave == "cb" & !is.na(hours) & !hours%in%c(-9,-2,-1) & sempderived>=0 & base_jbsic07_cc >=0) %>%
  add_count(group_industry_based_detailed) %>%
  group_by(group_industry_based_detailed) %>%
  mutate(
    n_ind = n(),
    share_yes = mean(hours > 0),
    industry_lab = paste0(group_industry_based_detailed, " (n=", n_ind, ")")
  ) %>%
  ungroup() %>%
  ggplot(aes(
    x = fct_reorder(industry_lab, share_yes, .desc = TRUE),
    fill = factor(hours > 0, levels = c(TRUE, FALSE), labels = c("Yes", "No"))
  )) +
  geom_bar(position = "fill") +
  coord_flip() +   scale_y_continuous(labels = percent_format()) +
  labs(fill = "Worked at all last week", x = NULL,y= NULL, title = "Worked last week, May 2020") +
  theme(legend.position = "bottom")
p
ggsave("worked_at_all_may20_detailed_groups_perc.png",p,path = fig_path,width = 12, height = 8)

# plot - work status, may 2020, by group ----

p <- df_sample %>%
  filter(wave == "cb" & !is.na(sempderived) & base_jbsic07_cc >=0) %>%
  add_count(group_industry_based) %>%
  mutate(industry = if_else(n<25,"other",industry),
         status = case_when(sempderived <0 ~ "Missing",
                            sempderived == 1 ~ "Employed",
                            sempderived == 2 ~ "Self-employed",
                            sempderived == 3 ~ "Both employed and self-employed",
                            T ~ "Not employed"),
         status = factor(
           status,
           levels = c(
             "Employed",
             "Self-employed",
             "Both employed and self-employed",
             "Not employed",
             "Missing"
           )
         )) %>% 
  ggplot(aes(x = fct_infreq(group_industry_based), fill =status)) +
  geom_bar(position= position_stack(reverse = TRUE)) +
  coord_flip()+scale_x_discrete(limits = rev)+labs(fill = "Work status", x = element_blank(), title = "Work status, May 2020")+
  theme(legend.position = "bottom")
p
ggsave("work_status_may20_groups.png",p,path = fig_path,width = 14, height = 8)

# in percentages
p <- df_sample %>%
  filter(wave == "cb" & !is.na(sempderived)& base_jbsic07_cc >=0) %>%
  add_count(group_industry_based) %>%
  mutate(
    industry = if_else(n < 25, "other", industry),
    status = case_when(
      sempderived < 0 ~ "Missing",
      sempderived == 1 ~ "Employed",
      sempderived == 2 ~ "Self-employed",
      sempderived == 3 ~ "Both employed and self-employed",
      TRUE ~ "Not employed"
    )
  ) %>%
  group_by(group_industry_based) %>%
  mutate(
    n_ind = n(),
    share_not = mean(status == "Not employed"),
    industry_lab = paste0(group_industry_based, " (n=", n_ind, ")"),
    status = factor(
      status,
      levels = c(
        "Not employed",
        "Employed",
        "Self-employed",
        "Both employed and self-employed",
        "Missing"
      )
    )
  ) %>%
  ungroup() %>%
  ggplot(aes(
    x = fct_reorder(industry_lab, share_not, .desc = TRUE),
    fill = status
  )) +
  geom_bar(position = position_fill(reverse = TRUE)) +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  labs(fill = "Work status", x = NULL, y = NULL, title = "Work status, May 2020") +
  theme(legend.position = "bottom")

p
ggsave("work_status_may20_groups_perc.png",p,path = fig_path,width = 14, height = 8)

# plot - WFH, May 2020, by group ----
p <- df_sample %>%
  filter(wave == "cb" & !is.na(sempderived) & sempderived > 0  & base_jbsic07_cc >=0 ) %>%
  add_count(group_industry_based) %>%
  mutate(industry = if_else(n<25,"other",industry),
         WFH = case_when(sempderived %in% c(4) ~ "Not employed",
                         wah == 1 ~ "Always",
                         wah == 2 ~ "Often",
                         wah == 3 ~ "Sometimes",
                         wah == 4 ~ "Never",
                         T ~ NA)) %>%
  filter(!is.na(WFH)) %>% 
  group_by(group_industry_based) %>%
  mutate(
    n_ind = n(),
    share_always = mean(WFH == "Always", na.rm = T),
    industry_lab = paste0(group_industry_based, " (n=", n_ind, ")"),
    WFH = factor(
      WFH,
      levels = c(
        "Always", "Often", "Sometimes","Never", "Not employed"  )
    )
  ) %>% ungroup() %>%
  ggplot(aes(
    x = fct_reorder(industry_lab, share_always, .desc = F),
    fill = WFH
  )) +
  geom_bar(position = position_fill(reverse = TRUE)) +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  labs(fill = "Work from home", x = NULL, y = NULL, title = "Work from home, May 2020") +
  theme(legend.position = "bottom")
p
ggsave("wfh_may20_groups_perc.png",p,path = fig_path,width = 12, height = 8)

# by detailed group
p <- df_sample %>%
  filter(wave == "cb" & !is.na(sempderived) & sempderived > 0  & base_jbsic07_cc >=0 ) %>%
add_count(group_industry_based_detailed) %>%
  mutate(industry = if_else(n<25,"other",industry),
         WFH = case_when(sempderived %in% c(4) ~ "Not employed",
                         wah == 1 ~ "Always",
                         wah == 2 ~ "Often",
                         wah == 3 ~ "Sometimes",
                         wah == 4 ~ "Never",
                         T ~ NA)) %>%
  filter(!is.na(WFH)) %>% 
  group_by(group_industry_based_detailed) %>%
  mutate(
    n_ind = n(),
    share_always = mean(WFH == "Always", na.rm = T),
    industry_lab = paste0(group_industry_based_detailed, " (n=", n_ind, ")"),
    WFH = factor(
      WFH,
      levels = c(
        "Always", "Often", "Sometimes","Never", "Not employed"  )
    )
  ) %>% ungroup() %>%
  ggplot(aes(
    x = fct_reorder(industry_lab, share_always, .desc = F),
    fill = WFH
  )) +
  geom_bar(position = position_fill(reverse = TRUE)) +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  labs(fill = "Work from home", x = NULL, y = NULL, title = "Work from home, May 2020") +
  theme(legend.position = "bottom")
p
ggsave("wfh_may20_detailed_groups_perc.png",p,path = fig_path,width = 12, height = 8)

# plot work at all over time by group ----

p <- df_sample %>%
  filter(!is.na(hours) & !hours%in%c(-9,-2,-1) & sempderived>=0 & base_jbsic07_cc >=0) %>%
  group_by(group_industry_based, wave) %>% 
  summarize(work = mean(hours > 0)) %>% 
  ggplot(aes(x = factor(wave, levels = c("2019", "baseline", "ca", "cb", "cc", "cd", "ce", "cf", "cg","ch", "ci"),
                        labels = c("2019","Jan-Feb 20","Apr 20", "May 20", "Jun 20", "Jul 20", "Sep 20", "Nov 20",
                                   "Jan 21", "Mar 21", "Sep 21")), y = work, color = group_industry_based)) +
  geom_point()+
  theme_minimal()+
  scale_y_continuous(labels = percent_format()) +
  labs(color = NULL, x = NULL, y = "Worked at all last week", title = "Worked last week") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1))
p
ggsave("worked_at_all_groups_overtime.png",p,path = fig_path,width = 12, height = 8)

# by detailed group
p <- df_sample %>%
  filter(!is.na(hours) & !hours%in%c(-9,-2,-1) & sempderived>=0 & base_jbsic07_cc >=0) %>%
  group_by(group_industry_based_detailed, wave) %>% 
  summarize(work = mean(hours > 0)) %>% 
  ggplot(aes(x = factor(wave, levels = c("2019", "baseline", "ca", "cb", "cc", "cd", "ce", "cf", "cg","ch", "ci"),
                        labels = c("2019","Jan-Feb 20","Apr 20", "May 20", "Jun 20", "Jul 20", "Sep 20", "Nov 20",
                                   "Jan 21", "Mar 21", "Sep 21")), y = work, color = group_industry_based_detailed, shape = group_industry_based_detailed)) +
  geom_point()+
  theme_minimal()+
  scale_y_continuous(labels = percent_format()) +
  labs(color = NULL, x = NULL,shape = NULL, y = "Worked at all last week", title = "Worked last week") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1))
p
ggsave("worked_at_all_detailed_groups_overtime.png",p,path = fig_path,width = 12, height = 8)

# plot working hours over time by group ----
p <- df_sample %>%
  filter(!is.na(hours) & !hours%in%c(-9,-2,-1) & sempderived>=0 & base_jbsic07_cc >=0) %>%
  mutate(hours = if_else(hours== -8, 0 ,hours)) %>% 
  group_by(group_industry_based, wave) %>% 
  summarize(work_hours = mean(hours)) %>% 
  ggplot(aes( x = factor(wave, levels = c("2019", "baseline", "ca", "cb", "cc", "cd", "ce", "cf", "cg","ch", "ci"),
                            labels = c("2019","Jan-Feb 20","Apr 20", "May 20", "Jun 20", "Jul 20", "Sep 20", "Nov 20",
                                       "Jan 21", "Mar 21", "Sep 21")),, y = work_hours, color = group_industry_based)) +
  geom_point()+
  theme_minimal()+
  
  labs(color = NULL, x = NULL, y = "Hours worked last week", title = "Hours worked last week") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1))
p
ggsave("hours_groups_overtime.png",p,path = fig_path,width = 12, height = 8)
# by detailed groups
p <- df_sample %>%
  filter(!is.na(hours) & !hours%in%c(-9,-2,-1) & sempderived>=0 & base_jbsic07_cc >=0) %>%
  mutate(hours = if_else(hours== -8, 0 ,hours)) %>% 
  group_by(group_industry_based_detailed, wave) %>% 
  summarize(work_hours = mean(hours)) %>% 
  ggplot(aes( x = factor(wave, levels = c("2019", "baseline", "ca", "cb", "cc", "cd", "ce", "cf", "cg","ch", "ci"),
                         labels = c("2019","Jan-Feb 20","Apr 20", "May 20", "Jun 20", "Jul 20", "Sep 20", "Nov 20",
                                    "Jan 21", "Mar 21", "Sep 21")),, y = work_hours,
              color = group_industry_based_detailed, shape = group_industry_based_detailed)) +
  geom_point()+
  theme_minimal()+
  
  labs(color = NULL, x = NULL,shape = NULL, y = "Hours worked last week", title = "Hours worked last week") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1))
p
ggsave("hours_detialed_groups_overtime.png",p,path = fig_path,width = 12, height = 8)
# plot - WFH, over time, by group ----
p <- df_sample %>%
  filter(wave != "2019" & !is.na(sempderived) & sempderived > 0  & base_jbsic07_cc >=0 ) %>%
  add_count(group_industry_based) %>%
  mutate(industry = if_else(n<25,"other",industry),
         WFH = case_when(sempderived %in% c(4) ~ "Not employed",
                         wah == 1 ~ "Always",
                         wah == 2 ~ "Often",
                         wah == 3 ~ "Sometimes",
                         wah == 4 ~ "Never",
                         T ~ NA)) %>%
  filter(!is.na(WFH)) %>% 
  group_by(group_industry_based) %>%
  mutate(
    n_ind = n(),
    share_always = mean(WFH == "Always", na.rm = T),
    industry_lab = paste0(group_industry_based, " (n=", n_ind, ")"),
    WFH = factor(
      WFH,
      levels = c(
        "Always", "Often", "Sometimes","Never", "Not employed"  )
    )
  ) %>% ungroup() %>%
  ggplot(aes(
    x = factor(wave, levels = c("2019", "baseline", "ca", "cb", "cc", "cd", "ce", "cf", "cg","ch", "ci"),
                labels = c("2019","Jan-Feb 20","Apr 20", "May 20", "Jun 20", "Jul 20", "Sep 20", "Nov 20",
                           "Jan 21", "Mar 21", "Sep 21")),
    fill = WFH
  )) +
  geom_bar(position = position_fill(reverse = TRUE)) +
  facet_grid(~group_industry_based) +
  scale_y_continuous(labels = percent_format()) +
  labs(fill = "Work from home", x = NULL, y = NULL, title = "Work from home") +
  theme_minimal() +
  theme(legend.position = "bottom") 
p
ggsave("wfh_groups_overtime.png",p,path = fig_path,width = 12, height = 8)

# detailed groups

p <- df_sample %>%
  filter(wave != "2019" & !is.na(sempderived) & sempderived > 0  & base_jbsic07_cc >=0 ) %>%
  add_count(group_industry_based_detailed) %>%
  mutate(industry = if_else(n<25,"other",industry),
         WFH = case_when(sempderived %in% c(4) ~ "Not employed",
                         wah == 1 ~ "Always",
                         wah == 2 ~ "Often",
                         wah == 3 ~ "Sometimes",
                         wah == 4 ~ "Never",
                         T ~ NA)) %>%
  filter(!is.na(WFH)) %>% 
  group_by(group_industry_based_detailed) %>%
  mutate(
    n_ind = n(),
    share_always = mean(WFH == "Always", na.rm = T),
    industry_lab = paste0(group_industry_based_detailed, " (n=", n_ind, ")"),
    WFH = factor(
      WFH,
      levels = c(
        "Always", "Often", "Sometimes","Never", "Not employed"  )
    )
  ) %>% ungroup() %>%
  ggplot(aes(
    x = factor(wave, levels = c("2019", "baseline", "ca", "cb", "cc", "cd", "ce", "cf", "cg","ch", "ci"),
               labels = c("2019","Jan-Feb 20","Apr 20", "May 20", "Jun 20", "Jul 20", "Sep 20", "Nov 20",
                          "Jan 21", "Mar 21", "Sep 21")),
    fill = WFH
  )) +
  geom_bar(position = position_fill(reverse = TRUE)) +
  facet_grid(~group_industry_based_detailed) +
  scale_y_continuous(labels = percent_format()) +
  labs(fill = "Work from home", x = NULL, y = NULL, title = "Work from home") +
  theme_minimal() +
  theme(legend.position = "bottom") 
p
ggsave("wfh_detialed_groups_overtime.png",p,path = fig_path,width = 12, height = 8)


# explore working last week and wfh ----

p <- df_sample %>% 
  filter(wave == "ca" & sempderived >0) %>% 
  mutate(WFH = case_when(sempderived %in% c(4) ~ "Not employed",
                         wah == 1 ~ "Always",
                         wah == 2 ~ "Often",
                         wah == 3 ~ "Sometimes",
                         wah == 4 ~ "Never",
                         T ~ NA),
         worked_last_week = if_else(hours>0,T,F),
         WFH = factor(
           WFH,
           levels = c(
             "Always", "Often", "Sometimes","Never", "Not employed"  ))) %>% 
  add_count(worked_last_week) %>% 
  mutate(worked_last_week = if_else(worked_last_week,paste0("Worked last week", " (n=", n, ")" ),paste0("Didn't work last week", " (n=", n, ")" )) ) %>% 
  ggplot(aes(
    x = worked_last_week,
    fill = WFH
  )) +
  geom_bar(position = position_fill(reverse = TRUE)) +
  scale_y_continuous(labels = percent_format()) +
  labs(fill = "Work from home", x = NULL, y = NULL, title = "WFH by hours worked, April 2020") +
  theme_minimal() +
  theme(legend.position = "bottom") 
  
ggsave("worked_at_all_wfh_april20.png",p, path = fig_path, width = 12, height = 8 )           


# explore keyworker definitions ----


p <- df_sample %>% 
  filter(wave == "ca" & sempderived >0) %>% 
  mutate(keyworker_slf = case_when(keyworker == 1 ~ "Yes",
                                   keyworker == 2 ~ "No",
                                   sempderived == 4 ~ "Not employed",
                                   T ~ "Missing"),
         keyworker_slf = factor(keyworker_slf, levels = c("Yes", "No", "Not employed", "Missing"))) %>% 
  
  ggplot(aes(
    x = group_industry_based,
    fill = keyworker_slf
  )) +
  geom_bar(position = position_fill(reverse = TRUE)) +
  scale_y_continuous(labels = percent_format()) +
  labs(fill = "Self reported key worker status", x = NULL, y = NULL,
       title = "Industry based groups and keyworker status, April 2020") +
  theme_minimal() +
  theme(legend.position = "bottom") 
p
ggsave("keyworker_def_explore_groups_april20.png",p, path = fig_path, width = 12, height = 8 )           

p <- df_sample %>% 
  filter(wave == "ca" & sempderived >0) %>% 
  mutate(keyworker_slf = case_when(keyworker == 1 ~ "Yes",
                                   keyworker == 2 ~ "No",
                                   sempderived == 4 ~ "Not employed",
                                   T ~ "Missing"),
         keyworker_slf = factor(keyworker_slf, levels = c("Yes", "No", "Not employed", "Missing"))) %>% 
  
  ggplot(aes(
    x = group_industry_based_detailed,
    fill = keyworker_slf
  )) +
  geom_bar(position = position_fill(reverse = TRUE)) +
  scale_y_continuous(labels = percent_format()) +
  labs(fill = "Self reported key worker status", x = NULL, y = NULL,
       title = "Industry based groups and keyworker status, April 2020") +
  theme_minimal() +
  theme(legend.position = "bottom")
p
ggsave("keyworker_def_explore_detailed_groups_april20.png",p, path = fig_path, width = 12, height = 8 )           


p <- df_sample %>% 
  filter(wave == "cb" & sempderived >0) %>% 
  mutate(keyworker_slf = case_when(keyworksector %in% 1:8 ~ "Yes",
                                   keyworksector == 9 ~ "No",
                                   sempderived == 4 ~ "Not employed",
                                   T ~ "Missing"),
         keyworker_slf = factor(keyworker_slf, levels = c("Yes", "No", "Not employed", "Missing"))) %>% 
  
  ggplot(aes(
    x = group_industry_based,
    fill = keyworker_slf
  )) +
  geom_bar(position = position_fill(reverse = TRUE)) +
  scale_y_continuous(labels = percent_format()) +
  labs(fill = "Self reported key worker status", x = NULL, y = NULL,
       title = "Industry based groups and keyworker status, May 2020") +
  theme_minimal() +
  theme(legend.position = "bottom") 
p
ggsave("keyworker_def_explore_groups_may20.png",p, path = fig_path, width = 12, height = 8 )           

p <- df_sample %>% 
  filter(wave == "cb" & sempderived >0) %>% 
  mutate(keyworker_slf = case_when(keyworksector %in% 1:8 ~ "Yes",
                                   keyworksector == 9 ~ "No",
                                   sempderived == 4 ~ "Not employed",
                                   T ~ "Missing"),
         keyworker_slf = factor(keyworker_slf, levels = c("Yes", "No", "Not employed", "Missing"))) %>% 
  
  ggplot(aes(
    x = group_industry_based_detailed,
    fill = keyworker_slf
  )) +
  geom_bar(position = position_fill(reverse = TRUE)) +
  scale_y_continuous(labels = percent_format()) +
  labs(fill = "Self reported key worker status", x = NULL, y = NULL,
       title = "Industry based groups and keyworker status, May 2020") +
  theme_minimal() +
  theme(legend.position = "bottom")
p
ggsave("keyworker_def_explore_detailed_groups_may20.png",p, path = fig_path, width = 12, height = 8 ) 

p <- df_sample %>% 
  filter(wave == "cb" & sempderived >0) %>% 
  mutate(keyworker_slf = case_when(keyworksector == 1 ~ "Health and social care",
                                   keyworksector == 2 ~ "Education and childcare",
                                   keyworksector == 3 ~ "Key public services",
                                   keyworksector == 4 ~ "Local and national government",
                                   keyworksector == 5 ~ "Food and other necessary goods",
                                   keyworksector == 6 ~ "Public safety and national security",
                                   keyworksector == 7 ~ "Transport",
                                   keyworksector == 8 ~ "Utilities, communications and financial services",
                                   keyworksector == 9 ~ "Not key worker",
                                   sempderived == 4 ~ "Not employed",
                                   T ~ "Missing"),
         keyworker_slf = factor(keyworker_slf, levels = c("Health and social care", "Education and childcare", "Key public services",
                                                          "Local and national government", "Food and other necessary goods", "Public safety and national security",
                                                          "Transport","Utilities, communications and financial services","Not key worker",
                                                          "Not employed", "Missing"))) %>% 
  
  ggplot(aes(
    x = group_industry_based_detailed,
    fill = keyworker_slf
  )) +
  geom_bar(position = position_fill(reverse = TRUE)) +
  scale_y_continuous(labels = percent_format()) +
  labs(fill = "Self reported key keywork sector", x = NULL, y = NULL,
       title = "Industry based groups and keywork sector, May 2020") +
  theme_minimal() +
  theme(legend.position = "bottom")
p
ggsave("keywork_sector_def_explore_detailed_groups_may20.png",p, path = fig_path, width = 12, height = 8 ) 

# Plot workoutside ----

p <- df_sample %>% 
  filter(wave != "2019") %>% 
  group_by(wave) %>% 
  summarize(workoutside = mean(workoutside, na.rm = T))%>% 
  ggplot(aes( x = factor(wave, levels = c("2019", "baseline", "ca", "cb", "cc", "cd", "ce", "cf", "cg","ch", "ci"),
                         labels = c("2019","Jan-Feb 20","Apr 20", "May 20", "Jun 20", "Jul 20", "Sep 20", "Nov 20",
                                    "Jan 21", "Mar 21", "Sep 21")), y = workoutside)) +
  geom_point()+
  theme_minimal()+
  scale_y_continuous(labels = percent_format()) +
  
  labs(color = NULL, x = NULL, y = "% Work outside last week", title = "Work outside") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1))
p
ggsave("workoutside_overtime.png",p, path = fig_path, width = 12, height = 8 ) 

# by group

p <- df_sample %>% 
  filter(wave != "2019") %>% 
  group_by(wave, group_industry_based) %>% 
  summarize(workoutside = mean(workoutside, na.rm = T))%>% 
  ggplot(aes( x = factor(wave, levels = c("2019", "baseline", "ca", "cb", "cc", "cd", "ce", "cf", "cg","ch", "ci"),
                         labels = c("2019","Jan-Feb 20","Apr 20", "May 20", "Jun 20", "Jul 20", "Sep 20", "Nov 20",
                                    "Jan 21", "Mar 21", "Sep 21")), y = workoutside,
              color = group_industry_based, shape = group_industry_based)) +
  geom_point()+
  theme_minimal()+
  scale_y_continuous(labels = percent_format()) +
  
  labs(color = NULL, x = NULL, y = "% Work outside last week", title = "Work outside", shape = NULL) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1))
p
ggsave("workoutside_overtime_groups.png",p, path = fig_path, width = 12, height = 8 ) 

# by detailed group

p <- df_sample %>% 
  filter(wave != "2019") %>% 
  group_by(wave, group_industry_based_detailed) %>% 
  summarize(workoutside = mean(workoutside, na.rm = T))%>% 
  ggplot(aes( x = factor(wave, levels = c("2019", "baseline", "ca", "cb", "cc", "cd", "ce", "cf", "cg","ch", "ci"),
                         labels = c("2019","Jan-Feb 20","Apr 20", "May 20", "Jun 20", "Jul 20", "Sep 20", "Nov 20",
                                    "Jan 21", "Mar 21", "Sep 21")), y = workoutside,
              color = group_industry_based_detailed, shape = group_industry_based_detailed)) +
  geom_point()+
  theme_minimal()+
  scale_y_continuous(labels = percent_format()) +
  
  labs(color = NULL, x = NULL, y = "% Work outside last week", title = "Work outside", sahpe = NULL) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1))
p
ggsave("workoutside_overtime_detailed_groups.png",p, path = fig_path, width = 12, height = 8 ) 

df_sample <- df_sample %>% 
  mutate(group_industry_based_detailed = relevel(factor(group_industry_based_detailed), ref = "other"))

lm(workoutside ~ factor(base_jbsic07_cc) + factor(base_jbsoc10_cc), data = df_sample %>% filter(wave == "ca")) %>% 
  summary()

lm(workoutside ~ factor(group_industry_based_detailed) , data = df_sample %>% filter(wave == "ca")) %>% 
  summary()

lm(workoutside ~ factor(group_industry_based_detailed)+base_age_dv + I(base_age_dv^2)+ I(base_age_dv^3) + factor(base_isced11_dv ) , data = df_sample %>% filter(wave == "ca")) %>% 
  summary()

lm(workoutside ~ factor(group_industry_based_detailed)+base_age_dv + I(base_age_dv^2)+ I(base_age_dv^3) + factor(base_isced11_dv ) , data = df_sample %>% filter(wave == "ca" & base_sex == 1)) %>% 
  summary()

lm(workoutside ~ factor(group_industry_based_detailed)+base_age_dv + I(base_age_dv^2)+ I(base_age_dv^3) + factor(base_isced11_dv ) , data = df_sample %>% filter(wave == "ca" & base_sex == 2)) %>% 
  summary()


cxc