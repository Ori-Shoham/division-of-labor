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

key_workers <- readxl::read_excel("C:/Users/USER/Dropbox/WFH_covid/UK project/policies/keyworkersreferencetableupdated2.xlsx", sheet = 4)
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
path_main  <- "understanding society uk all data/UKDA-6614-stata/stata/stata13_se/ukhls"  # SN 6614
path_covid <- "understanding society uk all data/UKDA-8644-stata/stata/stata13_se/"  # SN 8644

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
    
    # --- EMPLOYMENT ---
    paste0(prefix, "_jbstat"),        # Status
    paste0(prefix, "_jbsic07_cc"),       # Industry (SIC)
    paste0(prefix, "_jbsoc10_cc"),       # Occupation (SOC)
    paste0(prefix, "_jbft_dv"),          # Full-time/Part-time
    paste0(prefix, "_jbhrs_dv"),         # Hours
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
  rename_with(~ paste0("base_", .), -pidp) # Prefix everything with 'base_'

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
  mutate(group = case_when(shutdown_sec == 1 ~ "shutdown sector",
                           keyworker == 1 | keyworksector %in% 1:8 ~ "key worker",
                           T ~ "other"),
         group_my_def = case_when(shutdown_sec == 1 ~ "shutdown sector",
                                  keyworker_my_def == 1 ~ "key worker",
                                  T ~ "other"))

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
  add_count(group_my_def) %>%
  ggplot(aes(x = fct_infreq(group_my_def), fill = factor(hours>0, levels = c(T,F), labels = c("Yes", "No") ))) +
  geom_bar() +
  coord_flip()+scale_x_discrete(limits = rev)+labs(fill = "Worked at all last week", x = element_blank(), title = "Worked last week, April 2020")+
  theme(legend.position = "bottom")
p

ggsave("worked_at_all_april20_groups.png",p,path = fig_path,width = 12, height = 8)

# in percentages
p <- df_sample %>%
  filter(wave == "ca" & !is.na(hours) & !hours%in%c(-9,-2,-1) & sempderived>=0 & base_jbsic07_cc >=0) %>%
  add_count(group_my_def) %>%
  group_by(group_my_def) %>%
  mutate(
    n_ind = n(),
    share_yes = mean(hours > 0),
    industry_lab = paste0(group_my_def, " (n=", n_ind, ")")
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

# plot - work status, april 2020, by group ----

p <- df_sample %>%
  filter(wave == "ca" & !is.na(sempderived) & base_jbsic07_cc >=0) %>%
  add_count(group_my_def) %>%
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
  ggplot(aes(x = fct_infreq(group_my_def), fill =status)) +
  geom_bar(position= position_stack(reverse = TRUE)) +
  coord_flip()+scale_x_discrete(limits = rev)+labs(fill = "Work status", x = element_blank(), title = "Work status, April 2020")+
  theme(legend.position = "bottom")
p
ggsave("work_status_april20_groups.png",p,path = fig_path,width = 14, height = 8)

# in percentages
p <- df_sample %>%
  filter(wave == "ca" & !is.na(sempderived)& base_jbsic07_cc >=0) %>%
  add_count(group_my_def) %>%
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
  group_by(group_my_def) %>%
  mutate(
    n_ind = n(),
    share_not = mean(status == "Not employed"),
    industry_lab = paste0(group_my_def, " (n=", n_ind, ")"),
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
  add_count(group_my_def) %>%
  mutate(
         furlough = case_when(sempderived %in% c(2,4) ~ "Self- or not employed",
                              furlough == 1 ~ "Yes",
                              furlough == 2 ~ "No",
                              T ~ NA)) %>%
  filter(!is.na(furlough)) %>% 
  group_by(group_my_def) %>%
  mutate(
    n_ind = n(),
    share_fur = mean(furlough == "Yes", na.rm = T),
    industry_lab = paste0(group_my_def, " (n=", n_ind, ")"),
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

# plot - WFH, april 2020, by group ----
p <- df_sample %>%
  filter(wave == "ca" & !is.na(sempderived) & sempderived > 0  & base_jbsic07_cc >=0 ) %>%
  add_count(group_my_def) %>%
  mutate(industry = if_else(n<25,"other",industry),
         WFH = case_when(sempderived %in% c(4) ~ "Not employed",
                              wah == 1 ~ "Always",
                              wah == 2 ~ "Often",
                         wah == 3 ~ "Sometimes",
                         wah == 4 ~ "Never",
                              T ~ NA)) %>%
  filter(!is.na(WFH)) %>% 
  group_by(group_my_def) %>%
  mutate(
    n_ind = n(),
    share_always = mean(WFH == "Always", na.rm = T),
    industry_lab = paste0(group_my_def, " (n=", n_ind, ")"),
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

# plot - working any hour last week, may 2020, by group  ----
p <- df_sample %>%
  filter(wave == "cb" & !is.na(hours) & !hours%in%c(-9,-2,-1) & sempderived>=0 & base_jbsic07_cc >=0) %>%
  add_count(group_my_def) %>%
  ggplot(aes(x = fct_infreq(group_my_def), fill = factor(hours>0, levels = c(T,F), labels = c("Yes", "No") ))) +
  geom_bar() +
  coord_flip()+scale_x_discrete(limits = rev)+labs(fill = "Worked at all last week", x = element_blank(), title = "Worked last week, May 2020")+
  theme(legend.position = "bottom")
p

ggsave("worked_at_all_may20_groups.png",p,path = fig_path,width = 12, height = 8)

# in percentages
p <- df_sample %>%
  filter(wave == "cb" & !is.na(hours) & !hours%in%c(-9,-2,-1) & sempderived>=0 & base_jbsic07_cc >=0) %>%
  add_count(group_my_def) %>%
  group_by(group_my_def) %>%
  mutate(
    n_ind = n(),
    share_yes = mean(hours > 0),
    industry_lab = paste0(group_my_def, " (n=", n_ind, ")")
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

# plot - work status, may 2020, by group ----

p <- df_sample %>%
  filter(wave == "cb" & !is.na(sempderived) & base_jbsic07_cc >=0) %>%
  add_count(group_my_def) %>%
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
  ggplot(aes(x = fct_infreq(group_my_def), fill =status)) +
  geom_bar(position= position_stack(reverse = TRUE)) +
  coord_flip()+scale_x_discrete(limits = rev)+labs(fill = "Work status", x = element_blank(), title = "Work status, May 2020")+
  theme(legend.position = "bottom")
p
ggsave("work_status_may20_groups.png",p,path = fig_path,width = 14, height = 8)

# in percentages
p <- df_sample %>%
  filter(wave == "cb" & !is.na(sempderived)& base_jbsic07_cc >=0) %>%
  add_count(group_my_def) %>%
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
  group_by(group_my_def) %>%
  mutate(
    n_ind = n(),
    share_not = mean(status == "Not employed"),
    industry_lab = paste0(group_my_def, " (n=", n_ind, ")"),
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
  add_count(group_my_def) %>%
  mutate(industry = if_else(n<25,"other",industry),
         WFH = case_when(sempderived %in% c(4) ~ "Not employed",
                         wah == 1 ~ "Always",
                         wah == 2 ~ "Often",
                         wah == 3 ~ "Sometimes",
                         wah == 4 ~ "Never",
                         T ~ NA)) %>%
  filter(!is.na(WFH)) %>% 
  group_by(group_my_def) %>%
  mutate(
    n_ind = n(),
    share_always = mean(WFH == "Always", na.rm = T),
    industry_lab = paste0(group_my_def, " (n=", n_ind, ")"),
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

# plot work at all over time by group ----

p <- df_sample %>%
  filter(!is.na(hours) & !hours%in%c(-9,-2,-1) & sempderived>=0 & base_jbsic07_cc >=0) %>%
  group_by(group_my_def, wave) %>% 
  summarize(work = mean(hours > 0)) %>% 
  ggplot(aes(x = wave, y = work, color = group_my_def)) +
  geom_point()+
  theme_minimal()+
  scale_y_continuous(labels = percent_format()) +
  scale_x_discrete(labels = c("Apr 20", "May 20", "Jun 20", "Jul 20", "Sep 20", "Nov 20",
                              "Jan 21", "Mar 21", "Sep 21")) + 
  labs(color = NULL, x = NULL, y = "Worked at all last week", title = "Worked last week") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1))
p
ggsave("worked_at_all_groups_overtime.png",p,path = fig_path,width = 12, height = 8)


p <- df_sample %>%
  filter(!is.na(hours) & !hours%in%c(-9,-2,-1) & sempderived>=0 & base_jbsic07_cc >=0) %>%
  mutate(hours = if_else(hours== -8, 0 ,hours)) %>% 
  group_by(group_my_def, wave) %>% 
  summarize(work_hours = mean(hours)) %>% 
  ggplot(aes(x = wave, y = work_hours, color = group_my_def)) +
  geom_point()+
  theme_minimal()+
  scale_x_discrete(labels = c("Apr 20", "May 20", "Jun 20", "Jul 20", "Sep 20", "Nov 20",
                              "Jan 21", "Mar 21", "Sep 21")) + 
  labs(color = NULL, x = NULL, y = "Hours worked last week", title = "Hours worked last week") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1))
p
ggsave("hours_groups_overtime.png",p,path = fig_path,width = 12, height = 8)
