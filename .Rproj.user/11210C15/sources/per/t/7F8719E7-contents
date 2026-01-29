library(tidyverse)
library(haven)
library(data.table)
rm(list = ls())

# explore industry trends in total employment based on ONS data ----

a <- readxl::read_excel("C:/Users/USER/Downloads/jobs02sep2025.xls", sheet = 2,range = "A4:x194")
a <- a[-1,]
a <- a %>% 
  rename(month = "...1") %>% 
  select(!"...2") %>% 
  mutate(across(!month, as.numeric)) %>% 
  mutate(new_month = (1:nrow(a))-167) %>% 
  pivot_longer(!c(month, new_month), names_to = "industry", values_to = "jobs") %>% 
  group_by(industry) %>% 
  mutate(base = sum(jobs*(new_month==0))) %>% 
  mutate(relative_jobs = jobs/base) 

offset_to_yq <- function(x, base_year = 2019L, base_quarter = 4L) {
  if (!is.numeric(x)) {
    stop("x must be a numeric vector (can be integer or double).")
  }
  
  # total quarters since the start of base_year
  total_q <- (base_quarter - 1L) + as.integer(x)
  
  year    <- base_year + total_q %/% 4L
  quarter <- (total_q %% 4L) + 1L
  
  # return character vector like "2018 Q4"
  sprintf("%d Q%d", year, quarter)
}


a %>% 
  filter(new_month >=-15 & new_month<=20& industry!= "Private households 2" &
           industry %in% c("All jobs","Transport & storage","Construction","Professional scientific & technical activities", "Wholesale & retail trade; repair of motor vehicles and motorcycles","Education", "Manufacturing", "Human health & social work activities", "Accommod-ation & food service activities", "Arts, entertainment & recreation", "Administrative & support service activities", "Public admin & defence; compulsory social security1")) %>% 
  ggplot(aes(x = new_month, y = relative_jobs, color = industry)) + 
  geom_line()+
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(
    angle = 45,      # rotate text
    vjust = 0.5,     # vertical justification
    hjust = 1        # horizontal justification
  ))+
  scale_x_continuous(breaks = seq(-14,20,2), labels = offset_to_yq(seq(-14,20,2))) +
  facet_wrap(~industry) +
  geom_vline(xintercept = 0)+
  labs(x = element_blank(), y = "total jobs (2019 Q4 = 1)")

# Explore using understanding society data ----

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
# 2019 baseline data from the consolidated file in the covid study 
base <- read_dta("understanding society uk all data/UKDA-8644-stata/stata/stata13_se/mainstage_data_2019/jk_indresp_cv.dta")
base_19 <- base %>% 
  filter(jk_istrtdaty == 2019 & jk_age_dv >= 18)

base_19_small <- base_19 %>% 
  select(pidp, jk_jbsoc10_cc, jk_jbsic07_cc, jk_jbhrs)

# SOC definitions
SOC <- read.csv("policies/SOC.csv")
SIC <- read.csv("policies/SIC.csv")

# covid data ----
# April 2020 (24th April to April 30th data - all within lockdown)
covid_a_web <- read_dta("understanding society uk all data/UKDA-8644-stata/stata/stata13_se/ca_indresp_w.dta")
# May 2020 (27th May - 2nd June - )
covid_b_phone <- read_dta("understanding society uk all data/UKDA-8644-stata/stata/stata13_se/cb_indresp_t.dta")
covid_b_web <- read_dta("understanding society uk all data/UKDA-8644-stata/stata/stata13_se/cb_indresp_w.dta")
# June 2020 (June 25th - July 1st)
covid_c_web <- read_dta("understanding society uk all data/UKDA-8644-stata/stata/stata13_se/cc_indresp_w.dta")
# July 2020 (July 24th - July 31st)
covid_d_web <- read_dta("understanding society uk all data/UKDA-8644-stata/stata/stata13_se/cd_indresp_w.dta")
# September 2020 (September 24th - October 2nd)
covid_e_web <- read_dta("understanding society uk all data/UKDA-8644-stata/stata/stata13_se/ce_indresp_w.dta")
# November 2020 (November 24th - December 1st)
covid_f_web <- read_dta("understanding society uk all data/UKDA-8644-stata/stata/stata13_se/cf_indresp_w.dta")
# January 2021 (January 27th - February 3rd)
covid_g_web <- read_dta("understanding society uk all data/UKDA-8644-stata/stata/stata13_se/cg_indresp_w.dta")
# March 2021 (March 24th - March 31st)
covid_h_web <- read_dta("understanding society uk all data/UKDA-8644-stata/stata/stata13_se/ch_indresp_w.dta")
# September 2021 (September 23rd - October 1st)
covid_i_web <- read_dta("understanding society uk all data/UKDA-8644-stata/stata/stata13_se/ci_indresp_w.dta")
# September 2021 (September 23rd - October 1st)

# relevant vars -
# pidp - person identifier
# jk_jbsoc10_cc (or other prefix depending on wave) - main job occupation category (SOC2010)
# jk_jbsic07_cc (or other prefix depending on wave) - main job industry category (SIC2007)
# jk_jbhrs (or other prefix depending on wave) - typical weekly working hours
# ca_hours (or other cx prefix indicating interview round in the covid studies) - work hours last week
# ca_sempderived (or other cx prefix indicating interview round in the covid studies) - employed (1 - employed, 2 - self-employed, 3 - both, 4 - no)
# ca_blhours - baseline hours - weekly hours in Jan-Feb 2020
# blwork - employed in Jan-Feb 2020
# ca_furlough (or other cx prefix indicating interview round in the covid studies) - furloughed (1 - yes, 2 - no)
# ca_wah (or other cx prefix indicating interview round in the covid studies) - work from home (1 - always, 2 - often, 3 - sometimes, 4 - never)


# completely shut down sectors
# 55 - accommodation
# 56 - Food and beverage service activities
# 79 - Travel agency, tour operator and other reservation service and related activities
# 90 - Creative, arts and entertainment activities (except ‘artistic creation’ 9003)
# 91 - Libraries, archives, museums and other cultural activities
# 92 - Gambling and betting activities
# 93 - Sports activities and amusement and recreation activities
# 96 - Other personal service activities (except ‘funeral and related activities’ 9603)
# 97 - Activities of households as employers of domestic personnel

shutdown <- c(55,56,79,90, 91, 92, 93, 96, 97)


dat_small <- 
  base_19_small %>% 
  left_join(covid_a_web) %>% 
  left_join(covid_b_web)

dat_small %>% 
  mutate(hours_april_edit = case_when(ca_hours >= 0 ~ ca_hours,
                                ca_hours == -8 ~ 0,
                                T ~ NA),
         hours_may_edit = case_when(cb_hours >= 0 ~ cb_hours,
                                      cb_hours == -8 ~ 0,
                                      T ~ NA),
         blhours_edit = case_when(ca_blhours >= 0 ~ ca_blhours,
                                  ca_blhours == -8 ~0,
                                  T ~ NA)) %>% 
  group_by(jk_jbsoc10_cc) %>% 
  summarize(hours_april = mean(hours_april_edit, na.rm = T),hours_may = mean(hours_may_edit, na.rm = T),
            blhours = mean(blhours_edit, na.rm = T), emp_april = mean(ca_sempderived!= 4, na.rm = T),
            emp_may = mean(cb_sempderived!= 4, na.rm = T), blemp = mean(ca_blwork!= 4, na.rm = T), 
            furlough_april = mean(ca_furlough == 1, na.rm = T), furlough_may = mean(cb_furlough == 1, na.rm = T),
            n_april = sum(!is.na(ca_sempderived)),n_may = sum(!is.na(cb_sempderived))) %>% 
  left_join(SOC, by = c("jk_jbsoc10_cc" = "SOC")) %>% 
  mutate(diff_hours_april = hours_april - blhours, diff_emp_april = emp_april-blemp, diff_hours_may = hours_may - blhours, diff_emp_may = emp_may-blemp) %>% 
  select(jk_jbsoc10_cc, occupation, blhours,hours_april,hours_may, diff_hours_april, diff_hours_may, blemp, emp_april, emp_may, diff_emp_april, diff_emp_may,furlough_april,furlough_may, n_april, n_may) %>% 
  view()

dat_small %>% 
  mutate(hours_april_edit = case_when(ca_hours >= 0 ~ ca_hours,
                                      ca_hours == -8 ~ 0,
                                      T ~ NA),
         hours_may_edit = case_when(cb_hours >= 0 ~ cb_hours,
                                    cb_hours == -8 ~ 0,
                                    T ~ NA),
         blhours_edit = case_when(ca_blhours >= 0 ~ ca_blhours,
                                  ca_blhours == -8 ~0,
                                  T ~ NA)) %>% 
  group_by(jk_jbsic07_cc) %>% 
  summarize(hours_april = mean(hours_april_edit, na.rm = T),hours_may = mean(hours_may_edit, na.rm = T),
            blhours = mean(blhours_edit, na.rm = T), emp_april = mean(ca_sempderived!= 4, na.rm = T),
            emp_may = mean(cb_sempderived!= 4, na.rm = T), blemp = mean(ca_blwork!= 4, na.rm = T), 
            furlough_april = mean(ca_furlough == 1, na.rm = T), furlough_may = mean(cb_furlough == 1, na.rm = T),
            n_april = sum(!is.na(ca_sempderived)),n_may = sum(!is.na(cb_sempderived))) %>% 
  left_join(SIC, by = c("jk_jbsic07_cc" = "SIC")) %>% 
  mutate(diff_hours_april = hours_april - blhours, diff_emp_april = emp_april-blemp, diff_hours_may = hours_may - blhours, diff_emp_may = emp_may-blemp) %>% 
  select(jk_jbsic07_cc, industry, blhours,hours_april,hours_may, diff_hours_april, diff_hours_may, blemp, emp_april, emp_may, diff_emp_april, diff_emp_may,furlough_april,furlough_may, n_april, n_may) %>% 
  view()


