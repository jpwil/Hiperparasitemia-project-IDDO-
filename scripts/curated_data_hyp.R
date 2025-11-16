pacman:: p_load(janitor, tidyverse, flextable, rio, here, gtsummary, epikit)
#######################################################################################################################################################################

data_selected <- import("chosen_studies.xlsx")
data_clinical_mapper <- import(here("MAPPER", "Clinical.csv _new.csv"))
data_outcome_mapper <- import(here("MAPPER", "Malaria_parasitaemia.csv _new.csv"))
data_pcr_mapper <- import(here("MAPPER", "Malaria_pcr.csv _new.csv"))
data_subject_mapper <- import(here("MAPPER", "Subject.csv _new.csv"))
data_treatment_mapper <- import(here("MAPPER", "treatment.csv _new.csv"))
data_parasitaemia_mapper <- import(here("MAPPER", "Malaria_parasitaemia.csv _new.csv"))


### Vector with ID of studies chosen
data_selected_2 <- c(data_selected$sid)

### Filter and curated data using ID of studies chosen.
### 1. Clinical
data_clinical_mapper_2 <- data_clinical_mapper %>% 
  filter(sid == data_selected_2)

### 2. Outcome
data_outcome_mapper_2 <- data_outcome_mapper %>% 
  filter(sid == data_selected_2)

### 3. PCR
data_pcr_mapper_2 <- data_pcr_mapper %>% 
  filter(sid == data_selected_2)

### 4. Treatment
data_treatment_mapper_2 <- data_treatment_mapper %>% 
  filter(sid == data_selected_2)

### 5. Subject
data_subject_mapper_2 <-  data_subject_mapper %>% 
  filter(sid == data_selected_2)

### Joining databases and involve all variables
data_cons_mapper <- full_join(data_subject_mapper_2, data_clinical_mapper_2, by = c("pid", "sid", "site"))
data_cons_mapper_2 <- full_join(data_cons_mapper, data_outcome_mapper_2, by = c("pid", "sid", "site"))
data_cons_mapper_3 <- full_join(data_cons_mapper_2, data_pcr_mapper_2, by = c("pid", "sid", "site"))
data_cons_mapper_4 <- full_join(data_cons_mapper_3, data_treatment_mapper_2, by = c("pid", "sid", "site"))

### Review information available so far. 
colnames(data_cons_mapper_4)

data_cons_mapper_5 <- data_cons_mapper_4 %>% 
  mutate(dayofobs_1 = if_else(is.na(dayofobs.x)|dayofobs.x == 0, dayofobs.y, dayofobs.x),
         dayofobs_2 = if_else(is.na(dayofobs_1)|dayofobs_1 == 0, dayofobs.x.x, dayofobs_1),
         dayofobs_3 = if_else(is.na(dayofobs_2)|dayofobs_2 == 0, dayofobs.y.y, dayofobs_2),
         hourofobs_1 = if_else(is.na(hourofobs.x)|hourofobs.x == 0, hourofobs.y, hourofobs.x),
         hourofobs_2 = if_else(is.na(hourofobs_1)|hourofobs_1 == 0, hourofobs.x.x, hourofobs_1),
         hourofobs_3 = if_else(is.na(hourofobs_2)|hourofobs_2 == 0, hourofobs.y.y, hourofobs_2)) %>% 
  select(-c(dayofobs_1, dayofobs_2, dayofobs.x, dayofobs.x.x, dayofobs.y, dayofobs.y.y, hourofobs_1, 
         hourofobs_2, hourofobs.x, hourofobs.y, hourofobs.x.x, hourofobs.y.y)) %>% 
  rename("dayofobs" = dayofobs_3,
         "hourofobs" = hourofobs_3)

### Evaluating duplicates
data_cons_mapper_5 <- data_cons_mapper_5 %>% 
  distinct(pid, sid, site, .keep_all = T)

data_cons_mapper_5 %>% 
  group_by(sid) %>% 
  summarise(n = n_distinct(sid), .groups = "drop")

### Filter and curated database
data_final <- data_cons_mapper_5 %>% 
  filter((!is.na(pfmicl)|!is.na(gfmicl)|!is.na(gfmicl2))) %>% 
  filter(!str_detect(pfmicl, "0|NA|N") | !str_detect(gfmicl, "0|NA|N")) %>%
  mutate(pfmicl = if_else(str_detect(pfmicl, "0|NA|N"), "0", pfmicl),
         gfmicl = if_else(str_detect(gfmicl, "0|NA|N"), "0", gfmicl),
         pfmicl = as.numeric(pfmicl),
         gfmicl = as.numeric(gfmicl)) %>%  
  mutate(parasite_count = round(as.numeric(pfmicl) + as.numeric(gfmicl), 0))

data_subject_mapper_3 <- data_subject_mapper %>%
  filter(sid == "GPXJK") %>% 
  select(sid, pid, site, dayofobs)

data_outcome_mapper_3 <- data_outcome_mapper %>% 
  filter(sid == "GPXJK") %>% 
  select(sid, pid, site, dayofobs, pfmicl)

data_pcr_mapper_3<- data_pcr_mapper %>% 
  filter(sid == "GPXJK")

data_clinical_mapper_3<- data_clinical_mapper %>% 
  filter(sid == "GPXJK")

data_parasitaemia_mapper_2 <- data_parasitaemia_mapper %>% 
  filter(sid == "GPXJK") %>% 
  select()

data_treatment_mapper_3 <- data_treatment_mapper %>% 
  filter(sid == "GPXJK")


data_pcr_mapper_3 %>% 
  tabyl(pcr)
  

##############################################################################################################################################################################

### Checking information from SDTM 

data_DM <- import(here("SDTM", "DM 2025-01-10.csv"))
data_DS <- import(here("SDTM", "DS 2025-01-10.csv"))
data_IN <- import(here("SDTM", "IN 2025-01-10.csv"))            
