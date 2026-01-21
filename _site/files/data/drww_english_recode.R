# ---- Packages ----

library(foreign)
library(tidyverse)

# ---- Data ----

## Download raw data from: 
## https://github.com/dorussianswantwar/research1

raw_drww <- read.spss("https://github.com/dorussianswantwar/research1/raw/main/spss.sav",to.data.frame = T)

## Plug in names to Google Translate
names(raw_drww)

# ---- Rename Data ----

## Rename raw data, save output to `df_drww`
raw_drww %>%
  rename(
    # --- Demographics ---
    "sex" = "пол",
    "age" = "возраст",
    # --- War In Ukraine ---
    "support_war" = "поддержка",
    "trust_gov" = "доверие",
    # --- Employment  ---
    "employ_working" = "работает",
    "employ_student" = "учится",
    "employ_retired" = "пенсия",
    "employ_maternity_leave" = "декрет",
    "employ_homemaker" = "домашнее_хозяйство",
    "employ_unemployed" = "безработный",
    "employ_other_employment" = "другое_занятость",
    "other_employ_open_response" = "другое_что_именно",
    # --- Education  ---
    "education" = "образование",
    # --- Social Media Use  ---
    "social_classmates" = "Одноклассники",
    "social_in_contact_with" = "ВКонтакте",
    "social_facebook" =  "Фейсбук",
    "social_instagram" = "Инстаграм",
    "social_twitter" = "Твиттер",
    "social_telegram" = "Телеграм",
    "social_whatsapp" = "Вотсап",
    "social_viber" = "Вайбер",
    "social_tiktok" = "ТикТок",
    "social_other_social_networks" = "Другое_соцсети",
    "none_social" = "Никуда",
    "dk_social" = "Затрудняюсь",
    "other_social_group" = "Другое_группа",
    "other_social_open_response" = "Другое_соцсети_что_именно",
    # --- Geography  ---
    "geo_urban_rural" = "тнп",
    "geo_district" = "округа",
    "weight" = "вес"
    ) -> df_drww



# ---- Recode Data ----

df_drww %>%
  mutate(
    # --- Demographics  ---
    sex = ifelse(sex == "Мужской", "Male","Female"),
    # --- War  ---
    support_war_f = case_when(
      support_war == levels(support_war)[1] ~ "Yes",
      support_war == levels(support_war)[2] ~ "No",
      T ~ NA_character_
    ),
    support_war_f = factor(support_war_f, levels = c("Yes","No")),
    support_war01 = case_when(
      support_war == levels(support_war)[1] ~ 1,
      support_war == levels(support_war)[2] ~ 0,
      T ~ NA_real_
    ),
    trust_f = case_when(
      trust_gov == levels(trust_gov)[1] ~ "Fully",
      trust_gov == levels(trust_gov)[2] ~ "Largely",
      trust_gov == levels(trust_gov)[3] ~ "Partly",
      trust_gov == levels(trust_gov)[4] ~ "Not at all",
      T ~ NA_character_
    ),
    trust_f = factor(trust_f, levels =  c("Fully", "Largely", "Partly", "Not at all")),
    trust_n = case_when(
      trust_gov == levels(trust_gov)[1] ~ 3,
      trust_gov == levels(trust_gov)[2] ~ 2,
      trust_gov == levels(trust_gov)[3] ~ 1,
      trust_gov == levels(trust_gov)[4] ~ 0,
      T ~ NA_real_
    ),
    # --- Education ---
    eductation_f = case_when(
      education == levels(education)[1] ~ "Primary school",
      education == levels(education)[2] ~ "Secondary school",
      education == levels(education)[3] ~ "Vocational",
      education == levels(education)[4] ~ "College or some college",
      education == levels(education)[5] ~ "Graduate degree",
      T ~ NA_character_
    ),
    eductation_f = factor(education, levels = c("Primary school",
                                                "Secondary school",
                                                "Vocational",
                                                "College or some college",
                                                "Graduate degree")),
    eductation_n = case_when(
      education == levels(education)[1] ~ 1,
      education == levels(education)[2] ~ 2,
      education == levels(education)[3] ~ 3,
      education == levels(education)[4] ~ 4,
      education == levels(education)[5] ~ 5,
      T ~ NA_real_
    ),
    # --- Geography ---
    geo_urban_rural_f = case_when(
      geo_urban_rural == levels(geo_urban_rural)[1] ~ "Urban", # City
      geo_urban_rural == levels(geo_urban_rural)[2] ~ "Rural", # Village
      T ~ NA_character_
    ),
    geo_urban_rural_f = factor(geo_urban_rural_f, levels = c("Urban", "Rural")),
    geo_district_f = case_when(
      geo_district == levels(geo_district)[1] ~ "Central Federal District",
      geo_district == levels(geo_district)[2] ~ "Northwestern Federal District",
      geo_district == levels(geo_district)[3] ~ "Southern Federal District",
      geo_district == levels(geo_district)[4] ~ "North Caucasian Federal District",
      geo_district == levels(geo_district)[5] ~ "Volga Federal District",
      geo_district == levels(geo_district)[6] ~ "Ural Federal District",
      geo_district == levels(geo_district)[7] ~ "Siberian Federal District",
      geo_district == levels(geo_district)[1] ~ "Far Eastern Federal District",
      T ~ NA_character_
    ),
    geo_district_f = factor(geo_district_f, levels = c("Central Federal District", 
                                                       "Northwestern Federal District", 
                                                       "Southern Federal District", 
                                                       "North Caucasian Federal District", 
                                                       "Volga Federal District", 
                                                       "Ural Federal District", "Siberian Federal District", 
                                                       "Far Eastern Federal District") 
                            )
  ) %>%
  # --- Employment Indicators ---
  mutate_at(vars(starts_with("employ_")), function(x) ifelse(x == "НЕ ВЫБРАН", 0, 1)) %>%
  # --- Social Media Use Indicators ---
  mutate_at(vars(starts_with("social_")), function(x) ifelse(x == "НЕ ВЫБРАН", 0, 1)) %>%
  mutate(
    social_youtube = ifelse(other_social_group == levels(df_drww$other_social_group)[1], 1, 0 ),
    social_yandex = ifelse(other_social_group == levels(df_drww$other_social_group)[2], 1, 0 ),
    total_social_media_use = rowSums(select(., starts_with("social_")),na.rm = T),
    # Count none and don't know as no social media
    no_social_media = case_when(
      none_social == "ВЫБРАН" ~ 1,
      dk_social == "ВЫБРАН" ~ 1,
      T ~ 0
    )
  )->
  df_drww

# ---- Save Data ----
save(df_drww, file = "df_drww.rda")

