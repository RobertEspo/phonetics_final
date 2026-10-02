# demographic data
demographic_dat <- read_csv(here("data","demographic_dat.csv"))

demographic_dat$age %>% mean()
demographic_dat$age %>% range()

# language use dat
lang_use_dat <- read_csv(here("data","lang_use_dat.csv"))


# load raw data
dat_raw <- read_csv(here("data","formant_dat.csv"))

dat_tidy <- dat_raw %>%
  # remove unnecessary cols
  select(
    file_name:f2_90,
    -word
  ) %>%
  
  # separate file name into participant and two temporary cols
  separate(., file_name, into = c("participant","temp1","temp2"), sep = "_") %>%
  
  # separate temp1 into task type & session number
  extract(temp1, into = c("task","session"),
          regex = "(prodShadow)(\\d+)") %>%
  
  # separate temp2 into word and repetition
  extract(temp2, into = c("item","rep"),
          regex = "([a-zA-Z]+)(\\d*)"
  ) %>%
  
  # recode repetition col
  mutate(
    rep = as.integer(rep),
    rep = if_else(is.na(rep), 1L, rep + 1L),
    
    # add duration
    dur = end_time - start_time,
    
    # add (un)stressed /a/ cols
    unstressed_a = as.factor(if_else(following_phone == "boundary" & phoneme == "a", 1, 0)),
    stressed_a = as.factor(if_else(unstressed_a == 0 & phoneme == "a", 1, 0)),
    
    stress = as.factor(case_when(
      stressed_a == 1 & unstressed_a == 0 ~ "stressed",
      stressed_a == 0 & unstressed_a == 1 ~ "unstressed",
      TRUE ~ NA
    )),

    # calculate central spectroids for f1 & f2
    centroid_f1 = rowMeans(
      across(c(f1_20, f1_30, f1_40, f1_50, f1_60, f1_70, f1_80)),
      na.rm = TRUE
    ),
    centroid_f2 = rowMeans(
      across(c(f2_20, f2_30, f2_40, f2_50, f2_60, f2_70, f2_80)),
      na.rm = TRUE
    ),
    
    # calculate trajectory length (tl) from
    # from individual vowel section length
    
    tl =
      sqrt((f1_20 - f1_30)^2 + (f2_20 - f2_30)^2) +
      sqrt((f1_30 - f1_40)^2 + (f2_30 - f2_40)^2) +
      sqrt((f1_40 - f1_50)^2 + (f2_40 - f2_50)^2) +
      sqrt((f1_50 - f1_60)^2 + (f2_50 - f2_60)^2) +
      sqrt((f1_60 - f1_70)^2 + (f2_60 - f2_70)^2) +
      sqrt((f1_70 - f1_80)^2 + (f2_70 - f2_80)^2),
    
    # clean up col types
    participant = as.factor(participant),
    session = as.numeric(session),
    item = as.factor(item),
    rep = as.factor(rep),
    phoneme = as.factor(phoneme)
  ) %>%
  # get only /a/
  filter(stress %in% c("unstressed","stressed"))

dat_centroid <- dat_tidy %>%
  group_by(participant, session, stress) %>%
  summarise(
    centroid_f1 = mean(centroid_f1, na.rm = TRUE),
    centroid_f2 = mean(centroid_f2, na.rm = TRUE),
    .groups = "drop"
  )

dat_centroid_long <- dat_centroid %>%
  pivot_longer(
    cols = c(centroid_f1, centroid_f2),
    names_to = "formant",
    values_to = "hz"
  )




