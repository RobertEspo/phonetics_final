# load libs & data
source(here::here("scripts","r","00_libs.R"))
source(here::here("scripts","r","02_load_data.R"))

gam_f1 <- bam(
  centroid_f1 ~ 
    stress +
    s(session, by = stress, k = 5) +
    s(session, participant, bs = "fs", k = 5),
  data = dat_tidy,
  method = "ML"
)

gam_f2 <- bam(
  centroid_f2 ~ 
    stress +
    s(session, by = stress, k = 5) +
    s(session, participant, bs = "fs", k = 5),
  data = dat_tidy,
  method = "ML"
)

gam_tl <- bam(
  tl ~ 
    stress +
    s(session, by = stress, k = 5) +
    s(session, participant, bs = "fs", k = 5),
  data = dat_tidy,
  method = "ML"
)
