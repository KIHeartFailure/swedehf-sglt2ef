# Inclusion/exclusion criteria --------------------------------------------------------

flow_rs <- flow[c(1:8, 10), 1:2]

names(flow_rs) <- c("Criteria", "N")

flow_rs <- flow_rs %>%
  mutate(Criteria = if_else(
    Criteria == "Exclude posts with with index date > 2023-12-31 (SwedeHF)/2021-12-31 (NPR HF, Controls)",
    "Exclude posts with with index date > 2023-12-31", Criteria
  ))

flow_rs <- rbind(c("General inclusion/exclusion criteria", ""), flow_rs)

flow_rs <- rbind(flow_rs, c("Project specific inclusion/exclusion criteria", ""))

rsdata <- rsdata420 %>%
  filter(!is.na(shf_ef_cat))
flow_rs <- rbind(flow_rs, c("Exclude post with missing EF", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(shf_indexdtm >= ymd("2022-09-01"))
flow_rs <- rbind(flow_rs, c("Include posts >= 2022-09-01 (Deliver)", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(!is.na(shf_diabetestype))
flow_rs <- rbind(flow_rs, c("Exclude posts with missing DM type", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(shf_diabetestype != "Type I")
flow_rs <- rbind(flow_rs, c("Exclude posts with DM type I", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(!is.na(shf_gfrckdepi))
flow_rs <- rbind(flow_rs, c("Exclude posts with missing eGFR", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(shf_gfrckdepi >= 25)
flow_rs <- rbind(flow_rs, c("Exclude posts with eGFR < 25", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(sos_com_dialysis == "No")
flow_rs <- rbind(flow_rs, c("Exclude posts with previous dialysis (SHOULD BE WITHIN CERTAIN TME FRAME????", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(sos_outtime_death > 14)
flow_rs <- rbind(flow_rs, c("Exclude posts with <= 14 days follow-time (SGLT2i defined as dispensations up untill 14 days after baseline)", nrow(rsdata)))

rsdata <- rsdata %>%
  group_by(lopnr) %>%
  arrange(shf_indexdtm) %>%
  slice(1) %>%
  ungroup()

flow_rs <- rbind(flow_rs, c("First post / patient", nrow(rsdata)))

rm(rsdata420)
gc()
