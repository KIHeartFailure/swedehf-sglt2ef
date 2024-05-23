# Inclusion/exclusion criteria --------------------------------------------------------

flow <- flow[c(1:8, 10), 1:2]

names(flow) <- c("Criteria", "N")

flow <- flow %>%
  mutate(Criteria = if_else(
    Criteria == "Exclude posts with with index date > 2023-12-31 (SwedeHF)/2021-12-31 (NPR HF, Controls)",
    "Exclude posts with with index date > 2023-12-31", Criteria
  ))

flow <- rbind(c("General inclusion/exclusion criteria", ""), flow)

flow <- rbind(flow, c("Project specific inclusion/exclusion criteria", ""))

rsdata <- rsdata421 %>%
  filter(!is.na(shf_ef_cat))
flow <- rbind(flow, c("Exclude post with missing EF", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(!is.na(shf_diabetestype))
flow <- rbind(flow, c("Exclude posts with missing DM type", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(shf_diabetestype != "Type I")
flow <- rbind(flow, c("Exclude posts with DM type I", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(!is.na(shf_gfrckdepi))
flow <- rbind(flow, c("Exclude posts with missing eGFR", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(shf_gfrckdepi >= 25)
flow <- rbind(flow, c("Exclude posts with eGFR < 25", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(sos_com_dialysis == "No")
flow <- rbind(flow, c("Exclude posts with previous dialysis (SHOULD BE WITHIN CERTAIN TME FRAME????", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(sos_outtime_death > 14)
flow <- rbind(flow, c("Exclude posts with <= 14 days follow-time (SGLT2i defined as dispensations up untill 14 days after baseline)", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(shf_indexdtm >= ymd("2020-11-01"))
flow <- rbind(flow, c("Include posts >= 2020-11-01", nrow(rsdata)))

rsdata <- rsdata %>%
  group_by(lopnr) %>%
  arrange(shf_indexdtm) %>%
  slice(n()) %>%
  ungroup()

flow <- rbind(flow, c("Last post / patient (Used for temporal trends)", nrow(rsdata)))

rsdata <- rsdata %>%
  mutate(popmain = shf_indexdtm >= ymd("2022-09-01"))
flow <- rbind(flow, c("Include posts >= 2022-09-01 (Deliver) (used for all other analyses)", nrow(rsdata %>% filter(popmain))))

rm(rsdata421)
gc()
