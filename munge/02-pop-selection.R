flow <- flow[c(1:8), 1:2]
names(flow) <- c("Criteria", "N")

flow <- flow %>%
  add_row(
    Criteria = "General inclusion/exclusion criteria",
    N = NA, .before = 1
  )
flow <- flow %>%
  add_row(
    Criteria = "Project specific inclusion/exclusion criteria",
    N = NA
  )

# Inclusion/exclusion criteria --------------------------------------------------------

rsdata <- rsdata501 %>%
  filter(shf_indexdtm >= ymd("2020-11-01"))
flow <- flow %>%
  add_row(
    Criteria = "Include posts >= 2020-11-01",
    N = nrow(rsdata)
  )

rsdata <- rsdata %>%
  filter(!is.na(shf_ef_cat))
flow <- flow %>%
  add_row(
    Criteria = "Exclude post with missing EF",
    N = nrow(rsdata)
  )

rsdata <- rsdata %>%
  filter(!is.na(shf_diabetestype))
flow <- flow %>%
  add_row(
    Criteria = "Exclude posts with missing DM type",
    N = nrow(rsdata)
  )

rsdata <- rsdata %>%
  filter(shf_diabetestype != "Type I")
flow <- flow %>%
  add_row(
    Criteria = "Exclude posts with DM type I",
    N = nrow(rsdata)
  )

rsdata <- rsdata %>%
  filter(!is.na(shf_gfrckdepi))
flow <- flow %>%
  add_row(
    Criteria = "Exclude posts with missing eGFR",
    N = nrow(rsdata)
  )

rsdata <- rsdata %>%
  filter(shf_gfrckdepi >= 25)
flow <- flow %>%
  add_row(
    Criteria = "Exclude posts with eGFR < 25",
    N = nrow(rsdata)
  )

rsdata <- rsdata %>%
  filter(sos_com_dialysis == "No")
flow <- flow %>%
  add_row(
    Criteria = "Exclude posts with previous dialysis within 5 years",
    N = nrow(rsdata)
  )

rsdata <- rsdata %>%
  filter(sos_outtime_death > 14)
flow <- flow %>%
  add_row(
    Criteria = "Exclude posts with <= 14 days follow-time (SGLT2i defined as dispensations up untill 14 days after baseline)",
    N = nrow(rsdata)
  )

rsdata <- rsdata %>%
  group_by(lopnr) %>%
  arrange(shf_indexdtm) %>%
  slice(n()) %>%
  ungroup()
flow <- flow %>%
  add_row(
    Criteria = "Last post / patient (Used for temporal trends)",
    N = nrow(rsdata)
  )

rsdata <- rsdata %>%
  mutate(popmain = shf_indexdtm >= ymd("2022-09-01"))
flow <- flow %>%
  add_row(
    Criteria = "Include posts >= 2022-09-01 (Deliver) (used for all other analyses)",
    N = nrow(rsdata %>% filter(popmain))
  )

rm(rsdata501)
gc()
