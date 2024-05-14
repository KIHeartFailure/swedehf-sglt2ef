# Inclusion/exclusion criteria --------------------------------------------------------

hf <- patreg %>%
  mutate(indexdtm = coalesce(UTDATUM, INDATUM)) %>%
  filter(str_detect(HDIA, global_hficd) &
    indexdtm >= ymd("2000-01-01") &
    indexdtm <= ymd("2021-12-31")) %>%
  select(lopnr, indexdtm, sos_source)

hffirst <- hf %>%
  group_by(lopnr) %>%
  arrange(indexdtm) %>%
  slice(1) %>%
  ungroup() %>%
  rename(firsthfdtm = indexdtm)

hf2 <- hf %>%
  group_by(lopnr) %>%
  arrange(indexdtm) %>%
  slice(2:n()) %>%
  ungroup()

nprdata <- inner_join(hf2, hffirst, by = "lopnr") %>%
  filter(as.numeric(indexdtm - firsthfdtm) > 14) %>%
  group_by(lopnr) %>%
  arrange(indexdtm) %>%
  slice(1) %>%
  ungroup()

flow_npr <- tibble(
  Criteria = "Patients with HF in main position >= 2 times with at least 14 days between 2001-01-01 - 2021-12-31",
  N = nrow(nprdata)
)

hfhosp <- hf %>%
  filter(sos_source == "sv") %>%
  group_by(lopnr) %>%
  arrange(indexdtm) %>%
  slice(1) %>%
  ungroup()

nprdata <- bind_rows(
  nprdata,
  hfhosp
) %>%
  group_by(lopnr) %>%
  arrange(indexdtm) %>%
  slice(2) %>%
  ungroup() %>%
  select(lopnr, indexdtm)

flow_npr <- flow_npr %>% add_row(
  Criteria = "Patients with >= 1 HF hospitalization",
  N = nrow(nprdata)
)

# add info from scb

nprdata <- left_join(
  nprdata,
  demo %>% select(LopNr, fodelsear, kon),
  by = c("lopnr" = "LopNr")
) %>%
  mutate(
    shf_age = year(indexdtm) - as.numeric(fodelsear),
    shf_sex = factor(kon, levels = 1:2, labels = c("Male", "Female"))
  ) %>%
  filter(shf_age >= 18 & !is.na(shf_age))

flow_npr <- flow_npr %>% add_row(
  Criteria = "Exclude patients < 18 years",
  N = nrow(nprdata)
)

ejireg <- fall_ej_i_register %>%
  mutate(indexdtm = ymd(datum)) %>%
  select(lopnr, indexdtm) %>%
  group_by(lopnr, indexdtm) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(notinreg = 1)

nprdata <- left_join(nprdata,
  ejireg,
  by = c("lopnr", "indexdtm")
) %>%
  filter(is.na(notinreg)) %>% # not in scb register
  select(-notinreg)


flow_npr <- flow_npr %>% add_row(
  Criteria = "Exclude patients that are not present in SCB register",
  N = nrow(nprdata)
)

nprdata <- left_join(nprdata,
  ateranvpnr %>% mutate(AterPnr = 1),
  by = c("lopnr" = "LopNr")
) %>%
  filter(is.na(AterPnr)) %>% # reused personr
  select(-AterPnr)


flow_npr <- flow_npr %>% add_row(
  Criteria = "Exclude patients with reused PINs",
  N = nrow(nprdata)
)

# Migration ---------------------------------------------------------------

migration <- inner_join(
  nprdata %>%
    select(lopnr, indexdtm),
  migration %>%
    filter(Posttyp == "Utv"),
  by = c("lopnr" = "LopNr")
) %>%
  mutate(tmp_migrationdtm = ymd(Datum)) %>%
  filter(
    tmp_migrationdtm > indexdtm,
    tmp_migrationdtm <= ymd("2021-12-31")
  ) %>%
  group_by(lopnr, indexdtm) %>%
  slice(1) %>%
  ungroup() %>%
  select(lopnr, indexdtm, tmp_migrationdtm)

nprdata <- left_join(nprdata,
  migration,
  by = c("lopnr", "indexdtm")
)

# Death -------------------------------------------------------------------

nprdata <- left_join(nprdata,
  dors %>% select(lopnr, sos_deathcause, sos_deathdtm),
  by = "lopnr"
)

nprdata <- nprdata %>%
  mutate(
    censdtm = coalesce(
      pmin(sos_deathdtm, tmp_migrationdtm, na.rm = TRUE),
      ymd("2023-12-31")
    )
  ) %>%
  filter(censdtm > indexdtm + 14)


flow_npr <- flow_npr %>% add_row(
  Criteria = "Exclude patients with <= 14 days follow-time (SGLT2i defined as dispensations up untill 14 days after baseline)",
  N = nrow(nprdata)
)

nprdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = nprdata,
  patid = lopnr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  opvar = OP_all,
  type = "com",
  name = "dialysis",
  diakod = " Z491| Z492",
  opkod = " DR014| DR015| DR016| DR020| DR012| DR013| DR023| DR024| TJA33| TJA35",
  stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)

nprdata <- nprdata %>%
  filter(sos_com_dialysis == "No")

flow_npr <- flow_npr %>% add_row(
  Criteria = "Exclude patients with previous dialysis",
  N = nrow(nprdata)
)
