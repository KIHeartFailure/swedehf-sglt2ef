load(here(shfdbpath, "data", datadate, "/lmswedehf.RData"))

# Treatments from PDR ----------------------------------------

lmsglt2 <- lmswedehf %>%
  filter(str_detect(ATC, global_sglt2atc) & ANTAL >= 0) %>%
  select(lopnr, EDATUM, ATC)

# within 4 months prior to index

lmtmp <- inner_join(
  rsdata %>%
    select(lopnr, shf_indexdtm),
  lmsglt2,
  by = c("lopnr")
) %>%
  mutate(diff = as.numeric(EDATUM - shf_indexdtm))

lmtmp2 <- lmtmp %>%
  filter(diff >= -30.5 * 4, diff <= 14) %>%
  select(lopnr, shf_indexdtm, EDATUM, ATC)

rsdata <- create_medvar(
  atc = global_sglt2atc,
  medname = "sglt2", cohortdata = rsdata, meddata = lmtmp2, id = "lopnr", metatime = "-4mo-14days",
  valsclass = "fac"
)
rsdata <- create_medvar(
  atc = "^(A10BK01|A10BD15|A10BD21|A10BD25)",
  medname = "sglt2_Dapagliflozin", cohortdata = rsdata, meddata = lmtmp2, id = "lopnr", metatime = "-4mo-14days",
  valsclass = "fac"
)
rsdata <- create_medvar(
  atc = "^(A10BK03|A10BD19|A10BD20)",
  medname = "sglt2_Empagliflozin", cohortdata = rsdata, meddata = lmtmp2, id = "lopnr", metatime = "-4mo-14days",
  valsclass = "fac"
)
rm(lmtmp2)

# New/prevalent users -----------------------------------------------------

lmprevfunc <- function(timestart, timestop, medname) {
  lmtmp2 <- lmtmp %>%
    filter(diff <= timestart & diff >= timestop) %>%
    select(lopnr, shf_indexdtm, EDATUM, ATC)

  rsdata <<- create_medvar(
    atc = global_sglt2atc,
    medname = medname, cohortdata = rsdata, meddata = lmtmp2, id = "lopnr", metatime = NA,
    valsclass = "fac"
  )
}

lmprevfunc(timestart = -2 * 365, timestop = -20 * 365, "sglt2prevuser1")
lmprevfunc(-30.5 * 4 + 1, -2 * 365 - 1, "sglt2prevuser2")
lmprevfunc(-1, -30.5 * 4, "sglt2prevuser3")
lmprevfunc(timestart = 14, timestop = 0, "sglt2prevuser4")

# Discontinuation ---------------------------------------------------------

lmtmp <- left_join(
  rsdata %>%
    filter(popmain & sos_lm_sglt2 == "Yes") %>%
    select(lopnr, shf_indexdtm, censdtm),
  lmsglt2,
  by = c("lopnr")
) %>%
  mutate(diff = as.numeric(EDATUM - shf_indexdtm)) %>%
  filter(diff >= -30.5 * 4 & EDATUM <= censdtm) %>%
  select(lopnr, shf_indexdtm, EDATUM)

lmtmp <- bind_rows(
  lmtmp,
  rsdata %>%
    filter(popmain & sos_lm_sglt2 == "Yes") %>%
    mutate(
      EDATUM = censdtm,
      lastpost = 1
    ) %>%
    select(lopnr, shf_indexdtm, EDATUM, lastpost)
) %>%
  arrange(lopnr, EDATUM)

lmtmp2 <- lmtmp %>%
  group_by(lopnr) %>%
  arrange(EDATUM) %>%
  mutate(
    diff = as.numeric(lead(EDATUM) - EDATUM),
    disc = if_else(diff >= 30.5 * 5, 1, 0)
  ) %>%
  ungroup() %>%
  arrange(lopnr, EDATUM) %>%
  filter(disc == 1) %>%
  group_by(lopnr) %>%
  arrange(EDATUM) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(outtime_disctmp = as.numeric(EDATUM - shf_indexdtm) + 30.5 * 4) %>%
  select(lopnr, outtime_disctmp, disc)

rsdata <- left_join(rsdata,
  lmtmp2,
  by = "lopnr"
) %>%
  mutate(
    outtime_disc = pmin(sos_outtime_death, outtime_disctmp, na.rm = T),
    disc = replace_na(disc, 0)
  ) %>%
  select(-outtime_disctmp)
