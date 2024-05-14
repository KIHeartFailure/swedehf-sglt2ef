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
)

lmtmp2 <- lmtmp %>%
  mutate(diff = as.numeric(EDATUM - shf_indexdtm)) %>%
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
    mutate(diff = as.numeric(EDATUM - shf_indexdtm)) %>%
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

rsdata <- rsdata %>%
  mutate(
    sos_lm_sglt2prevusers = factor(
      case_when(
        sos_lm_sglt2prevuser1 == "Yes" ~ 5,
        sos_lm_sglt2prevuser2 == "Yes" ~ 4,
        sos_lm_sglt2prevuser3 == "Yes" ~ 3,
        sos_lm_sglt2prevuser4 == "Yes" ~ 2,
        TRUE ~ 1
      ),
      levels = 1:5, labels = c(
        "No previous use",
        "Baseline-14 days",
        "4 months-<Baseline",
        "4 months-2 years",
        ">=2 years"
      )
    ),
    sos_lm_sglt2num = if_else(sos_lm_sglt2 == "Yes", 1, 0),
  )


# Overtime  --------------------------------------------------------------

times <- ymd("2022-09-01"):ymd("2023-12-31")

overtimegenfunc <- function(lmtime, xvar, time, val = "Yes") {
  if (xvar == "shf_ef_cat") {
    lmtime <- lmtime %>%
      group_by(lopnr, shf_ef_cat) %>%
      arrange(sglt2) %>%
      slice(n()) %>%
      ungroup()

    lmtime <- lmtime %>%
      group_by(shf_ef_cat, .drop = F) %>%
      count(sglt2, .drop = F) %>%
      mutate(
        tot = sum(n),
        time = time
      ) %>%
      ungroup() %>%
      filter(sglt2 == val)
  } else {
    lmtime <- lmtime %>%
      filter(!is.na(!!sym(xvar))) %>%
      group_by(lopnr, shf_ef_cat, !!sym(xvar)) %>%
      arrange(sglt2) %>%
      slice(n()) %>%
      ungroup()

    lmtime <- lmtime %>%
      group_by(shf_ef_cat, !!sym(xvar), .drop = F) %>%
      count(sglt2, .drop = F) %>%
      mutate(
        tot = sum(n),
        time = time,
        xvars = xvar
      ) %>%
      rename(xval = !!sym(xvar)) %>%
      ungroup() %>%
      filter(sglt2 == val)
  }
  return(lmtime)
}

overtimefunc <- function(func, times = times) {
  ef <- lapply(times, func, xvar = "shf_ef_cat")
  ef <- bind_rows(ef)

  sex <- lapply(times, func, xvar = "shf_sex")
  sex <- bind_rows(sex)

  age <- lapply(times, func, xvar = "shf_age_cat")
  age <- bind_rows(age)

  ckd <- lapply(times, func, xvar = "shf_gfrckdepi_cat")
  ckd <- bind_rows(ckd)

  dm <- lapply(times, func, xvar = "shf_sos_com_diabetes")
  dm <- bind_rows(dm)

  hfh1yr <- lapply(times, func, xvar = "sos_prevhosphf1yr")
  hfh1yr <- bind_rows(hfh1yr)

  location <- lapply(times, func, xvar = "shf_location")
  location <- bind_rows(location)

  durhf <- lapply(times, func, xvar = "shf_durationhf")
  durhf <- bind_rows(durhf)

  overtime <- bind_rows(ef, sex, age, ckd, dm, hfh1yr, location, durhf) %>%
    mutate(
      per = n / tot * 100,
      perprint = fn(per, 0),
      time = as_date(time)
    )
  return(overtime)
}

lmtmpovertime <- inner_join(
  rsdata %>%
    select(lopnr, shf_indexdtm, censdtm, shf_ef_cat, !!!syms(subgroupvars_rs$variable)),
  lmsglt2,
  by = c("lopnr")
)

lmtmpovertime <- bind_rows(rsdata %>% select(lopnr, shf_indexdtm, censdtm, shf_ef_cat, !!!syms(subgroupvars_rs$variable)), lmtmpovertime)

# Overtime - prevalent use ------------------------------------------------

overtimeprevfunc <- function(time, xvar) {
  lmtime <- lmtmpovertime %>%
    filter(
      shf_indexdtm <= time &
        censdtm >= time &
        ((EDATUM <= time & EDATUM >= time - 4 * 30.5) | is.na(EDATUM))
    ) %>%
    mutate(sglt2 = factor(if_else(is.na(EDATUM), 0, 1), levels = 0:1, labels = c("No", "Yes"))) %>%
    select(lopnr, shf_ef_cat, !!sym(xvar), sglt2) %>%
    distinct()

  lmtime2 <- overtimegenfunc(lmtime = lmtime, xvar = xvar, time = time)
  return(lmtime2)
}

overtime_prev <- overtimefunc(overtimeprevfunc, times = times)

# Overtime - incident use -------------------------------------------------

overtimeincfunc <- function(time, xvar) {
  lmtime <- lmtmpovertime %>%
    filter(
      shf_indexdtm <= time &
        censdtm >= time &
        ((EDATUM <= time & EDATUM >= time - 16 * 30.5) | is.na(EDATUM))
    ) %>%
    mutate(sglt2 = case_when(
      !is.na(EDATUM) & EDATUM < time - 4 * 30.5 & EDATUM >= time - 16 * 30.5 ~ 2,
      EDATUM <= time & EDATUM >= time - 4 * 30.5 ~ 1,
      TRUE ~ 0
    )) %>%
    select(lopnr, shf_ef_cat, !!sym(xvar), sglt2) %>%
    distinct()

  lmtime <- anti_join(
    lmtime %>%
      filter(sglt2 != 2),
    lmtime %>%
      filter(sglt2 == 2) %>%
      select(lopnr),
    by = "lopnr"
  ) %>%
    mutate(sglt2 = factor(sglt2, levels = 0:1, labels = c("No", "Yes")))

  lmtime2 <- overtimegenfunc(lmtime = lmtime, xvar = xvar, time = time)
  return(lmtime2)
}

overtime_inc <- overtimefunc(overtimeincfunc, times = times)

# Overtime - discontinuation ----------------------------------------------

overtimediscfunc <- function(time, xvar) {
  lmtime <- lmtmpovertime %>%
    filter(
      shf_indexdtm <= time &
        censdtm >= time &
        ((EDATUM <= time & EDATUM >= time - 8 * 30.5) | is.na(EDATUM))
    ) %>%
    mutate(sglt2 = case_when(
      is.na(EDATUM) ~ 0, # no use
      EDATUM < time - 4 * 30.5 & EDATUM >= time - 8 * 30.5 ~ 2, # considered to be use
      EDATUM <= time & EDATUM >= time - 4 * 30.5 ~ 1 # not discontinued
    )) %>%
    select(lopnr, shf_ef_cat, !!sym(xvar), sglt2) %>%
    distinct()


  lmtime <- inner_join(
    lmtime %>%
      filter(sglt2 != 2),
    lmtime %>%
      filter(sglt2 == 2) %>%
      select(lopnr),
    by = "lopnr"
  ) %>%
    mutate(sglt2 = factor(sglt2, levels = 0:1, labels = c("No", "Yes")))

  lmtime2 <- overtimegenfunc(lmtime = lmtime, xvar = xvar, time = time, val = "No")
  return(lmtime2)
}

overtime_disc <- overtimefunc(overtimediscfunc, times = times)
