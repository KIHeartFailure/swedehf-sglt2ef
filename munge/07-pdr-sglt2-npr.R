# Treatments from PDR ----------------------------------------

lmsglt2 <- read_sas(here("data/raw-data/lmsglt2.sas7bdat"))

# Overtime  --------------------------------------------------------------

times <- ymd("2022-09-01"):ymd("2023-12-31")

overtimegenfunc <- function(lmtime, time, val = "Yes") {
  lmtime <- lmtime %>%
    group_by(lopnr) %>%
    arrange(sglt2) %>%
    slice(n()) %>%
    ungroup()

  lmtime <- lmtime %>%
    count(sglt2, .drop = F) %>%
    mutate(
      tot = sum(n),
      time = time
    ) %>%
    ungroup() %>%
    filter(sglt2 == val)
  return(lmtime)
}

lmtmpovertime <- inner_join(
  nprdata %>%
    select(lopnr, indexdtm, censdtm),
  lmsglt2,
  by = c("lopnr" = "LopNr")
)

lmtmpovertime <- bind_rows(nprdata %>% select(lopnr, indexdtm, censdtm), lmtmpovertime)

# Overtime - prevalent use ------------------------------------------------

overtimeprevfunc <- function(time) {
  lmtime <- lmtmpovertime %>%
    filter(
      indexdtm <= time &
        censdtm >= time &
        ((EDATUM <= time & EDATUM >= time - 4 * 30.5) | is.na(EDATUM))
    ) %>%
    mutate(sglt2 = factor(if_else(is.na(EDATUM), 0, 1), levels = 0:1, labels = c("No", "Yes"))) %>%
    select(lopnr, sglt2) %>%
    distinct()

  lmtime2 <- overtimegenfunc(lmtime = lmtime, time = time)
  return(lmtime2)
}

overtime_prev_npr <- lapply(times, overtimeprevfunc)
overtime_prev_npr <- bind_rows(overtime_prev_npr) %>%
  mutate(
    per = n / tot * 100,
    perprint = fn(per, 0),
    time = as_date(time)
  )

# Overtime - incident use -------------------------------------------------

overtimeincfunc <- function(time) {
  lmtime <- lmtmpovertime %>%
    filter(
      indexdtm <= time &
        censdtm >= time &
        ((EDATUM <= time & EDATUM >= time - 16 * 30.5) | is.na(EDATUM))
    ) %>%
    mutate(sglt2 = case_when(
      !is.na(EDATUM) & EDATUM < time - 4 * 30.5 & EDATUM >= time - 16 * 30.5 ~ 2,
      EDATUM <= time & EDATUM >= time - 4 * 30.5 ~ 1,
      TRUE ~ 0
    )) %>%
    select(lopnr, sglt2) %>%
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

  lmtime2 <- overtimegenfunc(lmtime = lmtime, time = time)
  return(lmtime2)
}

overtime_inc_npr <- lapply(times, overtimeincfunc)
overtime_inc_npr <- bind_rows(overtime_inc_npr) %>%
  mutate(
    per = n / tot * 100,
    perprint = fn(per, 0),
    time = as_date(time)
  )


# Overtime - discontinuation ----------------------------------------------

overtimediscfunc <- function(time) {
  lmtime <- lmtmpovertime %>%
    filter(
      indexdtm <= time &
        censdtm >= time &
        ((EDATUM <= time & EDATUM >= time - 8 * 30.5) | is.na(EDATUM))
    ) %>%
    mutate(sglt2 = case_when(
      is.na(EDATUM) ~ 0, # no use
      EDATUM < time - 4 * 30.5 & EDATUM >= time - 8 * 30.5 ~ 2, # considered to be use
      EDATUM <= time & EDATUM >= time - 4 * 30.5 ~ 1 # not discontinued
    )) %>%
    select(lopnr, sglt2) %>%
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

  lmtime2 <- overtimegenfunc(lmtime = lmtime, time = time, val = "No")
  return(lmtime2)
}

overtime_disc_npr <- lapply(times, overtimediscfunc)
overtime_disc_npr <- bind_rows(overtime_disc_npr) %>%
  mutate(
    per = n / tot * 100,
    perprint = fn(per, 0),
    time = as_date(time)
  )
