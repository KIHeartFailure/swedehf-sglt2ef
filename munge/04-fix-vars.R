rsdata <- rsdata %>%
  mutate(
    shf_indexyearmonth = factor(zoo::as.yearmon(shf_indexdtm)),
    shf_indexyearmonth_num = as.numeric(shf_indexyearmonth),
    shf_quarter = quarter(shf_indexdtm),
    shf_indexyearquarter = paste0(shf_indexyear, ":", shf_quarter),
    sos_prevhosphf1yr = factor(
      case_when(
        is.na(sos_timeprevhosphf) ~ 0,
        sos_timeprevhosphf <= 365 ~ 1,
        shf_location == "In-patient" ~ 1,
        TRUE ~ 0
      ),
      levels = 0:1,
      labels = c("No", "Yes")
    ),
    shf_bpsys_cat = factor(
      case_when(
        shf_bpsys < 140 ~ 1,
        shf_bpsys >= 140 ~ 2
      ),
      levels = 1:2, labels = c("<140", ">=140")
    ),
    sos_lm_sglt2type = factor(case_when(
      sos_lm_sglt2_Dapagliflozin == "Yes" & sos_lm_sglt2_Empagliflozin == "Yes" ~ 3,
      sos_lm_sglt2_Dapagliflozin == "Yes" ~ 1,
      sos_lm_sglt2_Empagliflozin == "Yes" ~ 2,
      TRUE ~ 0
    ), levels = 0:3, labels = c("None", "Dapagliflozin", "Empagliflozin", "Both")),
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

# income
inc <- rsdata %>%
  reframe(incsum = list(enframe(quantile(scb_dispincome,
    probs = c(0.33, 0.66),
    na.rm = TRUE
  ))), .by = shf_indexyear) %>%
  unnest(cols = c(incsum)) %>%
  pivot_wider(names_from = name, values_from = value)

rsdata <- left_join(
  rsdata,
  inc,
  by = "shf_indexyear"
) %>%
  mutate(
    scb_dispincome_cat = factor(
      case_when(
        scb_dispincome < `33%` ~ 1,
        scb_dispincome < `66%` ~ 2,
        scb_dispincome >= `66%` ~ 3
      ),
      levels = 1:3,
      labels = c("1st tertile within year", "2nd tertile within year", "3rd tertile within year")
    )
  ) %>%
  select(-`33%`, -`66%`)

# ntprobnp

nt <- rsdata %>%
  reframe(ntmed = list(enframe(quantile(shf_ntprobnp,
    probs = c(0.33, 0.66),
    na.rm = TRUE
  )))) %>%
  unnest(cols = c(ntmed)) %>%
  pivot_wider(names_from = name, values_from = value)

rsdata <- rsdata %>%
  mutate(
    shf_ntprobnp_cat = factor(
      case_when(
        shf_ntprobnp < nt$`33%` ~ 1,
        shf_ntprobnp < nt$`66%` ~ 2,
        shf_ntprobnp >= nt$`66%` ~ 3
      ),
      levels = 1:3,
      labels = c("1st tertile", "2nd tertile", "3rd tertile")
    )
  )

rsdata <- rsdata %>%
  mutate(across(where(is_character), factor))
