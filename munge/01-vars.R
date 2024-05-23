# Variables for baseline tables -----------------------------------------------

tabvars <- c(
  # type of sglt2
  "sos_lm_sglt2type",
  "sos_lm_sglt2prevusers",

  # demo
  "shf_indexyearquarter",
  "shf_sex",
  "shf_age",
  "shf_age_cat",

  # organizational
  "shf_location",
  "sos_prevhosphf1yr",
  "shf_followuphfunit",
  "shf_followuplocation_cat",

  # clinical factors and lab measurements
  "shf_durationhf",
  "shf_nyha",
  "shf_nyha_cat",
  "shf_bmi",
  "shf_bmi_cat",
  "shf_bpsys",
  "shf_bpsys_cat",
  "shf_bpdia",
  "shf_map",
  "shf_heartrate",
  "shf_heartrate_cat",
  "shf_gfrckdepi",
  "shf_gfrckdepi_cat",
  "shf_potassium",
  "shf_potassium_cat",
  "shf_hb",
  "shf_ntprobnp",
  "shf_ntprobnp_cat",

  # comorbs
  "shf_smoke_cat",
  "shf_sos_com_diabetes",
  "shf_sos_com_hypertension",
  "shf_sos_com_ihd",
  "sos_com_stroke",
  "shf_sos_com_af",
  "shf_anemia",
  "sos_com_valvular",
  "sos_com_liver",
  "sos_com_copd",
  "sos_com_cancer3y",
  "sos_com_charlsonci",
  "sos_com_charlsonci_cat",

  # treatments
  "shf_rasiarni",
  "shf_bbl",
  "shf_mra",
  "shf_diuretic",
  "shf_nitrate",
  "shf_digoxin",
  "shf_anticoagulantia",
  "shf_asaantiplatelet",
  "shf_statin",
  "shf_device_cat",

  # socec
  "scb_famtype",
  "scb_child",
  "scb_education",
  "scb_dispincome_cat",
  "shf_qol",
  "shf_qol_cat"
)

# Variables for models (imputation, log, cox reg) ----------------------------

tabvars_not_in_mod <- c(
  "sos_lm_sglt2type",
  "sos_lm_sglt2prevusers",
  "shf_age",
  "shf_nyha",
  "shf_bpsys",
  "shf_bpdia",
  "shf_map",
  "shf_map_cat",
  "shf_heartrate",
  "shf_gfrckdepi",
  "shf_hb",
  "shf_ntprobnp",
  "shf_potassium",
  "shf_potassium_cat",
  "shf_bmi",
  "sos_com_charlsonci",
  "sos_com_charlsonci_cat",
  "shf_qol",
  "shf_qol_cat"
)

modvars <- tabvars[!(tabvars %in% tabvars_not_in_mod)]

metavars <- bind_rows(
  metavars,
  tibble(
    variable = c(
      "sos_lm_sglt2",
      "sos_lm_sglt2type",
      "sos_lm_sglt2prevusers",
      "shf_indexyearquarter",
      "sos_prevhosphf1yr"
    ),
    label = c(
      "SGLT2i",
      "SGLT2i substance",
      "Previous SGLT2i use",
      "Baseline year:quarter",
      "Previous HFH < 1 year"
    )
  )
)

subgroupvars <- tibble(variable = c(
  "shf_sex", "shf_age_cat", "shf_gfrckdepi_cat", "shf_sos_com_diabetes",
  "sos_prevhosphf1yr", "shf_location", "shf_durationhf"
))

subgroupvars <- left_join(subgroupvars, metavars %>% select(variable, label, unit), by = "variable") %>%
  mutate(labelunit = if_else(!is.na(unit), paste0(label, " (", unit, ")"), label))
