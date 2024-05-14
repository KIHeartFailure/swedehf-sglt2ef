# Project specific packages, functions and settings -----------------------

source(here::here("setup/setup.R"))

# Load data ---------------------------------------------------------------

load(here(shfdbpath, "data/v420/rsdata420.RData"))

# Meta data ect -----------------------------------------------------------

metavars <- read.xlsx(here(shfdbpath, "metadata/meta_variables.xlsx"))
load(here(paste0(shfdbpath, "data/v420/meta_statreport.RData")))

# Munge data --------------------------------------------------------------

# swedehf
source(here("munge/01-vars-rs.R"))
source(here("munge/02-pop-selection-rs.R"))
source(here("munge/03-fix-vars-rs.R"))
source(here("munge/04-pdr-sglt2-rs.R"))
source(here("munge/05-mi-rs.R"))

# Cache/save data ---------------------------------------------------------

save(
  file = here("data/clean-data/rsdata.RData"),
  list = c(
    "rsdata",
    "imprsdata",
    "flow_rs",
    "modvars_rs",
    "tabvars_rs",
    "subgroupvars_rs",
    "metavars",
    "deathmeta",
    "outcommeta",
    "metalm",
    "overtime_prev",
    "overtime_inc",
    "overtime_disc"
  )
)

# npr hf pop

rm(list = ls())
gc()
source(here::here("setup/setup.R"))

# Load data ---------------------------------------------------------------

load(here(shfdbpath, "data/v420/rsdatafull420.RData"))
load(here(shfdbpath, "/data/", datadate, "/patreg.RData"))
load(here(shfdbpath, "/data/", datadate, "/rawData_scb.RData"))
load(here(shfdbpath, "/data/", datadate, "/prepdors.RData"))

# Munge data --------------------------------------------------------------

source(here("munge/06-pop-selection-npr.R"))

rm(list = setdiff(ls(), c("nprdata", "flow_npr")))
gc()
source(here::here("setup/setup.R"))

source(here("munge/07-pdr-sglt2-npr.R"))

# Cache/save data ---------------------------------------------------------

save(
  file = here("data/clean-data/nprdata.RData"),
  list = c(
    "flow_npr",
    "nprdata",
    "overtime_prev_npr",
    "overtime_inc_npr",
    "overtime_disc_npr"
  )
)

# create workbook to write tables to Excel
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, sheet = "Information")
openxlsx::writeData(wb, sheet = "Information", x = "Tables in xlsx format for tables in Statistical report: SGLT2i across the EF spectrum: an analysis from the Swedish Heart Failure registry", rowNames = FALSE, keepNA = FALSE)
openxlsx::saveWorkbook(wb,
  file = here::here("output/tabs/tables.xlsx"),
  overwrite = TRUE
)

# create powerpoint to write figs to PowerPoint
figs <- officer::read_pptx()
print(figs, target = here::here("output/figs/figs.pptx"))
