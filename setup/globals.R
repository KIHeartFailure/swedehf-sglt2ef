# default is to use tidyverse functions
select <- dplyr::select
rename <- dplyr::rename
filter <- dplyr::filter
mutate <- dplyr::mutate
complete <- tidyr::complete
fixed <- stringr::fixed

# used for calculation of ci
global_z05 <- qnorm(1 - 0.025)

shfdbpath <- "P:/k2_stat_heartfailure/Projects/20240619_shfdb5/dm/"
lev <- "leverans1"

global_cols <- RColorBrewer::brewer.pal(7, "Set1")

global_hficd <- " I110| I130| I132| I255| I420| I423| I425| I426| I427| I428| I429| I43| I50| J81| K761| R570| 414W| 425E| 425F| 425G| 425H| 425W| 425X| 428"
global_sglt2atc <- "^(A10BK01|A10BD15|A10BD21|A10BD25|A10BK03|A10BD19|A10BD20)"
