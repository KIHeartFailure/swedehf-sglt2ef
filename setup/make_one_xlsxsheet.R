make_one_xlsxsheet <- function(xlsdata, sheet_name = NULL, colnames = NULL) {
  if (is.null(sheet_name)) sheet_name <- knitr::opts_current$get()$label
  if (!is.null(colnames)) colnames(xlsdata) <- colnames
  if (!any(sheets %in% c(sheet_name))) {
    addWorksheet(wb, sheet = sheet_name)
  }
  writeData(wb, sheet = sheet_name, x = xlsdata, rowNames = FALSE, keepNA = FALSE)
  saveWorkbook(wb,
    file = here("output/tabs/tables.xlsx"),
    overwrite = TRUE
  )
}
