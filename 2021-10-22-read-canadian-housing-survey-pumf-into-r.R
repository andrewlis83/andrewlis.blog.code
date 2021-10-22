# 2021-10-22-read-canadian-housing-survey-pumf-into-r
# Author: andrewlis.blog

# Read 2018 CHS PUMF into R
library(memisc)

# Define locations of input spss files
spssPath <- paste0(getOption("DataServerPath"), "CHS_PUMF_2018\\2018\\Layouts\\SPSS")

col_f <- paste0(spssPath, "\\chs2018ecl_pumf_i.sps")
var_lab <- paste0(spssPath, "\\chs2018ecl_pumf_vare.sps")
var_val <- paste0(spssPath, "\\chs2018ecl_pumf_vale.sps")
mis_val <- paste0(spssPath, "\\chs2018ecl_pumf_miss.sps")
dat_f <- paste0(getOption("DataServerPath"), "CHS_PUMF_2018\\2018\\Data\\CHS2018ECL_PUMF.txt")

chs_pumf <- spss.fixed.file(
  dat_f,
  col_f,
  varlab.file = var_lab,
  codes.file = var_val,
  missval.file = mis_val,
  count.cases = TRUE,
  to.lower = TRUE
)

chs_pumf <- as.data.set(chs_pumf)

# Convert vars to interval or ratio so that if converted to a data frame, numerical vectors are obtained.
measurement(chs_pumf$pagep1) <- "ratio"
measurement(chs_pumf$pscr_d40) <- "ratio"
measurement(chs_pumf$pown_80) <- "ratio"
measurement(chs_pumf$pdv_shco) <- "ratio"
measurement(chs_pumf$phhttinc) <- "ratio"
measurement(chs_pumf$pwsa_d15) <- "ratio"

chs_pumf_df <- as.data.frame(chs_pumf)

# Cleanup
rm(col_f, var_lab, var_val, mis_val, dat_f, spssPath, chs_pumf)
