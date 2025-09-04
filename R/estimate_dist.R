library(fastverse)
library(pipdata)
source("R/utils.R")
source("R/scale_weights.R")


# main parameters ----------
#----------------------------
release <- "20250930_2017_01_02_PROD"
py <- strsplit(release, "_")[[1]][2] |>
  as.numeric()

aux_dir <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") |>
  fs::path(release,
           "_aux")
# temp saving location
dir <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") |>
  fs::path(release)

# clean coeff data -----------
#----------------------------
fs::path("data/cmd_coeff.Rda") |>
  load()

CF <- load_coeff()
if (py == 2021) {
  CF <- CF$ppp2021
} else if (py == 2017) {
  CF <- CF$ppp2017
}

# Quantiles
#----------------------------
qs <- calc_quantiles(n = 100000)


## MD countries
#----------------------------

md <- pipload::pip_load_aux("missing_data") |>
  # fs::path(aux_dir, "missing_data.fst") |>
  # fst::read_fst() |>
  ftransform(id = paste(country_code,
                        year,
                        sep = "_")) |>
  qDT()

# Estimate and save
#----------------------------
estimate_and_write_full_cmd(md  = md,
                            CF  = CF,
                            qs  = qs,
                            py  = py,
                            dir = dir)

# Test
#----------------------------

# # test that it works
# ABW1986 <- load_refy("ABW",
#                      1986,
#                      path = save_dir)
# attributes(ABW1986)
#
# # Remove NULL elements from l_cmd
#
# # Split l_cmd into two lists: non-NULL and NULL elements
# l_cmd_ok   <- l_cmd[!sapply(l_cmd, is.null)]
# l_cmd_fail <- l_cmd[sapply(l_cmd, is.null)]
# names(l_cmd_fail)


