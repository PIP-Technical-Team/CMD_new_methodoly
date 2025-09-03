library(fastverse)
library(pipdata)
source("R/utils.R")
source("R/scale_weights.R")

# Prep coefficients data ---------

## main parameters ----------
release <- "20250930_2021_01_02_PROD"
py <- strsplit(release, "_")[[1]][2] |>
  as.numeric()
#dir     <-
aux_dir <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") |>
  fs::path(release,
           "_aux")
# temp saving location
lineup_dir <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") |>
  fs::path(release,
           "lineup_data")
save_dir <- lineup_dir |>
  fs::path("CMD")

## clean coeff data -----------
fs::path("data/cmd_coeff.Rda") |>
  load()

CF <- load_coeff()
if (py == 2021) {
  CF <- CF$ppp2021
} else if (py == 2017) {
  CF <- CF$ppp2017
}

# Quantiles
qs <- calc_quantiles(n = 100000)


## MD countries
md <- fs::path(aux_dir, "missing_data.fst") |>
  fst::read_fst() |>
  ftransform(id = paste(country_code,
                        year,
                        sep = "_")) |>
  qDT()

# Usage:
t1 <- Sys.time()
l_cmd <- list_cmd_welfare(md,
                          CF,
                          qs,
                          py = py)
t2 <- Sys.time()
print(t2 - t1)
# Time difference of 7.6113007 mins when n = 1000



# Scale populations
pop   <- get_pop_to_scale(aux_dir = aux_dir)
l_cmd <- scale_weights(l_cmd = l_cmd,
                       pop   = pop)

# write
write_cmd_dist(l_cmd,
               path = save_dir)

















# test that it works
ABW1986 <- load_refy("ABW",
                     1986,
                     path = save_dir)
attributes(ABW1986)

# Remove NULL elements from l_cmd

# Split l_cmd into two lists: non-NULL and NULL elements
l_cmd_ok   <- l_cmd[!sapply(l_cmd, is.null)]
l_cmd_fail <- l_cmd[sapply(l_cmd, is.null)]
names(l_cmd_fail)


