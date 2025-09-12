library(fastverse)
library(pipdata)
source("R/utils.R")
source("R/scale_weights.R")

# main parameters ----------
#----------------------------
release <- "20250930_2021_01_02_PROD"
py <- strsplit(release, "_")[[1]][2] |>
  as.numeric()

aux_dir <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") |>
  fs::path(release,
           "_aux")
# temp saving location
dir <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") |>
  fs::path(release)
dir_dist_stats <-
  "P:/02.personal/wb612474/pip-technical/pip-lineups-pipeline-objects" |>
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
qs <- calc_quantiles(n = 10000)

## MD countries
#----------------------------

md <- pipload::pip_load_aux("missing_data") |>
  ftransform(id = paste(country_code,
                        year,
                        sep = "_")) |>
  qDT()

# Estimate and save
#----------------------------
env_acc_dist_stats <- new.env(parent = .GlobalEnv)
estimate_and_write_full_cmd(md      = md,
                            CF      = CF,
                            qs      = qs,
                            py      = py,
                            dir     = dir,
                            env_acc = env_acc_dist_stats)

# Dist stats
#----------------------------
all_dist_stats <- data.table::rbindlist(as.list(env_acc_dist_stats),
                                        use.names = TRUE,
                                        fill      = TRUE)
setorder(all_dist_stats,
         country_code,
         reporting_year)
fst::write.fst(all_dist_stats,
               path = fs::path(dir_dist_stats,
                               "CMD_dist_stats.fst"))
# Merge CMD and LD dist stats and save
#----------------------------
ld_dist <- fst::read_fst(path = fs::path(dir_dist_stats,
                                          "LD_dist_stats.fst"),
                          as.data.table = TRUE)
all_dist_stats <- fst::read_fst(
               path = fs::path(dir_dist_stats,
                               "CMD_dist_stats.fst"),
               as.data.table = TRUE)

all_dist_stats <-
  rowbind(all_dist_stats,
          ld_dist)
setorder(all_dist_stats,
         country_code,
         reporting_year)
fst::write.fst(all_dist_stats,
               path = fs::path(dir,
                               "estimations/lineup_dist_stats.fst"))

fst::write.fst(all_dist_stats,
               path = fs::path(dir_dist_stats,
                               "lineup_dist_stats.fst"))



