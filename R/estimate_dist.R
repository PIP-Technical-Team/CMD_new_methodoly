library(fastverse)


# Prep coefficients data
fs::path("data/cmd_coeff.Rda") |>
  load()

CF <- as.data.table(data)

CF[, `:=`(
  # Tier 1 component 1
  t1_comp1 = tier1_int + tier1_gdp * log(gdp) + tier1_u5m *
    log(u5m) + tier1_rps * rps + tier1_leb * leb + tier1_inc * inc + tier1_incgdp *
    inc * log(gdp),

  # Tier 1 quantile factor
  t1_qf = tier1_pct + tier1_eca*eca+tier1_lac*lac+tier1_ssa*ssa,

  # Tier 2 component 1
  t2_comp1 = tier2_int + tier2_u5m * log(u5m) + tier2_rps * rps + tier2_leb *
    leb + tier2_lmc * lmc + tier2_umc * umc + tier2_hic * hic,

  # Tier 2 quantile factor
  t2_qf = tier2_pct + tier2_eca*eca+tier2_lac*lac+tier2_ssa*ssa
)]


quantile = seq(1,1000,1)/1000 - 0.0005



#






eth2021 <- data.frame(quantile = seq(1,1000,1)/1000-0.0005,code="ETH",year=2021) |>
  joyn::joyn(data,match_type="m:1",by=c("code","year"), keep="left",reportvar=FALSE) |>
  mutate(lny_tier1_1stcomponent = tier1_int+tier1_gdp*log(gdp)+tier1_u5m*log(u5m)+tier1_rps*rps+tier1_leb*leb+tier1_inc*inc+tier1_incgdp*inc*log(gdp),
         lny_tier1_2ndcomponent = (tier1_pct + tier1_eca*eca+tier1_lac*lac+tier1_ssa*ssa)*log(quantile/(1-quantile)),
         lny_tier1 = lny_tier1_1stcomponent+lny_tier1_2ndcomponent,
         y_tier1   = exp(lny_tier1)*tier1_sme,
         lny_tier2_1stcomponent = tier2_int+tier2_u5m*log(u5m)+tier2_rps*rps+tier2_leb*leb+tier2_lmc*lmc+tier2_umc*umc+tier2_hic*hic,
         lny_tier2_2ndcomponent = (tier2_pct + tier2_eca*eca+tier2_lac*lac+tier2_ssa*ssa)*log(quantile/(1-quantile)),
         lny_tier2 = lny_tier2_1stcomponent+lny_tier2_2ndcomponent,
         y_tier2   = exp(lny_tier2)*tier2_sme)
