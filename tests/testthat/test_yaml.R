test_string <- c("REANALYSIS_air_sfc_DailyMean","REANALYSIS_shum_2m_DailyMean",
       "REANALYSIS_snowc_DailyMean","REANALYSIS_tcdc_DailyMean","USElevation_mea100")

test_that("check file paths/yaml loading",{
  check_string <- load_yaml(paste0(path.package("airpred"),"/yaml_files/logit_formula.yml"))
  for (i in 1:5) {
  expect_equal(check_string[i], test_string[i])
  }
}
)
