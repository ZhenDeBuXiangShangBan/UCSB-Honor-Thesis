rm(list = ls())
library(tidyverse)
library(haven)
library(broom)
library(knitr)
library(expss)

hh_origin <-  read_dta("hh2017_20191120_version13.dta",encoding="UTF-8")

wealth_characteristics_censor_include <- hh_origin |> select(hhid_2017,
                                              total_income,
                                              total_consump,
                                              asset,
                                              debt,
                                              censor_total_income,
                                              censor_total_consump,
                                              censor_asset,
                                              censor_debt)
save(wealth_characteristics_censor_include, file="wealth_characteristics_censor_include.Rdata")


wealth_characteristics_censor_exclude <- wealth_characteristics_censor_include |>
  filter((censor_total_income+
             censor_total_consump+
             censor_asset+
             censor_debt) == 0
         ) |> select(-c(censor_total_income,
                        censor_total_consump,
                        censor_asset,
                        censor_debt))
save(wealth_characteristics_censor_exclude, file="wealth_characteristics_censor_exclude.Rdata")


wealth_characteristics_censor_only <- wealth_characteristics_censor_include |>
  filter((censor_total_income+
            censor_total_consump+
            censor_asset+
            censor_debt) != 0
  )
save(wealth_characteristics_censor_only, file="wealth_characteristics_censor_only.Rdata")
