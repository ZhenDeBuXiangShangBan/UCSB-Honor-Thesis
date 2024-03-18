rm(list = ls())
library(tidyverse)
library(haven)
library(broom)
library(knitr)
library(expss)
library(sjPlot)
library(sjmisc)
library(sjlabelled)

load("ind_character.Rdata")

ind <- ind_character |> mutate(Role = if_else(Real_Relation == "child" & Edu_Paid == 1, "child", Role)) |> ungroup()
ind <- ind |> group_by(hhid_2017) |> filter(sum(Real_Relation == "child")== 1) |> ungroup()

father <- ind |> filter(Role == "father") |> select(hhid_2017, a2000c,educ_year,a2012) |>
  rename(father_educ_year = educ_year,
         father_educ_level = a2012,
         father_in_house = a2000c)

mother <- ind |> filter(Role == "mother") |> select(hhid_2017, a2000c,educ_year,a2012) |>
  rename(mother_educ_year = educ_year,
         mother_educ_level = a2012,
         mother_in_house = a2000c)


grand_father_m_side <- ind %>%
  filter(Role == "grand_father_m_side") %>% select(hhid_2017, a2000c, educ_year,a2012) %>%
  rename(grand_father_m_side_educ_year = educ_year,
         grand_father_m_side_educ_level = a2012,
         grand_father_m_side_in_house = a2000c) %>% distinct(hhid_2017, .keep_all = T)

grand_mother_m_side <- ind |> filter(Role == "grand_mother_m_side") |> select(hhid_2017, a2000c,educ_year,a2012) |>
  rename(grand_mother_m_side_educ_year = educ_year,
         grand_mother_m_side_educ_level = a2012,
         grand_mother_m_side_in_house = a2000c)  |> distinct(hhid_2017, .keep_all = T)

grand_mother_f_side <- ind |> filter(Role == "grand_mother_f_side") |> select(hhid_2017, a2000c,educ_year,a2012) |>
  rename(grand_mother_f_side_educ_year = educ_year,
         grand_mother_f_side_educ_level = a2012,
         grand_mother_f_side_in_house = a2000c) |> distinct(hhid_2017, .keep_all = T)

grand_father_f_side <- ind |> filter(Role == "grand_father_f_side") |> select(hhid_2017, a2000c,educ_year,a2012) |>
  rename(grand_father_f_side_educ_year = educ_year,
         grand_father_f_side_educ_level = a2012,
         grand_father_f_side_in_house = a2000c)|> distinct(hhid_2017, .keep_all = T)


child <- ind |> filter(Role == "child") |> select(hhid_2017,educ_year, a2003,a2005) |>
  mutate(a2005= 2017 - a2005) |>
  rename(child_current_educ_year = educ_year,
         child_gender = a2003,
         age = a2005)

family <- ind |> distinct(hhid_2017) |>
  left_join(grand_father_m_side, join_by(hhid_2017)) |>
  left_join(grand_mother_m_side, join_by(hhid_2017)) |>
  left_join(grand_father_f_side, join_by(hhid_2017)) |>
  left_join(grand_mother_f_side, join_by(hhid_2017)) |>
  left_join(father, join_by(hhid_2017)) |>
  left_join(mother, join_by(hhid_2017)) |>
  left_join(child, join_by(hhid_2017)) |>
  apply_labels(age = "current age")
rm(father,mother,
   grand_father_f_side,
   grand_father_m_side,
   grand_mother_f_side,
   grand_mother_m_side,
   ind_character)

load("wealth_characteristics_censor_include.Rdata")
load("educ_cost.Rdata")


wealth <- wealth_characteristics_censor_include |> mutate(pure_asset = asset - debt) |>
  select(hhid_2017, total_income,total_consump,pure_asset)

family2 <- family |> drop_all_labels() |> left_join( wealth) |> left_join(educ_cost)
source("functions.R")

family2 <- replace_labels(family, family2)



write.csv(family2, file = "one_child_family_characteristics.csv")
write.csv(family2, file = "all_family_characteristics.csv")
