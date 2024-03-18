rm(list = ls())
library(tidyverse)
library(haven)
library(broom)
library(knitr)
library(expss)

load("ind.Rdata")

ind_origin <- ind_origin |> mutate(a2000c = if_else((is.na(a2000c) == T & track ==0),1,a2000c ),
                                   educ_year = case_when(
                                     s3000d == 19 ~ 17, #master
                                     s3000d == 20 ~ 23,#phd
                                     is.na(s3000d) == F ~ s3000d -1,#otherwise, currently in the school
                                     a2012 == 1 ~ 0, # no schooling
                                     a2012 == 2 ~ 6, #elementary school
                                     a2012 == 3 ~ 9, #middle school
                                     a2012 == 4 ~ 12, # high school
                                     a2012 == 5 ~ 12, #technical high school
                                     a2012 == 6 ~ 15, #college
                                     a2012 == 7 ~ 15, #university
                                     a2012 == 8 ~ 17,#master
                                     a2012 == 9 ~ 23 # phd
                                   )) |>
  apply_labels(a2000c = "Live in House?")


ind <- ind_origin |>
  mutate(sum_educ_spending = rowSums(select(ind_origin, 
                                            s7000, 
                                            s7002a, 
                                            s7002b, 
                                            s7002c, 
                                            s7002d, 
                                            s7002e, 
                                            s7003a, 
                                            s7003b, 
                                            s7003c, 
                                            s7003d, 
                                            s7003e, 
                                            s7003f, 
                                            s7003g, 
                                            s7003h, 
                                            s7004a, 
                                            s7004b, 
                                            s7004c, 
                                            s7004d, 
                                            s7005a,
                                            s8003, 
                                            s8010, 
                                            s8014), na.rm = TRUE))
#s8017根据刚才提供的情况，过去12个月您家为【CAPI加载姓名】的教育一共支出【CAPI加载总教育费用】元，这个数字是
#1. 高了
#2. 低了
#3. 差不多（结束问卷）
#s8018:过去12个月，您家为【CAPI加载姓名】的教育一共支出多少钱？（仅在S8017=1、2时询问）
#CAPI：若S8017=1，则S8018<教育总费；若S8017=2，则S8018>教育总费。
#否则系统阻止访员继续访问，提示“S8018答案与S8017所选情况不相符，请确认答案”

ind <- ind |>  mutate(education_spending = case_when(
  s8017 == 1 | s8017 == 2 ~s8018,
  s8017 == 3 ~ sum_educ_spending)
) |> mutate(
  education_spending = if_else(is.na(education_spending) == T, s8018_imp, education_spending)
)|> select(education_spending, everything()) |>
  mutate(
    Generation = if_else(is.na(education_spending) == F,1, NA),
         Real_Relation = if_else(is.na(education_spending) == F, "child", NA),
    Edu_Paid = if_else(is.na(education_spending) == F, 1, 0)
    ) |>
  select(a2000c,
         educ_year,
         Generation,Edu_Paid, Real_Relation, hhid_2017,pline,
         a2001,
         a2003,
         a2005,
         a2006,
         a2012)





###only the group that the child is less than 20 years old is identified as child
ind_family_child_20_under <- ind %>%
  group_by(hhid_2017) %>%
  # Add an indicator for groups that meet the initial child condition with a2001 == 6
  mutate(has_child_6 = any(a2001 == 6 & Real_Relation == "child" & Edu_Paid == 1 &a2005 >= 1997),
         # Indicators for specific parent conditions
         parent_a2001_1_a2003_1 = any(a2001 == 1 & a2003 == 1),
         parent_a2001_1_a2003_2 = any(a2001 == 1 & a2003 == 2)) %>%
  # Update Real_Relation and Generation for a2001 == 1 or 2 within those groups
  mutate(Real_Relation = if_else(has_child_6 & (a2001 %in% c(1, 2)), "parent", Real_Relation),
         Generation = if_else(has_child_6 & (a2001 %in% c(1, 2)), 2, Generation)) %>%
  # Assign Real_Relation and Generation for a2001 == 6 with NA in Real_Relation
  mutate(Real_Relation = if_else(has_child_6 &a2001 == 6 & is.na(Real_Relation) & a2005 >1998, "child", Real_Relation),
         Generation = if_else(has_child_6 & a2001 == 6 & is.na(Generation), 1, Generation)) %>%
  # Additional conditions for grandparents
  mutate(Real_Relation = if_else(has_child_6 & a2001 == 3 & parent_a2001_1_a2003_1, "grandparent_f_side", Real_Relation),
         Generation = if_else(has_child_6 & a2001 == 3 & parent_a2001_1_a2003_1, 3, Generation),
         Real_Relation = if_else(has_child_6 & a2001 == 4 & parent_a2001_1_a2003_1, "grandparent_m_side", Real_Relation),
         Generation = if_else(has_child_6 & a2001 == 4 & parent_a2001_1_a2003_1, 3, Generation),
         Real_Relation = if_else(has_child_6 & a2001 == 3 & parent_a2001_1_a2003_2, "grandparent_m_side", Real_Relation),
         Generation = if_else(has_child_6 & a2001 == 3 & parent_a2001_1_a2003_2, 3, Generation),
         Real_Relation = if_else(has_child_6 & a2001 == 4 & parent_a2001_1_a2003_2, "grandparent_f_side", Real_Relation),
         Generation = if_else(has_child_6 & a2001 == 4 & parent_a2001_1_a2003_2, 3, Generation))


ind_6_is_child <- ind_family_child_20_under |> filter(has_child_6 == T) |>
  select(-has_child_6, -parent_a2001_1_a2003_1, -parent_a2001_1_a2003_2)|> filter(a2001!=7777) |> filter(is.na(Real_Relation) == F)



ind_family_child_as_grandchild <- ind_family_child_20_under %>%
  filter(has_child_6 == FALSE ) %>%
  select(-has_child_6, -parent_a2001_1_a2003_1, -parent_a2001_1_a2003_2) %>%
  filter(any(a2001 == 6 & Real_Relation == "child" & Edu_Paid == 1) == FALSE) %>%
  filter(a2001 != 7777) %>%
  filter(!(a2001 %in% c(3,4,5,9,10))) %>%
  filter(!(a2001==6 & a2005>1993)) %>%
  filter(!(any(Edu_Paid == 1 & a2001 == 7))) %>%
  mutate(Real_Relation = ifelse(a2001 == 8, "child", Real_Relation),
         Generation = ifelse(a2001 == 8, 1, Generation)) %>%
  group_by(hhid_2017) %>%
  mutate(Generation = ifelse(a2001 %in% c(6, 7), 2, Generation),
         Real_Relation = ifelse(a2001 == 7 & sum(a2001 == 7) == 1, "parent", Real_Relation),
         Real_Relation = ifelse(a2001 == 6 & sum(a2001 == 6) == 1, "parent", Real_Relation)) %>%
  mutate(Generation = ifelse(a2001 %in% c(1, 2), 3, Generation)) %>%
  # Create indicators for the conditions
  mutate(one_row_a2001_6 = sum(a2001 == 6) == 1,
         one_row_a2001_7 = sum(a2001 == 7) == 1,
         a2001_7_a2003_1 = any(a2001 == 7 & a2003 == 1),
         a2001_6_a2003_1 = any(a2001 == 6 & a2003 == 1)) %>%
  # Apply complex logic based on the conditions
  mutate(Real_Relation = case_when(
    Generation == 3 & one_row_a2001_6 & a2001_6_a2003_1 ~ "grandparent_f_side",
    Generation == 3 & one_row_a2001_6 ~ "grandparent_m_side",
    Generation == 3 & one_row_a2001_7 & a2001_7_a2003_1 == 1 ~ "grandparent_m_side",
    Generation == 3 & one_row_a2001_7 ~ "grandparent_f_side",
    TRUE ~ Real_Relation # Default case
  )) %>%
  ungroup() %>%
  # Optionally remove helper columns
  select(-one_row_a2001_6, -a2001_7_a2003_1, -a2001_6_a2003_1)







family_information_grandchild <- ind_family_child_as_grandchild %>%
  mutate(Real_Relation = ifelse(Generation == 3 & is.na(Real_Relation), "grandparent", Real_Relation)) %>%
  group_by(hhid_2017) %>%
  filter(!(sum(a2001 == 7)>1)) %>%
  mutate(count_parent = sum(Real_Relation == "parent", na.rm = TRUE))%>%
  mutate(by_07 = ifelse(any(a2001 == 7), a2005[a2001 == 7][1], NA_real_),
         gender_07 = ifelse(any(a2001 == 7), a2003[a2001 == 7][1], NA_real_)) %>%
  mutate(by_07 = replace(by_07, is.na(by_07), first(by_07, na.rm = TRUE))) %>%
  mutate(gender_07 = replace(gender_07, is.na(gender_07), first(gender_07, na.rm = TRUE)))%>%
  mutate(Real_Relation = ifelse(count_parent < 2 &
                                  a2001==6 & 
                                  a2003 != gender_07&
                                  abs(a2005-by_07)<5 
                                & is.na(Real_Relation) == T, "parent",Real_Relation))%>%
  mutate(count_parent = sum(Real_Relation == "parent", na.rm = TRUE))%>%
  filter(count_parent <= 2) |>
  filter(sum(a2001==7)!=0)|>
  ungroup()

family_info_grandchild_clean <- family_information_grandchild |> filter(count_parent == 2) |>
  select(-one_row_a2001_7,
         -count_parent,
         -by_07,
         -gender_07)

family_not_two_parents <- family_information_grandchild |> filter(count_parent != 2) %>% group_by(hhid_2017) %>%
  filter(!(sum(a2001==7) ==0))%>%
  mutate(a2001_7_a2003_1 = if_else(any(a2001 == 7 & a2003 == 1),1,2)) %>%
  mutate(Real_Relation = ifelse(a2001 == 6 & 
                                  a2003 != a2001_7_a2003_1 &
                                  sum(a2001 == 6 & a2003 != a2001_7_a2003_1) == 1,
                                "parent",
                                Real_Relation
                                )) |>
  mutate(count_parent = sum(Real_Relation == "parent", na.rm = TRUE)) |>
  filter(count_parent == 2) |> ungroup() |>
  select(-one_row_a2001_7,
         -count_parent,
         -by_07,
         -gender_07,
         -a2001_7_a2003_1)

ind_family_child_as_grandchild_is_child <- combine(family_not_two_parents,family_info_grandchild_clean)
rm(list = c("family_information_grandchild","family_not_two_parents","family_info_grandchild_clean","ind_family_child_as_grandchild"))

ind_char <- combine(ind_family_child_as_grandchild_is_child, ind_6_is_child)

ind_char <- ind_char |>
  mutate(
    Role = case_when(Real_Relation == "grandparent_m_side" & a2003 == 1 ~ "grand_father_m_side",
                     Real_Relation == "grandparent_m_side" & a2003 == 2 ~ "grand_mother_m_side",
                     Real_Relation == "grandparent_f_side" & a2003 == 1 ~ "grand_father_f_side",
                     Real_Relation == "grandparent_f_side" & a2003 == 2 ~ "grand_mother_f_side",
                     Real_Relation == "parent" & a2003 == 1 ~ "father",
                     Real_Relation == "parent" & a2003 == 2 ~ "mother"
                     )
    )|>
  filter(is.na(Real_Relation) == F) |>drop_all_labels()

load("hh.Rdata")
mothers <- hh_origin %>%
  filter(!is.na(a2032_1)) %>%
  reframe(hhid_2017=hhid_2017,
          a2003=2,
          a2012=a2032_1,
          a2022=a2034_1,
          a2001=3,
          relation = "Mother")|>
  drop_all_labels()

#Create a dataframe for father's education
fathers <- hh_origin %>%
  filter(!is.na(a2032_2)) %>%
  reframe(hhid_2017=hhid_2017,
          a2003=1,
          a2012=a2032_2,
          a2022=a2034_2,
          a2001=3,
          relation = "Father")|>
  drop_all_labels()


#Combine the two dataframes
combined <- rbind(mothers, fathers) |>
  mutate(educ_year = case_when(
    a2012 == 1 ~ 0, # no schooling
    a2012 == 2 ~ 6, #elementary school
    a2012 == 3 ~ 9, #middle school
    a2012 == 4 ~ 12, # high school
    a2012 == 5 ~ 12, #technical high school
    a2012 == 6 ~ 15, #college
    a2012 == 7 ~ 15, #university
    a2012 == 8 ~ 17,#master
    a2012 == 9 ~ 23 # phd
  ))



iden_gen <- ind_char |> filter(a2001==1 & Real_Relation == "parent") |>
  select(hhid_2017,Generation, Role)

combined <- combined |> filter(hhid_2017 %in% iden_gen$hhid_2017)|>
  left_join(iden_gen, join_by(hhid_2017), relationship = "many-to-many")

combined_clean <- combined |> mutate(
  Generation = Generation +1,
  Real_Relation = case_when(
    Role == "mother" & relation == "Mother" ~ "grandparent_m_side",
    Role == "mother" & relation == "Father" ~ "grandparent_m_side",
    Role == "father" & relation == "Mother" ~ "grandparent_f_side",
    Role == "father" & relation == "Father" ~ "grandparent_f_side",
  ),
  Role = case_when(
    Role == "mother" & relation == "Mother" ~ "grand_mother_m_side",
    Role == "mother" & relation == "Father" ~ "grand_father_m_side",
    Role == "father" & relation == "Mother" ~ "grand_mother_f_side",
    Role == "father" & relation == "Father" ~ "grand_father_f_side",
  ),
  a2005 = NA,
  a2006 = NA,
  a2000c = 2
) |>
  select(-relation) |>drop_all_labels()

ind_character <- bind_rows(combined_clean, ind_char) |> ungroup() |>
  mutate(a2000c = if_else(a2000c == 2,0,a2000c),
         a2003 = if_else(a2003==2,0,a2003))
source("functions.R")
ind_character <- replace_labels(ind_origin, ind_character)
ind_character$a2012 <- factor(ind_character$a2012,levels = c(1,2,3,4,5,6,7,8,9),
                              labels = c("no schooling",
                                         "elementary school",
                                         "middle school",
                                         "high school",
                                         "technical high school",
                                         "college",
                                         "bachelor degree",
                                         "master",
                                         "phd"))

is.factor(ind_character$a2012)

save(ind_character, file = "ind_character.Rdata")


