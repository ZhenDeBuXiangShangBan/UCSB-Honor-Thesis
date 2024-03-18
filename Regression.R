  rm(list=ls())
  library(tidyverse)
  library(expss)
  library(sjPlot)
  library(sjmisc)
  library(sjlabelled)
  library(haven)
  
  master <- read_dta("D:/ECON 196A/Structure/master2017_20210415_version13.dta",
                     encoding="UTF-8")
  master_weight <- master |> distinct(hhid_2017,swgt_h, prov_code)
  rm(master)
  
  help(tab_model)
  
  tab <- function(a){
    tab_model(a,digits=4, show.ci = F, show.se = T, string.se = "SE")
  }

  #data <- read.csv("one_child_family_characteristics.csv") |> select(-X)
  
  #data2 <- left_join(reg_data, data, join_by(hhid_2017)) |> distinct(hhid_2017, .keep_all = T)
  #save(data2, file = "data_for_regression.Rdata")
  load("data_for_regression.Rdata")
  data<-data2
  rm(data2)
  
  
  data <- data |> group_by(hhid_2017) |> mutate(hhid_2017 = as.character(hhid_2017),
                                                three_gen = if_else(sum(grand_father_m_side_in_house,
                                                                        grand_mother_m_side_in_house,
                                                                        grand_father_f_side_in_house,
                                                                        grand_mother_f_side_in_house,
                                                                        na.rm = T)==0,0,1),
                                                child_educ_year_gender_dummy = child_current_educ_year*child_gender) |> ungroup()
  
  data <- data |> left_join(master_weight, join_by(hhid_2017))
  
  
  
  data <- data |> apply_labels(
    three_gen = "Three-Generation Coresidence",
    father_educ_year = "Educated Years of Father",
    mother_educ_year = "Educated Years of Mother",
    total_income = "Household Total Income",
    total_consump = "Household Total Consumption",
    pure_asset = "Household Pure Asset",
    child_current_educ_year = "Educated Years of Child",
    child_gender = "Son",
    child_educ_year_gender_dummy = "Educated Years of Child X Son",
    gp_highest_educ_level = "Highest Education Attainment of Grandparent",
    education_spending = "Household Education Expenditure",
    gp_highest_educ_year = "Educated Years of the Most Educated Grandparent"
  )
  
  data <- data |> filter(child_current_educ_year <=9)
  
  
  load("ind_character.Rdata")
  
  three_gen_highest_gp_educ <- ind_character |> filter(Generation == 3) |> group_by(hhid_2017) |> filter(a2000c == 1) |>
    filter(educ_year == max(educ_year)) |>
    reframe(hhid_2017 = hhid_2017,
            gp_highest_educ_level = max(as.numeric(a2012)),
            gp_highest_educ_year = max(educ_year),
            gp_highest_educ_gender = max(a2003)
    ) |> 
    ungroup() |> 
    distinct(hhid_2017, .keep_all = T) |> 
    drop_all_labels()
  
  two_gen_highest_gp_educ <- ind_character |> filter(Generation == 3) |> group_by(hhid_2017) |> filter(!any(a2000c==1)) |>
    filter(educ_year == max(educ_year)) |>
    reframe(hhid_2017 = hhid_2017,
            gp_highest_educ_level = max(as.numeric(a2012)),
            gp_highest_educ_year = max(educ_year),
            gp_highest_educ_gender = max(a2003)
            ) |> ungroup() |> 
    distinct(hhid_2017, .keep_all = T) |> 
    drop_all_labels()
  
  
  
  
  all_gen_highest_gp_educ <- rbind(three_gen_highest_gp_educ,two_gen_highest_gp_educ)
  rm(two_gen_highest_gp_educ,three_gen_highest_gp_educ)
  
  all_gen <- left_join(data, all_gen_highest_gp_educ, join_by(hhid_2017)) |>
    mutate(gp_educ_year_dummy = three_gen*gp_highest_educ_year)
  
  all_gen$gp_highest_educ_level <- factor(all_gen$gp_highest_educ_level,levels = c(1,2,3,4,5,6,7,8,9),
                                          labels = c("no schooling",
                                                     "elementary school",
                                                     "middle school",
                                                     "high school",
                                                     "technical high school",
                                                     "college",
                                                     "bachelor degree",
                                                     "master",
                                                     "phd"))
  all_gen<- all_gen |> apply_labels(gp_highest_educ_year = "Highest Educated Years of Grandparents",
                                    father_educ_level = "Education Level of the Father",
                                    education_spending = "Household Education Expenditure",
                                    gp_highest_educ_level = "Education Attainment of Most Educated Grandparent",
                                    gp_highest_educ_year = "Education Year of Most Educated Grandparent",
                                    age = "Age of the Child",
                                    father_in_house = "Does Father Live in Household",
                                    mother_in_house = "Does Mother Live in Household",
                                    gp_educ_year_dummy = "Education Year of Most Educated Grandparent X Three-Generational Coresidence"
                                    )
  # all_gen <- all_gen |> mutate(mother_educ_level = case_when(mother_educ_level == "no schooling"~1,
  #                                                            mother_educ_level =="elementary school" ~2,
  #                                                            mother_educ_level =="middle school" ~3,
  #                                                            mother_educ_level =="technical high school"~4,
  #                                                            mother_educ_level =="high school"~5,
  #                                                            mother_educ_level == "college"~6,
  #                                                            mother_educ_level =="bachelor degree"~7,
  #                                                            mother_educ_level =="master"~8,
  #                                                            mother_educ_level == "phd"~9 ),
  #                              father_educ_level = case_when(father_educ_level == "no schooling"~1,
  #                                                            father_educ_level =="elementary school" ~2,
  #                                                            father_educ_level =="middle school" ~3,
  #                                                            father_educ_level =="technical high school"~4,
  #                                                            father_educ_level =="high school"~5,
  #                                                            father_educ_level == "college"~6,
  #                                                            father_educ_level =="bachelor degree"~7,
  #                                                            father_educ_level =="master"~8,
  #                                                            father_educ_level == "phd"~9 ),
  #                              gp_highest_educ_level = case_when(gp_highest_educ_level == "no schooling"~1,
  #                                                                gp_highest_educ_level =="elementary school" ~2,
  #                                                                gp_highest_educ_level =="middle school" ~3,
  #                                                                gp_highest_educ_level =="technical high school"~4,
  #                                                                gp_highest_educ_level =="high school"~5,
  #                                                                gp_highest_educ_level == "college"~6,
  #                                                                gp_highest_educ_level =="bachelor degree"~7,
  #                                                                gp_highest_educ_level =="master"~8,
  #                                                                gp_highest_educ_level == "phd"~9 ))
  # 
  # 
  # all_gen$mother_educ_level <- factor(all_gen$mother_educ_level,levels = c(1,2,3,4,5,6,7,8,9),
  #                                     labels = c("no schooling",
  #                                                "elementary school",
  #                                                "middle school",
  #                                                "high school",
  #                                                "technical high school",
  #                                                "college",
  #                                                "bachelor degree",
  #                                                "master",
  #                                                "phd"))
  # all_gen$father_educ_level <- factor(all_gen$father_educ_level,levels = c(1,2,3,4,5,6,7,8,9),
  #                                     labels = c("no schooling",
  #                                                "elementary school",
  #                                                "middle school",
  #                                                "high school",
  #                                                "technical high school",
  #                                                "college",
  #                                                "bachelor degree",
  #                                                "master",
  #                                                "phd"))
  # all_gen$gp_highest_educ_level <- factor(all_gen$gp_highest_educ_level,levels = c(1,2,3,4,5,6,7,8,9),
  #                                         labels = c("no schooling",
  #                                                    "elementary school",
  #                                                    "middle school",
  #                                                    "high school",
  #                                                    "technical high school",
  #                                                    "college",
  #                                                    "bachelor degree",
  #                                                    "master",
  #                                                    "phd"))
  # 
  # all_gen$child_current_educ_year <- factor(all_gen$child_current_educ_year, levels = c(1,2,3,4,5,6,7,8,9),
  #                                           labels = c("Elementary School 1" ,
  #                                                      "Elementary School 2" ,
  #                                                      "Elementary School 3" ,
  #                                                      "Elementary School 4" ,
  #                                                      "Elementary School 5" ,
  #                                                      "Elementary School 6" ,
  #                                                      "Middle School 1" ,
  #                                                      "Middle School 2" ,
  #                                                      "Middle School 3"))
  # 
  
  
  
  
  
  
  three_gen <- all_gen |> filter(three_gen == 1)
  two_gen <- all_gen |> filter(three_gen == 0)
  
  rm(all_gen_highest_gp_educ)
  
  # base <- c("father", "mother", "grand_father_m_side", "grand_father_f_side", "grand_mother_m_side", "grand_mother_f_side")
  # 
  # wealth_char <- "total_income + total_consump + pure_asset"
  # child_char <- "child_current_educ_year+age"
  # 
  # 
  # create_Formula <- function(dept,base_vars, main_vars, interaction = NULL, Factor = F, factor_sign = "*", more_var = NULL){
  #   if(is.null(interaction) == T){
  #     vars <- if_else(Factor == F,
  #                     paste(base_vars, "_", main_vars, sep = "", collapse = "+"),
  #                     paste("as.factor(", base_vars, "_",main_vars, ")", sep = "", collapse = "+"))
  #   }else{
  #     vars <- if_else(Factor == F,
  #                     paste(base_vars, "_", main_vars,factor_sign,base_vars,"_",interaction, sep = "", collapse = "+"),
  #                     paste("as.factor(", base_vars, "_",main_vars, ")", factor_sign,base_vars,"_",interaction, sep = "", collapse = "+"))
  #   }
  #   formula_final <- if_else(is.null(more_var) == T , 
  #                            paste(dept, "~", vars),
  #                            paste(dept, "~", vars, "+",more_var))
  # }
  # 
  # model <- lm(data = data,
  #             education_spending ~ create_Formula(base, "educ_year"))
  # 
  # 
  # 
  # model <- lm(data = data,
  #             formula = as.formula(
  #               create_Formula("education_spending", 
  #                              base, 
  #                              "educ_year",
  #                              Factor = F,
  #                              interaction = "in_house",
  #                              more_var = paste(wealth_char, child_char, sep="+")))
  #             )
  # 
  # model |> tab_model()
  # 
  # 
  # 
  # model <- lm (data=data,
  #              education_spending ~ 
  #                as.factor(father_educ_level)+
  #                as.factor(mother_educ_level)+
  #                as.factor(grand_father_m_side_educ_level)+
  #                as.factor(grand_father_f_side_educ_level)+
  #                as.factor(grand_mother_m_side_educ_level)+
  #                as.factor(grand_mother_f_side_educ_level) + 
  #                total_income + total_consump + pure_asset+child_current_educ_year+age,
  #              na.action = na.exclude)
  # 
  # 
  
  
  ###step 1
  
  #regression on coresidence
  model <- lm(data=all_gen,
              education_spending ~
                three_gen +
                father_educ_year+
                mother_educ_year+
                total_income + total_consump+ pure_asset + child_current_educ_year +
                child_educ_year_gender_dummy,
              na.action = na.omit)
  
  model |> tab()

  

  
  #3.3
  #at least some grandparent
  model <- lm(data=all_gen,
              education_spending ~
                three_gen +
                gp_educ_year_dummy +
                gp_highest_educ_year +
                father_educ_year+
                mother_educ_year+
                total_income + total_consump+ pure_asset + child_current_educ_year +
                child_educ_year_gender_dummy,
              na.action = na.omit)
  
  model |> tab()

  

  


###use factor
  model3.21 <- lm(data=all_gen,
                 education_spending ~
                   gp_highest_educ_level + 
                   gp_highest_educ_level:three_gen +
                   father_educ_year+
                   mother_educ_year+
                   total_income + total_consump+ pure_asset + child_current_educ_year +
                   child_educ_year_gender_dummy)
  model3.21 |> tab()
  
  # prov
  model <- lm(data=all_gen,
              education_spending ~
                as.factor(prov_code)+
                three_gen +
                gp_educ_year_dummy +
                gp_highest_educ_year +
                father_educ_year+
                mother_educ_year+
                total_income + total_consump+ pure_asset + child_current_educ_year +
                child_educ_year_gender_dummy,
              na.action = na.omit)
  
  model |> tab()

