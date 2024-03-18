rm(list = ls())
library(tidyverse)
library(haven)
library(broom)
library(knitr)
library(expss)

ind_origin <-  read_dta("ind2017_20191202_version13.dta",encoding="UTF-8")

ind<- ind_origin |> filter(s3000a == 1)
ind <- ind_origin |> filter(is.na(s8017) == F)
ind <- ind  |> select(hhid_2017, 
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
                                   s8014,
                            s8017,
                            s8018,
                            s8018_imp)


ind <- ind |>
  mutate(sum_educ_spending = rowSums(select(ind, 
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

ind <- ind |> mutate(education_spending = case_when(
  s8017 == 1 | s8017 == 2 ~s8018,
  s8017 == 3 ~ sum_educ_spending)
) |> mutate(
  education_spending = if_else(is.na(education_spending) == T, s8018_imp, education_spending)
)



educ_cost <- ind |> select(hhid_2017, education_spending)
##all from elementary school 1 to high school 3rd
educ_cost = apply_labels(educ_cost,
             education_spending = "Education Expenditure")
save(educ_cost, file = "educ_cost.Rdata")
