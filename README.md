# UCSB-Honor-Thesis
#Hi! For Whoever reads these documents, this is all the code I used for doing my honor thesis economic research in transgenerational coresidence and its impacts on family education expenditure.

I have imported all my coding in R to the code folder and all the data I used/created in the Data file.


**family_relationship.R** used the family relationship index to create a logic in identifying the position of each member in the family (who is the father, mother, child, grandparent?) to further clean the dataset
  The final product is **ind_character.Rdata**
  
**All_Member_Refill.R** used the supplemented information given in another origin dataset to add some missing information (e.g., grandparent's education information of who are not living in the household) to make ind_character.Rdata better in analyzing
  
**Education Cost.R** cleaned and calculated the real education spending of each family using the equation given by the code book
  The final product is **educ_cost.Rdata**

**wealth_characteristics.R** computed the total income, total consumption, and household pure asset according to the individual family.
  The final product is **wealth_characteristics_censor_include.Rdata**

**functions** is a self-written function to re-create the labels of the data frames. Labels will create errors in merging data frames, I use this function to re-given the dropped label

**Regression** is the space I did different regressions

