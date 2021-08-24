# Data Visualization
# Alice Lepissier
# alice.lepissier@gmail.com
# Chapter 3

## ## ## ## ## ## ## ## ## ## ##
# INDEX                     ####
## ## ## ## ## ## ## ## ## ## ##
# Preamble
# Import results



## ## ## ## ## ## ## ## ## ## ##
# PREAMBLE                  ####
## ## ## ## ## ## ## ## ## ## ##

library(arrow)
library(here)
library(tidyverse)



## ## ## ## ## ## ## ## ## ## ##
# IMPORT RESULTS            ####
## ## ## ## ## ## ## ## ## ## ##

test_smp <- arrow::read_feather(here("Results", "test_smp_trans.feather"))
preds <- arrow::read_feather("Results/preds.feather") %>%
  rename(preds = "0")

viz <- bind_cols(test_smp, preds)

ggplot(viz) +
  geom_point(aes(x = ln.Tot_IFF,
                 y = preds))


vars <- c("dist", "contig", "comcol", "col45", "comlang_off", "comleg_posttrans",
          "ln.gdp_o", "ln.gdp_d", "ln.pop_o", "ln.pop_d",
          "ihs.entry_cost_o", "ihs.entry_cost_d",
          "rta", "ihs.tariff",
          "ka_o", "ka_d", "kai_o", "kao_o", "kai_d", "kao_d",
          "rSecrecyScore", "pSecrecyScore",
          "rCPI", "pCPI",
          "rCorrCont", "pCorrCont",
          "rRegQual", "pRegQual", 
          "rRuleLaw", "pRuleLaw")



