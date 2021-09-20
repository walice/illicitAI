# Data Visualization
# Alice Lepissier
# alice.lepissier@gmail.com
# Chapter 3

## ## ## ## ## ## ## ## ## ## ##
# INDEX                     ####
## ## ## ## ## ## ## ## ## ## ##
# Preamble
# Import results
# .. For disaggregated panel
# .. For aggregated panel



## ## ## ## ## ## ## ## ## ## ##
# PREAMBLE                  ####
## ## ## ## ## ## ## ## ## ## ##

library(arrow)
library(here)
library(tidyverse)



## ## ## ## ## ## ## ## ## ## ##
# IMPORT RESULTS            ####
## ## ## ## ## ## ## ## ## ## ##

# .. For disaggregated panel ####
train_smp <- arrow::read_feather(here("Results", "train_smp.feather"))
test_smp <- arrow::read_feather(here("Results", "test_smp.feather"))

preds.LM.train <- arrow::read_feather("Results/preds.LM.train.feather") %>%
  rename(preds_LM_train = "0")
preds.lightGBM.train <- arrow::read_feather("Results/preds.lightGBM.train.feather") %>%
  rename(preds_lightGBM_train = "0")
preds.NN.train <- arrow::read_feather("Results/preds.NN.train.feather") %>%
  rename(preds_NN_train = "0")
preds.SVM.train <- arrow::read_feather("Results/preds.SVM.train.feather") %>%
  rename(preds_SVM_train = "0")
preds.RF.train <- arrow::read_feather("Results/preds.RF.train.feather") %>%
  rename(preds_RF_train = "0")

preds.LM.test <- arrow::read_feather("Results/preds.LM.test.feather") %>%
  rename(preds_LM_test = "0")
preds.lightGBM.test <- arrow::read_feather("Results/preds.lightGBM.test.feather") %>%
  rename(preds_lightGBM_test = "0")
preds.NN.test <- arrow::read_feather("Results/preds.NN.test.feather") %>%
  rename(preds_NN_test = "0")
preds.SVM.test <- arrow::read_feather("Results/preds.SVM.test.feather") %>%
  rename(preds_SVM_test = "0")
preds.RF.test <- arrow::read_feather("Results/preds.RF.test.feather") %>%
  rename(preds_RF_test = "0")

# train_results <- bind_cols(train_smp, 
#                            preds.LM.train, preds.lightGBM.train, preds.NN.train,
#                            preds.SVM.train, preds.RF.train)
train_results <- bind_cols(train_smp, 
                           preds.LM.train, preds.lightGBM.train)

# test_results <- bind_cols(test_smp, 
#                           preds.LM.test, preds.lightGBM.test, preds.NN.test,
#                           preds.SVM.test, preds.RF.test)
test_results <- bind_cols(test_smp, 
                          preds.LM.test, preds.lightGBM.test)

results <- bind_rows(train_results, test_results)

results <- results %>%
  mutate_at(vars(ends_with("train")),
            ~exp(.)) %>%
  mutate_at(vars(ends_with("test")),
            ~exp(.))
  
results <- results %>%
  mutate(preds_LM = ifelse(year <= 2014, preds_LM_train, preds_LM_test),
         preds_lightGBM = ifelse(year <= 2014, preds_lightGBM_train, preds_lightGBM_test),
         preds_NN = ifelse(year <= 2014, preds_NN_train, preds_NN_test),
         preds_SVM = ifelse(year <= 2014, preds_SVM_train, preds_SVM_test),
         preds_RF = ifelse(year <= 2014, preds_RF_train, preds_RF_test))

# viz <- results %>%
#   pivot_longer(c("ln.Tot_IFF", "Tot_IFF",
#                  "preds_LM", "preds_lightGBM", "preds_NN",
#                  "preds_SVM", "preds_RF")) %>%
#   select(c("reporter.ISO", "partner.ISO",
#            "year", "name", "value"))
viz <- results %>%
  pivot_longer(c("GER_Tot_IFF",
                 "preds_LM", "preds_lightGBM")) %>%
  select(c("reporter.ISO", "partner.ISO", "commodity.code", "SITC.section",
           "year", "name", "value"))

ggplot(viz %>%
         filter(name != "ln.Tot_IFF") %>%
         #filter(name != "preds_LM") %>%
         group_by(year, name) %>%
         summarize(value = sum(value)) %>%
         ungroup()) +
  geom_line(aes(x = year,
                y = value / 10^9,
                col = name))

reporters <- c("USA", "CHN", "GBR",
               "CHE", "FRA", "SGP",
               "COG", "YEM", "KHM", "TZA")
for (r in 1:length(reporters)){
  g <- ggplot(viz %>%
           filter(reporter.ISO == reporters[r]) %>%
           #filter(partner.ISO == "CHN") %>%
           filter(name != "ln.Tot_IFF") %>%
           filter(name != "preds_LM") %>%
           group_by(year, name) %>%
           summarize(value = sum(value)) %>%
           ungroup()) +
    geom_line(aes(x = year,
                  y = value / 10^9,
                  col = name))
  ggsave(g,
         file = (here("Figures", paste0("preds_", reporters[r], ".png"))),
         width = 6, height = 5, units = "in")
}

ggplot(viz %>%
              filter(reporter.ISO == "USA") %>%
              filter(SITC.section == "Manufactured Goods") %>%
              filter(name != "ln.Tot_IFF") %>%
              #filter(name != "preds_LM") %>%
              group_by(year, name) %>%
              summarize(value = sum(value)) %>%
              ungroup()) +
  geom_line(aes(x = year,
                y = value / 10^9,
                col = name))


# .. For aggregated panel ####
train_agg_smp <- arrow::read_feather(here("Results", "train_agg_smp.feather"))
test_agg_smp <- arrow::read_feather(here("Results", "test_agg_smp.feather"))

preds.LM.train_agg <- arrow::read_feather("Results/preds.LM.train_agg.feather") %>%
  rename(preds_LM_train_agg = "0")
preds.lightGBM.train_agg <- arrow::read_feather("Results/preds.lightGBM.train_agg.feather") %>%
  rename(preds_lightGBM_train_agg = "0")
preds.NN.train_agg <- arrow::read_feather("Results/preds.NN.train_agg.feather") %>%
  rename(preds_NN_train_agg = "0")
preds.SVM.train_agg <- arrow::read_feather("Results/preds.SVM.train_agg.feather") %>%
  rename(preds_SVM_train_agg = "0")
preds.RF.train_agg <- arrow::read_feather("Results/preds.RF.train_agg.feather") %>%
  rename(preds_RF_train_agg = "0")

preds.LM.test_agg <- arrow::read_feather("Results/preds.LM.test_agg.feather") %>%
  rename(preds_LM_test_agg = "0")
preds.lightGBM.test_agg <- arrow::read_feather("Results/preds.lightGBM.test_agg.feather") %>%
  rename(preds_lightGBM_test_agg = "0")
preds.NN.test_agg <- arrow::read_feather("Results/preds.NN.test_agg.feather") %>%
  rename(preds_NN_test_agg = "0")
preds.SVM.test_agg <- arrow::read_feather("Results/preds.SVM.test_agg.feather") %>%
  rename(preds_SVM_test_agg = "0")
preds.RF.test_agg <- arrow::read_feather("Results/preds.RF.test_agg.feather") %>%
  rename(preds_RF_test_agg = "0")

train_agg_results <- bind_cols(train_agg_smp,
                               preds.LM.train_agg, preds.lightGBM.train_agg, preds.NN.train_agg,
                               preds.SVM.train_agg, preds.RF.train_agg)

test_agg_results <- bind_cols(test_agg_smp,
                              preds.LM.test_agg, preds.lightGBM.test_agg, preds.NN.test_agg,
                              preds.SVM.test_agg, preds.RF.test_agg)

results_agg <- bind_rows(train_agg_results, test_agg_results)

results_agg <- results_agg %>%
  mutate_at(vars(ends_with("train_agg")),
            ~exp(.)) %>%
  mutate_at(vars(ends_with("test_agg")),
            ~exp(.))

results_agg <- results_agg %>%
  mutate(preds_LM = ifelse(year <= 2014, preds_LM_train_agg, preds_LM_test_agg),
         preds_lightGBM = ifelse(year <= 2014, preds_lightGBM_train_agg, preds_lightGBM_test_agg),
         preds_NN = ifelse(year <= 2014, preds_NN_train_agg, preds_NN_test_agg),
         preds_SVM = ifelse(year <= 2014, preds_SVM_train_agg, preds_SVM_test_agg),
         preds_RF = ifelse(year <= 2014, preds_RF_train_agg, preds_RF_test_agg))

viz <- results_agg %>%
  pivot_longer(c("ln.Tot_IFF", "Tot_IFF",
                 "preds_LM", "preds_lightGBM", "preds_NN",
                 "preds_SVM", "preds_RF")) %>%
  select(c("reporter.ISO", "partner.ISO",
           "year", "name", "value"))

ggplot(viz %>%
         filter(name != "ln.Tot_IFF") %>%
         filter(name != "preds_LM") %>%
         group_by(year, name) %>%
         summarize(value = sum(value)) %>%
         ungroup()) +
  geom_line(aes(x = year,
                y = value / 10^9,
                col = name))

reporters <- c("USA", "CHN", "GBR",
               "CHE", "FRA", "SGP",
               "COG", "YEM", "KHM", "TZA")
for (r in 1:length(reporters)){
  g <- ggplot(viz %>%
                filter(reporter.ISO == reporters[r]) %>%
                #filter(partner.ISO == "CHN") %>%
                filter(name != "ln.Tot_IFF") %>%
                filter(name != "preds_LM") %>%
                group_by(year, name) %>%
                summarize(value = sum(value)) %>%
                ungroup()) +
    geom_line(aes(x = year,
                  y = value / 10^9,
                  col = name))
  ggsave(g,
         file = (here("Figures", paste0("preds_", reporters[r], ".png"))),
         width = 6, height = 5, units = "in")
}

ggplot(viz %>%
              filter(reporter.ISO == "USA") %>%
              filter(partner.ISO == "CHN") %>%
              filter(name != "ln.Tot_IFF") %>%
              filter(name != "preds_LM") %>%
              group_by(year, name) %>%
              summarize(value = sum(value)) %>%
              ungroup()) +
  geom_line(aes(x = year,
                y = value / 10^9,
                col = name))
