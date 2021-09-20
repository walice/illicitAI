# Data Visualization
# Alice Lepissier
# alice.lepissier@gmail.com
# Chapter 3

## ## ## ## ## ## ## ## ## ## ##
# INDEX                     ####
## ## ## ## ## ## ## ## ## ## ##
# Preamble
# Import results
# Disaggregated Predictions
# Aggregated Predictions
# .. Gross outflows
# .. Gross inflows



## ## ## ## ## ## ## ## ## ## ##
# PREAMBLE                  ####
## ## ## ## ## ## ## ## ## ## ##

library(arrow)
library(here)
library(tidyverse)



## ## ## ## ## ## ## ## ## ## ##
# DISAGGREGATED PREDICTIONS ####
## ## ## ## ## ## ## ## ## ## ##
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



## ## ## ## ## ## ## ## ## ## ##
# AGGREGATED PREDICTIONS    ####
## ## ## ## ## ## ## ## ## ## ##

train_agg_smp <- arrow::read_feather(here("Results", "train_agg_smp.feather"))
test_agg_smp <- arrow::read_feather(here("Results", "test_agg_smp.feather"))


# .. Gross outflows ####
preds.LM.train_out_agg <- arrow::read_feather("Results/preds.LM.train_out_agg.feather") %>%
  rename(preds_LM_train_out_agg = "0")
preds.lightGBM.train_out_agg <- arrow::read_feather("Results/preds.lightGBM.train_out_agg.feather") %>%
  rename(preds_lightGBM_train_out_agg = "0")
# preds.NN.train_out_agg <- arrow::read_feather("Results/preds.NN.train_out_agg.feather") %>%
#   rename(preds_NN_train_out_agg = "0")
# preds.SVM.train_out_agg <- arrow::read_feather("Results/preds.SVM.train_out_agg.feather") %>%
#   rename(preds_SVM_train_out_agg = "0")
preds.RF.train_out_agg <- arrow::read_feather("Results/preds.RF.train_out_agg.feather") %>%
  rename(preds_RF_train_out_agg = "0")

preds.LM.test_out_agg <- arrow::read_feather("Results/preds.LM.test_out_agg.feather") %>%
  rename(preds_LM_test_out_agg = "0")
preds.lightGBM.test_out_agg <- arrow::read_feather("Results/preds.lightGBM.test_out_agg.feather") %>%
  rename(preds_lightGBM_test_out_agg = "0")
# preds.NN.test_out_agg <- arrow::read_feather("Results/preds.NN.test_out_agg.feather") %>%
#   rename(preds_NN_test_out_agg = "0")
# preds.SVM.test_out_agg <- arrow::read_feather("Results/preds.SVM.test_out_agg.feather") %>%
#   rename(preds_SVM_test_out_agg = "0")
preds.RF.test_out_agg <- arrow::read_feather("Results/preds.RF.test_out_agg.feather") %>%
  rename(preds_RF_test_out_agg = "0")

train_out_agg_results <- bind_cols(train_agg_smp,
                                   preds.LM.train_out_agg, 
                                   preds.lightGBM.train_out_agg,
                                   # preds.SVM.train_out_agg, 
                                   preds.RF.train_out_agg)

test_out_agg_results <- bind_cols(test_agg_smp,
                                  preds.LM.test_out_agg, 
                                  preds.lightGBM.test_out_agg,
                                  # preds.SVM.test_out_agg, 
                                  preds.RF.test_out_agg)

results_out_agg <- bind_rows(train_out_agg_results, test_out_agg_results)

results_out_agg <- results_out_agg %>%
  mutate(Tot_IFF_t = exp(ln.Tot_IFF_t)) %>%
  mutate_at(vars(ends_with("out_agg")),
            ~exp(.))

results_out_agg <- results_out_agg %>%
  mutate(preds_LM = ifelse(year <= 2014, preds_LM_train_out_agg, preds_LM_test_out_agg),
         preds_lightGBM = ifelse(year <= 2014, preds_lightGBM_train_out_agg, preds_lightGBM_test_out_agg),
         # preds_SVM = ifelse(year <= 2014, preds_SVM_train_out_agg, preds_SVM_test_out_agg),
         preds_RF = ifelse(year <= 2014, preds_RF_train_out_agg, preds_RF_test_out_agg))
         # preds_NN = ifelse(year <= 2014, preds_NN_train_out_agg, preds_NN_test_out_agg))

viz_out <- results_out_agg %>%
  pivot_longer(c("Tot_IFF_t",
                 "preds_LM", 
                 "preds_lightGBM",
                 # "preds_SVM", 
                 "preds_RF")) %>%
               # , "preds_NN"))
  select(c("reporter.ISO", "partner.ISO",
           "year", "name", "value"))

# viz_out <- bind_rows(out_viz_train %>%
#                    select(true.value = ln.Tot_IFF_t, predicted.value = out_preds_trn, year),
#                  out_viz_test %>%
#                    select(true.value = ln.Tot_IFF_t, predicted.value = out_preds_tst, year)) %>%
#   mutate_at(vars(ends_with("value")),
#             ~exp(.)) %>%
#   pivot_longer(c("true.value", "predicted.value"))
  

g_out <- ggplot(viz_out %>%
           # filter(name != "preds_SVM") %>%
           group_by(year, name) %>%
           summarize(value = sum(value)) %>%
           ungroup() %>%
           mutate(name = factor(name,
                                levels = c("Tot_IFF_t",
                                           "preds_LM",
                                           "preds_RF",
                                           "preds_lightGBM")))) +
  geom_line(aes(x = year,
                y = value / 10^6,
                col = name)) +
  # geom_line(data = out_viz %>%
  #             group_by(year, name) %>%
  #             summarize(value = sum(value)) %>%
  #             ungroup(),
  #           aes(x = year,
  #               y = value / 10^6,
  #               col = name)) +
  labs(x = "Year",
       y = "Illicit flow in billion USD",
       title = "Predictions from all models",
       subtitle = "For gross outflows")
g_out
ggsave(g_out,
       file = here("Figures", "preds_out_fullmod_all.png"),
       width = 6, height = 5, units = "in")


# .. Gross inflows ####
preds.LM.train_in_agg <- arrow::read_feather("Results/preds.LM.train_in_agg.feather") %>%
  rename(preds_LM_train_in_agg = "0")
preds.lightGBM.train_in_agg <- arrow::read_feather("Results/preds.lightGBM.train_in_agg.feather") %>%
  rename(preds_lightGBM_train_in_agg = "0")
# preds.NN.train_in_agg <- arrow::read_feather("Results/preds.NN.train_in_agg.feather") %>%
#   rename(preds_NN_train_in_agg = "0")
# preds.SVM.train_in_agg <- arrow::read_feather("Results/preds.SVM.train_in_agg.feather") %>%
#   rename(preds_SVM_train_in_agg = "0")
preds.RF.train_in_agg <- arrow::read_feather("Results/preds.RF.train_in_agg.feather") %>%
  rename(preds_RF_train_in_agg = "0")

preds.LM.test_in_agg <- arrow::read_feather("Results/preds.LM.test_in_agg.feather") %>%
  rename(preds_LM_test_in_agg = "0")
preds.lightGBM.test_in_agg <- arrow::read_feather("Results/preds.lightGBM.test_in_agg.feather") %>%
  rename(preds_lightGBM_test_in_agg = "0")
# preds.NN.test_in_agg <- arrow::read_feather("Results/preds.NN.test_in_agg.feather") %>%
#   rename(preds_NN_test_in_agg = "0")
# preds.SVM.test_in_agg <- arrow::read_feather("Results/preds.SVM.test_in_agg.feather") %>%
#   rename(preds_SVM_test_in_agg = "0")
preds.RF.test_in_agg <- arrow::read_feather("Results/preds.RF.test_in_agg.feather") %>%
  rename(preds_RF_test_in_agg = "0")

train_in_agg_results <- bind_cols(train_agg_smp,
                                  preds.LM.train_in_agg, 
                                  preds.lightGBM.train_in_agg,
                                  # preds.SVM.train_in_agg, 
                                  preds.RF.train_in_agg)

test_in_agg_results <- bind_cols(test_agg_smp,
                                 preds.LM.test_in_agg, 
                                 preds.lightGBM.test_in_agg,
                                 # preds.SVM.test_in_agg, 
                                 preds.RF.test_in_agg)

results_in_agg <- bind_rows(train_in_agg_results, test_in_agg_results)

results_in_agg <- results_in_agg %>%
  mutate(In_Tot_IFF_t = exp(ln.In_Tot_IFF_t)) %>%
  mutate_at(vars(ends_with("in_agg")),
            ~exp(.))

results_in_agg <- results_in_agg %>%
  mutate(preds_LM = ifelse(year <= 2014, preds_LM_train_in_agg, preds_LM_test_in_agg),
         preds_lightGBM = ifelse(year <= 2014, preds_lightGBM_train_in_agg, preds_lightGBM_test_in_agg),
         # preds_SVM = ifelse(year <= 2014, preds_SVM_train_in_agg, preds_SVM_test_in_agg),
         preds_RF = ifelse(year <= 2014, preds_RF_train_in_agg, preds_RF_test_in_agg))
         # preds_NN = ifelse(year <= 2014, preds_NN_train_in_agg, preds_NN_test_in_agg))

viz_in <- results_in_agg %>%
  pivot_longer(c("In_Tot_IFF_t",
                 "preds_LM", 
                 "preds_lightGBM",
                 # "preds_SVM", 
                 "preds_RF")) %>%
  # , "preds_NN"))
  select(c("reporter.ISO", "partner.ISO",
           "year", "name", "value"))

g_in <- ggplot(viz_in %>%
                # filter(name != "preds_SVM") %>%
                group_by(year, name) %>%
                summarize(value = sum(value)) %>%
                ungroup() %>%
                mutate(name = factor(name,
                                     levels = c("In_Tot_IFF_t",
                                                "preds_LM",
                                                "preds_RF",
                                                "preds_lightGBM")))) +
  geom_line(aes(x = year,
                y = value / 10^6,
                col = name)) +
  # geom_line(data = in_viz %>%
  #             group_by(year, name) %>%
  #             summarize(value = sum(value)) %>%
  #             ungroup(),
  #           aes(x = year,
  #               y = value / 10^6,
  #               col = name)) +
  labs(x = "Year",
       y = "Illicit flow in billion USD",
       title = "Predictions from all models",
       subtitle = "For gross inflows")
g_in
ggsave(g_in,
       file = here("Figures", "preds_in_fullmod_all.png"),
       width = 6, height = 5, units = "in")


########
reporters <- c("USA", "CHN", "GBR",
               "CHE", "FRA", "SGP",
               "COG", "YEM", "KHM", "TZA")
for (r in 1:length(reporters)){
  g <- ggplot(viz %>%
                filter(reporter.ISO == reporters[r]) %>%
                #filter(partner.ISO == "CHN") %>%
                filter(name != "ln.Tot_IFF") %>%
                # filter(name != "preds_LM") %>%
                group_by(year, name) %>%
                summarize(value = sum(value)) %>%
                ungroup()) +
    geom_line(aes(x = year,
                  y = value / 10^6,
                  col = name))
  # ggsave(g,
         # file = (here("Figures", paste0("preds_", reporters[r], ".png"))),
         # width = 6, height = 5, units = "in")
}

ggplot(viz %>%
              filter(reporter.ISO == "USA") %>%
              filter(partner.ISO == "CHN") %>%
              filter(name != "ln.Tot_IFF") %>%
              # filter(name != "preds_LM") %>%
              group_by(year, name) %>%
              summarize(value = sum(value)) %>%
              ungroup()) +
  geom_line(aes(x = year,
                y = value / 10^9,
                col = name))
