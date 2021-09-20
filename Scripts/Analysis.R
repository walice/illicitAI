# Analysis
# Alice Lepissier
# alice.lepissier@gmail.com
# Chapter 3

## ## ## ## ## ## ## ## ## ## ##
# INDEX                     ####
## ## ## ## ## ## ## ## ## ## ##
# Preamble
# Import results
# Set Up Samples
# .. For disaggregated panel
# .. For aggregated panel
# OLS Gravity Models
# .. Functions
# .. Baseline specification (Walker TBML gravity model)
# Train and Test
# .. Visualize predictions



## ## ## ## ## ## ## ## ## ## ##
# PREAMBLE                  ####
## ## ## ## ## ## ## ## ## ## ##

library(arrow)
library(here)
# source(here("Scripts", "Data Pre-Processing.R"))
library(caret)
library(kableExtra)
library(lfe)
# library(parsnip)
library(plm)
library(readxl)
library(stargazer)
library(tidymodels)
library(tidyverse)



## ## ## ## ## ## ## ## ## ## ##
# IMPORT RESULTS            ####
## ## ## ## ## ## ## ## ## ## ##

load(here("Data", "IFF", "panel_trans.Rdata"))
load(here("Data", "IFF", "panel_agg_trans.Rdata"))
load(here("Results", "vars.Rdata"))



## ## ## ## ## ## ## ## ## ## ##
# SET UP SAMPLES            ####
## ## ## ## ## ## ## ## ## ## ##

# .. For disaggregated panel ####
train.panel <- panel_trans %>%
  filter(year <= 2014)
test.panel <- panel_trans %>%
  filter(year > 2014)

nrow(train.panel)
nrow(test.panel)
nrow(train.panel) + nrow(test.panel) == nrow(panel_trans)
# TRUE

round(nrow(test.panel) / nrow(panel_trans), 2)
# 0.21

write_feather(train.panel, here("Results", "train.feather"))
write_feather(test.panel, here("Results", "test.feather"))


# .. For aggregated panel ####
train.panel_agg <- panel_agg_trans %>%
  filter(year <= 2014)
test.panel_agg <- panel_agg_trans %>%
  filter(year > 2014)

nrow(train.panel_agg)
nrow(test.panel_agg)
nrow(train.panel_agg) + nrow(test.panel_agg) == nrow(panel_agg_trans)
# TRUE

round(nrow(test.panel_agg) / nrow(panel_agg_trans), 2)
# 0.19

train.panel_agg %>% distinct(reporter.ISO) %>% nrow
# 166
train.panel_agg %>% distinct(partner.ISO) %>% nrow
# 166

test.panel_agg %>% distinct(reporter.ISO) %>% nrow
# 147
test.panel_agg %>% distinct(partner.ISO) %>% nrow
# 147

train.panel_agg %>% distinct(year)
# 2000-2014
test.panel_agg %>% distinct(year)
# 2015-2018

write_feather(train.panel_agg, here("Results", "train_agg.feather"))
write_feather(test.panel_agg, here("Results", "test_agg.feather"))



## ## ## ## ## ## ## ## ## ## ##
# OLS GRAVITY MODELS        ####
## ## ## ## ## ## ## ## ## ## ##

# .. Functions ####
subset_complete <- function(data, DV, IVs, 
                            id_vars = c("reporter.ISO", "partner.ISO", "year", "id")){
  subset <- data[, c(DV, IVs, id_vars)]
  subset <- subset[complete.cases(subset), ]
  return(subset)
}

fit_lm <- function(data, DV, IVs,
                   id_vars = c("reporter.ISO", "partner.ISO", "year", "id")) {
  data <- subset_complete(data, DV, IVs, id_vars)
  f <- as.formula(
    paste(DV,
          paste(IVs, collapse = " + "), 
          sep = " ~ "))
  model <- lm(f, data = data)
  return(model)
}

ln.pred <- function(model, data, DV, IVs,
                 id_vars = c("reporter.ISO", "partner.ISO", "year", "id")){
  data <- subset_complete(data, DV, IVs, id_vars)
  ln.preds <- predict(model, newdata = data)
  return(ln.preds)
}

pred <- function(model, data, DV, IVs,
                 id_vars = c("reporter.ISO", "partner.ISO", "year", "id")){
  data <- subset_complete(data, DV, IVs, id_vars)
  preds <- exp(predict(model, newdata = data))
  return(preds)
}

calc_RMSE <- function(data, true.value, predicted.value, IVs){
  data <- subset_complete(data, true.value, IVs)
  true.value <- data[[true.value]]
  # MSE <- mean((predicted.value - true.value)^2)
  RMSE <- sqrt(mean((predicted.value - true.value)^2))
  return(RMSE)
}

calc_MSE <- function(data, true.value, predicted.value, IVs){
  data <- subset_complete(data, true.value, IVs)
  true.value <- data[[true.value]]
  MSE <- mean((predicted.value - true.value)^2)
  return(MSE)
}

calc_dollar_RMSE <- function(data, true.value, predicted.value, IVs){
  data <- subset_complete(data, true.value, IVs)
  true.value <- data[[true.value]]
  dollar.RMSE <- sqrt(mean((exp(predicted.value) - exp(true.value))^2))
  return(dollar.RMSE)
}

calc_dollar_MSE <- function(data, true.value, predicted.value, IVs){
  data <- subset_complete(data, true.value, IVs)
  true.value <- data[[true.value]]
  dollar.MSE <- mean((exp(predicted.value) - exp(true.value))^2)
  return(dollar.MSE)
}


# .. Baseline specifications (gravity variables) ####
grav_vars <- c("ln.gdp_o", "ln.gdp_d", "ln.pop_o", "ln.pop_d",
               "dist", "contig", 
               "comlang_off", "comcol", "col45",
               "ihs.entry_cost_o", "ihs.entry_cost_d", "rta")
governance_vars <- c("rCorrCont", "pCorrCont",
                     "rRegQual", "pRegQual",
                     "rRuleLaw", "pRuleLaw")
secrecy_vars <- c("rSecrecyScore", "pSecrecyScore",
                  "rFSI.rank", "pFSI.rank",
                  "rKFSI13", "rKFSI17", "rKFSI20",
                  "pKFSI13", "pKFSI17", "pKFSI20",
                  "rFATF", "pFATF")
regul_vars <- c("ihs.tariff",
                "kai_o", "kai_d",
                "kao_o", "kao_d",
                "cc_o", "cc_d",
                "cci_o", "cci_d",
                "cco_o", "cco_d",
                "di_o", "di_d",
                "dii_o", "dii_d",
                "dio_o", "dio_d")
vars <- c(grav_vars, governance_vars, secrecy_vars, regul_vars)

depvars <- c("ln.Imp_IFF_t", "ln.Exp_IFF_t", "ln.Tot_IFF_t",
             "ln.In_Imp_IFF_t", "ln.In_Exp_IFF_t", "ln.In_Tot_IFF_t")

store.RMSE <- matrix(NA, nrow = 2, ncol = length(depvars),
                     dimnames = list(c("RMSE", "dollar.RMSE"),
                                     depvars))

store.preds <- list()
store.preds$ln.preds <- vector(mode = "list", length = length(depvars))
names(store.preds$ln.preds) <- depvars
store.preds$preds <- vector(mode = "list", length = length(depvars))
names(store.preds$preds) <- gsub("ln.", "", depvars)

store.fits <- vector(mode = "list", length = length(depvars))
names(store.fits) <- depvars

counter <- 0
for(v in depvars){
  counter <- counter + 1
  store.fits[[counter]] <- fit_lm(train.panel_agg, v, vars)
  store.preds[["ln.preds"]][[counter]] <- ln.pred(store.fits[[counter]], test.panel_agg, v, vars)
  store.preds[["preds"]][[counter]] <- pred(store.fits[[counter]], test.panel_agg, v, vars)
  store.RMSE["RMSE", counter] <- calc_RMSE(test.panel_agg, v, store.preds[["ln.preds"]][[counter]], vars)
  store.RMSE["dollar.RMSE", counter] <- calc_dollar_RMSE(test.panel_agg, v, store.preds[["ln.preds"]][[counter]], vars) / 10^9
}

stargazer(store.fits, type = "text")
kable(t(store.RMSE["RMSE",]),
      format = "rst")
kable(t(store.RMSE["dollar.RMSE",]),
      format = "rst")



## ## ## ## ## ## ## ## ## ## ##
# TRAIN AND TEST            ####
## ## ## ## ## ## ## ## ## ## ##

# .. Visualize predictions ####
viz_test <- subset_complete(test.panel_agg, depvars[1], vars) %>%
  rename(true.value = depvars[1]) %>%
  bind_cols(data.frame(predicted.value = store.preds[["ln.preds"]][[1]]))

ggplot(viz_test) +
  geom_point(aes(x = true.value,
                 y = predicted.value)) +
  geom_abline(slope = 1)

ggplot(viz_test %>%
         filter(reporter.ISO == "CHN")) +
  geom_point(aes(x = true.value,
                 y = predicted.value)) +
  geom_abline(slope = 1)

viz_test <- subset_complete(test.panel_agg, "Tot_IFF", vars) %>%
  rename(true.value = "Tot_IFF") %>%
  bind_cols(data.frame(predicted.value = store.preds[["preds"]][["Tot_IFF"]]))

ggplot(viz_test %>%
         group_by(year) %>%
         summarize(true.value = sum(true.value, na.rm = TRUE),
                   predicted.value = sum(predicted.value, na.rm = TRUE)) %>%
         ungroup()) +
  geom_line(aes(x = year,
                y = true.value),
            col = "red") +
  geom_line(aes(x = year,
                y = predicted.value),
            col = "blue")

data <- subset_complete(train.panel_agg, "ln.Tot_IFF", vars)
preds <- pred(store.fits$ln.Tot_IFF, data, "ln.Tot_IFF", vars)
data$Tot_IFF <- exp(data$ln.Tot_IFF)

viz_train <- bind_cols(data,
                       data.frame(preds))

ggplot(viz_train %>%
         group_by(year) %>%
         summarize(true.value = sum(Tot_IFF, na.rm = TRUE),
                   predicted.value = sum(preds, na.rm = TRUE)) %>%
         ungroup()) +
  geom_line(aes(x = year,
                y = true.value),
            col = "red") +
  geom_line(aes(x = year,
                y = predicted.value),
            col = "blue")

viz <- bind_rows(viz_train %>%
                   select(true.value = Tot_IFF, predicted.value = preds, year),
                 viz_test %>%
                   select(true.value, predicted.value, year))
ggplot(viz %>%
         group_by(year) %>%
         summarize(true.value = sum(true.value, na.rm = TRUE) / 10^9,
                   predicted.value = sum(predicted.value, na.rm = TRUE) / 10^9) %>%
         ungroup()) +
  geom_line(aes(x = year,
                y = true.value),
            col = "red") +
  geom_line(aes(x = year,
                y = predicted.value),
            col = "blue")

check <- viz %>%
  group_by(year) %>%
  summarize(true.value = sum(true.value, na.rm = TRUE),
            predicted.value = sum(predicted.value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(true.value_bn = true.value / 10^9,
         predicted.value_bn = predicted.value / 10^9) %>%
  arrange(year)

