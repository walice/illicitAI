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
# library(ggpubr)
# install.packages("rstatix")
# install.packages("ggpubr")
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
# 1934438
nrow(test.panel)
# 512241
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
# 81249
nrow(test.panel_agg)
# 18910
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


# .. Baseline specification (Walker-type model) ####
grav_vars <- c("ln.gdp_o", "ln.gdp_d",
               "comlang", "comcol",
               "rta")
out_governance_vars <- c("rCorrCont", "pCorrCont",
                         "pRegQual")
in_governance_vars <- c("rCorrCont", "pCorrCont",
                        "rRegQual")
out_secrecy_vars <- c("pSecrecyScore",
                      "rFATF", "pFATF")
in_secrecy_vars <- c("rSecrecyScore",
                     "rFATF", "pFATF")
out_regul_vars <- c("ihs.tariff",
                    "kao_o",
                    "kai_d")
in_regul_vars <- c("ihs.tariff",
                   "kai_o",
                   "kao_d")
out_vars <- c(grav_vars, out_governance_vars, out_secrecy_vars, out_regul_vars)
in_vars <- c(grav_vars, in_governance_vars, in_secrecy_vars, in_regul_vars)

out_trn <- subset_complete(train.panel_agg, "ln.Tot_IFF_t", out_vars)
in_trn <- subset_complete(train.panel_agg, "ln.In_Tot_IFF_t", in_vars)

out_tst <- subset_complete(test.panel_agg, "ln.Tot_IFF_t", out_vars)
in_tst <- subset_complete(test.panel_agg, "ln.In_Tot_IFF_t", in_vars)

out_fit <- fit_lm(train.panel_agg, "ln.Tot_IFF_t", out_vars)
in_fit <- fit_lm(train.panel_agg, "ln.In_Tot_IFF_t", in_vars)

out_preds_trn <- ln.pred(out_fit, out_trn, "ln.Tot_IFF_t", out_vars)
in_preds_trn <- ln.pred(in_fit, in_trn, "ln.In_Tot_IFF_t", in_vars)

out_preds_tst <- ln.pred(out_fit, out_tst, "ln.Tot_IFF_t", out_vars)
in_preds_tst <- ln.pred(in_fit, in_tst, "ln.In_Tot_IFF_t", in_vars)

stargazer(out_fit, in_fit, type = "text")
stargazer(out_fit, in_fit, type = "latex")

calc_MSE(out_tst, "ln.Tot_IFF_t", out_preds_tst, out_vars)
# 5.193806
calc_dollar_MSE(out_tst, "ln.Tot_IFF_t", out_preds_tst, out_vars) / 10^9
# 9725.067
calc_MSE(in_tst, "ln.In_Tot_IFF_t", in_preds_tst, in_vars)
# 6.205944
calc_dollar_MSE(in_tst, "ln.In_Tot_IFF_t", in_preds_tst, in_vars) / 10^9
# 12162.63


# .. Full specification (all predictors) ####
grav_vars <- c("ln.gdp_o", "ln.gdp_d", "ln.pop_o", "ln.pop_d",
               "dist", "contig", 
               "comlang", "comcol", "col45",
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

store.MSE <- matrix(NA, nrow = 2, ncol = length(depvars),
                    dimnames = list(c("MSE", "dollar.MSE"),
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
  store.MSE["MSE", counter] <- calc_MSE(test.panel_agg, v, store.preds[["ln.preds"]][[counter]], vars)
  store.MSE["dollar.MSE", counter] <- calc_dollar_MSE(test.panel_agg, v, store.preds[["ln.preds"]][[counter]], vars) / 10^9
}

stargazer(store.fits, type = "text")
stargazer(store.fits, type = "latex")
kable(t(store.MSE["MSE",]),
      format = "rst")
kable(t(store.MSE["dollar.MSE",]),
      format = "rst")
kable(t(store.MSE["MSE",]),
      format = "latex")
kable(t(store.MSE["dollar.MSE",]),
      format = "latex")



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

viz_test <- subset_complete(test.panel_agg, "ln.Tot_IFF_t", vars) %>%
  rename(true.value = "ln.Tot_IFF_t") %>%
  bind_cols(data.frame(predicted.value = store.preds$ln.preds$ln.Tot_IFF_t))

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

data <- subset_complete(train.panel_agg, "ln.Tot_IFF_t", vars)
preds <- ln.pred(store.fits$ln.Tot_IFF_t, data, "ln.Tot_IFF_t", vars)

viz_train <- bind_cols(data,
                       data.frame(preds))

ggplot(viz_train %>%
         group_by(year) %>%
         summarize(true.value = sum(ln.Tot_IFF_t, na.rm = TRUE),
                   predicted.value = sum(preds, na.rm = TRUE)) %>%
         ungroup()) +
  geom_line(aes(x = year,
                y = true.value),
            col = "red") +
  geom_line(aes(x = year,
                y = predicted.value),
            col = "blue")

viz <- bind_rows(viz_train %>%
                   select(true.value = ln.Tot_IFF_t, predicted.value = preds, year),
                 viz_test %>%
                   select(true.value, predicted.value, year))
g <- ggplot(viz %>%
         group_by(year) %>%
         summarize(true.value = sum(true.value, na.rm = TRUE) / 10^9,
                   predicted.value = sum(predicted.value, na.rm = TRUE) / 10^9) %>%
         ungroup() %>%
         pivot_longer(c(true.value, predicted.value))) +
  geom_line(aes(x = year,
                y = value,
                col = name))
ggsave(g,
       file = here("Figures", "log_preds_fullmod_LM.png"),
       width = 6, height = 5, units = "in")

# out model
out_viz_train <- bind_cols(out_trn,
                       data.frame(out_preds_trn))
out_viz_test <- bind_cols(out_tst,
                      data.frame(out_preds_tst))

viz <- bind_rows(out_viz_train %>%
                   select(true.value = ln.Tot_IFF_t, predicted.value = out_preds_trn, year),
                 out_viz_test %>%
                   select(true.value = ln.Tot_IFF_t, predicted.value = out_preds_tst, year))

g1 <- ggplot(viz %>%
              mutate_at(vars(ends_with("value")),
                        ~exp(.)) %>%
              group_by(year) %>%
              summarize(true.value = sum(true.value, na.rm = TRUE) / 10^6,
                        predicted.value = sum(predicted.value, na.rm = TRUE) / 10^6) %>%
              ungroup() %>%
              pivot_longer(c(true.value, predicted.value))) +
  geom_line(aes(x = year,
                y = value,
                col = name)) +
  labs(x = "Year",
       y = "Illicit flow in billion USD",
       title = "Predictions of theoretically guided linear model",
       subtitle = "For gross outflows")
g1
ggsave(g1,
       file = here("Figures", "dollar_preds_baseLMmod_out.png"),
       width = 6, height = 5, units = "in")

# in model
in_viz_train <- bind_cols(in_trn,
                           data.frame(in_preds_trn))
in_viz_test <- bind_cols(in_tst,
                          data.frame(in_preds_tst))

viz <- bind_rows(in_viz_train %>%
                   select(true.value = ln.In_Tot_IFF_t, predicted.value = in_preds_trn, year),
                 in_viz_test %>%
                   select(true.value = ln.In_Tot_IFF_t, predicted.value = in_preds_tst, year))

g2 <- ggplot(viz %>%
              mutate_at(vars(ends_with("value")),
                        ~exp(.)) %>%
              group_by(year) %>%
              summarize(true.value = sum(true.value, na.rm = TRUE) / 10^6,
                        predicted.value = sum(predicted.value, na.rm = TRUE) / 10^6) %>%
              ungroup() %>%
              pivot_longer(c(true.value, predicted.value))) +
  geom_line(aes(x = year,
                y = value,
                col = name)) +
  labs(x = "Year",
       y = "Illicit flow in billion USD",
       title = "Predictions of theoretically guided linear model",
       subtitle = "For gross inflows")
ggsave(g2,
       file = here("Figures", "dollar_preds_baseLMmod_in.png"),
       width = 6, height = 5, units = "in")


