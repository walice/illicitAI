# Analysis
# Alice Lepissier
# alice.lepissier@gmail.com
# Chapter 3

## ## ## ## ## ## ## ## ## ## ##
# INDEX                     ####
## ## ## ## ## ## ## ## ## ## ##
# Preamble
# Import results
# Functions
# Set Up Samples
# .. Subset data for LMICs
# .. For generalization test: hold out specific countries
# .. For pooled panel
# .. Check sample dimensions and export
# OLS Gravity Models
# .. Baseline specification (Walker TBML gravity model)
# .. Full specification (all predictors)
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
# install.packages("naniar")
library(modelr)
# library(naniar)
# library(parsnip)
library(plm)
library(readxl)
library(stargazer)
library(tidymodels)
library(tidyverse)
set.seed(1509)



## ## ## ## ## ## ## ## ## ## ##
# IMPORT RESULTS            ####
## ## ## ## ## ## ## ## ## ## ##

load(here("Data", "IFF", "panel_agg_trans.Rdata"))



## ## ## ## ## ## ## ## ## ## ##
# FUNCTIONS                 ####
## ## ## ## ## ## ## ## ## ## ##

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



## ## ## ## ## ## ## ## ## ## ##
# SET UP SAMPLES            ####
## ## ## ## ## ## ## ## ## ## ##

# .. Check missingness of data on all features
features <- c('reporter.ISO', 'reporter', 'partner.ISO', 'partner', 'year',
              'ln.gdp_o', 'ln.gdp_d', 'ln.pop_o', 'ln.pop_d', 
              'dist', 'contig', 
              'comlang', 'comcol', 'col45', 
              'ihs.entry_cost_o', 'ihs.entry_cost_d', 'rta',
              'rCorrCont', 'pCorrCont',
              'rRegQual', 'pRegQual', 
              'rRuleLaw', 'pRuleLaw',
              'pSecrecyScore',
              'pFSI.rank',
              'pKFSI13',
              'pKFSI17',
              'pKFSI20',
              'rFATF', 'pFATF',
              'ihs.tariff',
              'kai_o', 'kai_d', 'kao_o', 'kao_d',
              'cc_o', 'cc_d', 'cci_o', 'cci_d', 'cco_o', 'cco_d',
              'di_o', 'di_d', 'dii_o', 'dii_d', 'dio_o', 'dio_d')

check_missing <- panel_agg_trans %>%
  filter(rRegion == "Africa")
  # filter(rIncome == "LIC" | rIncome == "LMC")
miss_var_summary(check_missing) %>% view


# .. Subset data for LMICs ####
LMIC_agg <- panel_agg_trans %>%
  filter(rIncome == "LIC" | rIncome == "LMC")
nrow(LMIC_agg)
# 20249

LMIC_agg %>%
  distinct(reporter.ISO) %>%
  nrow
# 63

LMIC_agg %>%
  distinct(partner.ISO) %>%
  nrow
# 151

# Individual countries with most observations
LMIC_agg %>%
  group_by(reporter.ISO) %>%
  tally() %>%
  top_n(5) %>%
  arrange(desc(n)) %>%
  pull(reporter.ISO, n)


# .. For generalization test: hold out specific countries ####
LMICs <- LMIC_agg %>%
  distinct(reporter.ISO) %>%
  pull

set.seed(1509)
train.set <- sample(LMICs, floor(length(LMICs)*0.8))

train.panel_agg <- LMIC_agg %>%
  filter(reporter.ISO %in% train.set)
test.panel_agg <- LMIC_agg %>%
  filter(!(reporter.ISO %in% train.set))


# .. For pooled model ####
set.seed(1509)
train.id <- sample(seq_len(nrow(LMIC_agg)), 
                   size = floor(0.8*nrow(LMIC_agg)))

train.panel_agg <- LMIC_agg[train.id, ]
test.panel_agg <- LMIC_agg[-train.id, ]


# .. Check sample dimensions and export ####
nrow(train.panel_agg)
# 16199
nrow(test.panel_agg)
# 4050
nrow(train.panel_agg) + nrow(test.panel_agg) == nrow(LMIC_agg)
# TRUE

round(nrow(test.panel_agg) / nrow(LMIC_agg), 2)
# 0.2

train.panel_agg %>% distinct(reporter.ISO) %>% nrow
# 63
train.panel_agg %>% distinct(partner.ISO) %>% nrow
# 149

test.panel_agg %>% distinct(reporter.ISO) %>% nrow
# 61
test.panel_agg %>% distinct(partner.ISO) %>% nrow
# 142

write_feather(LMIC_agg, here("Results", "LMIC_agg.feather"))
write_feather(train.panel_agg, here("Results", "train_agg.feather"))
write_feather(test.panel_agg, here("Results", "test_agg.feather"))


# .. Subset data for Africa ####
Africa_agg <- panel_agg_trans %>%
  filter(rRegion == "Africa")
nrow(Africa_agg)
# 13030

Africa_agg %>%
  distinct(reporter.ISO) %>%
  nrow
# 44

Africa_agg %>%
  distinct(partner.ISO) %>%
  nrow
# 135

# Individual countries with most observations
Africa_agg %>%
  group_by(reporter.ISO) %>%
  tally() %>%
  top_n(5) %>%
  arrange(desc(n)) %>%
  pull(reporter.ISO, n)

write_feather(Africa_agg, here("Results", "Africa_agg.feather"))


# .. For pooled model ####
set.seed(1509)
train.id <- sample(seq_len(nrow(Africa_agg)), 
                   size = floor(0.8*nrow(Africa_agg)))

train.panel_agg <- Africa_agg[train.id, ]
test.panel_agg <- Africa_agg[-train.id, ]

write_feather(train.panel_agg, here("Results", "train_agg.feather"))
write_feather(test.panel_agg, here("Results", "test_agg.feather"))


# .. For placebo trials ####
X <- arrow::read_feather(here("Results", "X_train.feather"))

X_placebo <- X[sample(nrow(X)), ]


X_placebo[] <- lapply(X, sample)

write_feather(X_placebo, here("Results", "X_placebo.feather"))

X_placebo <- X_placebo %>%
  select(-dist, -contig, -comlang, -comcol, -col45, -rta, -ihs.tariff)

X_placebo <- X %>%
  select(-ln.gdp_o, -ln.gdp_d, -ln.pop_o, -ln.pop_d)

X_placebo <- X %>%
  select(-dist, -contig, -comlang, -comcol, -col45, -rta, -ihs.tariff) %>%
  select(-ln.gdp_o, -ln.gdp_d, -ln.pop_o, -ln.pop_d)

X_placebo <- X %>%
  select(-rCorrCont, pCorrCont, rRegQual, pRegQual,
         rRuleLaw, pRuleLaw)

X_placebo <- X %>%
  select(-dist, -contig, -ihs.tariff,
         -rCorrCont, -pCorrCont, -rRegQual, -pRegQual)

Xi <- X %>%
  select(c('ln.gdp_o', 'ln.pop_o', 'ihs.entry_cost_o', 
           'rCorrCont', 'rRegQual', 'rRuleLaw',
           'rFATF', 
           'kai_o', 'kao_o', 'cc_o', 'cci_o', 'cco_o', 'di_o', 'dii_o', 'dio_o'))

Xj <-  X %>%
  select(c('ln.gdp_d', 'ln.pop_d', 'ihs.entry_cost_d', 
                      'pCorrCont', 'pRegQual', 'pRuleLaw',
                      'pSecrecyScore', 'pFSI.rank', 'pKFSI13', 'pKFSI17', 'pKFSI20',
                      'pFATF', 
                      'kai_d', 'kao_d', 'cc_d', 'cci_d', 'cco_d', 'di_d', 'dii_d', 'dio_d'))

Xij <- X %>%
  select(c('dist', 'contig', 'comlang', 'comcol', 'col45', 'rta',
           'ihs.tariff'))

Xj_shuffle <- Xj[sample(nrow(Xj)), ]


X_placebo <- cbind(Xi, Xj_shuffle, Xij)


####### Create full x placeblo, shuffle rows of bilat partners
X <- arrow::read_feather(here("Results", "X_train.feather"))

# Shuffle row-wise
X_placebo <- X[sample(nrow(X)),]

write_feather(X_placebo, here("Results", "X_placebo.feather"))


## .. Can the model travel? Train on Africa and see if it applies to LMIC more broadly
# .. Subset data for HICs ####
HIC_agg <- panel_agg_trans %>%
  filter(rIncome == "HIC")
nrow(HIC_agg)
# 52025
write_feather(HIC_agg, here("Results", "HIC_agg.feather"))

# .. Subset data for individual countries ####
ZAF <- panel_agg_trans %>%
  filter(reporter.ISO == "ZAF")
nrow(ZAF)
# 1659
write_feather(ZAF, here("Results", "ZAF.feather"))

Africa_noZAF <- panel_agg_trans %>%
  filter(rRegion == "Africa") %>%
  filter(reporter.ISO != "ZAF")
nrow(Africa_noZAF)
# 11371
write_feather(Africa_noZAF, here("Results", "Africa_noZAF.feather"))

EGY <- panel_agg_trans %>%
  filter(reporter.ISO == "EGY")
nrow(EGY)
# 1159
write_feather(EGY, here("Results", "EGY.feather"))

Africa_noEGY <- panel_agg_trans %>%
  filter(rRegion == "Africa") %>%
  filter(reporter.ISO != "EGY")
nrow(Africa_noEGY)
# 11871
write_feather(Africa_noEGY, here("Results", "Africa_noEGY.feather"))


NGA <- panel_agg_trans %>%
  filter(reporter.ISO == "NGA")
nrow(NGA)
# 305
write_feather(NGA, here("Results", "NGA.feather"))

Africa_noNGA <- panel_agg_trans %>%
  filter(rRegion == "Africa") %>%
  filter(reporter.ISO != "NGA")
nrow(Africa_noNGA)
# 12725
write_feather(Africa_noNGA, here("Results", "Africa_noNGA.feather"))

DZA <- panel_agg_trans %>%
  filter(reporter.ISO == "DZA")
nrow(DZA)
# 278
write_feather(DZA, here("Results", "DZA.feather"))

Africa_noDZA <- panel_agg_trans %>%
  filter(rRegion == "Africa") %>%
  filter(reporter.ISO != "DZA")
nrow(Africa_noDZA)
# 12752
write_feather(Africa_noDZA, here("Results", "Africa_noDZA.feather"))

AGO <- panel_agg_trans %>%
  filter(reporter.ISO == "AGO")
nrow(AGO)
# 74
write_feather(AGO, here("Results", "AGO.feather"))

Africa_noAGO <- panel_agg_trans %>%
  filter(rRegion == "Africa") %>%
  filter(reporter.ISO != "AGO")
nrow(Africa_noAGO)
# 12956
write_feather(Africa_noAGO, here("Results", "Africa_noAGO.feather"))

MAR <- panel_agg_trans %>%
  filter(reporter.ISO == "MAR")
nrow(MAR)
# 797
write_feather(MAR, here("Results", "MAR.feather"))

Africa_noMAR <- panel_agg_trans %>%
  filter(rRegion == "Africa") %>%
  filter(reporter.ISO != "MAR")
nrow(Africa_noMAR)
# 12233
write_feather(Africa_noMAR, here("Results", "Africa_noMAR.feather"))

## ## ## ## ## ## ## ## ## ## ##
# OLS GRAVITY MODELS        ####
## ## ## ## ## ## ## ## ## ## ##

# .. Baseline specification (Walker-type model) ####
grav_vars <- c("ln.gdp_o", "ln.gdp_d",
               "comlang", "comcol",
               "rta")
out_governance_vars <- c("rCorrCont", "pCorrCont",
                         "pRegQual")
in_governance_vars <- c("rCorrCont", "pCorrCont",
                        "rRegQual")
out_secrecy_vars <- c("rFATF", "pFATF")
in_secrecy_vars <- c("rFATF", "pFATF")
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
write_feather(out_trn, here("Results", "out_trn.feather"))

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
# 5.527022
# calc_dollar_MSE(out_tst, "ln.Tot_IFF_t", out_preds_tst, out_vars) / 10^9
# 1438.389
calc_MSE(in_tst, "ln.In_Tot_IFF_t", in_preds_tst, in_vars)
# 5.879005
# calc_dollar_MSE(in_tst, "ln.In_Tot_IFF_t", in_preds_tst, in_vars) / 10^9
# 10.22042
rsquare(out_fit, out_tst)
# 0.471822


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
         filter(reporter.ISO == "GHA")) +
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


