# Analysis
# Alice Lepissier
# alice.lepissier@gmail.com
# Chapter 3

## ## ## ## ## ## ## ## ## ## ##
# INDEX                     ####
## ## ## ## ## ## ## ## ## ## ##
# Preamble
# Import results
# Pre-Processing
# .. Categorize variable names
# .. Summary statistics
# .. Visualize distributions
# .. Transform continuous variables
# Set Up Samples
# .. Create design matrix with dummies
# .. Create training and test set
# OLS Gravity Models
# .. Functions
# .. Baseline specifications (gravity models)
# .. Add country FE
# Train and Test
# .. Visualize predictions



## ## ## ## ## ## ## ## ## ## ##
# PREAMBLE                  ####
## ## ## ## ## ## ## ## ## ## ##

library(caret)
library(here)
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

load(here("Data", "IFF", "panel_agg.Rdata"))



## ## ## ## ## ## ## ## ## ## ##
# PRE-PROCESSING            ####
## ## ## ## ## ## ## ## ## ## ##

# .. Categorize variable names ####
id_vars <- c("year",
              "id", "id_bilateral",
             "reporter", "reporter.ISO",
             "rRegion", "rIncome", "rDev", "rHDI",
             "partner", "partner.ISO",
             "pRegion", "pIncome", "pDev", "pHDI",
             "rOECD", "pOECD",
             "gatt_o", "gatt_d",
             "wto_o", "wto_d",
             "eu_o", "eu_d")

dep_vars <- c("Imp_IFF", "Exp_IFF", "Tot_IFF",
              "ln.Imp_IFF", "ln.Exp_IFF", "ln.Tot_IFF",
              "In_Imp_IFF", "In_Exp_IFF", "In_Tot_IFF",
              "ln.In_Imp_IFF", "ln.In_Exp_IFF", "ln.In_Tot_IFF")

grav_vars <- c("dist", "contig",
               "comcol", "col45", "comlang_off",
               "legal_new_o", "legal_new_d", "comleg_posttrans",
               "gdp_o", "gdp_d",
               "gdp_ppp_o", "gdp_ppp_d",
               "pop_o", "pop_d",
               "gdpcap_o", "gdpcap_d",
               "entry_cost_o", "entry_cost_d",
               "entry_proc_o", "entry_proc_d",
               "rta")

capcontrol_vars <- c("ka_o", "kai_o", "kao_o",
                     "eq_o", "eqi_o", "eqo_o",
                     "bo_o", "boi_o", "boo_o",
                     "mm_o", "mmi_o", "mmo_o",
                     "ci_o", "cii_o", "cio_o",
                     "de_o", "dei_o", "deo_o",
                     "cc_o", "cci_o", "cco_o",
                     "fc_o", "fci_o", "fco_o",
                     "gs_o", "gsi_o", "gso_o",
                     "di_o", "dii_o", "dio_o",
                     "re_o", "rei_o", "reo_o",
                     "ka_d", "kai_d", "kao_d",
                     "eq_d", "eqi_d", "eqo_d",
                     "bo_d", "boi_d", "boo_d",
                     "mm_d", "mmi_d", "mmo_d",
                     "ci_d", "cii_d", "cio_d",
                     "de_d", "dei_d", "deo_d",
                     "cc_d", "cci_d", "cco_d",
                     "fc_d", "fci_d", "fco_d",
                     "gs_d", "gsi_d", "gso_d",
                     "di_d", "dii_d", "dio_d",
                     "re_d", "rei_d", "reo_d")

macro_vars <- c("tariff")

secrecy_vars <- c("rSecrecyScore", "pSecrecyScore",
                  "rFSI.rank", "pFSI.rank",
                  "KFSI13", "KFSI17", "KFSI20")

governance_vars <- c("FATF",
                     "rCPI", "pCPI",
                     "rCorrCont", "pCorrCont",
                     "rRegQual", "pRegQual",
                     "rRuleLaw", "pRuleLaw")

(length(id_vars) + length(dep_vars) + length(grav_vars) + length(capcontrol_vars) + 
  length(macro_vars) + length(secrecy_vars) + length(governance_vars)) == ncol(panel_agg)
# TRUE

ncol(panel_agg[, id_vars])
ncol(panel_agg[, dep_vars])
ncol(panel_agg[, grav_vars])
ncol(panel_agg[, capcontrol_vars])
ncol(panel_agg[, macro_vars])
ncol(panel_agg[, secrecy_vars])
ncol(panel_agg[, governance_vars])


# .. Summary statistics ####
summary(panel_agg)

panel_agg %>%
  filter(is.na(Imp_IFF) & is.na(Exp_IFF) & is.na(Tot_IFF) &
           is.na(In_Imp_IFF) & is.na(In_Exp_IFF) & is.na(In_Tot_IFF)) %>%
  nrow
# 0

(panel_agg %>%
  filter(complete.cases(Imp_IFF, Exp_IFF, Tot_IFF,
                        In_Imp_IFF, In_Exp_IFF, In_Tot_IFF,
                        ln.Imp_IFF, ln.Exp_IFF, ln.Tot_IFF,
                        ln.In_Imp_IFF, ln.In_Exp_IFF, ln.In_Tot_IFF)) %>%
  nrow) / nrow(panel_agg)*100
# 69.75808

lapply(panel_agg, class)

panel_agg <- panel_agg %>%
  mutate_at(vars("rRegion", "rIncome", "rDev", "rHDI",
                 "pRegion", "pIncome", "pDev", "pHDI"),
            ~as.factor(.))

panel_agg %>%
  select(c("rRegion", "rIncome", "rDev", "rHDI",
           "pRegion", "pIncome", "pDev", "pHDI",
           "legal_new_o", "legal_new_d")) %>%
  sapply(levels)

panel_agg <- panel_agg %>%
  select(all_of(id_vars), all_of(dep_vars),
         all_of(grav_vars), all_of(governance_vars), all_of(secrecy_vars),
         all_of(macro_vars), all_of(capcontrol_vars))


# .. Visualize distributions ####
plot(density(panel_agg$tariff)) # Right-skewed

par(mfrow = c(3, 2))
hist(panel_agg$gdp_d) # Right-skewed
hist(panel_agg$gdp_o) # Right-skewed
hist(panel_agg$pop_d) # Right-skewed
hist(panel_agg$pop_o) # Right-skewed
hist(panel_agg$gdpcap_d) # Right-skewed
hist(panel_agg$gdpcap_o) # Right-skewed
dev.off()

par(mfrow = c(2, 2))
hist(panel_agg$entry_cost_o) # Right-skewed
hist(panel_agg$entry_cost_d) # Right-skewed
hist(panel_agg$rCPI)
hist(panel_agg$pCPI)
dev.off()

par(mfrow = c(3, 2))
hist(panel_agg$rCorrCont)
hist(panel_agg$pCorrCont)
hist(panel_agg$rRegQual)
hist(panel_agg$pRegQual)
hist(panel_agg$rRuleLaw)
hist(panel_agg$pRuleLaw)
dev.off()

par(mfrow = c(2, 2))
boxplot(panel_agg$rSecrecyScore)
boxplot(panel_agg$pSecrecyScore)
boxplot(panel_agg$rFSI.rank)
boxplot(panel_agg$pFSI.rank)
dev.off()

par(mfrow = c(3, 1))
hist(panel_agg$KFSI13)
hist(panel_agg$KFSI17)
hist(panel_agg$KFSI20)
dev.off()


# .. Transform continuous variables ####
ihs <- function(x){
  x <- log(x + sqrt(x^2 + 1))
  return(x)
}

# Check whether there are zeros in the data
summary(log(panel_agg$tariff))
# Need inverse hyperbolic sine transformation
summary(log(panel_agg$gdp_o))
# Fine to log
summary(log(panel_agg$gdp_d))
# Fine to log
summary(log(panel_agg$pop_o))
# Fine to log
summary(log(panel_agg$pop_d))
# Fine to log
summary(log(panel_agg$gdpcap_o))
# Fine to log
summary(log(panel_agg$gdpcap_d))
# Fine to log
summary(log(panel_agg$entry_cost_o))
# Need inverse hyperbolic sine transformation
summary(log(panel_agg$entry_cost_d))
# Need inverse hyperbolic sine transformation

panel_agg <- panel_agg %>%
  mutate(ihs.tariff = ihs(tariff),
         ln.gdp_o = log(gdp_o),
         ln.gdp_d = log(gdp_d),
         ln.pop_o = log(pop_o),
         ln.pop_d = log(pop_d),
         ln.gdpcap_o = log(gdpcap_o),
         ln.gdpcap_d = log(gdpcap_d),
         ihs.entry_cost_o = ihs(entry_cost_o),
         ihs.entry_cost_d = ihs(entry_cost_d))

par(mfrow = c(3, 2))
plot(density(panel_agg$ihs.tariff))
plot(density(panel_agg$ln.gdp_d, na.rm = TRUE))
plot(density(panel_agg$ln.gdp_o, na.rm = TRUE))
plot(density(panel_agg$ln.pop_d, na.rm = TRUE))
plot(density(panel_agg$ln.pop_o, na.rm = TRUE))
dev.off()

par(mfrow = c(2, 2))
plot(density(panel_agg$ln.gdpcap_d, na.rm = TRUE))
plot(density(panel_agg$ln.gdpcap_o, na.rm = TRUE))
plot(density(panel_agg$ihs.entry_cost_o, na.rm = TRUE))
plot(density(panel_agg$ihs.entry_cost_d, na.rm = TRUE))
dev.off()



## ## ## ## ## ## ## ## ## ## ##
# SET UP SAMPLES            ####
## ## ## ## ## ## ## ## ## ## ##

# .. Create design matrix with dummies ####

# .. Create training and test set ####
train.panel_agg <- panel_agg %>%
  filter(year <= 2014)
test.panel_agg <- panel_agg %>%
  filter(year > 2014)

nrow(train.panel_agg)
nrow(test.panel_agg)
nrow(train.panel_agg) + nrow(test.panel_agg) == nrow(panel_agg)
# TRUE

round(nrow(test.panel_agg) / nrow(panel_agg), 2)
# 0.19



## ## ## ## ## ## ## ## ## ## ##
# OLS GRAVITY MODELS        ####
## ## ## ## ## ## ## ## ## ## ##

# .. Functions ####
subset_complete <- function(data, DV, IVs){
  subset <- data[, c(DV, IVs)]
  subset <- subset[complete.cases(subset), ]
  return(subset)
}

fit_lm <- function(data, DV, IVs) {
  data <- data[, c(DV, IVs)]
  data <- data[complete.cases(data), ]
  f <- as.formula(
    paste(DV,
          paste(IVs, collapse = " + "), 
          sep = " ~ "))
  model <- lm(f, data = data)
  return(model)
}

pred <- function(model, data, DV, IVs){
  data <- data[, c(DV, IVs)]
  data <- data[complete.cases(data), ]
  preds <- predict(model, newdata = data)
  return(preds)
}

calc_RMSE <- function(data, true.value, predicted.value, IVs){
  data <- data[, c(true.value, IVs)]
  data <- data[complete.cases(data), ]
  true.value <- data[[true.value]]
  MSE <- mean((predicted.value - true.value)^2)
  RMSE <- sqrt(mean((predicted.value - true.value)^2))
  dollar.RMSE <- sqrt(mean((exp(predicted.value) - exp(true.value))^2))
  return(dollar.RMSE)
}


# .. Baseline specifications (gravity variables) ####
grav_vars <- grav_vars[!grav_vars %in% c("col45", "legal_new_o", "legal_new_d",
                                         "gdp_ppp_o", "gdp_ppp_d",
                                         "gdpcap_o", "gdpcap_d",
                                         "entry_proc_o", "entry_proc_d")]
grav_vars <- c("dist", "contig", "comcol", "col45", "comlang_off", "comleg_posttrans",
               "ln.gdp_o", "ln.gdp_d", "ln.pop_o", "ln.pop_d",
               "ihs.entry_cost_o", "ihs.entry_cost_d", "rta")

vars <- c(grav_vars, "ihs.tariff")
vars <- c(vars, "rSecrecyScore", "pSecrecyScore")
governance_vars <- governance_vars[!governance_vars %in% c("rCPI", "pCPI")]
vars <- c(vars, governance_vars)

# new_vars <- c("kao_o", "kai_d")
# new_vars <- c("kai_o", "kao_d")
# new_vars <- c("ka_o", "eq_o", "bo_o", "mm_o", "ci_o", "de_o", "cc_o", "fc_o", "gs_o", "di_o", "re_o")
# vars <- c(vars, new_vars)
# new_vars <- c("ka_d", "eq_d", "bo_d", "mm_d", "ci_d", "de_d", "cc_d", "fc_d", "gs_d", "di_d", "re_d")
# vars <- c(vars, new_vars)
new_vars <- c("kai_o", "kao_o", "kai_d", "kao_d")
vars <- c(vars, new_vars)
vars

# Import over-invoicing
fit.GER.Imp <- fit_lm(train.panel_agg, "ln.Imp_IFF", vars)
summary(fit.GER.Imp)
preds.GER.Imp <- pred(fit.GER.Imp, test.panel_agg, "ln.Imp_IFF", vars)
RMSE.GER.Imp <- calc_RMSE(test.panel_agg, "ln.Imp_IFF", preds.GER.Imp, vars) / 10^9

# Export under-invoicing
fit.GER.Exp <- fit_lm(train.panel_agg, "ln.Exp_IFF", vars)
summary(fit.GER.Exp)
preds.GER.Exp <- pred(fit.GER.Exp, test.panel_agg, "ln.Exp_IFF", vars)
RMSE.GER.Exp <- calc_RMSE(test.panel_agg, "ln.Exp_IFF", preds.GER.Exp, vars) / 10^9

# Gross outflows (import over-invoicing + export under-invoicing)
fit.GER.Tot <- fit_lm(train.panel_agg, "ln.Tot_IFF", vars)
summary(fit.GER.Tot)
preds.GER.Tot <- pred(fit.GER.Tot, test.panel_agg, "ln.Tot_IFF", vars)
RMSE.GER.Tot <- calc_RMSE(test.panel_agg, "ln.Tot_IFF", preds.GER.Tot, vars) / 10^9

# Import under-invoicing
fit.In.GER.Imp <- fit_lm(train.panel_agg, "ln.In_Imp_IFF", vars)
summary(fit.In.GER.Imp)
preds.In.GER.Imp <- pred(fit.In.GER.Imp, test.panel_agg, "ln.In_Imp_IFF", vars)
RMSE.In.GER.Imp <- calc_RMSE(test.panel_agg, "ln.In_Imp_IFF", preds.In.GER.Imp, vars) / 10^9

# Export over-invoicing
fit.In.GER.Exp <- fit_lm(train.panel_agg, "ln.In_Exp_IFF", vars)
summary(fit.In.GER.Exp)
preds.In.GER.Exp <- pred(fit.In.GER.Exp, test.panel_agg, "ln.In_Exp_IFF", vars)
RMSE.In.GER.Exp <- calc_RMSE(test.panel_agg, "ln.In_Exp_IFF", preds.In.GER.Exp, vars) / 10^9

# Gross inflows (import under-invoicing + export over-invoicing)
fit.In.GER.Tot <- fit_lm(train.panel_agg, "ln.In_Tot_IFF", vars)
summary(fit.In.GER.Tot)
preds.In.GER.Tot <- pred(fit.In.GER.Tot, test.panel_agg, "ln.In_Tot_IFF", vars)
RMSE.In.GER.Tot <- calc_RMSE(test.panel_agg, "ln.In_Tot_IFF", preds.In.GER.Tot, vars) / 10^9

stargazer(fit.GER.Imp, fit.GER.Exp, fit.GER.Tot,
          fit.In.GER.Imp, fit.In.GER.Exp, fit.In.GER.Tot,
          type = "text")

kable(t(c(RMSE.GER.Imp, RMSE.GER.Exp, RMSE.GER.Tot,
          RMSE.In.GER.Imp, RMSE.In.GER.Exp, RMSE.In.GER.Tot)),
      col.names = c("Import outflow", "Export outflow", "Gross outflows",
                    "Import inflow", "Export inflow", "Gross inflows"),
      format = "rst")


# # .. Add country FE ####
# fit1 <- lm(ln.Imp_IFF ~ gdp_o + gdp_d + pop_o + pop_d,
#            data = panel_agg)
# 
# fit2 <- plm(ln.Imp_IFF ~ gdp_o + gdp_d + pop_o + pop_d,
#             index = c("id_bilateral", "year"),
#             model = "pooling",
#             data = panel_agg)
# stargazer(fit1, fit2,
#           type = "text")
# coef(fit1)
# coef(fit2)
# # Pooled OLS: same coefficients
# 
# fit3 <- lm(ln.Imp_IFF ~ gdp_o + gdp_d + pop_o + pop_d +
#              as.factor(year) -1,
#            data = panel_agg)
# 
# fit4 <- plm(ln.Imp_IFF ~ gdp_o + gdp_d + pop_o + pop_d,
#             index = c("id_bilateral", "year"),
#             effect = "time",
#             model = "within",
#             data = panel_agg)
# coef(fit3)
# coef(fit4)
# # Time FE: same coefficients
# 
# fit5 <- felm(ln.Imp_IFF ~ gdp_o + gdp_d + pop_o + pop_d | 
#                factor(id_bilateral),
#              data = panel_agg)
# 
# fit6 <- plm(ln.Imp_IFF ~ gdp_o + gdp_d + pop_o + pop_d,
#             index = c("id_bilateral", "year"),
#             effect = "individual",
#             model = "within",
#             data = panel_agg)
# coef(fit5)
# coef(fit6)
# # Bilateral FE: same coefficients
# # Too computationally hard with lm
# 
# fit7 <- plm(ln.Imp_IFF ~ gdp_o + gdp_d + pop_o + pop_d,
#             index = c("id_bilateral", "year"),
#             effect = "twoways",
#             model = "within",
#             data = panel_agg)
# 
# fit8 <- felm(ln.Imp_IFF ~ gdp_o + gdp_d + pop_o + pop_d | 
#                factor(id_bilateral) + factor(year), 
#              data = panel_agg)
# coef(fit7)
# coef(fit8)
# # Twoway FE: same coefficients



## ## ## ## ## ## ## ## ## ## ##
# TRAIN AND TEST            ####
## ## ## ## ## ## ## ## ## ## ##

# .. Visualize predictions ####
IVs <- vars
DV <- "ln.In_Imp_IFF"

data <- test.panel_agg %>%
  filter_at(vars(DV, IVs), all_vars(complete.cases(.)))

viz <- data %>%
  select(ln.In_Imp_IFF, id_bilateral, reporter.ISO, partner.ISO) %>%
  bind_cols(preds = preds.In.GER.Imp)
# %>%
#   filter(reporter.ISO == "USA")

ggplot(viz) +
  geom_point(aes(x = ln.In_Imp_IFF,
                 y = preds))

# +
#   coord_trans(x = scales::exp_trans(1), 
#               y = scales::exp_trans(1))
