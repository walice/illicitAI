# Data Pre-Processing
# Alice Lepissier
# alice.lepissier@gmail.com
# Chapter 3

## ## ## ## ## ## ## ## ## ## ##
# INDEX                     ####
## ## ## ## ## ## ## ## ## ## ##
# Preamble
# Import Panel
# Organize Variables
# Summary Statistics
# .. For panel
# .. For aggregated panel
# Transformations
# .. Visualize distributions
# .. Transform continuous variables
# Export Transformed Data



## ## ## ## ## ## ## ## ## ## ##
# PREAMBLE                  ####
## ## ## ## ## ## ## ## ## ## ##

library(here)
# source(here("Scripts", "Data Preparation_panel_agg.R"))
# source(here("Scripts", "Data Preparation_panel.R"))
library(tidyverse)



## ## ## ## ## ## ## ## ## ## ##
# IMPORT PANEL              ####
## ## ## ## ## ## ## ## ## ## ##

load(here("Data", "IFF", "panel_agg.Rdata"))
load(here("Data", "IFF", "panel.Rdata"))



## ## ## ## ## ## ## ## ## ## ##
# ORGANIZE VARIABLES        ####
## ## ## ## ## ## ## ## ## ## ##

id_vars_agg <- c("year",
                 "id", "id_bilateral",
                 "reporter", "reporter.ISO",
                 "rRegion", "rIncome", "rDev", "rHDI",
                 "partner", "partner.ISO",
                 "pRegion", "pIncome", "pDev", "pHDI",
                 "rOECD", "pOECD",
                 "gatt_o", "gatt_d",
                 "wto_o", "wto_d",
                 "eu_o", "eu_d")

id_vars <- c(id_vars_agg, "id_trilateral",
             "commodity.code", "section", "section.code", "SITC.section", "SITC.code")

dep_vars_agg <- c("Imp_IFF", "Exp_IFF", "Tot_IFF",
                  "ln.Imp_IFF", "ln.Exp_IFF", "ln.Tot_IFF",
                  "In_Imp_IFF", "In_Exp_IFF", "In_Tot_IFF",
                  "ln.In_Imp_IFF", "ln.In_Exp_IFF", "ln.In_Tot_IFF",
                  "Net_Imp_IFF", "Net_Exp_IFF", "Net_IFF")

dep_vars <- c("Imp_IFF", "Exp_IFF",
              "Net_Tot_IFF", "GER_Tot_IFF", "In_GER_Tot_IFF",
              "ln.GER_Tot_IFF", "ln.In_GER_Tot_IFF")

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

flow_vars <- c("Import_value", "NetExport_value",
               "pImport_value", "pNetExport_value")

(length(id_vars_agg) + length(dep_vars_agg) + length(grav_vars) + length(capcontrol_vars) + 
    length(macro_vars) + length(secrecy_vars) + length(governance_vars)) == ncol(panel_agg)
# TRUE

(length(id_vars) + length(dep_vars) + length(grav_vars) + length(capcontrol_vars) + 
    length(macro_vars) + length(secrecy_vars) + length(governance_vars) + length(flow_vars)) == ncol(panel)
# TRUE

ncol(panel_agg[, id_vars_agg])
ncol(panel[, id_vars])
ncol(panel_agg[, dep_vars_agg])
ncol(panel[, dep_vars])
ncol(panel_agg[, grav_vars])
ncol(panel_agg[, capcontrol_vars])
ncol(panel_agg[, macro_vars])
ncol(panel_agg[, secrecy_vars])
ncol(panel_agg[, governance_vars])
ncol(panel[, flow_vars])



## ## ## ## ## ## ## ## ## ## ##
# SUMMARY STATISTICS        ####
## ## ## ## ## ## ## ## ## ## ##

# .. For panel ####
summary(panel)

panel %>%
  filter(is.na(Net_Tot_IFF)) %>%
  nrow
# 0

panel %>%
  filter(is.na(Imp_IFF) & is.na(Exp_IFF)) %>%
  nrow
# 0

panel %>%
  filter(is.na(ln.GER_Tot_IFF) & is.na(ln.In_GER_Tot_IFF)) %>%
  nrow
# 0

(panel %>%
    filter(complete.cases(Imp_IFF, Exp_IFF,
                          Net_Tot_IFF, GER_Tot_IFF, In_GER_Tot_IFF,
                          ln.GER_Tot_IFF, ln.In_GER_Tot_IFF)) %>%
    nrow) / nrow(panel)*100
# 49.74228

lapply(panel, class)

panel <- panel %>%
  mutate_at(vars("rRegion", "rIncome", "rDev", "rHDI",
                 "pRegion", "pIncome", "pDev", "pHDI",
                 "section", "section.code",
                 "SITC.section", "SITC.code"),
            ~as.factor(.))

panel <- panel %>%
  mutate_at(vars("commodity.code"),
            ~as.numeric(.))

panel %>%
  select(c("rRegion", "rIncome", "rDev", "rHDI",
           "pRegion", "pIncome", "pDev", "pHDI",
           "legal_new_o", "legal_new_d",
           "section", "section.code",
           "SITC.section", "SITC.code")) %>%
  sapply(levels)

panel <- panel %>%
  select(all_of(id_vars), all_of(dep_vars),
         all_of(grav_vars), all_of(governance_vars), all_of(secrecy_vars),
         all_of(macro_vars), all_of(capcontrol_vars), all_of(flow_vars))


# .. For aggregated panel ####
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
  select(all_of(id_vars_agg), all_of(dep_vars_agg),
         all_of(grav_vars), all_of(governance_vars), all_of(secrecy_vars),
         all_of(macro_vars), all_of(capcontrol_vars))



## ## ## ## ## ## ## ## ## ## ##
# TRANSFORMATIONS           ####
## ## ## ## ## ## ## ## ## ## ##

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
summary(log(panel$tariff))
# Need inverse hyperbolic sine transformation
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

panel <- panel %>%
  mutate(ihs.tariff = ihs(tariff),
         ln.gdp_o = log(gdp_o),
         ln.gdp_d = log(gdp_d),
         ln.pop_o = log(pop_o),
         ln.pop_d = log(pop_d),
         ln.gdpcap_o = log(gdpcap_o),
         ln.gdpcap_d = log(gdpcap_d),
         ihs.entry_cost_o = ihs(entry_cost_o),
         ihs.entry_cost_d = ihs(entry_cost_d))

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
plot(density(panel$ihs.tariff))
plot(density(panel$ln.gdp_d, na.rm = TRUE))
plot(density(panel$ln.gdp_o, na.rm = TRUE))
plot(density(panel$ln.pop_d, na.rm = TRUE))
plot(density(panel$ln.pop_o, na.rm = TRUE))
dev.off()

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
# EXPORT TRANSFORMED DATA   ####
## ## ## ## ## ## ## ## ## ## ##

panel_trans <- panel
save(panel_trans, file = here("Data", "IFF", "panel_trans.Rdata"))

panel_agg_trans <- panel_agg
save(panel_agg_trans, file = here("Data", "IFF", "panel_agg_trans.Rdata"))

save(id_vars, dep_vars, 
     id_vars_agg, dep_vars_agg,
     grav_vars, secrecy_vars,
     governance_vars, capcontrol_vars, macro_vars,
     flow_vars,
     file = here("Results", "vars.Rdata"))

rm(panel, panel_agg, ihs)
