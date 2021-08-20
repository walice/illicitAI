# Analysis
# Alice Lepissier
# alice.lepissier@gmail.com
# Chapter 3

## ## ## ## ## ## ## ## ## ## ##
# INDEX                     ####
## ## ## ## ## ## ## ## ## ## ##
# Preamble
# Import results
# Split Samples
# OLS
# Train and Test



## ## ## ## ## ## ## ## ## ## ##
# PREAMBLE                  ####
## ## ## ## ## ## ## ## ## ## ##

library(here)
library(lfe)
library(parsnip)
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
# SPLIT SAMPLES             ####
## ## ## ## ## ## ## ## ## ## ##


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
# OLS                       ####
## ## ## ## ## ## ## ## ## ## ##

# Import over-invoicing
fit.GER.Imp <- lm(ln.Imp_IFF ~ gdp_o + gdp_d + pop_o + pop_d +
                    dist + contig + rta + 
                    comcol + col45 + comlang_off + 
                    rOECD + pOECD +
                    gatt_o + gatt_d +
                    wto_o + wto_d +
                    eu_o + eu_d +
                    entry_cost_o + entry_cost_d +
                    entry_proc_o + entry_proc_d, 
                  data = panel_agg)
summary(fit.GER.Imp)

# Export under-invoicing
fit.GER.Exp <- lm(ln.Exp_IFF ~ gdp_o + gdp_d + pop_o + pop_d +
                    dist + contig + rta + 
                    comcol + col45 + comlang_off + 
                    rOECD + pOECD +
                    gatt_o + gatt_d +
                    wto_o + wto_d +
                    eu_o + eu_d +
                    entry_cost_o + entry_cost_d +
                    entry_proc_o + entry_proc_d, 
                  data = panel_agg)
summary(fit.GER.Exp)

# Gross outflows (import over-invoicing + export under-invoicing)
fit.GER.Tot <- lm(ln.Tot_IFF ~ gdp_o + gdp_d + pop_o + pop_d +
                    dist + contig + rta + 
                    comcol + col45 + comlang_off + 
                    rOECD + pOECD +
                    gatt_o + gatt_d +
                    wto_o + wto_d +
                    eu_o + eu_d +
                    entry_cost_o + entry_cost_d +
                    entry_proc_o + entry_proc_d, 
                  data = panel_agg)
summary(fit.GER.Tot)

# Import under-invoicing
fit.In.GER.Imp <- lm(ln.In_Imp_IFF ~ gdp_o + gdp_d + pop_o + pop_d +
                       dist + contig + rta + 
                       comcol + col45 + comlang_off + 
                       rOECD + pOECD +
                       gatt_o + gatt_d +
                       wto_o + wto_d +
                       eu_o + eu_d +
                       entry_cost_o + entry_cost_d +
                       entry_proc_o + entry_proc_d, 
                     data = panel_agg)
summary(fit.In.GER.Imp)

# Export over-invoicing
fit.In.GER.Exp <- lm(ln.In_Exp_IFF ~ gdp_o + gdp_d + pop_o + pop_d +
                       dist + contig + rta + 
                       comcol + col45 + comlang_off + 
                       rOECD + pOECD +
                       gatt_o + gatt_d +
                       wto_o + wto_d +
                       eu_o + eu_d +
                       entry_cost_o + entry_cost_d +
                       entry_proc_o + entry_proc_d, 
                     data = panel_agg)
summary(fit.In.GER.Exp)

# Gross inflows (import under-invoicing + export over-invoicing)
fit.In.GER.Tot <- lm(ln.Tot_IFF ~ gdp_o + gdp_d + pop_o + pop_d +
                       dist + contig + rta + 
                       comcol + col45 + comlang_off + 
                       rOECD + pOECD +
                       gatt_o + gatt_d +
                       wto_o + wto_d +
                       eu_o + eu_d +
                       entry_cost_o + entry_cost_d +
                       entry_proc_o + entry_proc_d, 
                     data = panel_agg)
summary(fit.In.GER.Tot)

stargazer(fit.GER.Imp, fit.GER.Exp, fit.GER.Tot,
          fit.In.GER.Imp, fit.In.GER.Exp, fit.In.GER.Tot,
          type = "text")


# .. Add country FE ####

fit1 <- lm(ln.Imp_IFF ~ gdp_o + gdp_d + pop_o + pop_d,
           data = panel_agg)

fit2 <- plm(ln.Imp_IFF ~ gdp_o + gdp_d + pop_o + pop_d,
            index = c("id_bilateral", "year"),
            model = "pooling",
            data = panel_agg)
stargazer(fit1, fit2,
          type = "text")
coef(fit1)
coef(fit2)
# Pooled OLS: same coefficients

fit3 <- lm(ln.Imp_IFF ~ gdp_o + gdp_d + pop_o + pop_d +
             as.factor(year) -1,
           data = panel_agg)

fit4 <- plm(ln.Imp_IFF ~ gdp_o + gdp_d + pop_o + pop_d,
            index = c("id_bilateral", "year"),
            effect = "time",
            model = "within",
            data = panel_agg)
coef(fit3)
coef(fit4)
# Time FE: same coefficients

fit5 <- felm(ln.Imp_IFF ~ gdp_o + gdp_d + pop_o + pop_d | 
               factor(id_bilateral),
             data = panel_agg)

fit6 <- plm(ln.Imp_IFF ~ gdp_o + gdp_d + pop_o + pop_d,
            index = c("id_bilateral", "year"),
            effect = "individual",
            model = "within",
            data = panel_agg)
coef(fit5)
coef(fit6)
# Bilateral FE: same coefficients
# Too computationally hard with lm

fit7 <- plm(ln.Imp_IFF ~ gdp_o + gdp_d + pop_o + pop_d,
            index = c("id_bilateral", "year"),
            effect = "twoways",
            model = "within",
            data = panel_agg)

fit8 <- felm(ln.Imp_IFF ~ gdp_o + gdp_d + pop_o + pop_d | 
               factor(id_bilateral) + factor(year), 
             data = panel_agg)
coef(fit7)
coef(fit8)
# Twoway FE: same coefficients



## ## ## ## ## ## ## ## ## ## ##
# TRAIN AND TEST            ####
## ## ## ## ## ## ## ## ## ## ##

load(here("Data", "IFF", "panel_agg.Rdata"))

panel_agg <- panel_agg %>%
  filter(complete.cases(ln.Imp_IFF, year,
                        gdp_o, gdp_d, pop_o, pop_d,
                        dist, contig, rta, 
                        comcol, col45, comlang_off, 
                        rOECD, pOECD,
                        gatt_o, gatt_d,
                        wto_o, wto_d,
                        eu_o, eu_d,
                        entry_cost_o, entry_cost_d,
                        entry_proc_o, entry_proc_d)) %>%
  arrange(year)

# %>%
#   select(ln.Imp_IFF, year,
#          gdp_o, gdp_d, pop_o, pop_d,
#          dist, contig, rta, 
#          comcol, col45, comlang_off, 
#          rOECD, pOECD,
#          gatt_o, gatt_d,
#          wto_o, wto_d,
#          eu_o, eu_d,
#          entry_cost_o, entry_cost_d,
#          entry_proc_o, entry_proc_d)

min(which(panel_agg$year == "2014"))
# 52810

train.panel_agg <- panel_agg %>%
  slice(1:52809)
test.panel_agg <- panel_agg %>%
  slice(52810:nrow(panel_agg))

summary(train.panel_agg$year)
summary(test.panel_agg$year)

# train.panel_agg <- train.panel_agg %>%
#   select(ln.Imp_IFF,
#          gdp_o, gdp_d, pop_o, pop_d, dist)
# test.panel_agg <- test.panel_agg %>%
#   select(ln.Imp_IFF,
#          gdp_o, gdp_d, pop_o, pop_d, dist)
# 
# train.panel_agg <- train.panel_agg %>%
#   select(-year)
# test.panel_agg <- test.panel_agg %>%
#   select(-year)

train.fit.GER.Imp <- lm(ln.Imp_IFF ~ gdp_o + gdp_d + pop_o + pop_d + dist +
                          dist + contig + rta +
                          comcol + col45 + comlang_off +
                          rOECD + pOECD +
                          gatt_o + gatt_d +
                          wto_o + wto_d +
                          eu_o + eu_d +
                          entry_cost_o + entry_cost_d +
                          entry_proc_o + entry_proc_d,
                        data = train.panel_agg)
summary(train.fit.GER.Imp)

lm_model <- linear_reg() %>% 
  set_engine('lm') %>% # adds lm implementation of linear regression
  set_mode('regression')

train.fit.GER.Imp <- lm_model %>% 
  fit(ln.Imp_IFF ~ gdp_o + gdp_d + pop_o + pop_d + dist +
      dist + contig + rta +
      comcol + col45 + comlang_off +
      rOECD + pOECD +
      gatt_o + gatt_d +
      wto_o + wto_d +
      eu_o + eu_d +
      entry_cost_o + entry_cost_d +
      entry_proc_o + entry_proc_d,
      data = train.panel_agg)
summary(train.fit.GER.Imp)

mydata <- data.frame(gdp_o = test.panel_agg$gdp_o,
                     gdp_d = test.panel_agg$gdp_d,
                     pop_o = test.panel_agg$pop_o,
                     pop_d = test.panel_agg$pop_d,
                     dist = test.panel_agg$dist)

preds <- predict(train.fit.GER.Imp, new_data = test.panel_agg)  %>% 
  bind_cols(test.panel_agg)

rmse(preds, 
     truth = ln.Imp_IFF,
     estimate = .pred)

rsq(preds, 
     truth = ln.Imp_IFF,
     estimate = .pred)

viz <- preds %>%
  filter(reporter.ISO == "USA",
         partner.ISO == "CHN")

ggplot(data = viz,
       mapping = aes(x = .pred, y = ln.Imp_IFF)) +
  geom_point(color = '#006EA1') +
  geom_abline(intercept = 0, slope = 1, color = 'orange')

ggplot(data = viz) +
  geom_line(aes(x = year, y = ln.Imp_IFF)) +
  geom_line(aes(x = year, y = .pred))

