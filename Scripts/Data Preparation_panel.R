# Data Preparation for disaggregated panel
# Alice Lepissier
# alice.lepissier@gmail.com
# Chapter 3

## ## ## ## ## ## ## ## ## ## ##
# INDEX                     ####
## ## ## ## ## ## ## ## ## ## ##
# Preamble
# Codes Masterlist
# Import Panel
# .. Misinvoicing for reporter-partner-commodity-year
# .. Generate and transform outcome variables
# .. Merge with OECD code
# Import CEPII
# .. Merge CEPII with panel
# Import Capital Controls
# .. Merge capital controls with panel
# Import FATF Membership
# .. Merge FATF with panel
# Import Financial Secrecy
# .. Import global secrecy score and rank
# .. Import Key Financial Secrecy Indicators
# .. Merge FSI with panel
# Import WGI
# .. Interpolate missing data
# .. Merge WGI with panel
# Import WDI
# Import CPI
# .. Merge CPI with panel
# Import WITS
# .. Import average tariff line data
# .. Generate unique identifier
# .. Merge with panel
# Export Clean Data



## ## ## ## ## ## ## ## ## ## ##
# PREAMBLE                  ####
## ## ## ## ## ## ## ## ## ## ##

library(ggridges)
library(here)
library(janitor)
library(readxl)
library(scales)
library(tidyverse)
library(WDI)



## ## ## ## ## ## ## ## ## ## ##
# CODES MASTERLIST          ####
## ## ## ## ## ## ## ## ## ## ##

# download.file("https://github.com/walice/Codes-Masterlist/raw/master/Codes_Masterlist.xlsx",
#               here("Data", "Codes_Masterlist.xlsx"))

codes <- read_excel(here("Data", "Codes_Masterlist.xlsx"), sheet = "Codes") %>%
  mutate_all(as.character)



## ## ## ## ## ## ## ## ## ## ##
# IMPORT PANEL              ####
## ## ## ## ## ## ## ## ## ## ##

# .. Misinvoicing for reporter-partner-commodity-year ####
load(here("Data", "IFF", "panel_results.Rdata"))

panel <- panel %>%
  select(c(id, reporter.ISO, partner.ISO, year, commodity.code,
           reporter, rRegion, rIncome, rDev, rHDI,
           partner, pRegion, pIncome, pDev, pHDI,
           section, section.code, SITC.section, SITC.code,
           Import_value, NetExport_value,
           pImport_value, pNetExport_value,
           Imp_IFF, Exp_IFF)) %>%
  mutate(Net_Tot_IFF = Imp_IFF + Exp_IFF,
         GER_Tot_IFF = ifelse(Imp_IFF > 0, Imp_IFF, 0) +
           ifelse(Exp_IFF > 0, Exp_IFF, 0),
         In_GER_Tot_IFF = ifelse(Imp_IFF < 0, Imp_IFF, 0) +
           ifelse(Exp_IFF < 0, Exp_IFF, 0))

sum(is.na(panel$Net_Tot_IFF))
# No NAs
# This is net misinvoicing
summary(panel$GER_Tot_IFF)
summary(panel$In_GER_Tot_IFF)


# .. Generate and transform outcome variables ####
panel <- panel %>%
  mutate(GER_Tot_IFF = ifelse(GER_Tot_IFF == 0, NA, GER_Tot_IFF),
         In_GER_Tot_IFF = ifelse(In_GER_Tot_IFF == 0, NA, In_GER_Tot_IFF)) %>%
  mutate(ln.GER_Tot_IFF = log(GER_Tot_IFF),
         ln.In_GER_Tot_IFF = log(abs(In_GER_Tot_IFF)))

panel <- panel %>%
  mutate(id_bilateral = paste(reporter.ISO, partner.ISO, sep = "_"))

panel <- panel %>%
  mutate(id_trilateral = paste(reporter.ISO, partner.ISO, year, sep = "_"))


# .. Merge with OECD code ####
panel <- panel %>%
  left_join(codes %>%
              distinct(ISO3166.3, .keep_all = TRUE) %>%
              select(ISO3166.3, OECD),
            by = c("reporter.ISO" = "ISO3166.3")) %>%
  rename(rOECD = OECD) %>%
  left_join(codes %>%
              distinct(ISO3166.3, .keep_all = TRUE) %>%
              select(ISO3166.3, OECD),
            by = c("partner.ISO" = "ISO3166.3")) %>%
  rename(pOECD = OECD) %>%
  mutate_at(vars("rOECD", "pOECD"), ~as.integer(.))



## ## ## ## ## ## ## ## ## ## ##
# IMPORT CEPII              ####
## ## ## ## ## ## ## ## ## ## ##

CEPII <- readRDS(file = here("Data", "CEPII", "Gravity_V202102.Rds"))


# .. Merge CEPII with panel ####
panel <- left_join(panel, CEPII %>%
                     select(iso3_o, iso3_d, year,
                            contig, dist,
                            comlang_off, comcol, col45,
                            legal_new_o, legal_new_d, comleg_posttrans,
                            pop_o, pop_d,
                            gdp_o, gdp_d, gdp_ppp_d, gdp_ppp_o, gdpcap_o, gdpcap_d,
                            gatt_o, gatt_d, wto_o, wto_d, eu_o, eu_d,
                            rta,
                            entry_cost_o, entry_cost_d,
                            entry_proc_o, entry_proc_d),
                   by = c("reporter.ISO" = "iso3_o",
                          "partner.ISO" = "iso3_d",
                          "year" = "year"))
rm(CEPII)



## ## ## ## ## ## ## ## ## ## ##
# IMPORT CAPITAL CONTROLS   ####
## ## ## ## ## ## ## ## ## ## ##

capital <- read_excel(here("Data", "IMF", "FKRSU_Update_Jun_13_2019.xls"), sheet = "DATASET") %>%
  select(-c(Country, code_ifs, region, incomegroup, incomesubgroup))

summary(capital$Year)
capital %>% distinct(code_wdi) %>% nrow

capital <- capital %>%
  select(c(Year, code_wdi,
           ka, kai, kao, # Overall restrictions index, inflow, outflow
           eq, eqi, eqo, # Average equity restrictions, inflow, outlflow
           bo, boi, boo, # Average bond restrictions, inflow, outflow
           mm, mmi, mmo, # Average money market restrictions, inflow, outflow
           ci, cii, cio, # Average collective investments restrictions, inflow, outflow
           de, dei, deo, # Average derivatives restrictions, inflow, outflow
           cc, cci, cco, # Average commercial credits restrictions, inflow, outflow
           fc, fci, fco, # Average financial credits restrictions, inflow, outflow
           gs, gsi, gso, # Guarantees, sureties and financial backup facilities restrictions, inflow, outflow
           di, dii, dio, # Average direct investments restrictions, inflow, outflow
           re, rei, reo, # Average real estate restrictions, inflow, outflow
  )) %>%
  mutate_at(vars(-("code_wdi")), ~ifelse(. == "n.a" |
                                           . == "n.r" |
                                           . == "d.n.e", NA, .)) %>%
  mutate_at(vars(-("code_wdi")), ~as.numeric(.))

viz <- capital %>% 
  select(-c(Year, code_wdi)) %>%
  gather("measure", "value") %>%
  mutate(group = as.factor(rep(seq_len(11), each = 6900)),
         value = as.numeric(value))

ggplot(viz,
       aes(x = value, y = measure, fill = group)) +
  geom_density_ridges()
rm(viz)


# .. Merge capital controls with panel ####
panel <- panel %>%
  left_join(capital %>%
              rename_at(vars(-c("code_wdi", "Year")), function(x) paste0(x, "_o")),
            by = c("reporter.ISO" = "code_wdi",
                   "year" = "Year"))

panel <- panel %>%
  left_join(capital %>%
              rename_at(vars(-c("code_wdi", "Year")), function(x) paste0(x, "_d")),
            by = c("partner.ISO" = "code_wdi",
                   "year" = "Year"))

rm(capital)



## ## ## ## ## ## ## ## ## ## ##
# IMPORT FATF MEMBERSHIP    ####
## ## ## ## ## ## ## ## ## ## ##

FATF <- read.csv(here("Data", "FATF", "FATF.csv")) %>%
  select(-Note)


# .. Merge FATF with panel ####
panel <- left_join(panel, FATF %>%
                     select(-Country),
                   by = c("reporter.ISO" = "ISO3166.3")) %>%
  mutate(FATF = ifelse(is.na(FATF), 0, FATF))
rm(FATF)



## ## ## ## ## ## ## ## ## ## ##
# IMPORT FINANCIAL SECRECY  ####
## ## ## ## ## ## ## ## ## ## ##

# .. Import global secrecy score and rank ####
FSI <- read_excel(here("Data", "TJN", "FSI-Rankings-2018.xlsx"), 
                  sheet = "FSI Results")

FSI$Jurisdiction<- gsub("2$", "", FSI$Jurisdiction)
FSI <- FSI %>%
  select(Jurisdiction, secrecy.score = `Secrecy Score4`, Rank)

FSI <- FSI %>%
  left_join(codes %>% select(Country, ISO3166.3),
            by = c("Jurisdiction" = "Country"))

FSI %>% filter(is.na(ISO3166.3))
# Only footnotes remain, we can drop the NAs.

FSI <- FSI %>%
  filter(!is.na(ISO3166.3)) %>%
  mutate(Rank = as.numeric(Rank))


# .. Import Key Financial Secrecy Indicators ####
KFSI <- read_excel(here("Data", "TJN", "FSI-Rankings-2018.xlsx"), 
                   sheet = "SS")

KFSI <- KFSI %>%
  select(Jurisdiction, 
         KFSI13 = `KI-13`, 
         KFSI17 = `KI-17`, 
         KFSI20 = `KI-20`) %>%
  left_join(codes %>% select(Country, ISO3166.3),
            by = c("Jurisdiction" = "Country"))

KFSI %>% filter(is.na(ISO3166.3))
# Only subtitle remains, we can drop the NAs.

KFSI <- KFSI %>%
  filter(!is.na(ISO3166.3))


# .. Merge FSI with panel ####
panel <- left_join(panel, FSI %>%
                     select(-Jurisdiction) %>%
                     rename(rFSI.rank = Rank,
                            rSecrecyScore = secrecy.score),
                   by = c("reporter.ISO" = "ISO3166.3"))

panel <- left_join(panel, FSI %>%
                     select(-Jurisdiction) %>%
                     rename(pFSI.rank = Rank,
                            pSecrecyScore = secrecy.score),
                   by = c("partner.ISO" = "ISO3166.3"))

panel <- left_join(panel, KFSI %>%
                     select(-Jurisdiction) %>%
                     mutate_at(vars(starts_with("KFSI")), list(~ as.numeric(.))),
                   by = c("reporter.ISO" = "ISO3166.3"))

rm(FSI, KFSI)



## ## ## ## ## ## ## ## ## ## ##
# IMPORT WGI                ####
## ## ## ## ## ## ## ## ## ## ##

# WGI <- WDI(indicator = c(corruption = "CC.PER.RNK",
#                          regulatory.qual = "RQ.PER.RNK",
#                          rule.law = "RL.PER.RNK"),
#            start = 2000, extra = TRUE) %>%
#   select(year, iso3c, corruption, regulatory.qual, rule.law) %>%
#   filter(!is.na(iso3c)) %>%
#   filter(!(is.na(corruption) & is.na(regulatory.qual) & is.na(rule.law))) %>%
#   mutate(iso3c = as.character(iso3c)) %>%
#   arrange(year, iso3c)
# save(WGI, file = here("Data", "WB", "WGI.Rdata"))
load(here("Data", "WB", "WGI.Rdata"))


# .. Interpolate missing data ####
WGI %>% distinct(year)
# Missing data for 2001, so interpolate from 2000-2002

WGI00 <- WGI %>%
  filter(year == 2000)
WGI02 <- WGI %>%
  filter(year == 2002)

WGI01 <- full_join(WGI00, WGI02,
                   by = "iso3c") %>%
  mutate(year = 2001)
WGI01$corruption <- rowMeans(WGI01[, c("corruption.x", "corruption.y")], na.rm = TRUE)
WGI01$regulatory.qual <- rowMeans(WGI01[, c("regulatory.qual.x", "regulatory.qual.y")], na.rm = TRUE)
WGI01$rule.law <- rowMeans(WGI01[, c("rule.law.x", "rule.law.y")], na.rm = TRUE)
WGI01 <- WGI01 %>% select(iso3c, year, corruption, regulatory.qual, rule.law)

WGI <- rbind(WGI, WGI01)
rm(WGI00, WGI01, WGI02)


# .. Merge WGI with panel ####
panel <- left_join(panel, WGI %>%
                     rename(rCorrCont = corruption,
                            rRegQual = regulatory.qual,
                            rRuleLaw = rule.law),
                   by = c("reporter.ISO" = "iso3c",
                          "year" = "year"))

panel <- left_join(panel, WGI %>%
                     rename(pCorrCont = corruption,
                            pRegQual = regulatory.qual,
                            pRuleLaw = rule.law),
                   by = c("partner.ISO" = "iso3c",
                          "year" = "year"))

rm(WGI)



## ## ## ## ## ## ## ## ## ## ##
# IMPORT WDI                ####
## ## ## ## ## ## ## ## ## ## ##

# WDI <- WDI(indicator = c("NY.GDP.MKTP.CD",
#                          "NY.GDP.PCAP.CD"), 
#            start = 1999)
# WDI <- left_join(WDI, codes %>%
#                    select(ISO3166.2, ISO3166.3) %>% 
#                    distinct(ISO3166.2, .keep_all = T),
#                  by = c("iso2c" = "ISO3166.2"))
# WDI %>% filter(is.na(ISO3166.3)) %>% distinct(country)
# # Only aggregates remain
# WDI <- WDI %>%
#   filter(!is.na(ISO3166.3)) %>%
#   select(-c(iso2c,country)) %>%
#   rename(GDP = NY.GDP.MKTP.CD,
#          GDPpc = NY.GDP.PCAP.CD)
# 
# save(WDI, file = here("Data", "WB", "WDI.Rdata"))



## ## ## ## ## ## ## ## ## ## ##
# IMPORT CPI                ####
## ## ## ## ## ## ## ## ## ## ##

CPI <- read_excel(here("Data", "TI", "CPI2020_GlobalTablesTS_210125.xlsx"), 
                  sheet = "CPI Timeseries 2012 - 2020", skip = 2)

CPI <- CPI %>%
  select(Country, ISO3, starts_with("CPI score")) %>%
  clean_names() %>%
  pivot_longer(cols = starts_with("cpi_score_"),
               names_to = "CPI",
               names_prefix = "cpi_score_") %>%
  mutate(CPI = as.numeric(CPI))

CPI <- CPI %>%
  select(-iso3) %>%
  left_join(codes %>% select(Country, ISO3166.3),
            by = c("country" = "Country")) %>%
  rename(year = CPI)

CPI %>%
  filter(is.na(ISO3166.3)) %>%
  distinct(country)
# None


# .. Merge CPI with panel ####
panel <- left_join(panel, CPI %>%
                     select(-country) %>%
                     rename(rCPI = value),
                   by = c("reporter.ISO" = "ISO3166.3",
                          "year" = "year"))

panel <- left_join(panel, CPI %>%
                     select(-country) %>%
                     rename(pCPI = value),
                   by = c("partner.ISO" = "ISO3166.3",
                          "year" = "year"))

rm(CPI)



## ## ## ## ## ## ## ## ## ## ##
# IMPORT WITS               ####
## ## ## ## ## ## ## ## ## ## ##

# # .. Import average tariff line data ####
# tariff <- read.csv(here("Data", "WITS", "DataJobID-2109586_2109586_2digitmisinvoicingnon.csv")) %>%
#   select(Reporter.Name, Partner.Name, Product, Tariff.Year, Simple.Average) %>%
#   rename(reporter = Reporter.Name,
#          partner = Partner.Name,
#          year = Tariff.Year,
#          commodity.code = Product,
#          tariff = Simple.Average) %>%
#   mutate_at(vars(reporter, partner, commodity.code),
#             funs(as.character(.)))
# 
# tariff <- left_join(tariff, codes %>%
#                       select(Country, ISO3166.3),
#                     by = c("reporter" = "Country")) %>%
#   rename(reporter.ISO = ISO3166.3)
# 
# tariff %>% 
#   filter(is.na(reporter.ISO)) %>%
#   distinct(reporter)
# # European Union
# 
# tariff <- left_join(tariff, codes %>%
#                       select(Country, ISO3166.3),
#                     by = c("partner" = "Country")) %>%
#   rename(partner.ISO = ISO3166.3)
# 
# tariff %>% 
#   filter(is.na(partner.ISO)) %>%
#   distinct(partner)
# # Bunkers; Unspecified;
# # Special Categories; Free Zones; Neutral Zone; Other Asia, nes
# 
# tariff <- tariff %>%
#   filter(!is.na(reporter.ISO) & !is.na(partner.ISO))
# 
# tariff %>% distinct(reporter.ISO) %>% nrow
# # 195
# tariff %>% distinct(partner.ISO) %>% nrow
# # 240
# 
# 
# # .. Generate unique identifier ####
# tariff <- tariff %>%
#   mutate(commodity.code = str_pad(str_trim(commodity.code), 
#                                   width = 2, side = "left", pad = "0"))
# 
# tariff$id <- paste(tariff$reporter.ISO, 
#                    tariff$partner.ISO, 
#                    tariff$commodity.code, 
#                    tariff$year, sep = "_")
# save(tariff, file = here("Data", "WITS", "tariff.Rdata"))
load(here("Data", "WITS", "tariff.Rdata"))


# .. Merge with panel ####
panel <- left_join(panel, tariff %>% select(-c(reporter, partner)),
                   by = c("id" = "id",
                          "reporter.ISO" = "reporter.ISO",
                          "partner.ISO" = "partner.ISO",
                          "commodity.code" = "commodity.code",
                          "year" = "year"))

panel %>% filter(duplicated(panel$id)) %>% nrow
# 0

rm(tariff)



## ## ## ## ## ## ## ## ## ## ##
# EXPORT CLEAN DATA         ####
## ## ## ## ## ## ## ## ## ## ##

panel %>%
  filter(duplicated(panel$id)) %>%
  nrow
# 0

panel_agg %>%
  filter(is.na(ln.Tot_IFF)) %>%
  nrow
# 0

panel %>%
  filter(is.na(ln.GER_Tot_IFF)) %>%
  nrow
# 599956

panel %>%
  filter(is.na(ln.In_GER_Tot_IFF)) %>%
  nrow
# 629689

panel %>%
  filter(is.na(ln.GER_Tot_IFF) & is.na(ln.In_GER_Tot_IFF)) %>%
  nrow
# 0

panel %>%
  filter(is.na(Net_Tot_IFF)) %>%
  nrow
# 0

save(panel, file = here("Data", "IFF", "panel.Rdata"))
write.csv(panel, file = here("Data", "IFF", "panel.csv"),
          row.names = F)
