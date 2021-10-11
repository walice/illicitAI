# IFFe - data visualizations for summary statistics
# Alice Lepissier
# alice.lepissier@gmail.com
# Chapter 3

## ## ## ## ## ## ## ## ## ## ##
# INDEX                     ####
## ## ## ## ## ## ## ## ## ## ##
# Preamble
# .. Add custom font to theme
# Codes Masterlist
# Choropleths
# Bar Graphs
# .. Africa
# .. LMIC



## ## ## ## ## ## ## ## ## ## ##
# PREAMBLE                  ####
## ## ## ## ## ## ## ## ## ## ##
library(gridExtra)
library(here)
library(mapproj)
library(readxl)
library(reshape2)
library(scales)
library(showtext)
library(sysfonts)
library(tidyverse)


# .. Add custom font to theme ####
theme_set(theme_minimal())
font_add_google("Montserrat", "montserrat")
font_add_google("Lato", "lato")
showtext_auto()
showtext_opts(dpi = 96)

theme_update(text = element_text(size = 36,
                                 family = "montserrat"),
             legend.spacing.x = unit("0.2", "cm"))
my_theme <- theme(text = element_text(size = 36,
                                      family = "montserrat"),
                  legend.spacing.x = unit("0.2", "cm"),
                  legend.spacing.y = unit("0.2", "cm"))



## ## ## ## ## ## ## ## ## ## ##
# CODES MASTERLIST          ####
## ## ## ## ## ## ## ## ## ## ##

# download.file("https://github.com/walice/Codes-Masterlist/raw/master/Codes_Masterlist.xlsx",
#               here("Data", "Codes_Masterlist.xlsx"))

codes <- read_excel(here("Data", "Codes_Masterlist.xlsx"), sheet = "Codes") %>%
  mutate_all(as.character)



## ## ## ## ## ## ## ## ## ## ##
# CHOROPLETHS               ####
## ## ## ## ## ## ## ## ## ## ##

# .. Gross outflows
load(here("Data", "IFF", "GER_Orig_Avg_Africa.Rdata"))

map <- map_data("world")
map <- left_join(map, codes %>% dplyr::select(Country, ISO3166.3, UN_Region),
                 by = c("region" = "Country")) %>%
  dplyr::select(-subregion) %>%
  filter(UN_Region == "Africa")

ditch_axes <- theme(axis.title.x = element_blank(),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank(),
                    axis.title.y = element_blank(),
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank(),
                    panel.border = element_blank(),
                    panel.grid = element_blank())

# my_theme <- theme(text = element_text(size = 36,
#                                       family = "montserrat"),
#                   legend.spacing.y = unit("0.2", "cm"))


viz <- left_join(map, GER_Orig_Avg_Africa,
                 by = c("ISO3166.3" = "reporter.ISO"))

# Average gross IFF dollar value
g1 <- ggplot() +
  geom_polygon(data = viz,
               aes(x = long, y = lat, group = group,
                   fill = Tot_IFF_bn), color = "white", lwd = 0.2) +
  coord_fixed(1.3) +
  theme_bw() +
  ditch_axes +
  my_theme +
  scale_fill_viridis_c("Outflow (billion USD)", direction = -1) +
  labs(title = "Average annual gross outflows during 2000-2018") +
  theme(legend.position = "bottom",
        legend.margin = margin(t = -2, r = 0, b = 0, l = 0, unit = "cm")) +
  guides(fill = guide_colourbar(title.vjust = 0.8))
ggsave(g1,
       file = here("Figures", "Choro_GER Out_Yearly Average_Dollars_Africa.png"),
       width = 6, height = 5, units = "in")


# .. Gross inflows
load(here("Data", "IFF", "Inflow_GER_Orig_Avg.Rdata"))

map <- map_data("world")
map <- left_join(map, codes %>% dplyr::select(Country, ISO3166.3, UN_Region),
                 by = c("region" = "Country")) %>%
  dplyr::select(-subregion) %>%
  filter(UN_Region == "Africa")

viz <- left_join(map, Inflow_GER_Orig_Avg %>%
                   filter(rRegion == "Africa"),
                 by = c("ISO3166.3" = "reporter.ISO"))

# Average gross IFF dollar value
g2 <- ggplot() +
  geom_polygon(data = viz %>%
                 mutate(Tot_IFF_bn = abs(Tot_IFF_bn)),
               aes(x = long, y = lat, group = group,
                   fill = Tot_IFF_bn), color = "white", lwd = 0.2) +
  coord_fixed(1.3) +
  theme_bw() +
  ditch_axes +
  my_theme +
  scale_fill_viridis_c("Inflow (billion USD)", option = "C", direction = -1,
                       breaks= pretty_breaks()) +
  labs(title = "Average annual gross inflows during 2000-2018") +
  theme(legend.position = "bottom",
        legend.margin = margin(t = -2, r = 0, b = 0, l = 0, unit = "cm")) +
  guides(fill = guide_colourbar(title.vjust = 0.8))
ggsave(g2,
       file = here("Figures", "Choro_GER In_Yearly Average_Dollars_Africa.png"),
       width = 6, height = 5, units = "in")

g <- grid.arrange(g1, g2, ncol = 2, top = "Average annual gross flows during 2000-2018")
ggsave(g,
file = here("Figures", "Choro_GER_Yearly Average_Dollars_Africa.png"),
width = 6, height = 5, units = "in")



## ## ## ## ## ## ## ## ## ## ##
# BAR GRAPHS                ####
## ## ## ## ## ## ## ## ## ## ##

# .. Africa ####
load(here("Data", "IFF", "GER_Year_Africa.Rdata"))
load(here("Data", "IFF", "Inflow_GER_Year_Africa.Rdata"))

viz <- full_join(GER_Year_Africa %>% 
                   select(year, 
                          GER_Tot_IFF = Tot_IFF, 
                          GER_Tot_IFF_GDP = Tot_IFF_GDP, 
                          GER_Tot_IFF_Trade = Tot_IFF_trade,
                          GER_Imp_IFF = Imp_IFF,
                          GER_Exp_IFF = Exp_IFF),
                 Inflow_GER_Year_Africa %>%
                   select(year, 
                          In_GER_Tot_IFF = Tot_IFF, 
                          In_GER_Tot_IFF_GDP = Tot_IFF_GDP, 
                          In_GER_Tot_IFF_Trade = Tot_IFF_trade,
                          In_GER_Imp_IFF = Imp_IFF,
                          In_GER_Exp_IFF = Exp_IFF),
                 by = c("year"))

# GER In and Out dollar value
g <- ggplot(viz %>% 
              mutate(year = as.character(year)) %>% 
              melt(id.vars = "year") %>%
              filter(variable == "In_GER_Tot_IFF" | variable == "GER_Tot_IFF"), 
            aes(x = year, y = value, fill = fct_rev(variable))) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = dollar_format(scale = 1/10^9, accuracy = 1)) +
  scale_fill_manual(values = c("#FFD84D", "#00B0F6"),
                    name = "Estimate",
                    labels = c("Gross inflow", "Gross outflow")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Trade mis-invoicing in African countries",
       subtitle = "Gross inflows and outflows",
       x = "", y = "Illicit flow in billion USD") +
  geom_text(aes(label = round(value/10^9)), family = "montserrat",
            size = 9, position = position_dodge(1), vjust = -0.4) +
  scale_x_discrete(expand = c(0.1, 0.1))
g
ggsave(g,
       file = "Figures/GER Out and In_Yearly_Dollars_Africa.png",
       width = 6, height = 5, units = "in")


# .. LMIC ####
load(here("GER_Year_LMIC.Rdata"))
load(here("Net_Year_LMIC.Rdata"))
load(here("Inflow_GER_Year_LMIC.Rdata"))

viz <- full_join(GER_Year_LMIC %>% 
                   select(year, 
                          GER_Tot_IFF = Tot_IFF, 
                          GER_Tot_IFF_GDP = Tot_IFF_GDP, 
                          GER_Tot_IFF_Trade = Tot_IFF_trade,
                          GER_Imp_IFF = Imp_IFF,
                          GER_Exp_IFF = Exp_IFF),
                 Net_Year_LMIC %>%
                   select(year, 
                          Net_Tot_IFF = Tot_IFF, 
                          Net_Tot_IFF_GDP = Tot_IFF_GDP, 
                          Net_Tot_IFF_Trade = Tot_IFF_trade),
                 by = c("year"))

viz <- full_join(viz, Inflow_GER_Year_LMIC %>%
                   select(year, 
                          In_GER_Tot_IFF = Tot_IFF, 
                          In_GER_Tot_IFF_GDP = Tot_IFF_GDP, 
                          In_GER_Tot_IFF_Trade = Tot_IFF_trade,
                          In_GER_Imp_IFF = Imp_IFF,
                          In_GER_Exp_IFF = Exp_IFF),
                 by = c("year"))

# GER and Net dollar value
g <- ggplot(viz %>% 
              mutate(year = as.character(year)) %>% 
              melt(id.vars = "year") %>%
              filter(variable == "In_GER_Tot_IFF" | variable == "GER_Tot_IFF"), 
            aes(x = year, y = value, fill = fct_rev(variable))) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = dollar_format(scale = 1/10^9, accuracy = 1)) +
  scale_fill_manual(values = c("#FFD84D", "#00B0F6"),
                    name = "Estimate",
                    labels = c("Gross inflow", "Gross outflow")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Trade mis-invoicing in low and lower-middle income",
       subtitle = "Gross inflows and outflows",
       x = "", y = "Illicit flow in billion USD") +
  geom_text(aes(label = round(value/10^9)), family = "montserrat",
            size = 9, position = position_dodge(1), vjust = -0.4) +
  scale_x_discrete(expand = c(0.1, 0.1))
g
ggsave(g,
       file = "Figures/GER Out and In_Yearly_Dollars_LMIC.png",
       width = 6, height = 5, units = "in")


