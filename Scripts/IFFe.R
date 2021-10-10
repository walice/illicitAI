library(mapproj)
library(reshape2)
library(scales)

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


######### CHORO

map <- map_data("world")
map <- left_join(map, codes %>% dplyr::select(Country, ISO3166.3, UN_Region),
                 by = c("region" = "Country")) %>%
  dplyr::select(-subregion) %>%
  filter(UN_Region == "Africa")

map %>% distinct(region)
