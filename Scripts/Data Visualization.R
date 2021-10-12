# Data Visualization
# Alice Lepissier
# alice.lepissier@gmail.com
# Chapter 3

## ## ## ## ## ## ## ## ## ## ##
# INDEX                     ####
## ## ## ## ## ## ## ## ## ## ##
# Preamble
# Import results
# Aggregated Predictions
# .. Gross outflows
# .. Gross inflows



## ## ## ## ## ## ## ## ## ## ##
# PREAMBLE                  ####
## ## ## ## ## ## ## ## ## ## ##

library(arrow)
library(GGally)
library(gridExtra)
library(ggridges)
library(here)
library(readxl)
library(showtext)
library(sysfonts)
library(tidyverse)
library(wesanderson)


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
# IMPORT RESULTS            ####
## ## ## ## ## ## ## ## ## ## ##

load(here("Data", "IFF", "panel_agg_trans.Rdata"))

LMIC_agg <- panel_agg_trans %>%
  filter(rIncome == "LIC" | rIncome == "LMC")

Africa_agg <- panel_agg_trans %>%
  filter(rRegion == "Africa")

HIC_agg <- panel_agg_trans %>%
  filter(rIncome == "HIC")



## ## ## ## ## ## ## ## ## ## ##
# EDA                       ####
## ## ## ## ## ## ## ## ## ## ##

viz <- LMIC_agg %>%
  select(ln.Tot_IFF_t, ln.In_Tot_IFF_t) %>%
  pivot_longer(c("ln.Tot_IFF_t", 
                 "ln.In_Tot_IFF_t"))

g <- ggplot(viz,
       aes(x = value,
           y = name,
           col = name,
           fill = name)) +
  geom_density_ridges() +
  scale_fill_manual(name = "Flow direction",
                    labels = c("Gross inflow",
                               "Gross outflow"),
                    values = wes_palette("Darjeeling1")) +
  labs(title = "Densities of transformed outcome variables",
       x = "Logged illicit financial flow ($)",
       y = "") +
  theme(axis.text.y = element_blank())
g

g <- ggplot(viz,
            aes(x = value,
                # group = name,
                fill = name)) +
  geom_density(alpha = 0.5,
               col = NA) +
  scale_fill_manual(name = "Flow direction",
                    labels = c("Gross inflow",
                               "Gross outflow"),
                    values = c("#FFD84D", "#00B0F6")) +
  labs(title = "Densities of transformed outcome variables",
       x = "Logged illicit financial flow ($)",
       y = "") +
  theme(axis.text.y = element_blank())
g
ggsave(g,
       file = here("Figures", "densities_trans.png"),
       width = 6, height = 4, units = "in")


### Africa
viz <- Africa_agg %>%
  select(ln.Tot_IFF_t, ln.In_Tot_IFF_t) %>%
  pivot_longer(c("ln.Tot_IFF_t", 
                 "ln.In_Tot_IFF_t"))
g <- ggplot(viz,
            aes(x = value,
                # group = name,
                fill = name)) +
  geom_density(alpha = 0.5,
               col = NA) +
  scale_fill_manual(name = "Flow direction",
                    labels = c("Gross inflow",
                               "Gross outflow"),
                    values = c("#FFD84D", "#00B0F6")) +
  labs(title = "Densities of transformed outcome variables",
       x = "Logged illicit financial flow ($)",
       y = "") +
  theme(axis.text.y = element_blank())
g
ggsave(g,
       file = here("Figures", "densities_trans_Africa.png"),
       width = 6, height = 4, units = "in")




# .. Correlogram ####
X <- arrow::read_feather(here("Results", "X.feather"))

X_cont <- X %>%
  select("ln.gdp_o", "ln.gdp_d", "ln.pop_o", "ln.pop_d", 
         "dist",
         "ihs.entry_cost_o", "ihs.entry_cost_d",
         "rCorrCont", "pCorrCont",
         "rRegQual", "pRegQual",
         "rRuleLaw", "pRuleLaw",
         "pSecrecyScore",
         "pFSI.rank",
         "pKFSI13",
         "pKFSI17",
         "pKFSI20",
         "ihs.tariff",
         "kai_o", "kai_d", "kao_o", "kao_d")

g <- ggcorr(X_cont, size = 10, family = "montserrat",
            layout.exp = 10) +
  theme(legend.text = element_text(size = 30)) +
  ggtitle("Correlation matrix of feature space")
g
ggsave(g,
       file = here("Figures", "correlogram.png"),
       width = 5, height = 5, units = "in")

# features <- LMIC_agg %>%
#   select(c(
#     'ln.gdp_o', 'ln.gdp_d', 'ln.pop_o', 'ln.pop_d', 
#                 'dist',
#                 'ihs.entry_cost_o', 'ihs.entry_cost_d',
#                 'rCorrCont', 'pCorrCont',
#                 'rRegQual', 'pRegQual', 
#                 'rRuleLaw', 'pRuleLaw',
#                 'pSecrecyScore',
#                 'pFSI.rank',
#                 'pKFSI13',
#                 'pKFSI17',
#                 'pKFSI20',
#                 'ihs.tariff',
#                 'kai_o', 'kai_d', 'kao_o', 'kao_d'))
# 
# feats <- features %>%
#   select(-ln.gdp_d, -ln.pop_d,
#          -ihs.entry_cost_d,
#          kai_d, kao_d)
# 
# 
# ch <- features %>%
#   pivot_longer(c("ln.gdp_o", "ln.gdp_d"),
#                values_to = "ln.gdp") %>%
#   select(-name) %>%
#   pivot_longer(c("ln.pop_o", "ln.pop_d"),
#                values_to = "ln.pop") %>%
#   select(-name) %>%
#   pivot_longer(c("ihs.entry_cost_o", "ihs.entry_cost_d"),
#                values_to = "ihs.entry_cost") %>%
#   select(-name) %>%
#   pivot_longer(c("kai_o", "kai_d"),
#                values_to = "kai") %>%
#   select(-name) %>%
#   pivot_longer(c("kao_o", "kao_d"),
#                values_to = "kao") %>%
#   select(-name)



## ## ## ## ## ## ## ## ## ## ##
# IMPORT PREDICTIONS        ####
## ## ## ## ## ## ## ## ## ## ##

idx <- arrow::read_feather(here("Results", "idx.feather"))
# X <- arrow::read_feather(here("Results", "X.feather"))
X <- arrow::read_feather(here("Results", "X_train.feather"))
# Y_out <- arrow::read_feather(here("Results", "Y_out.feather"))
Y_out <- arrow::read_feather(here("Results", "Y_train_out.feather"))
Y_in <- arrow::read_feather(here("Results", "Y_train_in.feather"))

idx %>%
  distinct(reporter.ISO) %>%
  nrow

idx <- idx %>% 
  left_join(codes %>%
              distinct(ISO3166.3, .keep_all = TRUE) %>%
              select(ISO3166.3, Country),
            by = c("reporter.ISO" = "ISO3166.3"))

preds.LM.CV_out <- arrow::read_feather("Results/preds.LM.CV_out.feather") %>%
  rename(preds_LM_CV_out = "0")
preds.RF.CV_out <- arrow::read_feather("Results/preds.RF.CV_out.feather") %>%
  rename(preds_RF_CV_out = "0")
preds.RF.CV_in <- arrow::read_feather("Results/preds.RF.CV_in.feather") %>%
  rename(preds_RF_CV_in = "0")


preds.lightGBM.CV_out <- arrow::read_feather("Results/preds.lightGBM.CV_out.feather") %>%
  rename(preds_lightGBM_CV_out = "0")

results_out_CV <- bind_cols(idx,
                            Y_out,
                            preds.LM.CV_out,
                            preds.RF.CV_out,
                            preds.lightGBM.CV_out)

results_in_CV <- bind_cols(idx,
                            Y_in,preds.RF.CV_in)


# .. Predictions in dollars ####
results_out_CV_doll <- results_out_CV %>%
  mutate(Tot_IFF_t = exp(ln.Tot_IFF_t)) %>%
  mutate_at(vars(ends_with("CV_out")),
            ~exp(.))

viz_out_doll <- results_out_CV_doll %>%
  pivot_longer(c("Tot_IFF_t",
                 "preds_LM_CV_out", 
                 "preds_RF_CV_out",
                 "preds_lightGBM_CV_out")) %>%
  select(c("reporter.ISO", "partner.ISO",
           "year", "name", "value"))

g_out_doll <- ggplot(viz_out_doll %>%
           group_by(year, name) %>%
           summarize(value = sum(value, na.rm = TRUE)) %>%
           ungroup() %>%
           mutate(name = factor(name,
                                levels = c("Tot_IFF_t",
                                           "preds_LM_CV_out",
                                           "preds_RF_CV_out",
                                           "preds_lightGBM_CV_out")))) +
  geom_line(aes(x = year,
                y = value / 10^6,
                col = name)) +
  labs(x = "Year",
       y = "Illicit flow in billion USD",
       title = "Predictions from all models",
       subtitle = "For gross outflows")


# .. Scatter plot of out-of-fold predictions for countries ####
test_countries <- LMIC_agg %>%
  group_by(reporter.ISO) %>%
  tally() %>%
  top_n(5) %>%
  arrange(desc(n)) %>%
  pull(reporter.ISO)

names <- data.frame(test_countries) %>%
  left_join(codes %>%
            distinct(ISO3166.3, .keep_all = TRUE) %>%
            select(ISO3166.3, Country),
          by = c("test_countries" = "ISO3166.3")) %>%
  pull(Country)

ggplots <- list()
for (r in seq(1, length(test_countries))){
  viz <- results_out_CV %>%
    filter(reporter.ISO == test_countries[r])
  lim <- max(viz$ln.Tot_IFF_t, viz$preds_RF_CV_out, na.rm = TRUE)
  g <- ggplot(viz,
              aes(x = ln.Tot_IFF_t,
                  y = preds_RF_CV_out)) +
    geom_point(alpha = 0.5) +
    xlim(0, lim) +
    ylim(0, lim) +
    geom_smooth(method = "lm", 
                formula = y ~ x,
                color = "darkorchid") +
    labs(title = names[r],
         x = "True value (logged gross outflows)",
         y = "Predicted value (logged gross outflows)")
  ggplots[[r]] <- g
  ggsave(g,
         file = here("Figures", paste0("RF_scatterpreds_", test_countries[r], ".png")),
         width = 5, height = 5, units = "in")
}


g <- grid.arrange(ggplots[[1]], ggplots[[2]],
             ggplots[[3]], ggplots[[4]],
             nrow = 2)
ggsave(g,
       file = here("Figures", paste0("RF_scatterpreds_grid.png")),
       width = 5, height = 5, units = "in")


# .. Scatter plot of out-of-fold predictions for Africa countries ####
Africa_agg %>%
  group_by(reporter.ISO) %>%
  tally() %>%
  top_n(5) %>%
  arrange(desc(n)) %>%
  pull(reporter.ISO)

Africa_agg %>% 
  group_by(reporter.ISO) %>%
  summarize(Sum = sum(Tot_IFF_t, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(Sum)) %>%
  select(reporter.ISO, Sum, everything())

countries <- idx %>%
  distinct(reporter.ISO) %>%
  arrange(reporter.ISO) %>%
  pull

names <- data.frame(countries) %>%
  left_join(codes %>%
              distinct(ISO3166.3, .keep_all = TRUE) %>%
              select(ISO3166.3, Country),
            by = c("countries" = "ISO3166.3")) %>%
  pull(Country)

store.r2 <- matrix(NA, nrow = length(ISOs), ncol = 2)
colnames(store.r2) <- c("ISO", "r2")

for (r in seq(1, length(countries))){
  viz <- results_out_CV %>%
    filter(reporter.ISO == countries[r])
  
  lim <- max(viz$ln.Tot_IFF_t, viz$preds_RF_CV_out, na.rm = TRUE)
  r2 <- cor(viz$ln.Tot_IFF_t, viz$preds_RF_CV_out)^2
  r2.label <- paste("R^2 == ", round(r2, 2))
  store.r2[r, 1] <- countries[r]
  store.r2[r, 2] <- r2
  
  g <- ggplot(viz,
              aes(x = ln.Tot_IFF_t,
                  y = preds_RF_CV_out)) +
    geom_point(alpha = 0.5) +
    xlim(0, lim) +
    ylim(0, lim) +
    geom_smooth(method = "lm", 
                formula = y ~ x,
                color = "darkorchid") +
    geom_abline(intercept = 0 , slope = 1,
                col = "grey") +
    labs(title = names[r],
         x = "True value (logged gross outflows)",
         y = "Predicted value (logged gross outflows)") +
    geom_text(x = -Inf,
              y = Inf,
              hjust = 0, vjust = 1,
              label = r2.label,
              family = "montserrat",
              size = 10,
              parse = TRUE)
  ggsave(g,
         file = here("Figures", paste0("RF_scatterpreds_", countries[r], ".png")),
         width = 5, height = 5, units = "in")
}


### inflows
store.r2 <- matrix(NA, nrow = length(countries), ncol = 2)
colnames(store.r2) <- c("ISO", "r2")

for (r in seq(1, length(countries))){
  viz <- results_in_CV %>%
    filter(reporter.ISO == countries[r])
  
  lim <- max(viz$ln.In_Tot_IFF_t, viz$preds_RF_CV_in, na.rm = TRUE)
  r2 <- cor(viz$ln.In_Tot_IFF_t, viz$preds_RF_CV_in)^2
  r2.label <- paste("R^2 == ", round(r2, 2))
  store.r2[r, 1] <- countries[r]
  store.r2[r, 2] <- r2
  
  g <- ggplot(viz,
              aes(x = ln.In_Tot_IFF_t,
                  y = preds_RF_CV_in)) +
    geom_point(alpha = 0.5) +
    xlim(0, lim) +
    ylim(0, lim) +
    geom_smooth(method = "lm", 
                formula = y ~ x,
                color = "darkslategray4") +
    geom_abline(intercept = 0 , slope = 1,
                col = "grey") +
    labs(title = names[r],
         x = "True value (logged gross inflows)",
         y = "Predicted value (logged gross inflows)") +
    geom_text(x = -Inf,
              y = Inf,
              hjust = 0, vjust = 1,
              label = r2.label,
              family = "montserrat",
              size = 10,
              parse = TRUE)
  ggsave(g,
         file = here("Figures", paste0("RF_scatterpreds_in_", countries[r], ".png")),
         width = 5, height = 5, units = "in")
}




## ## ## ## ## ## ## ## ## ## ##
# PLACEBO TRIALS            ####
## ## ## ## ## ## ## ## ## ## ##


placebo_in <- arrow::read_feather(here("Results", "placebo_results_100.feather"))

viz <- placebo_in %>%
  # select(ln.Tot_IFF_t, ln.In_Tot_IFF_t) %>%
  pivot_longer(c("R2_train", 
                 "R2_test",
                 "MSE_test"))

g <- ggplot(viz %>%
              filter(name == "MSE_test"),
            aes(x = value,
                # group = name,
                fill = name)) +
  geom_density(alpha = 0.5,
               col = NA) +
  # geom_vline(xintercept = 3.04,
  #            col = "#FFD84D") +
  geom_segment(aes(x=4, 
                   y=0, 
                   xend=4, 
                   yend=1.1), 
               color = "#FFD84D") +
  geom_text(x = 4,
            y = 1.15,
            label = "MSE from true trades",
            col = "black",
            family = "montserrat",
            size = 10) +
  geom_text(x = 12,
            y = 1.15,
            label = "MSE from placebo trades",
            col = "black",
            family = "montserrat",
            size = 10) +
  xlim(0,15) +
  scale_fill_manual(name = "Flow direction",
                    labels = c("Gross inflow",
                               "Gross outflow"),
                    values = c("#FFD84D", "#00B0F6")) +
  labs(title = "Placebo trials for reshuffled bilateral IDs",
       x = "Mean Square Error in test set",
       y = "") +
  theme(axis.text.y = element_blank())
g
ggsave(g,
       file = here("Figures", "placebo_MSE.png"),
       width = 6, height = 4, units = "in")

