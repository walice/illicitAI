library(bestNormalize)
library(DescTools)

panel_agg <- panel_agg %>%
  mutate(Net_Imp_IFF = Imp_IFF + In_Imp_IFF,
         Net_Exp_IFF = Exp_IFF + In_Exp_IFF)

yeo <- yeojohnson(panel_agg$Net_Imp_IFF)
panel_agg$yeo.Net_Imp_IFF = predict(yeo, newdata = panel_agg$Net_Imp_IFF)
yeo <- yeojohnson(panel_agg$Net_Exp_IFF)
panel_agg$yeo.Net_Exp_IFF = predict(yeo, newdata = panel_agg$Net_Exp_IFF)

panel_agg <- panel_agg %>%
  mutate(yeo.Net_Imp_IFF = yeojohnson(Net_Imp_IFF),
         yeo.Net_Exp_IFF = yeojohnson(Net_Exp_IFF))

summary(panel_agg$Net_Imp_IFF)
summary(panel_agg$Net_Exp_IFF)

boxplot(panel_agg$Net_Imp_IFF)
boxplot(panel_agg$Net_Exp_IFF)

# panel_agg <- panel_agg %>%
#   mutate(ln.Net_Imp_IFF = log())

test <- panel_agg %>%
  filter(year == 2005) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = TRUE) /10^9,
            Exp_IFF = sum(Exp_IFF, na.rm = TRUE) /10^9,
            In_Imp_IFF = sum(In_Imp_IFF, na.rm = TRUE) /10^9,
            In_Exp_IFF = sum(In_Exp_IFF, na.rm = TRUE) /10^9)
test


fit.Net.Imp <- fit_lm(train.panel_agg, "yeo.Net_Imp_IFF", vars)
summary(fit.Net.Imp)
preds.Net.Imp <- pred(fit.Net.Imp, test.panel_agg, "Net_Imp_IFF", vars)
RMSE.Net.Imp <- calc_RMSE(test.panel_agg, "Net_Imp_IFF", preds.Net.Imp, vars) / 10^9

fit.Net.Exp <- fit_lm(train.panel_agg, "yeo.Net_Exp_IFF", vars)
summary(fit.Net.Exp)
preds.Net.Exp <- pred(fit.Net.Exp, test.panel_agg, "Net_Exp_IFF", vars)
RMSE.Net.Exp <- calc_RMSE(test.panel_agg, "Net_Exp_IFF", preds.Net.Exp, vars) / 10^9

plot(density(panel_agg$Net_Imp_IFF, na.rm = TRUE))
plot(density(panel_agg$yeo.Net_Imp_IFF, na.rm = TRUE))

panel_agg$Z.Net_Imp_IFF <- FisherZ(panel_agg$Net_Imp_IFF)
plot(density(panel_agg$Z.Net_Imp_IFF, na.rm = TRUE))

panel_agg$Z.Net_Exp_IFF <- FisherZ(panel_agg$Net_Exp_IFF)
plot(density(panel_agg$Z.Net_Exp_IFF, na.rm = TRUE))

fit.Net.Imp <- fit_lm(train.panel_agg, "Z.Net_Imp_IFF", vars)
summary(fit.Net.Imp)
preds.Net.Imp <- pred(fit.Net.Imp, test.panel_agg, "Net_Imp_IFF", vars)
RMSE.Net.Imp <- calc_RMSE(test.panel_agg, "Net_Imp_IFF", preds.Net.Imp, vars) / 10^9

fit.Net.Exp <- fit_lm(train.panel_agg, "yeo.Net_Exp_IFF", vars)
summary(fit.Net.Exp)
preds.Net.Exp <- pred(fit.Net.Exp, test.panel_agg, "Net_Exp_IFF", vars)
RMSE.Net.Exp <- calc_RMSE(test.panel_agg, "Net_Exp_IFF", preds.Net.Exp, vars) / 10^9
