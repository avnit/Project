# Plots
install.packages(ggvis)
library(ggvis);
library(dplyr);
# Plotting Sharpe Ratio vs Profit factor
tstats %>% ggvis(~Ann.Sharpe,~Profit.Factor) %>% layer_points() %>%
  layer_smooths()

tstats %>% ggvis(~Avg.Trade.PL,~Num.Txns) %>% layer_points() %>%
  layer_smooths()

tstats %>% ggvis(~Ann.Sharpe,~Num.Txns, fill=~factor(Symbol)) %>% layer_points() %>%
  group_by(Symbol) %>%
  layer_model_predictions(model = "lm")

tstats %>% ggvis(~Profit.Factor,~Num.Txns, fill=~factor(Symbol)) %>% layer_points() %>%
  group_by(Symbol) %>%
  layer_model_predictions(model = "lm")
