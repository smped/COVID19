library(tidyverse)
library(scales)
library(ggrepel)

theme_set(theme_bw())

infections <- seq(1e3, 1e5, by = 1e3)
n <- 1.771e6
fp <- 0.004
tn <- 1 - fp
fn_symp <- 0.28
tp_symp <- 1 - fn_symp
fn_asymp <- 0.42
tp_asymp <- 1 - fn_asymp

tibble(
  infections,
  p_covid = infections / n,
  `Negative (Symptomatic)` = (fn*p_covid) / (tn*(1 - p_covid) + fn_symp*p_covid),
  `Negative (Asymptomatic)` = (fn*p_covid) / (tn*(1 - p_covid) + fn_asymp*p_covid),
  `Positive (Symptomatic)` = (tp_symp*p_covid) / (fp*(1 - p_covid) + tp_symp*p_covid),
  `Positive (Asymptomatic)` = (tp_asymp*p_covid) / (fp*(1 - p_covid) + tp_asymp*p_covid)
) %>%
  pivot_longer(cols = contains("tive"), names_to = "RAT", values_to = "P(COVID)") %>%
  separate(RAT, c("result", "status"), extra = "drop") %>%
  mutate(
    p = case_when(
      `P(COVID)` < 0.01 ~ percent(`P(COVID)`, 0.01),
      `P(COVID)` >= 0.01 ~ percent(`P(COVID)`, 0.1),
    )
  ) %>%
  ggplot(
    aes(infections, `P(COVID)`, colour = status, linetype = result)
  ) +
  geom_line() +
  geom_text(
    aes(x, y, label = lab),
    data = tribble(
      ~x, ~y, ~lab,
      1e3, 1, paste("False Positive Rate:", percent(fp, 0.1)),
      1e3, 0.95^2, paste("False Negative Rate (Asymptomatic):", percent(fn_asymp)),
      1e3, 0.9^2, paste("False Negative Rate (Symptomatic):", percent(fn_symp))
    ),
    hjust = 0,
    show.legend = FALSE,
    inherit.aes = FALSE
  ) +
  geom_label_repel(
    aes(infections, `P(COVID)`, label = p),
    data = . %>%
      dplyr::filter(infections %in% c(1e3, 3e3, 1e4, 3e4, 1e5)),
    show.legend = FALSE
  ) +
  scale_x_log10(label = comma, breaks = c(1e3, 3e3, 1e4, 3e4, 1e5)) +
  scale_y_continuous(labels = percent, trans = "sqrt", breaks = c(0.005, 0.02, 0.05, seq(0.1, 1, by = 0.1))) +
  scale_linetype_manual(values = c(2, 1)) +
  scale_colour_manual(values = c(rgb(0.8, 0.2, 0.2), rgb(0.2, 0.2, 0.8))) +
  labs(
    x = "Active Infections in SA",
    y = "P(COVID | Result)",
    linetype = "RAT Result",
    colour = "Status"
  )
