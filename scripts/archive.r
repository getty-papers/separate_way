
# new_group <- sw_data %>%
#     left_join(select(data_rr, -country), by = c("iso2c", "year")) %>%
#     mutate(country = case_match(
#         country,
#         "Dominican Republic" ~ "Domin. Rep.",
#         "Russian Federation" ~ "Russian Fed.",
#         "Czech Republic" ~ "Czech Rep.",
#         "United Kingdom" ~ "UK",
#         .default = country
#     )) %>%
#     group_by(country) %>%
#     reframe(
#         years = paste0(
#             min(year), " - ", max(year)
#             # , " (", length(year), ")"
#         ),
#         # f = label_percent(accuracy = 0.01, suffix = "")(mean(f, na.rm = T)),
#         # g = label_percent(accuracy = 0.01, suffix = "")(mean(g, na.rm = T)),
#         # r = label_percent(accuracy = 0.01, suffix = "")(mean(r, na.rm = T)),
#
#         income_pc = dplyr::last(income_pc),
#
#         # `Equity Dividend rtn.` = rate_format(eq_div_rtn),
#         # `Equity Capital gain` = rate_format(eq_capgain),
#         # `Equity Total rtn.` = rate_format(eq_tr),
#         # `Housing Rental rtn.` = rate_format(housing_rent_rtn),
#         # `Housing Capital gain` = rate_format(housing_capgain),
#         # `Housing Total rtn.` = rate_format(housing_capgain),
#         # `Gov. Bill rtn.` = rate_format(bill_rate),
#         # `Gov. Bond rtn.` = rate_format(bond_rate),
#
#         # share_capital_income,
#         # share_labor_income,
#         # labor_sh = label_percent(accuracy = 0.01, suffix = "")(mean(share_labor_income, na.rm = T)),
#         # capital_sh = label_percent(accuracy = 0.01, suffix = "")(mean(share_capital_income, na.rm = T)),
#
#         labor_sh = label_percent(accuracy = 0.01, suffix = "")(mean((Pi) / C, na.rm = TRUE)),
#         capital_sh = label_percent(accuracy = 0.01, suffix = "")(mean((C - Pi) / C, na.rm = TRUE)),
#         # new = round(mean(new, na.rm = T), 2),
#         # new2 = round(mean(new2, na.rm = T), 2)
#     ) %>%
#     drop_na() %>%
#     arrange(desc(income_pc)) %>%
#     mutate(split = !(row_number() <= max(row_number() / 2))) %>%
#     select(-income_pc) %>%
#     split(.$split) %>%
#     imap(~ kable(.x %>% select(-split), "latex",
#         booktabs = T,
#         col.names = c(
#             "Country", "Period"
#             # ,"$f(K)$", "$g(K)$", "$r(K)$"
#             # ,"div", "ecap", "etot"
#             # ,"rent", "hcap", "htot"
#             # ,"bill", "bond"
#             , "Labor Share", "Capital Share"
#         ), escape = FALSE
#     ))
#
# rr_part <- data_rr %>%
#     select(
#         year,
#         country,
#         eq_div_rtn,
#         eq_capgain,
#         eq_tr,
#         housing_rent_rtn,
#         housing_capgain,
#         housing_tr,
#         bill_rate,
#         bond_rate
#     ) |>
#     summarize(
#         `Equity Dividend rtn.` = rate_format(eq_div_rtn),
#         `Equity Capital gain` = rate_format(eq_capgain),
#         `Equity Total rtn.` = rate_format(eq_tr),
#         `Housing Rental rtn.` = rate_format(housing_rent_rtn),
#         `Housing Capital gain` = rate_format(housing_capgain),
#         `Housing Total rtn.` = rate_format(housing_tr),
#         `Gov. Bill rtn.` = rate_format(bill_rate),
#         `Gov. Bond rtn.` = rate_format(bond_rate),
#         .by = country
#     ) %>%
#     drop_na()
#
#
# rg_part <- sw_data %>%
#     mutate(country = case_match(
#         country,
#         "Dominican Republic" ~ "Domin. Rep.",
#         "Russian Federation" ~ "Russian Fed.",
#         "Czech Republic" ~ "Czech Rep.",
#         "United Kingdom" ~ "UK",
#         .default = country
#     )) %>%
#     group_by(country) %>%
#     reframe(
#         f = label_percent(accuracy = 0.01, suffix = "")(mean(f, na.rm = TRUE)),
#         g = label_percent(accuracy = 0.01, suffix = "")(mean(g, na.rm = TRUE)),
#         r = label_percent(accuracy = 0.01, suffix = "")(mean(r, na.rm = TRUE))
#     )
#
# right_join(rg_part, rr_part) |>
#     kable("latex",
#         booktabs = TRUE,
#         col.names = c(
#             "Country", "\\(f(K)]\\)", "\\(g(K)\\)", "\\(r(K)\\)",
#             "\\(\\shortstack{Div\\Yld}\\)",
#             "\\(\\shortstack{Cap\\Appr}\\)",
#             "\\(\\shortstack{Total\\Rtn}\\)",
#             "\\(\\shortstack{Rent\\Yld}\\)",
#             "\\(\\shortstack{Cap\\Appr}\\)",
#             "\\(\\shortstack{Total\\Rtn}\\)",
#             "Bills",
#             "Bonds"
#         ),
#         escape = FALSE
#     )





### Plots

# plot_10_data <- sw_data %>%
#     summarize(
#         income_pc = last(income_pc),
#         n = n(),
#         .by = country
#     ) |>
#     filter(n > 10) |>
#     arrange(desc(income_pc)) |>
#     select(-c(income_pc, n)) |>
#     mutate(group = (row_number() - 1) %/% 4) |>
#     group_nest(group)
#
#
# make_10_plots <- function(countries) {
#     temp <- sw_data %>%
#         filter(country %in% countries) |>
#         mutate(country = factor(country, countries)) |>
#         drop_na() |>
#         select(country, year, `f(K)` = f, `g(K)` = g, `r(K)` = r) |>
#         pivot_longer(-c(country, year)) |>
#         ggplot(aes(x = as.integer(year), y = value, color = name)) +
#         geom_hline(yintercept = 0, color = "darkgrey") +
#         geom_line(linewidth = 1) +
#         facet_wrap(vars(country),
#             scales = "free",
#             ncol = 2
#         ) +
#         theme_minimal(base_size = 14) +
#         # scale_y_continuous(name = "Value", labels = scales::label_dollar(scale = 1/10^6, suffix = "B")) +
#         theme(
#             legend.position = "top",
#             legend.title = element_blank(),
#             axis.text.x = element_text(angle = 30),
#             plot.margin = margin(l = 10, r = 15)
#         ) +
#         labs(
#             title = NULL,
#             caption = "Data from WID World",
#             x = NULL,
#             y = NULL
#         ) +
#         scale_colour_Publication()
#     temp
#     # ggsave("~/Downloads/fig-1-new.png", width = 17, height = 20, units = "cm")
# }
#
# i <- 1
# walk(plot_10_data$data, \(x) {
#     tmp <- make_10_plots(x$country)
#     ggsave(paste0("~/Downloads/fig-", i, "-new.jpeg"), plot = tmp, width = 17, height = 20, units = "cm")
#     i <<- i + 1
# })
#
# make_10_plots(plot_10_data$data[[1]]$country)
#
#
#
#
#
# data_rr <- readxl::read_excel("data/JSTdatasetR6.xlsx")
# data_rr <- data_rr %>%
#     select(
#         year,
#         country,
#         cpi,
#         iso3c = iso, # iso 3 letter
#         real_gdppp = rgdpmad, # Real GDP per capita (PPP, 1990 Int$, Maddison)
#         nom_gdp = gdp, # Nominal GDP
#         cpi, # Consumer prices (index, 1990=100)
#         xrusd, # USD exchange rate (local currency/USD)
#
#         eq_tr, # Equity total return, nominal. r[t] = [[p[t] + d[t]] / p[t-1] ] - 1
#         eq_capgain, # Equity capital gain, nominal. cg[t] = [ p[t] / p[t-1] ] - 1
#         eq_div_rtn, # Equity dividend return. dp_rtn[t] = dividend[t]/p[t-1],
#         eq_dp, # Equity dividend yield. dp[t] = dividend[t]/p[t]
#
#         housing_tr, # Housing total return, nominal. r[t] = [[p[t] + d[t]] / p[t-1] ] - 1
#         housing_capgain, # Housing capital gain, nominal. cg[t] = [ p[t] / p[t-1] ] - 1
#         housing_rent_rtn, # Housing rental return. dp_rtn[t] = rent[t]/p[t-1]
#
#         bond_tr, # Government bond total return, nominal. r[t] = [[p[t] + coupon[t]] / p[t-1] ] - 1
#         bond_rate, # Gov. bond rate, rate[t] = coupon[t] / p[t-1], or yield to maturity at t
#         bill_rate, # Bill rate, nominal. r[t] = coupon[t] / p[t-1]
#
#         capital_tr, # Tot. rtn. on wealth, nominal. Wtd. avg. of housing, equity, bonds and bills
#         risky_tr, # Tot. rtn. on risky assets, nominal. Wtd. avg. of housing and equity
#         safe_tr, # Tot. rtn. on safe assets, nominal. Equally wtd. avg. of bonds and bills
#     ) %>%
#     mutate(
#         iso2c = countrycode::countrycode(iso3c, origin = "iso3c", destination = "iso2c"),
#         .keep = "unused"
#     )
