source("./scripts/load_libraries.r")

indicators <- c(
    "Net National Income" = "mnninc",
    "Net Capital Income" = "mfkpin",
    "Share of Capital in National Income" = "wcapsh", # Capital Income + 30% of net mixed income
    "Share of Labor in National Income" = "wlabsh",
    "Market Wealth" = "mnweal",
    "Net National Savings" = "msavin",
    "PP ER USD" = "xlcusx",
    "GDP" = "mgdpro",
    "final_consumption" = "mcongo",
    "consumption of fixed capital" = "mconfc",
    "household expenditure" = "mconhn",
    "Price Index" = "inyixx",
    "PPP local to usd conversion" = "xlcusp",
    "Per Capita Income" = "anninc"
)


wid_sw_data <- wid::download_wid(
    indicators = indicators,
    areas = "all",
    years = "all",
    ages = "all",
    pop = "all",
    verbose = TRUE
)


sw_data <- wid_sw_data |>
    rename("iso2c" = "country") %>%
    select(-percentile) %>%
    as_tibble() %>%
    mutate(variable = str_remove_all(variable, "999i")) %>%
    # bind_rows(net_national_income) %>%
    mutate(
        variable = case_match(
            variable,
            "mnninc" ~ "income",
            "mfkpin" ~ "capital_income",
            "wcapsh" ~ "share_capital_income", # Capital Income + 30% of net mixed income
            "wlabsh" ~ "share_labor_income",
            "mnweal" ~ "market_wealth",
            "msavin" ~ "savings",
            "xlcusx" ~ "pp_er_usd",
            "mgdpro" ~ "gdp",
            "mcongo" ~ "final_consumption",
            "mconhn" ~ "household_expenditure",
            "mconfc" ~ "delta",
            "inyixx" ~ "cpi",
            "xlcusp" ~ "loc_usd_ppp",
            "anninc" ~ "income_pc"
        )
    ) %>%
    pivot_wider(names_from = "variable", values_from = "value") %>%
    arrange(iso2c, year) %>%
    mutate(
        pp_conv = last(loc_usd_ppp),
        .by = iso2c
    ) %>%
    mutate(
        income = income / pp_conv,
        final_consumption = final_consumption / pp_conv,
        household_expenditure = household_expenditure / pp_conv,
        market_wealth = market_wealth / pp_conv,
        labor_income = share_labor_income * income,
        income_pc = income_pc / pp_conv
    ) %>%
    mutate(
        Pi = labor_income,
        C = final_consumption + household_expenditure,
        K = market_wealth,
        f = (C - Pi) / lag(K),
        g = (K - lag(K)) / lag(K),
        r = g + f,
        share_capital_income,
        share_labor_income,
        year,
        iso2c,
        income_pc,
        .by = iso2c,
        .keep = "used"
    ) |>
    drop_na() |>
    mutate(country = countrycode(iso2c, origin = "iso2c", destination = "country.name"))

write.csv(sw_data, "data/sw_data.csv")
