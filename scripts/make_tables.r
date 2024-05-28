source("./scripts/load_libraries.r")
sw_data <- read_csv("./data/sw_data.csv")

mean_round <- function(x) {
    y <- round(mean(x, na.rm = T), 2)
    y[is.nan(y)] <- NA
    y <- sprintf(y, fmt = "%0.2f")
    y <- ifelse(y == "NA", NA, y)
    y
}
rate_format <- function(x) {
    y <- mean(x, na.rm = T)
    y <- scales::label_percent(accuracy = 0.01, suffix = "")(y)
    # coalesce(y, "-")
    return(y)
}
get_content <- function(table) {
    start <- str_which(table, "\\\\toprule")
    end <- str_which(table, "\\\\bottomrule")
    return(table[start:end])
}



split_data <- sw_data %>%
    mutate(country = case_match(
        country,
        "Dominican Republic" ~ "Domin. Rep.",
        "Russian Federation" ~ "Russian Fed.",
        "Czech Republic" ~ "Czech Rep.",
        "United Kingdom" ~ "UK",
        .default = country
    )) %>%
    group_by(country) %>%
    reframe(
        years = paste0(
            min(year), " - ", max(year)
        ),
        income_pc = dplyr::last(income_pc),
        labor_sh = label_percent(accuracy = 0.01, suffix = "")(mean((Pi) / C, na.rm = TRUE)),
        capital_sh = label_percent(accuracy = 0.01, suffix = "")(mean((C - Pi) / C, na.rm = TRUE)),
        f = label_percent(accuracy = 0.01, suffix = "")(mean(f, na.rm = TRUE)),
        g = label_percent(accuracy = 0.01, suffix = "")(mean(g, na.rm = TRUE)),
        r = label_percent(accuracy = 0.01, suffix = "")(mean(r, na.rm = TRUE)),
        # new = round(mean(new, na.rm = T), 2),
        # new2 = round(mean(new2, na.rm = T), 2)
    ) %>%
    drop_na() %>%
    arrange(desc(income_pc)) %>%
    mutate(split = !(row_number() <= max(row_number() / 2))) %>%
    select(-income_pc) 

share_table <- split_data %>%
    select(country, years, labor_sh, capital_sh, split) %>%
    split(.$split) %>%
    imap(~ kable(.x %>% select(-split), "latex",
        booktabs = TRUE,
        col.names = c(
            "Country", "Period"
            # ,"$f(K)$", "$g(K)$", "$r(K)$"
            # ,"div", "ecap", "etot"
            # ,"rent", "hcap", "htot"
            # ,"bill", "bond"
            , "Labor Share", "Capital Share"
        ), escape = FALSE
    ))
right_share_table <- share_table$`TRUE` %>%
    strsplit("\n") |>
    pluck(1) |>
    get_content() |>
    paste(collapse = "\n")
left_share_table <- share_table$`FALSE` %>%
    strsplit("\n") |>
    pluck(1) |>
    get_content() |>
    paste(collapse = "\n")


fgr_table <- split_data %>%
    select(country, years, f, g, r, split) %>%
    split(.$split) %>%
    imap(~ kable(.x %>% select(-split), "latex",
        booktabs = TRUE,
        col.names = c(
            "Country", "Period",
            "$f(K)$", "$g(K)$", "$r(K)$"
        ), escape = FALSE
    ))
right_fgr_table <- fgr_table$`TRUE` %>%
    strsplit("\n") |>
    pluck(1) |>
    get_content() |>
    paste(collapse = "\n")
left_fgr_table <- fgr_table$`FALSE` %>%
    strsplit("\n") |>
    pluck(1) |>
    get_content() |>
    paste(collapse = "\n")

tex_fgr_table <- paste("\\begin{table}[pos = H]
    \\vspace*{-2cm}
    \\caption{Average \\(f(K)\\), \\(g(K)\\) and \\(r(K)\\) in all countries, in descending order of net income per capita(\\%).}
    \\makebox[\\textwidth][c]{
    {\\centering
        \\begin{tabular}{lllll}
", left_fgr_table, "\\end{tabular}
}

\\centering{
\\begin{tabular}{lllll}
    ", right_fgr_table, "\n\\end{tabular}
}
}
\\label{tbl-fgr_table}
\\end{table}
")

write_file(tex_fgr_table, "./tables/fgr_table.tex")




tex_share_table <- paste("\\begin{table}[pos = H]
\\caption{Labor and capital shares\\(^a\\) in consumption in 68 countries in descending order of income per capita(\\%).}%
\\makebox[\\textwidth][c]{
{\\centering 
\\begin{tabular}{llll}",
left_share_table,
"\\end{tabular}
    }
    \\centering{
        \\begin{tabular}{llll}",
        right_share_table,
        "\\end{tabular}
    }
}
\\raggedright{\\footnotesize \\(^a\\) Derived at \\(\\frac{\\Pi}{C}\\) and \\(\\frac{C-\\Pi}{C}\\) respectively.}
\\label{tbl-shares_table}
\\end{table}
")

write_file(tex_share_table, "./tables/shares_table.tex")
