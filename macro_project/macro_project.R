#header
####################################################################################################
require(data.table); setDTthreads(0)
require(ggplot2)
require(car)
require(modelsummary)
require(quantmod)
require(tseries)
require(zoo)
require(lubridate)
require(gt)
require(tidyr)
require(dplyr)
require(fixest); setFixest_nthreads(0)
rstudioapi::getActiveDocumentContext()$path |> dirname() |> setwd() 
####################################################################################################

#import data
####################################################################################################
dt <- fread('namibia_data.csv')
dt2 <- fread('us_data.csv')

getSymbols("PURANUSDM", src = "FRED", from = "1990-01-01", end = "2024-01-01")
    
####################################################################################################

#clean data
####################################################################################################

### World databank cleaning

dt[dt == ".."] <- NA
dt2[dt2 == ".."] <- NA

id_vars = c("Series Name")
measure_vars <- list(5:ncol(dt))
measure_vars <- list(5:ncol(dt2))

dt <- melt(dt, id.vars = id_vars,
           measure.vars = measure_vars,
           variable.name = "Year", value.name = "Value")

dt2 <- melt(dt2, id.vars = id_vars,
           measure.vars = measure_vars,
           variable.name = "Year", value.name = "Value")
rm(id_vars, measure_vars)

dt[, Year := gsub(".*\\[YR(\\d{4})\\].*", "\\1", Year)]
dt[, Year := as.integer(Year)]
dt[, Value := as.numeric(Value)]

dt2[, Year := gsub(".*\\[YR(\\d{4})\\].*", "\\1", Year)]
dt2[, Year := as.integer(Year)]
dt2[, Value := as.numeric(Value)]

setnames(dt, old = c('Series Name'), new = c('Factor')) |> names()
dt <- dcast(dt, Year ~ Factor, value.var = "Value")

setnames(dt2, old = c('Series Name'), new = c('Factor')) |> names()
dt2 <- dcast(dt2, Year ~ Factor, value.var = "Value")

old_names <- c("Foreign direct investment, net (BoP, current US$)",
               "GDP (current US$)",
               "Gross capital formation (current US$)",
               "Gross domestic savings (current US$)",
               "Individuals using the Internet (% of population)",
               "Life expectancy at birth, total (years)",
               "Population, total",
               "Primary education, pupils",
               "Unemployment, total (% of total labor force) (modeled ILO estimate)")

new_names <- c("for_dir_inv",
               "gdp",
               "gross_cap_form",
               "gross_dom_sav",
               "internet_use",
               "life_exp",
               "pop",
               "student_primary",
               "unemployment")

setnames(dt, old = old_names, new = new_names) |> names()
setnames(dt2, old = old_names, new = new_names) |> names()
rm(old_names, new_names)

dt2 <- dt2[, .(Year,  gdp, pop, gross_cap_form, life_exp,
               for_dir_inv, unemployment, internet_use, gross_dom_sav)]

dt <- na.omit(dt)
dt2 <- na.omit(dt2)

dt[, unemployment := log(unemployment / 100)]
dt[, internet_use := log((internet_use / 100) + 0.0001)]

dt2[, unemployment := log(unemployment / 100)]
dt2[, internet_use := log((internet_use / 100) + 0.0001)]

dt[, ":="(gdp_pc = log(gdp) - log(shift(gdp)),
          pop_growth = log(pop) - log(shift(pop)),
          gross_cap_growth = log(gross_cap_form) - log(shift(gross_cap_form)),
          life_exp_pc = log(life_exp) - log(shift(life_exp)),
          for_dir_pc = sign(for_dir_inv) * log(abs(for_dir_inv))
          - sign(shift(for_dir_inv)) * log(abs(shift(for_dir_inv))),
          student_pc = log(student_primary) - log(shift(student_primary)),
          unemp_chng = unemployment - shift(unemployment),
          internet_chng = internet_use - shift(internet_use),
          savings_pc = sign(gross_dom_sav) * log(abs(gross_dom_sav))
          - sign(shift(gross_dom_sav)) * log(abs(shift(gross_dom_sav))))]

dt2[, ":="(gdp_pc = log(gdp) - log(shift(gdp)),
          pop_growth = log(pop) - log(shift(pop)),
          gross_cap_growth = log(gross_cap_form) - log(shift(gross_cap_form)),
          life_exp_pc = log(life_exp) - log(shift(life_exp)),
          for_dir_pc = sign(for_dir_inv) * log(abs(for_dir_inv))
          - sign(shift(for_dir_inv)) * log(abs(shift(for_dir_inv))),
          unemp_chng = unemployment - shift(unemployment),
          internet_chng = internet_use - shift(internet_use),
          savings_pc = sign(gross_dom_sav) * log(abs(gross_dom_sav))
          - sign(shift(gross_dom_sav)) * log(abs(shift(gross_dom_sav))))]


dt <- dt[, .(Year, gdp_pc, pop_growth, gross_cap_growth, life_exp_pc,
             for_dir_pc, student_pc, unemp_chng, internet_chng, savings_pc,
             gdp, pop, gross_cap_form, life_exp,
             for_dir_inv, student_primary, unemployment, internet_use, gross_dom_sav)]

dt2 <- dt2[, .(Year, gdp_pc, pop_growth, gross_cap_growth, life_exp_pc,
             for_dir_pc, unemp_chng, internet_chng, savings_pc,
             gdp, pop, gross_cap_form, life_exp,
             for_dir_inv, unemployment, internet_use, gross_dom_sav)]


fwrite(dt2, 'us_clean.csv')

### FRED data cleaning
uran <- PURANUSDM |> as.data.table()

setnames(uran, old = c("PURANUSDM"), new = c("uran_prc")) |> names()

uran[, Year := year(as.Date(index))]
uran <- uran[, .(Year, uran_prc)]

uran <- uran[, .(uran_prc = mean(uran_prc)), by = Year]

uran[, ":="(uran_pc = log(uran_prc) - log(shift(uran_prc)))]

### merge with namibia data
dt <- merge(dt, uran, by = "Year", all.x = TRUE)

fwrite(dt, 'namibia_clean.csv')

####################################################################################################

# visualize the data
####################################################################################################

### Summary Stats table
stats <- dt %>%
    summarise(
        mean_gdp_pc = mean(gdp_pc, na.rm = TRUE),
        median_gdp_pc = median(gdp_pc, na.rm = TRUE),
        sd_gdp_pc = sd(gdp_pc, na.rm = TRUE),
        min_gdp_pc = min(gdp_pc, na.rm = TRUE),
        max_gdp_pc = max(gdp_pc, na.rm = TRUE),

        mean_pop_growth = mean(pop_growth, na.rm = TRUE),
        median_pop_growth = median(pop_growth, na.rm = TRUE),
        sd_pop_growth = sd(pop_growth, na.rm = TRUE),
        min_pop_growth = min(pop_growth, na.rm = TRUE),
        max_pop_growth = max(pop_growth, na.rm = TRUE),

        mean_gross_cap_growth = mean(gross_cap_growth, na.rm = TRUE),
        median_gross_cap_growth = median(gross_cap_growth, na.rm = TRUE),
        sd_gross_cap_growth = sd(gross_cap_growth, na.rm = TRUE),
        min_gross_cap_growth = min(gross_cap_growth, na.rm = TRUE),
        max_gross_cap_growth = max(gross_cap_growth, na.rm = TRUE),

        mean_life_exp_pc = mean(life_exp_pc, na.rm = TRUE),
        median_life_exp_pc = median(life_exp_pc, na.rm = TRUE),
        sd_life_exp_pc = sd(life_exp_pc, na.rm = TRUE),
        min_life_exp_pc = min(life_exp_pc, na.rm = TRUE),
        max_life_exp_pc = max(life_exp_pc, na.rm = TRUE),

        mean_for_dir_pc = mean(for_dir_pc, na.rm = TRUE),
        median_for_dir_pc = median(for_dir_pc, na.rm = TRUE),
        sd_for_dir_pc = sd(for_dir_pc, na.rm = TRUE),
        min_for_dir_pc = min(for_dir_pc, na.rm = TRUE),
        max_for_dir_pc = max(for_dir_pc, na.rm = TRUE),

        mean_student_pc = mean(student_pc, na.rm = TRUE),
        median_student_pc = median(student_pc, na.rm = TRUE),
        sd_student_pc = sd(student_pc, na.rm = TRUE),
        min_student_pc = min(student_pc, na.rm = TRUE),
        max_student_pc = max(student_pc, na.rm = TRUE),

        mean_unemp_chng = mean(unemp_chng, na.rm = TRUE),
        median_unemp_chng = median(unemp_chng, na.rm = TRUE),
        sd_unemp_chng = sd(unemp_chng, na.rm = TRUE),
        min_unemp_chng = min(unemp_chng, na.rm = TRUE),
        max_unemp_chng = max(unemp_chng, na.rm = TRUE),

        mean_internet_chng = mean(internet_chng, na.rm = TRUE),
        median_internet_chng = median(internet_chng, na.rm = TRUE),
        sd_internet_chng = sd(internet_chng, na.rm = TRUE),
        min_internet_chng = min(internet_chng, na.rm = TRUE),
        max_internet_chng = max(internet_chng, na.rm = TRUE),
        
        mean_savings_pc = mean(savings_pc, na.rm = TRUE),
        median_savings_pc = median(savings_pc, na.rm = TRUE),
        sd_savings_pc = sd(savings_pc, na.rm = TRUE),
        min_savings_pc = min(savings_pc, na.rm = TRUE),
        max_savings_pc = max(savings_pc, na.rm = TRUE),
        
        mean_uran_pc = mean(uran_pc, na.rm = TRUE),
        median_uran_pc = median(uran_pc, na.rm = TRUE),
        sd_uran_pc = sd(uran_pc, na.rm = TRUE),
        min_uran_pc = min(uran_pc, na.rm = TRUE)

    ) %>%
    pivot_longer(cols = everything(), names_to = "metric", values_to = "value") %>%
    separate(metric, into = c("stat", "var"), sep = "_", extra = "merge") %>%
    pivot_wider(names_from = "var", values_from = "value") %>%
    rename(Statistic = stat)

# Create gt table
gt_table <- gt(stats) %>%
    tab_header(
        title = "Descriptive Statistics for Growth Model Variables",
        subtitle = "Includes GDP growth, Population Growth, and more"
    ) %>%
    cols_label(
        gdp_pc = "GDP growth",
        pop_growth = "Population Growth",
        gross_cap_growth = "Capital Formation Growth",
        life_exp_pc = "Life Expectancy Change",
        for_dir_pc = "Foreign Direct Investment Growth",
        student_pc = "Primary School Students Growth",
        unemp_chng = "Unemployment Change",
        internet_chng = "Internet Use Change",
        savings_pc = "Domestic Savings Growth",
        uran_pc = "Uranium Price Change"
        # Add labels for other variables
    ) %>%
    fmt_number(
        columns = vars(gdp_pc, pop_growth, gross_cap_growth, life_exp_pc,
                       for_dir_pc, student_pc, unemp_chng, internet_chng, savings_pc),  # Add other variables here
        decimals = 2
    ) %>%
    tab_style(
        style = cell_text(align = 'center'),
        locations = cells_body(columns = everything())
    )

### Graphs
fig1 <- ggplot(dt, aes(x = Year, y = gdp)) +
    geom_line(color = "blue", linewidth = 0.2) +
    labs(x = NULL, y = "GDP (Current US $)")

fig2 <- ggplot(dt, aes(x = Year, y = gdp_pc)) +
    geom_line(color = "red", linewidth = 0.2) +
    labs(x = NULL, y = "GDP Growth")

fig3 <- fig1 / fig2 +
    plot_annotation(title = "GDP v GDP Growth")

nam_gdp_pc <- ggplot(dt, aes(x = Year, y =  gdp_pc)) +
    geom_line(color = "blue", linewidth = 0.2) +
    labs(x = NULL, y = "Namibia GDP Growth")

us_gdp_pc <- ggplot(dt2, aes(x = Year, y =  gdp_pc)) +
    geom_line(color = "red", linewidth = 0.2) +
    labs(x = NULL, y = "United States GDP Growth")

nam_us_gpd_pc <- nam_gdp_pc / us_gdp_pc +
    plot_annotation(title = "Namibian v US GDP Growth")

print(nam_us_gpd_pc)

nam_gdp <- ggplot(dt, aes(x = Year, y =  gdp)) +
    geom_line(color = "blue", linewidth = 0.2) +
    labs(x = NULL, y = "Namibia GDP")

us_gdp <- ggplot(dt2, aes(x = Year, y =  gdp)) +
    geom_line(color = "red", linewidth = 0.2) +
    labs(x = NULL, y = "United States GDP")

nam_us_gdp <- (nam_gdp + nam_gdp_pc) / (us_gdp + us_gdp_pc) +
    plot_annotation(title = "Namibian v US")
####################################################################################################

#analyze data 
####################################################################################################
growth_mod <- lm(gdp_pc ~ pop_growth + gross_cap_growth +
                     life_exp_pc + for_dir_pc + student_pc +
                     unemp_chng + internet_chng + savings_pc + uran_pc, data = dt)

vif_result <- vif(growth_mod)

summary(growth_mod)

fols_growth <- feols(gdp_pc ~ pop_growth + gross_cap_growth +
                      life_exp_pc + for_dir_pc + student_pc +
                      unemp_chng + internet_chng + savings_pc + uran_pc, data = dt)

mod_summary <- modelsummary(list("OLS" = growth_mod),
                            output = "gt",
                            fmt = 3,
                            stars = TRUE,
                            title = "Namibian Growth Model Regression Results",
                            exponentiate = FALSE,
                            shape = term ~ model + statistic,
                            transpose = TRUE,
                            statistic = c("std.error", "p.value"),
                            #align = "lccccc",
                            gof_omit = "BIC|AIC|Within|F|RMSE")
print(mod_summary)

us_mod <- lm(gdp_pc ~ pop_growth + gross_cap_growth +
                 life_exp_pc + for_dir_pc  + unemp_chng +
                 internet_chng + savings_pc, data = dt2)

summary(us_mod)

us_mod_summary <- modelsummary(list("OLS" = us_mod),
                            output = "gt",
                            fmt = 3,
                            stars = TRUE,
                            title = "U.S. Growth Model Regression Results",
                            exponentiate = FALSE,
                            shape = term ~ model + statistic,
                            transpose = TRUE,
                            statistic = c("std.error", "p.value"),
                            #align = "lccccc",
                            gof_omit = "BIC|AIC|Within|F|RMSE")

####################################################################################################