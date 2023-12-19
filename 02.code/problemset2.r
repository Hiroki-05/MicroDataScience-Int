# ---------------------------------------------------------------------------- #
#                                     Setup                                    #
# ---------------------------------------------------------------------------- #

library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(tidyr)
library(stringr)
library(stringi)
library(kableExtra)
library(gtExtras)
library(fixest)
library(cobalt)
library(fastDummies)
library(data.table)
library(dtplyr)

# ---------------------------------------------------------------------------- #
#                                     Main                                     #
# ---------------------------------------------------------------------------- #

# データの読み込み
df <- readr::read_csv(here::here('01.data', 'based_df.csv'))
df_stata <- readr::read_csv(here::here('01.data', 'stata', 'test_IPEDS_Analysis_Figure.csv'))

# 分析に使う列のみを抽出
# df <- extract_cols(df)

# 分析に必要な変数の追加
df <- add_more_varibales(df)
colnames(df)


plot_eventstudy(estimate_eventstudy)


# ---------------------------------------------------------------------------- #
#                                   functions                                  #
# ---------------------------------------------------------------------------- #

# -------------------------------- 分析に使う列のみを抽出 ------------------------------- #
extract_cols <- function(df) {
    list_cols <- c(
        'unitid',
        'year',
        'yearstosem',
        'yearofsem',
        'instnm',
        'city',
        'totcohortsize',
        'm_cohortsize',
        'w_cohortsize',
        'white_cohortsize',
        'urm_cohortsize',
        'tot4yrgrads',
        'm_4yrgrads',
        'w_4yrgrads',
        'white_4yrgrads',
        'urm_4yrgrads',
        'gradrate4yr',
        'mengradrate4yr',
        'womengradrate4yr',
        'whitegradrate4yr',
        'urmgradrate4yr',
        'costs',
        'revenue',
        'faculty',
        'instatetuition',
        'outofstatetuition',
        'semester2',
        'quarter2',
        'meansize',
        'public',
        'per_white',
        'per_urm',
        'per_male',
        'per_fem'
        )

    df_output <- df |> 
        dplyr::select(any_of(list_cols))

    return(df_output)

}


# -------------------------------- 分析に必要な変数の追加 ------------------------------- #
add_more_varibales <- function(df) {
    
    # yearstosemが-3から−1の時に1をとるダミー
    # これは、partially treatedのグループに属するダミーであり、入学後1−3年の間にsemesterに変更があった学生

    # yearstosemが0以上で1を取るダミー
    # これは、fully treatedのグループに属するダミーであり、入学開始時点からsemesterだった学生
    df_output <- df |> 
        dplyr::mutate(
            block1 = dplyr::if_else(yearstosem >= -3 & yearstosem <= -1, 1, 0),
            block2 = dplyr::if_else(yearstosem >= 0 & !is.na(yearstosem), 1, 0),
            block1 = ifelse(is.na(block1), 0, block1),
            treated = dplyr::if_else(!is.na(yearstosem), 1, 0), # treated_dummy
            yearstosem = dplyr::if_else(is.na(yearstosem), 0, yearstosem)
        )

    # df_output <- df_output |> 
    #     fastDummies::dummy_cols(select_columns = 'year')

    return(df_output)

}




descriptive_stats <- function(df) {

    list_mean_vars <- c(
        'semester2',
        'gradrate4yr',
        'mengradrate4yr',
        'womengradrate4yr',
        'whitegradrate4yr',
        'instatetuition',
        'costs',
        'faculty',
        'per_white',
        'per_urm',
        'per_fem')

    df_output <- df %>% 
        dtplyr::lazy_dt() |> 
        dplyr::summarize(
            mean = drplyr::across(list_mean_vars, ~mean(., na.rm = TRUE)),
            sd = dplyr::across(list_mean_vars, ~sd(., na.rm = TRUE)),
        ) |> 
        t() |> 
        tidyr::as_tibble(rownames = 'category') |> 
        data.table::setnames(c('cateogry', 'value')) |> 
        dplyr::mutate(
            value = round(value, digits = 2)
        ) |> 
        kableExtra::kbl() |> 
        kableExtra::kable_styling(full_width = FALSE)


    df_output <- df |> 
        dtplyr::lazy_dt() 
        summarize(
            mean = across(list_mean_vars, ~mean(., na.rm = TRUE)),
            sd = across(list_mean_vars, ~sd(., na.rm = TRUE)),
        ) |> 
        t() dfffgfg
        tidyr::as_tibble(rownames = 'category') |> 
        data.table::setnames(c('cateogry', 'value')) |> 
        mutate(
            value = round(value, digits = 2)
        )

        df_output
        summarytools::descr(
            stats = c("mean", "sd", "min", "max", "median", "IQR", "n.valid", "n.missing"),
            transpose = TRUE,
            style = "rmarkdown"
        ) |> 
        as.data.frame()

    return(df_output)

}


df_based <- readr::read_csv("01.data/based_df.csv")

df_based |> View()



estimate_table3 <- function(df) {

    estimate_simple <- estimatr::lm_robust(
        gradrate4yr ~ block1 + block2,
        data = df)

    col1 <- fixest::feols(
        gradrate4yr ~ block1 + block2| year + unitid,
        data = df,
        weights = ~ meansize,
        cluster = 'unitid') |> 
        etable()

    col1

    col2 <- fixest::feols(
        gradrate4yr ~ block1 + block2 + i(year) | year + unitid,
        data = df,
        weights = ~ meansize,
        cluster = 'unitid') |>
        etable()

    col2
    return(model_output)
}


estimate_eventstudy <- function(df) {

    df_estimate <- df |> 
        fastDummies::dummy_cols(select_columns = 'yearstosem')

    estimate_fixest <- fixest::feols(
        gradrate4yr ~ i(yearstosem, treated, ref = -4) + instatetuition + costs + faculty + per_white + per_urm + per_fem | year + unitid,
        # gradrate4yr ~ i(yearstosem, treated, ref = -4) + instatetuition + costs + faculty| year + unitid,
        data = df,
        # weights = ~ meansize,
        cluster = 'unitid') 

    df_lm_robust <- df |> 
        dplyr::mutate(
            yearstosem = stringr::str_replace_all(yearstosem, "-", "m")
        ) |> 
        fastDummies::dummy_cols(select_columns = 'yearstosem') |> 
        dplyr::mutate(
            yearstosem_0 = dplyr::if_else(treated == 0, 0, yearstosem_0),
            yearminus10 = dplyr::if_else(
                yearstosem_m10 == 1 | yearstosem_m11 == 1 | yearstosem_m12 == 1 | yearstosem_m13 == 1 | yearstosem_m14 == 1 | yearstosem_m15 == 1 | yearstosem_m16 == 1 | yearstosem_m17 == 1 | yearstosem_m18 == 1 | yearstosem_m19 == 1 | yearstosem_m20 == 1 | yearstosem_m21 == 1 | yearstosem_m22 == 1 | yearstosem_m23 == 1 | yearstosem_m24 == 1 | yearstosem_m25 == 1, 1, 0
            ),
            yearover10 = dplyr::if_else(
                yearstosem_10 == 1 | yearstosem_11 == 1 | yearstosem_12 == 1 | yearstosem_13 == 1 | yearstosem_14 == 1 | yearstosem_15 == 1 | yearstosem_16 == 1 | yearstosem_17 == 1 | yearstosem_18 == 1 | yearstosem_19 == 1 | yearstosem_20 == 1 | yearstosem_21 == 1 | yearstosem_22 == 1, 1, 0
            )
        )

    df_lm_robust |> View()

    plm(Y ~ A + B, 
    data = df, index = c('region', 'year'), model = 'within',
    effect = 'twoways')

    estimate_lm_robust <- lm_robust(
        gradrate4yr ~ yearminus10 + 
        # gradrate4yr ~ yearstosem_m25 + 
                    #   yearstosem_m24 +
                    #   yearstosem_m23 + 
                    #   yearstosem_m22 +
                    #   yearstosem_m21 + 
                    #   yearstosem_m20 +
                    #   yearstosem_m19 +
                    #   yearstosem_m18 +
                    #   yearstosem_m17 +
                    #   yearstosem_m16 +
                    #   yearstosem_m15 +
                    #   yearstosem_m14 +
                    #   yearstosem_m13 +
                    #   yearstosem_m12 +
                    #   yearstosem_m11 +
                    #   yearstosem_m10 + 
                      yearstosem_m9 + 
                      yearstosem_m8 + 
                      yearstosem_m7 + 
                      yearstosem_m6 + 
                      yearstosem_m5 + 
                    #   yearstosem_m4 + 
                      yearstosem_m3 + 
                      yearstosem_m2 + 
                      yearstosem_m1 + 
                      yearstosem_0 + 
                      yearstosem_1 + 
                      yearstosem_2 + 
                      yearstosem_3 + 
                      yearstosem_4 +
                      yearstosem_5 +
                      yearstosem_6 +
                      yearstosem_7 +
                      yearstosem_8 +
                      yearstosem_9 +
                      yearover10 +
                    #   yearstosem_10 +
                    #   yearstosem_11 +
                    #   yearstosem_12 +
                    #   yearstosem_13 +
                    #   yearstosem_14 +
                    #   yearstosem_15 +
                    #   yearstosem_16 +
                    #   yearstosem_17 +
                    #   yearstosem_18 +
                    #   yearstosem_19 +
                    #   yearstosem_20 +
                    #   yearstosem_21 +
                    #   yearstosem_22 +
                      instatetuition + costs + faculty + per_white + per_urm + per_fem,
        fixed_effects = ~ year + unitid,
        weights = meansize,
        data = df_lm_robust) 


    estimate_plm <- plm::plm(
        # formula = gradrate4yr ~ yearminus10 + 
        gradrate4yr ~ yearstosem_m25 + 
                      yearstosem_m24 +
                      yearstosem_m23 + 
                      yearstosem_m22 +
                      yearstosem_m21 + 
                      yearstosem_m20 +
                      yearstosem_m19 +
                      yearstosem_m18 +
                      yearstosem_m17 +
                      yearstosem_m16 +
                      yearstosem_m15 +
                      yearstosem_m14 +
                      yearstosem_m13 +
                      yearstosem_m12 +
                      yearstosem_m11 +
                      yearstosem_m10 + 
                      yearstosem_m9 + 
                      yearstosem_m8 + 
                      yearstosem_m7 + 
                      yearstosem_m6 + 
                      yearstosem_m5 + 
                    #   yearstosem_m4 + 
                      yearstosem_m3 + 
                      yearstosem_m2 + 
                      yearstosem_m1 + 
                      yearstosem_0 + 
                      yearstosem_1 + 
                      yearstosem_2 + 
                      yearstosem_3 + 
                      yearstosem_4 +
                      yearstosem_5 +
                      yearstosem_6 +
                      yearstosem_7 +
                      yearstosem_8 +
                      yearstosem_9 +
                    #   yearover10 +
                      yearstosem_10 +
                      yearstosem_11 +
                      yearstosem_12 +
                      yearstosem_13 +
                      yearstosem_14 +
                      yearstosem_15 +
                      yearstosem_16 +
                      yearstosem_17 +
                      yearstosem_18 +
                      yearstosem_19 +
                      yearstosem_20 +
                      yearstosem_21 +
                      yearstosem_22 +
                      instatetuition + costs + faculty + per_white + per_urm + per_fem,
        index = c('year', 'unitid'),
        # weights = meansize,
        model = 'within',
        data = df_lm_robust) 

}

estimate_input <- estimate_fixest

plot_eventstudy <- function(estimate_input) {

    df_based <- estimate_input |>
        coeftable() |> 
        as_tibble(rownames = "year") |> 
        dplyr::rename(
            "coef" = "Estimate",
            "se" = "Std. Error",
            "t_value" = "t value",
            "p_value" = "Pr(>|t|)")

    df_output <- df_based |> 
        dplyr::mutate(
            year = stringr::str_replace_all(year, pattern = 'yearstosem::', replacement = ''),
            year = stringr::str_replace_all(year, pattern = ':treated', replacement = ''),
            year = as.numeric(year)
        ) |> 
        dplyr::filter(dplyr::between(year, -10, 10)) |> 
        dplyr::mutate(
            lci = coef - 1.96 * se,
            rci = coef + 1.96 * se
        ) |> 
        dplyr::add_row(
            year = -4,
            coef = 0,
            lci = 0,
            rci = 0)

    plot_output <- ggplot(df_output, aes(x = year, y = coef, ymin = lci, ymax = rci)) +
        geom_point() +
        geom_line() +
        geom_errorbar() +
        geom_hline(yintercept = 0, linetype = 'dashed') +
        geom_vline(xintercept = -0, linetype = 'dashed') +
        geom_vline(xintercept = -3, linetype = 'dashed') +
        theme_bw() +
        scale_y_continuous(
            limits = c(-0.1, 0.05),
            breaks = seq(-0.1, 0.05, 0.05)
            ) +
        scale_x_continuous(
            limits = c(-10, 10),
            breaks = seq(-10, 10)
            )+
        theme(
            axis.text.x = element_text(size = 13),
            axis.text.y = element_text(size = 13)
        )
            
    plot_output
}


plot_scatter <- function(df, x_var, y_var) {

    x_var <- rlang::enquo(x_var)
    y_var <- rlang::enquo(y_var)

    df_plot_based <- df |> 
        dplyr::summarise(
            y_mean = mean(!!(y_var), na.rm = TRUE),
            x_mean = mean(!!(x_var), na.rm = TRUE),
            .by = ('unitid')
        )

    plot_output <- ggplot(df_plot_based, aes(x = x_mean, y = y_mean)) +
        geom_point() +
        theme_bw() +
        geom_smooth(method = 'lm', se = FALSE) +
        theme(
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10)
        ) +
        labs(
            x = rlang::quo_text(x_var),
            y = rlang::quo_text(y_var)
        )

    return(plot_output)
}

plot_scatter(df, instatetuition, gradrate4yr)
plot_scatter(df, instatetuition, semester2)

