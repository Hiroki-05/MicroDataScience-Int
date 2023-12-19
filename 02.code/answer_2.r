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
library(patchwork)

# ---------------------------------------------------------------------------- #
#                                     Main                                     #
# ---------------------------------------------------------------------------- #

# データの読み込み
df <- readr::read_csv(here::here('01.data', 'problem1_dataset', 'master.csv'))
# 分析に必要な変数の追加
df <- add_more_varibales(df)

df |> select(unitid, year, totcohortsize, meansize) |> distinct() |> View()

plot_eventstudy(estimate_eventstudy)

a <- df |> filter(unitid == 100654)
df_raw <- read.csv("/Users/ito_hiroki/03.Work/YNU/TA_MicroDataSci/01.data/ipeds_cleaned_final.csv")

b <- df_raw |> filter(unitid == 100654) |> select(unitid, year, totcohortsize, meansize)
b
mean(b$totcohortsize, na.rm = TRUE)

mean(a$totcohortsize, na.rm = TRUE)



# ---------------------------------------------------------------------------- #
#                                   functions                                  #
# ---------------------------------------------------------------------------- #


# -------------------------------- 分析に必要な変数の追加 ------------------------------- #

# (b)回帰分析
# 1 
add_more_varibales <- function(df) {

    df_first_year  <- df |> 
        dplyr::distinct(unitid, semester, quarter, .keep_all = TRUE) |> 
        dplyr::group_by(unitid) |> 
        dplyr::filter(
            dplyr:::n() > 1
        ) |> 
        dplyr::filter(
            semester == 1
        ) |> 
        dplyr::distinct(unitid, year) |> 
        dplyr::rename(
            yearofsem = year
        ) |> 
        ungroup()

    df <- df |> 
        dplyr::left_join(df_first_year, by = 'unitid')

    # yearstosemが-3から−1の時に1をとるダミー
    # これは、partially treatedのグループに属するダミーであり、入学後1−3年の間にsemesterに変更があった学生

    # yearstosemが0以上で1を取るダミー
    # これは、fully treatedのグループに属するダミーであり、入学開始時点からsemesterだった学生
    df_output <- df |> 
        dplyr::mutate(
            yearstosem = year - yearofsem,
            yearstosem = dplyr::if_else(is.na(yearstosem), -1000, yearstosem),
            after = dplyr::if_else(yearstosem >= 0, 1, 0),
            group1 = dplyr::if_else(yearstosem >= -3 & yearstosem <= -1, 1, 0),
            group2 = dplyr::if_else(yearstosem >= 0 & !is.na(yearstosem), 1, 0),
            group1 = dplyr::if_else(is.na(group1), 0, group1),
            treated = dplyr::if_else(!is.na(yearstosem), 1, 0) # treated_dummy
        ) |>
        dplyr::group_by(unitid) |>
        dplyr::mutate(
            meansize = mean(totcohortsize, na.rm = TRUE)
        ) |> 
        dplyr::ungroup()


    # df_output <- df_output |> 
    #     fastDummies::dummy_cols(select_columns = 'year')

    return(df_output)

}


# ----------------------------------- Estimate 1 ----------------------------------- #

descriptive_stats <- function(df) {

    list_mean_vars <- c(
        'semester',
        'gradrate',
        'men_gradrate',
        'women_gradrate_4yr',
        'instatetuition',
        'costs',
        'per_white_cohort',
        'per_women_cohort')

    kbl_output <- df %>% 
        # dtplyr::lazy_dt() |> 
        dplyr::summarize(
            mean = dplyr::across(list_mean_vars, ~mean(., na.rm = TRUE)),
            sd = dplyr::across(list_mean_vars, ~sd(., na.rm = TRUE)),
        ) |> 
        t() |> 
        tidyr::as_tibble(rownames = 'category') |> 
        data.table::setnames(c('category', 'value')) |> 
        dplyr::mutate(
            value = round(value, digits = 2)
        ) |> 
        tidyr::separate(
            col = category, 
            into = c("category", "variable"),
            sep = '\\.',
        ) |> 
        tidyr::pivot_wider(
            names_from = category,
            values_from = value
        ) |> 
        kableExtra::kbl() |> 
        kableExtra::kable_styling(full_width = FALSE)

    return(kbl_output)
}

colnames(df)

estimate_table3 <- function(df) {

    estimate_simple <- estimatr::lm_robust(
        gradrate ~ group1 + group2,
        weights = ~ meansize,
        data = df)

    col1 <- fixest::feols(
        gradrate ~ group1 + group2| year + unitid,
        data = df_mod,
        weights = ~meansize,
        cluster = 'unitid') |> 
        etable()

    col1

    col2 <- fixest::feols(
        gradrate ~ group1 + group2 + i(year) | year + unitid,
        data = df,
        weights = ~ meansize,
        cluster = 'unitid') |>
        etable()

    col2


    model_output <- list()


    # estimate_after <- estimatr::lm_robust(
    estimatr::lm_robust(
        gradrate4yr ~ after,
        data = df) |> 
        modelsummary::msummary()
    


    return(model_output)
}


# estimatr::lm_robust(
fixest::feols(
    instatetuition ~ semester + i(year) + i(unitid, year) | year + unitid,
    weights = ~ meansize,
    data = df) |> 
    etable()




estimate_eventstudy <- function(df) {


    estimate_fixest <- fixest::feols(
        gradrate4yr ~ i(yearstosem, treated, ref = c(-4, -1000))　| year + unitid,
        # gradrate ~ i(yearstosem, treated, ref = c(-4, -1000)) | year + unitid,
        data = df,
        weights = ~ meansize,
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


plot_scatter <- function(df, x_var) {

    # x_var <- rlang::enquo(x_var)
    x_var <- rlang::sym(x_var)

    df_plot_based <- df |> 
        dplyr::summarise(
            gradrate = mean(gradrate, na.rm = TRUE),
            x_mean = mean(!!x_var, na.rm = TRUE),
            semester = mean(semester, na.rm = TRUE),
            .by = 'unitid'
        )

    plot_1 <- ggplot(df_plot_based, aes(x = x_mean, y = gradrate)) +
        geom_point() +
        theme_bw() +
        geom_smooth(method = 'lm', se = FALSE) +
        theme(
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            axis.title = element_text(size = 16)
        ) +
        labs(
            x = x_var
        )

    # plot_2 <- ggplot(df_plot_based, aes(x = x_mean, y = semester)) +
    #     geom_point() +
    #     theme_bw() +
    #     geom_smooth(method = 'lm', se = FALSE) +
    #     theme(
    #         axis.text.x = element_text(size = 10),
    #         axis.text.y = element_text(size = 10)
    #     ) +
    #     labs(
    #         x = x_var
    #     )

    plot_output <- plot_1

    file_name <- paste0(x_var, '.png')

    ggsave(plot_output, filename = here::here('02.code', 'output', 'scatter', file_name),
           width = 8, height = 6)

    # return(plot_output)
}


list_x_vars  <- c(
    # "year",
    "w_cohortsize",
    "m_cohortsize",
    "instatetuition",
    "costs",
    "faculty",
    "white_cohortsize",
    "per_white_cohort"
)

for (x_var in list_x_vars) {
    print(x_var)
    # plot_scatter(df, 'w_cohortsize', gradrate)
    plot_scatter(df, x_var)
}

plot_scatter(df, instatetuition, gradrate)
plot_scatter(df, instatetuition, gradrate)
plot_scatter(df, , gradrate)
plot_scatter(df, instatetuition, gradrate)


as.matrix(colnames(df))

"year"
"w_cohortsize"
"m_cohortsize"
"instatetuition"
"costs"
"faculty"
"white_cohortsize"
"per_white_cohort"



test_fun <- function(a, b){

    print(class(a))

    # a <- rlang::enquo(a)

    print(class(rlang::sym(a)))
}

test_fun('q')


