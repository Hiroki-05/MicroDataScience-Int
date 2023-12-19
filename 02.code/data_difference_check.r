# setup
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(kableExtra)


# Read Data
df_stata <- readr::read_csv('01.data/stata/test_IPEDS_Analysis_Figure.csv')
df_original <- readr::read_csv('01.data/based_df.csv')



# 分析に使う列のみを抽出
list_cols <- c(
    'unitid',
    'year',
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

df_stata_mod <- df_stata |> 
    dplyr::select(list_cols) |> 
    dplyr::rename(
        semester = semester2,
        quarter = quarter2) |> 
        dplyr::arrange(unitid, year) |> 
        dplyr::mutate(index = row_number())

df_original_mod <- df_original |> 
    dplyr::select(list_cols) |> 
        dplyr::rename(
            semester = semester2,
            quarter = quarter2) |> 
        dplyr::arrange(unitid, year) |> 
        dplyr::mutate(index = row_number())


# 縦に結合し、重複を調べることで同じかどうか確かめる。
df_merged <- df_original_mod |> 
    dplyr::bind_rows(df_stata_mod) %>%
    dplyr::mutate(
        across(where(is.numeric), ~round(., digits = 2))
    ) |> 
    dplyr::arrange(unitid, year) 

df_unitid_index <- df_merged |> 
    dplyr::select(unitid, year, index)

df_check <- df_merged |> 
    dplyr::distinct() |> 
    dplyr::summarise(
        n = dplyr::n(),
        .by = c(unitid, year, index)
    ) 

# stataの処理とRの処理で一致しない項目を精査
list_error <- df_check |> 
    dplyr::filter(n != 1) |> 
    dplyr::distinct(index) |> 
    dplyr::pull()

# 有効数字1桁でroundすると完全一致
df_error <- df_merged |> 
    dplyr::filter(index %in% list_error) %>%  
    dplyr::mutate(across(where(is.numeric), round))

