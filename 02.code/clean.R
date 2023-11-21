main <- function() {
  
  df_main <- readr::read_csv(here::here('01.data', 'based_df.csv'))
  df_raw <- haven::read_dta(here::here('01.data', 'ipeds_cleaned_final.dta'))

  # write.csv(df_raw, here::here('01.data', 'ipeds_cleaned_final.csv'))
  
  df_sq_dummy <- subset_sq_dummy(df_main)
  divide_df(df_sq_dummy, data_category = 'dummy')
  
  df_grad_rate <- subset_grad_rate(df_main, df_raw)
  divide_df(df_grad_rate, data_category = 'outcome')
  
  df_covariates <- subset_covariates(df_raw)

}

# ?f?[?^????? -----------------------------------------------------------
# semester??quarter??_?~?[?f?[?^?Z?b?g
subset_sq_dummy <- function(df_main) {
  
 df_output <- df_main |> 
    dplyr::select(
      unitid, 
      instnm,
      semester2,
      quarter2, # trimester??quater?????????quater2
      year
    ) |> 
    dplyr::rename(
      semester = semester2,
      quarter = quarter2
    ) |> 
    dplyr::mutate(
      across(everything(), as.character)
    ) |> 
    mutate(Y = year)
  
  list_col <- colnames(df_output)
  
  df_empty <- tidyr::tibble(
    "unitid" = "unitid",
    "instnm" = "instnm",
    "semester" = "semester",
    "quarter" = "quarter",
    "year" = "year",
    "Y" = "Y",
  )

  df_output <- df_empty |> 
    dplyr::bind_rows(df_output)
  
  colnames(df_output) <- c('x1', 'x2', 'x3', 'x4', 'x5', 'x6')

  # write.csv(df_output, file = here::here('01.data', 'problem1_dataset', 'dummy', 'semeter_dummy_data.csv'))
  
  return(df_output)
  
}

# semester or quarter??_?~?[?f?[?^?Z?b?g?????
divide_df <- function(df_input, data_category) {

  if (data_category == 'outcome'){

    list_year <- df_input |> 
      dplyr::distinct(year) |> 
      # dplyr::filter(x5 != 'year') |> 
      pull()

    # i = list_year[1]

    for(i in list_year) {

      df_output <- df_input |> 
        dplyr::filter(year == i)

      openxlsx::write.xlsx(df_output, file = here::here('01.data', 'problem1_dataset', 'outcome', paste(i, '.xlsx')))

    }
  } else if (data_category == 'dummy'){
    list_unitid <- df_input |> 
      dplyr::distinct(x1) |> 
      dplyr::slice_head(n = 350) |> 
      pull()
    
    df_1 <- df_sq_dummy |> 
      dplyr::filter(x1 %in% list_unitid)
    
    write.csv(df_1, here::here('01.data', 'problem1_dataset', 'semester_dummy', 'semester_data_1.csv'),
    row.names = FALSE)
    
    df_2 <- df_sq_dummy |> 
      dplyr::filter(!x1 %in% list_unitid)
    
    write.csv(df_2, here::here('01.data', 'problem1_dataset', 'semester_dummy', 'semester_data_2.csv'),
    row.names = FALSE)

  }

}


# 4?N????????A6?N????????????v?Z????f?[?^?Z?b?g
subset_grad_rate <- function(df_main, df_raw) {
  
  df_na <- df_raw |> 
    dplyr::select(
      unitid,
      year,
      totcohortsize,
      w_cohortsize,
      m_cohortsize,
      tot4yrgrads,
      m_4yrgrads,
      w_4yrgrads
    ) |> 
    dplyr::filter(year > 2010)
  

  df_output <- df_main |> 
    dplyr::select(
      unitid,
      year,
      totcohortsize,
      w_cohortsize,
      m_cohortsize,
      tot4yrgrads,
      m_4yrgrads,
      w_4yrgrads,
      # tot6yrgrads,
      # m_6yrgrads,
      # w_6yrgrads,
      # gradrate4yr,
      # gradrate6yr,
      # mengradrate4yr,
      # mengradrate6yr,
      # womengradrate4yr,
      # womengradrate6yr
    ) |> 
    dplyr::bind_rows(df_na) |> 
    dplyr::mutate(
      # grad_rate_4yr = (tot4yrgrads/totcohortsize),
      # men_gradrate_4yr =(m_4yrgrads/m_cohortsize), # mengradrate4yr??????l
      women_gradrate_4yr = round((w_4yrgrads/w_cohortsize)*100, digits = 2) # womangradrate4yr??????l
    ) |> 
    dplyr::mutate(
      across(c(totcohortsize, m_4yrgrads), as.character)
    )
  
  openxlsx::write.xlsx(df_output, file = here::here('01.data', 'problem1_dataset', 'gradrate.xlsx'))
  
  return(df_output)
}


# ?????f?[?^?Z?b?g
subset_covariates <- function(df_raw) {
  
  df_output <- df_raw |> 
    dplyr::select(
      unitid, 
      year, 
      instatetuition,
      costs,
      faculty,
      white_cohortsize
    ) |> 
    dplyr::mutate(
      unitid = paste0(as.character(unitid), 'aaaa') 
    ) |> 
    dplyr::rename(university_id = unitid)

  df_output <- df_output |> 
    dplyr::mutate(across(everything(), as.character)) |> 
    tidyr::pivot_longer(
      cols = -c('university_id', 'year'),
      names_to = 'category',
      values_to = 'value'
    )
  
  openxlsx::write.xlsx(df_output, file = here::here('01.data', 'problem1_dataset', 'covariates', 'covariates.xlsx'))

  return(df_output)
}




