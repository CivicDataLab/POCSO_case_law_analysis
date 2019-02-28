

library(tidyverse)
library(progress)

sample_data_dir <- "datasets/Maharashtra/"


all_files <-
  Sys.glob(glue::glue(("{sample_data_dir}/*/*/*.json")))

pb <- progress::progress_bar$new(total = length(all_files))


case_fir_details <- list()
case_history_list <- 
for (i in 1:length(all_files)) {
  x <- jsonlite::read_json(all_files[[i]])

  cino <- ifelse(is.null(x$cino),"",x$cino)
  ## List of variables from Vivek
  
  fil_year <- ifelse(is.null(x$fil_year),"",as.character(x$fil_year))
  jcode <- ifelse(is.null(x$jcode),"",as.character(x$jcode))
  date_of_decision <- ifelse(is.null(x$date_of_decision),"",as.character(x$date_of_decision))
  reg_year <- ifelse(is.null(x$reg_year),"",as.character(x$reg_year))
  transfer_est_name <- ifelse(is.null(x$transfer_est_name),"",as.character(x$transfer_est_name))
  lower_court_dec_dt <- ifelse(is.null(x$lower_court_dec_dt),"",as.character(x$lower_court_dec_dt))
  fir_year <- ifelse(is.null(x$fir_year),"",as.character(x$fir_year))
  pet_adv <- ifelse(is.null(x$pet_adv),"",as.character(x$pet_adv))
  type_name<- ifelse(is.null(x$type_name),"",as.character(x$type_name))
  res_name <- ifelse(is.null(x$res_name),"",as.character(x$res_name))
  petNameAdd <- ifelse(is.null(x$petNameAdd),"",as.character(x$petNameAdd))
  subordinateCourtInfoStr <- ifelse(is.null(x$subordinateCourtInfoStr),"",as.character(x$subordinateCourtInfoStr))
  lower_court_code <- ifelse(is.null(x$lower_court_code),"",as.character(x$lower_court_code))
  res_adv <- ifelse(is.null(x$res_adv),"",as.character(x$res_adv))
  petparty_name <- ifelse(is.null(x$petparty_name),"",as.character(x$petparty_name))
  purpose_next <- ifelse(is.null(x$purpose_next),"",as.character(x$purpose_next))
  est_code <- ifelse(is.null(x$est_code),"",as.character(x$est_code))
  state_name <- ifelse(is.null(x$state_name),"",as.character(x$state_name))
  date_last_list <- ifelse(is.null(x$date_last_list),"",as.character(x$date_last_list))
  court_no <- ifelse(is.null(x$court_no),"",as.character(x$court_no))
  reg_no <- ifelse(is.null(x$reg_no),"",as.character(x$reg_no))
  dt_regis <- ifelse(is.null(x$dt_regis),"",as.character(x$dt_regis))
  date_of_filing <- ifelse(is.null(x$date_of_filing),"",as.character(x$date_of_filing))
  purpose_name <- ifelse(is.null(x$purpose_name),"",as.character(x$purpose_name))
  fir_no <- ifelse(is.null(x$fir_no),"",as.character(x$fir_no))
  police_st_code <- ifelse(is.null(x$police_st_code),"",as.character(x$police_st_code))
  district_code <- ifelse(is.null(x$district_code),"",as.character(x$district_code))
  courtno <- ifelse(is.null(x$courtno),"",as.character(x$courtno))
  case_no <- ifelse(is.null(x$case_no),"",as.character(x$case_no))
  desgname <- ifelse(is.null(x$desgname),"",as.character(x$desgname))
  pet_name <- ifelse(is.null(x$pet_name),"",as.character(x$pet_name))
  resparty_name <- ifelse(is.null(x$resparty_name),"",as.character(x$resparty_name))
  fir_details <- ifelse(is.null(x$fir_details),"",as.character(x$fir_details))
  court_name <- ifelse(is.null(x$court_name),"",as.character(x$court_name))
  district_name<- ifelse(is.null(x$district_name),"",as.character(x$district_name))

  case_fir_details[[i]] <-
    data.frame(
      fil_year,
      jcode,
      date_of_decision,
      reg_year,
      transfer_est_name,
      lower_court_dec_dt,
      fir_year,
      pet_adv,
      type_name,
      res_name,
      petNameAdd,
      subordinateCourtInfoStr,
      lower_court_code,
      res_adv,
      petparty_name,
      purpose_next,
      est_code,
      state_name,
      date_last_list,
      court_no,
      reg_no,
      dt_regis,
      date_of_filing,
      purpose_name,
      fir_no,
      police_st_code,
      district_code,
      courtno,
      case_no,
      desgname,
      pet_name,
      resparty_name,
      fir_details,
      court_name,
      district_name,stringsAsFactors = FALSE, check.names = FALSE
    )
  
  ## Processing hearing details from cases
  
  case_history <- ifelse(is.null(x$historyOfCaseHearing),"",x$historyOfCaseHearing)
  if(case_history != ""){
   case_history_table <- case_history %>% read_html() %>% html_table() 
   case_history_table <- case_history_table[[1]]
   case_history_table$cino <- cino
  } else {
    case_history_table <- data.frame("cino"=cino,"Judge"="", "Business On Date" = "", "Hearing Date" = "",check.names=FALSE)
  }
  
  case_history_list[[i]] <- case_history_table
  
  # View progress
  pb$tick()
}

# Transforming case meta variables
case_fir_df <- dplyr::bind_rows(case_fir_details)
case_fir_df$police_station <- stringr::str_replace_all(case_fir_df$fir_details, pattern = "\\^", replacement = "")
case_fir_df$police_station <- stringr::str_replace_all(case_fir_df$police_station, pattern = "[:digit:]+", replacement = "")

#Transforming case history variables
case_history_df <- dplyr::bind_rows(case_history_list)

# Exporting datasets
data.table::fwrite(case_fir_df, "datasets/Maharashtra/processed/mh_pocso_01.csv")
data.table::fwrite(case_history_df, "datasets/Maharashtra/processed/mh_pocso_case_history_02.csv")








# To get the co-ordinates

get_coordinates <- function(police_station_name) {
  police_station_name <- paste0(police_station_name," India")
  police_station_name <- stringr::str_replace_all(police_station_name,pattern = " ",replacement = "%20")
  geocode_url <- glue::glue('https://api.opencagedata.com/geocode/v1/json?q={police_station_name}%2C%20India&key=22a580c8eaf74006ae634edfdd4d514a&language=en&pretty=1&no_annotations=1')
  geocode_request <- httr::VERB(verb = 'GET',geocode_url)
  geocode_response <- httr::content(geocode_request)
  
}
