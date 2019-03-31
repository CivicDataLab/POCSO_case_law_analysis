# Get only cases with Judgements

all_cases_json <- dir("judgements/sample_cases/")
all_cases_id <- stringr::str_replace_all(string = all_cases_json, pattern = "\\.json",replacement = "")
all_cases_judgements <- dir("judgements/sample_judgements/")
all_judgements_id <- stringr::str_replace_all(string = all_cases_judgements, pattern = "\\.pdf",replacement = "")

all_cases_df <- all_cases_id %>% data.frame() %>% setNames(c('case_id'))
all_cases_df$judgement_flag <-  ifelse(all_cases_df$case_id %in% all_judgements_id, 1,0)


## Getting Date of case registration for time series
get_case_details <- function(case_id){
  case_details <- list()
  print(glue::glue('processing {case_id} ... '))  
  case_law <-
      jsonlite::read_json(glue::glue("judgements/sample_cases/{case_id}.json"))
    registration_date <- case_law$dt_regis
    date_of_decision <- case_law$date_of_decision
    filing_date <- case_law$date_of_filing
    case_details$case_id <- case_id
    case_details$registration_date <- ifelse(length(registration_date)>0,registration_date,"")
    case_details$date_of_decision <- ifelse(length(date_of_decision)>0,date_of_decision,"")
    case_details$filing_date <- ifelse(length(filing_date)>0,filing_date,"")
    return(case_details)
}

# sample_case_ids <- all_cases_json[sample(length(all_cases_json),10)]
all_case_details <- lapply(all_cases_id, get_case_details)
all_case_details <- dplyr::bind_rows(all_case_details)  
all_case_details <- left_join(all_case_details, all_cases_df, by='case_id')
write.csv(all_case_details, "judgements/all_details.csv", row.names = FALSE)

all_details <- read_csv("judgements/all_details.csv", 
                        col_types = cols(date_of_decision = col_date(format = "%Y-%m-%d"), 
                        filing_date = col_date(format = "%Y-%m-%d"), 
                        registration_date = col_date(format = "%Y-%m-%d")), 
                        na = "empty")
