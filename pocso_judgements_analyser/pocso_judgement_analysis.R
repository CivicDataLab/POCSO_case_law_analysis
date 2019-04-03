
library(pdftools)
library(tidytext)
library(dplyr)
library(ggplot2)

#For NLP - https://github.com/quanteda/spacyr
library(spacyr)
spacyr::spacy_initialize()

# For language detection
library(cld2)

pdf_path <- "judgements/sample_judgements/"
list_all_judgements <- dir(pdf_path)

judgement_link_db <- data.frame('judgement_link' = list_all_judgements, document = 1:length(list_all_judgements),judgement_year = stringr::str_sub(list_all_judgements,-8,-5))

title_pattern_1 <- 'POCSO Spl.Case No.'
title_pattern_2 <- 'Pocso Spl.case No.'
title_pattern_3 <- 'POCSO SPECIAL CASE No.'

combine_all_judgements <- function(pdf_name){
  judgement <- list()
  print(glue::glue("Processing judgement -> {pdf_name} ... "))
  judgement_text <- pdftools::pdf_text(paste0(pdf_path, pdf_name))
  judgement_text <- sub("[^_]+_([A-Za-z]+)$", "_\\1", judgement_text)
  judgement$id <- stringr::str_replace_all(pdf_name,pattern = "\\.pdf",replacement = "")
  
  #Cleaning the text
  # if(length(judgement_text) > 1){
  #   para_to_check_title <- judgement_text[[2]]
  #   # title_pos <- stringr::str_locate(para_to_check_title,pattern = "\\n")[[1]]
  #   # title_pattern <- stringr::str_sub(para_to_check_title,start = 1, end = title_pos-1) %>% stringr::str_trim()
  #   # title_pattern <- stringr::str_replace_all(title_pattern,pattern = "\\/[:digit:] \\/",replacement = "") %>% stringr::str_trim()
  #   # cnr_number <- stringr::str_extract(judgement_text[[1]],pattern = "CNR NO\\.[A-Z 0-9]+")    
  # }
  
  read_all_paras <- paste0(judgement_text,collapse = " ")
  # remove title from all places
  read_all_paras <- stringr::str_replace_all(string = read_all_paras,pattern = title_pattern_1, replacement = '') %>% stringr::str_squish()
  read_all_paras <- stringr::str_replace_all(string = read_all_paras,pattern = title_pattern_2, replacement = '') %>% stringr::str_squish()
  read_all_paras <- stringr::str_replace_all(string = read_all_paras,pattern = title_pattern_3, replacement = '') %>% stringr::str_squish()
  judgement$text <- read_all_paras
  judgement_language <- cld2::detect_language(read_all_paras)
  
  if(!is.na(judgement_language)){
    judgement$language <- judgement_language
    parsing_age <- grep("victim", unlist(strsplit(read_all_paras, split = "\\.")), value=TRUE)
    if(length(parsing_age) > 0){
      # remoming pocso details
      pocso_age_strings <- grep('below age of 18 years',x = parsing_age) %>% unlist()   
      
      parsing_age <- parsing_age[!parsing_age %in% parsing_age[pocso_age_strings]]
      all_age_strings <- stringr::str_extract_all('[:digit:]+ years',string = parsing_age) %>% unlist()   
      if(length(all_age_strings)>0){
        victim_age <- all_age_strings[[1]] %>% readr::parse_number()  
      } else {
        victim_age <- 0
      }
    } else {
      victim_age <- 0
    }
    
    judgement$victim_age <- victim_age
    judgement$victim_age_bucket <-
      ifelse(victim_age == 0,
             NA_character_ ,
             ifelse(
               victim_age >= 1 & victim_age < 5,
               '< 5',
               ifelse(
                 victim_age >= 5  & victim_age < 10,
                 '5-10',
                 ifelse(
                   victim_age >= 10  & victim_age < 15,
                   '10-15',
                   ifelse(victim_age >= 15  & victim_age <= 18, '15-18', '>18')
                 )
               )
             ))
    
    #### Get Order details from judgements
    parsed_order <- spacyr::spacy_parse(read_all_paras, nounphrase=TRUE)
    # Get the last instance or order - get statement ID
    order_statement_id <- max(parsed_order$sentence_id[stringr::str_to_lower(parsed_order$token) == 'order'])
    
    # Some judgements have an 'Order' keyword within the order such as:
    # Copy of the order be sent to the Superintendent,Central Jail 
    # Taking the previous instance of order in such cases
    
    if(c('copy') %in% stringr::str_to_lower(parsed_order$token)){
      order_statement_id <- parsed_order$sentence_id[stringr::str_to_lower(parsed_order$token) == 'order']  
      order_statement_id <- order_statement_id[length(order_statement_id) - 1]
    } 
    #Get the last statement id
    last_statement_id <- max(parsed_order$sentence_id)
    # Get all tokens between the two ID's
    all_order_tokens <- parsed_order[parsed_order$sentence_id >= order_statement_id & parsed_order$sentence_id <= last_statement_id,]
    convicted_count <- length(all_order_tokens$token_id[stringr::str_to_lower(all_order_tokens$token) == 'convicted'])
    acquitted_count <- length(all_order_tokens$token_id[stringr::str_to_lower(all_order_tokens$token) == 'acquitted'])
    order_verb_adj <- unique(all_order_tokens$token[all_order_tokens$pos %in% c('VERB','ADJ')])
    judgement_verb_adj <- unique(parsed_order$token[all_order_tokens$pos %in% c('VERB','ADJ')])
    judgement$convicted_count <- convicted_count
    judgement$acquitted_count <- acquitted_count
    judgement$order_verb_adj <- paste0(order_verb_adj,collapse = ",")
    judgement$judgement_verb_adj <- paste0(judgement_verb_adj,collapse = ",")
  } else {
    judgement$language <- NA_character_
    judgement$victim_age <- NA_character_
    judgement$victim_age_bucket <- NA_character_
    judgement$convicted_count <- NA_character_
    judgement$acquitted_count <- NA_character_
    judgement$order_verb_adj <- NA_character_
    judgement$judgement_verb_adj <- NA_character_
  }
  return(judgement)
}

# all_judgements <- lapply(list_all_judgements[sample(length(list_all_judgements),20)], combine_all_judgements)
all_judgements <- lapply(list_all_judgements, combine_all_judgements)
# check language of judgements

# all_language <- purrr::map(all_judgements,"language") %>% unlist()
# not_in_en <- which(all_language!= "en")

# jsonlite::write_json(jsonlite::toJSON(all_judgements,auto_unbox = TRUE),"judgements/judgement_corpus.json")

all_judgements_df <- dplyr::bind_rows(all_judgements)
all_judgements_df <- all_judgements_df %>% mutate(document = row_number())
all_judgements_df <- all_judgements_df[all_judgements_df$language == "en",]
all_judgements_df <- left_join(all_judgements_df, judgement_link_db[,c('document', 'judgement_year')])
# data.table::fwrite(all_judgements_df, "pocso_judgements_analyser/processed_judgements.csv", row.names = FALSE)

# Document wise
doc_words <- all_judgements_df %>% 
  unnest_tokens(input = text, output = word) %>% 
  count(document, word,sort = TRUE)
doc_words <- left_join(doc_words, judgement_link_db, by=NULL)


# Replicaiting analysis from https://www.tidytextmining.com/tfidf.html

all_words <- all_judgements_df %>% 
  unnest_tokens(input = text, output = word) %>% 
  count(judgement_year, word,sort = TRUE)

year_words <- all_words %>% 
  group_by(judgement_year) %>% 
  summarise(total = sum(n))

year_wise_judgements <- data.frame(table(all_judgements_df$judgement_year))
names(year_wise_judgements)[] <- c('judgement_year', 'total_judgements')

year_words <- year_words[!is.na(year_words$judgement_year),]

all_words_year <- left_join(all_words, year_words) %>% 
  left_join(year_wise_judgements) %>% 
  anti_join(get_stopwords())

all_words_year$num_remove <- as.numeric(all_words_year$word) + 0
all_words_year <- all_words_year[is.na(all_words_year$num_remove),-6]
all_words_year <- all_words_year[!grepl(all_words_year$word,pattern = "[0-9]+"),]


# Removing cnr related words
all_words_year$cnr_flag <- 0
all_words_year$cnr_flag[grepl(all_words_year$word,pattern = 'cnr*',ignore.case = TRUE)] <- 1
all_words_year <- all_words_year[all_words_year$cnr_flag == 0,]
all_words_year$cnr_flag <- NULL

all_words_year <- all_words_year %>%
  bind_tf_idf(word, judgement_year, n)

all_words_year %>%
  select(-total) %>%
  arrange(desc(tf_idf))

all_words_year <- all_words_year[!is.na(all_words_year$n),]

top_15_year <- all_words_year %>%
  arrange(desc(n)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(judgement_year) %>% 
  top_n(15) %>% ungroup()


ggplot(top_15_year, aes(word, tf_idf,fill=judgement_year)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf_idf") + coord_flip() +
  facet_wrap(~judgement_year, ncol = 3, scales = "free")
  
## QuantEDA - 

















# years <- seq(2014,2019)
# all_words_document <- all_words_document[!is.na(all_words_document$word),]
# words <- all_words_document$word[all_words_document$judgement_year == years[1]] 
# freq <- all_words_document$n[all_words_document$judgement_year == years[1]] 
# 
# 
# wordcloud::wordcloud(words,freq,min.freq = 1,max.words = 200,scale = c(3,  0.3), random.order = FALSE)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# combine_text <- stringr::str_split(judgement_text,pattern = "\n")
# 
# case_df <- combine_text %>% unlist() %>% data.frame(check.names = TRUE) %>% setNames('case_text')
# # x %>% unnest_tokens(input = x1,output = word)
# tidy_case <- case_df %>%
#   unnest_tokens(word, case_text) %>%
#   group_by(word) %>%
#   filter(n() > 10) %>%
#   ungroup()
# 
# tidy_case %>%
#   count(word, sort = TRUE) %>%
#   anti_join(get_stopwords()) %>%
#   top_n(50) %>%
#   ungroup() %>%
#   ggplot(aes(reorder_within(word, n, title), n,
#              fill = title
#   ))