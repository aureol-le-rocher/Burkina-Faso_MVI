
# Tidy the data frame -----------------------------------------------------


 raw_db <- here("data","raw_data.xlsx") %>% 
            import(which = "Disopsition2") %>% 
             clean_names()
 
 
 
 raw_db_pivot <- raw_db %>% 
                  pivot_longer(
                    cols = 6:59,
                    values_to = "count",
                    names_to = "variable") %>% 
                    mutate(
                      period = str_extract(variable,
                                           "fevrier|mars|avril|mai|juin|juillet|aout|septembre|octobre"                    
                      ),
                      period_date = case_when(
                        period ==  "fevrier" ~ "01-02-2024",
                        period ==  "mars" ~ "01-03-2024",
                        period ==  "avril" ~ "01-04-2024",
                        period ==  "mai" ~ "01-05-2024",
                        period ==  "juin" ~ "01-06-2024",
                        period ==  "juillet" ~ "01-07-2024",
                        period ==  "aout" ~ "01-08-2024",
                        period ==  "septembre" ~ "01-09-2024",
                        period ==  "octobre" ~ "01-10-2024",
                        TRUE ~ NA_character_
                      ),
                      period_date = dmy(period_date),
                      vaccine_id = str_extract(variable,
                              "rts_s_1_avancee|rts_s_1_fixe|rts_s_2_avancee|rts_s_2_fixe|rts_s_3_avancee|rts_s_3_fixe"                 
                              ),
                      vaccine_id = case_when(
                        vaccine_id == "rts_s_1_avancee" ~ "VAP1_avancee",
                        vaccine_id == "rts_s_1_fixe" ~ "VAP1_fixe",
                        vaccine_id == "rts_s_2_avancee" ~ "VAP2_avancee",
                        vaccine_id == "rts_s_2_fixe" ~ "VAP2_fixe",
                        vaccine_id == "rts_s_3_avancee" ~ "VAP3_avancee",
                        vaccine_id == "rts_s_3_fixe" ~ "VAP3_fixe")) %>%
                        select(-c(organisationunitname,variable,period)) %>% 
                        pivot_wider(
                          names_from = vaccine_id,
                          values_from = count) %>%
                       mutate_if(is.numeric,~replace(.,is.na(.),0))


#export(raw_db_pivot,file = here("data","admin_doses.xlsx"))
















