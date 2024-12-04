
# Install the packages ----------------------------------------------------

pacman::p_load(
  shiny,
  fontawesome,
  lubridate,
  rio,
  here,
  janitor,
  lubridate,
  highcharter,
  shinyjs,
  waiter,
  sf,
  DT,
  leaflet,
  leaflet.extras,
  officer,
  tidyverse
)

source("rought_burkina.R")

introdate <- ymd("2024-02-05")
days_since_intro <- as.numeric(Sys.Date() - introdate)
last_update <- ymd("2024-11-15")

MONTHS <- c("2024-02-01" = 2, "2024-03-01" = 3, "2024-04-01" = 4, "2024-05-01" = 5, 
            "2024-06-01" = 6, "2024-07-01" = 7, "2024-08-01" = 8, "2024-09-01" = 9,"2024-10-01" = 10)

annual_coverage <- 218625

# Import the dataset ------------------------------------------------------

db_vaccination <- here("data","admin_doses.xlsx") %>% 
                  import() %>% 
                    clean_names() %>% 
                    mutate(total_vap1 = vap1_fixe+vap1_avancee,
                           total_vap2 = vap2_fixe+vap2_avancee,
                           total_vap3 = vap3_fixe+vap3_avancee,
                           orgunitlevel4 = str_remove(orgunitlevel4,"DS"),
                           orgunitlevel4 = str_remove_all(orgunitlevel4," ")
                           )

## Admin doses by region -----------------------------------

db_vaccination_regions <- db_vaccination %>% 
                            select(orgunitlevel2,period_date,contains("vap")) %>% 
                            group_by(
                              orgunitlevel2,period_date) %>% 
                              summarise_all(sum) %>% 
                              ungroup()

db_vaccination_regions_cum <- db_vaccination_regions %>%
  group_by(orgunitlevel2) %>%
  arrange(period_date) %>%
  mutate(across(where(is.numeric), cumsum)) %>%
  ungroup()


## Admin doses by districts ---------------------------------


# Import the target -------------------------------------------------------


db_target <- here("data","target.xlsx") %>%   
               import(skip = 2) %>% 
                clean_names() %>% 
                drop_na(district)

db_target_region <- db_target %>% 
                      select(-district) %>% 
                      group_by(region) %>% 
                      summarise_all(sum) %>% 
                      ungroup()

## Add the target to the administered doses df

db_vaccination_regions_cum <- db_vaccination_regions_cum %>% 
  left_join(db_target_region %>% select(-cibles_mensuelles),
            by = c("orgunitlevel2" = "region"))

db_vaccination_regions_cum <- db_vaccination_regions_cum %>% 
            mutate(
              cumulative_target_this_month = cibles_annuelles/12*(month(period_date)-1),
              cumulative_target_this_month2 = ifelse(cibles_annuelles/12*(month(period_date)-2)<=0,cibles_annuelles/12,cibles_annuelles/12*(month(period_date)-2)),
              cumulative_target_this_month3 = ifelse(cibles_annuelles/12*(month(period_date)-3)<=0,cibles_annuelles/12,cibles_annuelles/12*(month(period_date)-3)) ,
              cumulative_coverage_vap1 = round(total_vap1/cumulative_target_this_month*100,1),
              cumulative_coverage_vap2 = round(total_vap2/cumulative_target_this_month2*100,1),
              cumulative_coverage_vap3 = round(total_vap3/cumulative_target_this_month3*100,1)
            )


### Distrist analysis
#### Cumulative analysis 

db_vaccination_district_cum <- db_vaccination %>%
  group_by(orgunitlevel4) %>%
  arrange(period_date,orgunitlevel4) %>%
  mutate(across(where(is.numeric), cumsum)) %>%
  ungroup()

db_vaccination_district_cum <- db_vaccination_district_cum %>%
                            left_join(
                              db_target %>% select(-cibles_mensuelles),
                              by = c("orgunitlevel4" = "district","orgunitlevel2" = "region"))

db_vaccination_district_cum <- db_vaccination_district_cum %>% 
  mutate(
    cumulative_target_this_month = cibles_annuelles/12*(month(period_date)-1),
    cumulative_target_this_month2 = ifelse(cibles_annuelles/12*(month(period_date)-2)<=0,cibles_annuelles/12,cibles_annuelles/12*(month(period_date)-2)),
    cumulative_target_this_month3 = ifelse(cibles_annuelles/12*(month(period_date)-3)<=0,cibles_annuelles/12,cibles_annuelles/12*(month(period_date)-3)) ,
    cumulative_coverage_vap1 = round(total_vap1/cumulative_target_this_month*100,1),
    cumulative_coverage_vap2 = round(total_vap2/cumulative_target_this_month2*100,1),
    cumulative_coverage_vap3 = round(total_vap3/cumulative_target_this_month3*100,1)
  )

### Monthly analysis

db_vaccination_district <- db_vaccination %>%
  left_join(
    db_target %>% select(-cibles_annuelles),
    by = c("orgunitlevel4" = "district","orgunitlevel2" = "region")
  )

db_vaccination_district <- db_vaccination_district %>% 
  mutate(
    cumulative_target_this_month = cibles_mensuelles,
    cumulative_coverage_vap1 = round(total_vap1/cumulative_target_this_month*100,1),
    cumulative_coverage_vap2 = round(total_vap2/cumulative_target_this_month*100,1),
    cumulative_coverage_vap3 = round(total_vap3/cumulative_target_this_month*100,1)
  )



# Import the shapefiles ---------------------------------------------------

 bkfaso_shp <- sf::read_sf(here("data","shapefiles","burkina.shp")) %>% 
                clean_names() %>% 
                mutate(
                  adm1_altna = str_to_sentence(adm1_altna),
                  adm1_altna = fct_recode(adm1_altna,
                                   "Centre Est" = "Centre-est",
                                   "Centre Ouest" = "Centre-ouest",
                                   "Centre Sud" = "Centre-sud",
                                   "Sud Ouest" = "Sud-ouest")
                )




bkfaso_shp <- bkfaso_shp %>% 
              left_join(
                db_vaccination_regions_cum %>% filter(period_date == max(as.Date(period_date))),
                by = c("adm1_altna" = "orgunitlevel2")) %>% 
                mutate(
                  colors_vap1 = case_when(
                    cumulative_coverage_vap1 < 30 ~ "red",
                    cumulative_coverage_vap1 >= 30 & cumulative_coverage_vap1 < 50  ~ "orange",
                    cumulative_coverage_vap1 >= 50 & cumulative_coverage_vap1 < 80  ~ "yellow",
                    cumulative_coverage_vap1 >= 80 ~ "darkgreen",
                    TRUE ~ "darkgrey"
                  ),
                  colors_vap2 = case_when(
                    cumulative_coverage_vap2 < 30 ~ "red",
                    cumulative_coverage_vap2 >= 30 & cumulative_coverage_vap2 < 50  ~ "orange",
                    cumulative_coverage_vap2 >= 50 & cumulative_coverage_vap2 < 80  ~ "yellow",
                    cumulative_coverage_vap2 >= 80 ~ "darkgreen",
                    TRUE ~ "darkgrey"
                  ),
                  colors_vap3 = case_when(
                    cumulative_coverage_vap3 < 30 ~ "red",
                    cumulative_coverage_vap3 >= 30 & cumulative_coverage_vap3 < 50  ~ "orange",
                    cumulative_coverage_vap3 >= 50 & cumulative_coverage_vap3 < 80  ~ "yellow",
                    cumulative_coverage_vap3 >= 80 ~ "darkgreen",
                    TRUE ~ "darkgrey"
                  )
                )

lat_center <- 12.238333
lon_center <- -1.561593
initial_zoom <- 6

# Summary table

### This month

tab1 <- db_vaccination_regions %>% 
  select(
    orgunitlevel2,period_date,total_vap1,total_vap2,total_vap3
  )

tab1 <- tab1 %>% 
  arrange(orgunitlevel2,period_date) %>% 
  mutate(
    evolution_vap1 = c(NA, diff(total_vap1)),
    evolution_vap2 = c(NA, diff(total_vap2)),
    evolution_vap3 = c(NA, diff(total_vap3))) %>%
  rename(
    admin_doses_vap1_month = total_vap1,
    admin_doses_vap2_month = total_vap2,
    admin_doses_vap3_month = total_vap3,
  )

tab2 <- db_vaccination_regions_cum %>% 
  select(
    orgunitlevel2,period_date,total_vap1,total_vap2,total_vap3,cumulative_coverage_vap1,cumulative_coverage_vap2,cumulative_coverage_vap3) %>%
  rename(
    admin_doses_vap1_cumulative = total_vap1,
    admin_doses_vap2_cumulative = total_vap2,
    admin_doses_vap3_cumulative = total_vap3,
  )

tab1_national <- tab2 %>%
  select(-contains("cumulative_coverage"),orgunitlevel2,period_date) %>% 
  group_by(period_date) %>%
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE))) %>% 
  mutate(orgunitlevel2 = "NATIONAL",
         period_date = as.Date(period_date),
         n_month = month(period_date),
         annual_coverage = annual_coverage,
         target_this_month = annual_coverage/12*(n_month-1),
         target_this_month2 = ifelse(annual_coverage/12*(n_month-2)<=0,annual_coverage/12,annual_coverage/12*(n_month-2)),
         target_this_month3 = ifelse(annual_coverage/12*(n_month-3)<=0,annual_coverage/12,annual_coverage/12*(n_month-3)),
         cumulative_coverage_vap1 = round(admin_doses_vap1_cumulative/target_this_month*100,2),
         cumulative_coverage_vap2 = round(admin_doses_vap2_cumulative/target_this_month2*100,2),
         cumulative_coverage_vap3 = round(admin_doses_vap3_cumulative/target_this_month3*100,2),
  ) %>% ungroup()

tab2_national <- tab1 %>% 
  group_by(period_date) %>%
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE))) %>% 
  mutate(orgunitlevel2 = "NATIONAL") %>% ungroup()


tab_total <- tab1_national %>% 
  left_join(tab2_national,by = c("period_date","orgunitlevel2")) %>% 
  select(orgunitlevel2,period_date,admin_doses_vap1_month,evolution_vap1,admin_doses_vap1_cumulative,cumulative_coverage_vap1,
         admin_doses_vap2_month,evolution_vap2,admin_doses_vap2_cumulative,cumulative_coverage_vap2,
         admin_doses_vap3_month,evolution_vap3,admin_doses_vap3_cumulative,cumulative_coverage_vap3)



tab_join <- tab1 %>% 
  left_join(tab2,by = c("orgunitlevel2","period_date")) %>% 
  select(orgunitlevel2,period_date,admin_doses_vap1_month,evolution_vap1,admin_doses_vap1_cumulative,cumulative_coverage_vap1,
         admin_doses_vap2_month,evolution_vap2,admin_doses_vap2_cumulative,cumulative_coverage_vap2,
         admin_doses_vap3_month,evolution_vap3,admin_doses_vap3_cumulative,cumulative_coverage_vap3)


tab_join_overall <- bind_rows(tab_join,tab_total)


evolution_formatter <- function(x) {
  sapply(x, function(val) {
    if (is.na(val)) return("")
    if (val > 0) sprintf("<span style='color: green'>%d ▲</span>", val)
    else if (val < 0) sprintf("<span style='color: red'>%d ▼</span>", val)
    else sprintf("%d", val)
  })
}


# Apply formatting to data
formatted_data <- tab_join_overall %>%
  mutate(
    evolution_vap1 = evolution_formatter(evolution_vap1),
    evolution_vap2 = evolution_formatter(evolution_vap2),
    evolution_vap3 = evolution_formatter(evolution_vap3)
  )


 








































# Define CSS styles -------------------------------------------------------

css <- '
  /* Global styles */
  body {
    font-family: "Helvetica Neue", Arial, sans-serif;
    background-color: #f0f4f8;
    margin: 0;
    padding: 20px;
  }

  /* Container styling */
  .container-fluid {
    max-width: 1400px;
    margin: 0 auto;
    background: white;
    padding: 20px;
    border-radius: 12px;
    box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
  }

  /* Header styling */
  .header-container {
    display: flex;
    align-items: center;
    gap: 20px;
    padding: 10px;
    border-bottom: 3px solid #2c5282;
    margin-bottom: 20px;
  }

  .header-title {
    margin: 0;
    flex-grow: 1;
  }

  .header-title h2 {
    color: #1a365d;
    margin: 0;
    padding: 0;
    border-bottom: none;
  }

  .header-logos {
    display: flex;
    align-items: center;
    gap: 15px;
  }

  .metric-value {
    font-size: 24px;
    font-weight: bold;
    color: #2c5282;
  }

  .metric-label {
    color: #4a5568;
    font-size: 14px;
    display: flex;
    align-items: center;
    gap: 5px;
  }
  
  /* Tab panels */
  .nav-tabs {
    border-bottom: 2px solid #2c5282;
  }

  .nav-tabs > li > a {
    color: #4a5568;
    border: none;
    background: transparent;
    padding: 10px 20px;
    margin-right: 5px;
  }

  .nav-tabs > li.active > a {
    color: #2c5282;
    font-weight: bold;
    border: none;
    border-bottom: 3px solid #2c5282;
  }

  /* Input styling */
  .form-control {
    border: 2px solid #e2e8f0;
    border-radius: 6px;
    padding: 8px;
    transition: all 0.3s ease;
  }

  .form-control:focus {
    border-color: #2c5282;
    box-shadow: 0 0 0 2px rgba(44, 82, 130, 0.2);
  }

  /* Button styling */
  .btn {
    background-color: #2c5282;
    color: white;
    border: none;
    padding: 10px 20px;
    border-radius: 6px;
    cursor: pointer;
    transition: all 0.3s ease;
  }

  .btn:hover {
    background-color: #1a365d;
    transform: translateY(-1px);
  }

  /* Table styling */
  .dataTables_wrapper {
    padding: 20px;
    background: white;
    border-radius: 8px;
    box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
  }

  /* Plot styling */
  .shiny-plot-output {
    background: white;
    padding: 15px;
    border-radius: 8px;
    box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
  }
  /* Dashboard cards */
  .metric-card {
    background: white;
    padding: 15px;
    border-radius: 8px;
    box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
    margin-bottom: 20px;
    border-left: 4px solid #2c5282;
  }
  
  .metric-card2 {
    background: white;
    padding: 15px;
    border-radius: 8px;
    box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
    margin-bottom: 20px;
    border-left: 4px solid #833C0C;
  }

  .metric-value {
    font-size: 24px;
    font-weight: bold;
    color: #2c5282;
  }
  
  .footer {
  background-color: #2c3e50;
  color: white;
  padding: 40px 0 20px 0;
  margin-top: 50px;
  width: 100%;
}

.footer-content {
  display: flex;
  justify-content: space-around;
  flex-wrap: wrap;
  max-width: 1200px;
  margin: 0 auto;
  padding: 0 20px;
}

.footer-section {
  margin: 20px;
  min-width: 250px;
}

.footer-section h4 {
  color: #ecf0f1;
  margin-bottom: 20px;
  font-size: 1.2em;
}

.footer-section p {
  color: #bdc3c7;
  line-height: 1.6;
}

.footer-section ul {
  list-style: none;
  padding: 0;
}

.footer-section ul li {
  margin-bottom: 10px;
}

.footer-section ul li a {
  color: #bdc3c7;
  text-decoration: none;
  transition: color 0.3s ease;
}

.footer-section ul li a:hover {
  color: #3498db;
}

.footer-bottom {
  text-align: center;
  margin-top: 20px;
  padding-top: 20px;
  border-top: 1px solid #34495e;
}

.footer-bottom p {
  color: #bdc3c7;
  margin: 0;
  font-size: 0.9em;
}

.footer i {
  margin-right: 10px;
  color: #3498db;
}


.footer-link {
    color: #bdc3c7 !important;
    text-decoration: none;
    cursor: pointer;
}

.footer-link:hover {
    color: #3498db !important;
    text-decoration: none;
}

.modal-content {
    border-radius: 8px;
}

.modal-header {
    background-color: #2c3e50;
    color: white;
    border-radius: 8px 8px 0 0;
}

.modal-title {
    color: white;
}

.modal-footer {
    border-top: 1px solid #eee;
}

/* Waiter styling */

.waiter-overlay {
  background: linear-gradient(135deg, #2c3e50 0%, #3498db 100%);
}

.waiter-overlay h3 {
  margin: 20px 0;
  font-weight: 300;
  text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.2);
}

.waiter-overlay p {
  margin: 10px 0;
  font-size: 1.1em;
  text-shadow: 1px 1px 2px rgba(0, 0, 0, 0.2);
}

.waiter-overlay img {
  animation: pulse 2s infinite;
  border-radius: 10px;
  box-shadow: 0 0 20px rgba(255, 255, 255, 0.2);
}

@keyframes pulse {
  0% {
    transform: scale(1);
    opacity: 1;
  }
  50% {
    transform: scale(1.05);
    opacity: 0.8;
  }
  100% {
    transform: scale(1);
    opacity: 1;
  }
}

.header-content {
  flex-grow: 1;
}

.header-title-row {
  display: flex;
  align-items: center;
  justify-content: space-between;
  margin-bottom: 15px;
}

.header-title-row h2 {
  margin: 0;
}

.last-update {
  font-size: 0.9em;
  color: #666;
  font-style: italic;
  margin-left: 20px;
  margin-top: 40px;
}



.map-container {
  margin-bottom: 20px;
}

.map-container .box {
  border-radius: 8px;
  box-shadow: 0 2px 12px rgba(0, 0, 0, 0.1);
}

.leaflet-container {
  border-radius: 4px;
  border: 1px solid #e0e0e0;
}
  
'
