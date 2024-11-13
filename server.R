
# Server ------------------------------------------------------------------

server <- function(input,output,session){
  
# Initialize waiter
  
  w <- Waiter$new(
    html = tagList(
      tags$img(src = "logo2.png", height = "100px"),
      h3("Mise à jour des données...", style = "color: white;"),
      spin_flower(),
      p("Veuillez patienter", style = "color: white;")
    ),
    color = "#2c3e50"
  )  
  
  # Simulate loading time (remove this in production)
  #Sys.sleep(2)
  waiter_hide()
  
#  observeEvent(input$panel_tab, {
#    w$show()
#    Sys.sleep(1) # Simulate loading time (remove in production)
#    w$hide()
#  })
  
  output$last_update <- renderText({
    format(last_update, "%d %B %Y")
  })
  
 # Value box -------------------------------------
   
  output$introduction_date <- renderText({
    paste0(format(introdate,format = "%d %B %Y"))
  })
  
  output$days_intro <- renderText({
    paste0(days_since_intro)
  })
  
  output$total_districts <- renderText({
    paste0(27)
  })
  
  output$districts_one_reports <- renderText({
    paste0(100,"%")
  })
  
  # Use prettynum
  
  db_valuebox <- reactive({
    
    db_vaccination_regions_cum %>% 
      select(-orgunitlevel2,-contains("coverage")) %>% 
      group_by(period_date) %>% 
      summarise_all(sum) %>% 
      ungroup() %>% 
      mutate(
        cumulative_coverage_vap1 = round(total_vap1/cumulative_target_this_month*100,1),
        cumulative_coverage_vap2 = round(total_vap2/cumulative_target_this_month*100,1),
        cumulative_coverage_vap3 = round(total_vap3/cumulative_target_this_month*100,1),
        orgunitlevel2 = "National",period_date = as.Date(period_date)) %>% filter(period_date == max(period_date))
    
  })
  
  output$coverage_vap1 <- renderText({
    
  data <- db_valuebox()  
  
  cvrg_vap1 <- data %>% 
                pull(cumulative_coverage_vap1) %>% 
                as.numeric()
  
  adm_vap1 <- data %>% 
    pull(total_vap1) %>% 
    as.numeric()
    
    
   paste0(stringr::str_glue("{prettyNum(adm_vap1)} ({prettyNum(cvrg_vap1)}%)"))
    
  })
  
  output$coverage_vap2 <- renderText({
    
    data <- db_valuebox()  
    
    cvrg_vap2 <- data %>% 
      pull(cumulative_coverage_vap2) %>% 
      as.numeric()
    
    adm_vap2 <- data %>% 
      pull(total_vap2) %>% 
      as.numeric()
    
    
    paste0(stringr::str_glue("{prettyNum(adm_vap2)} ({prettyNum(cvrg_vap2)}%)"))
    
  })
  
  output$coverage_vap3 <- renderText({
    
    data <- db_valuebox()  
    
    cvrg_vap3 <- data %>% 
      pull(cumulative_coverage_vap3) %>% 
      as.numeric()
    
    adm_vap3 <- data %>% 
      pull(total_vap3) %>% 
      as.numeric()
    
    
    paste0(stringr::str_glue("{prettyNum(adm_vap3)} ({prettyNum(cvrg_vap3)}%)"))
    
  })
  
  output$aefi_total <- renderText({
    
    data <- db_valuebox()  
    
    adm_vap1 <- data %>% 
      pull(total_vap1) %>% 
      as.numeric()
    
    adm_vap2 <- data %>% 
      pull(total_vap2) %>% 
      as.numeric()
    
    adm_vap3 <- data %>% 
      pull(total_vap3) %>% 
      as.numeric()
    
    adm_total <- adm_vap1 + adm_vap2 + adm_vap3
    
    paste0(stringr::str_glue("{prettyNum(adm_total)}"))
    
  }) 
  
  # National level panel -------------------------------
  
  is_valid_period <- reactive({
    MONTHS[input$startMonth] <= MONTHS[input$endMonth]
  })
  
  ##District level panel
  
  is_valid_period1 <- reactive({
    MONTHS[input$startMonth1] <= MONTHS[input$endMonth1]
  })
  
  # Update endMonth choices based on startMonth
  
  observe({
    start_month_num <- MONTHS[input$startMonth]
    valid_end_months <- names(MONTHS)[MONTHS >= start_month_num]
    
    updateSelectInput(session, "endMonth",
                      choices = valid_end_months,
                      selected = if(input$endMonth %in% valid_end_months) 
                        input$endMonth 
                      else valid_end_months[1])
  })
  
  
  observe({
    start_month_num1 <- MONTHS[input$startMonth1]
    valid_end_months1 <- names(MONTHS)[MONTHS >= start_month_num1]
    
    updateSelectInput(session, "endMonth1",
                      choices = valid_end_months1,
                      selected = if(input$endMonth1 %in% valid_end_months1) 
                        input$endMonth1 
                      else max(valid_end_months1))
  })
  
  
  observe({
    
    districts_choices <- db_vaccination %>% 
                          filter(orgunitlevel2 == input$choose_region1) %>% 
                          pull(orgunitlevel4) %>% unique()
    
    updateSelectInput(session,"choose_district",
                      "Choisir le district",
                      choices = districts_choices
                      )
    
  })
  
  # Display validation message
  output$validationMessage <- renderText({
    if (!is_valid_period()) {
      return("La période sélectionnée n'est pas valide. Le mois de fin doit être après le mois de début.")
    }
    return("")
  })
  
  output$validationMessage1 <- renderText({
    if (!is_valid_period1()) {
      return("La période sélectionnée n'est pas valide. Le mois de fin doit être après le mois de début.")
    }
    return("")
  })
  
  
  output$selectedPeriod <- renderText({
    if (is_valid_period()) {
      paste("Période sélectionnée: ", format(ymd(input$startMonth),format = "%B-%Y"), 
            "a",format(ymd(input$endMonth),format = "%B-%Y"))
    } else {
      "Veuillez sélectionner une période valide"
    }
  })
  
  output$selectedPeriod1 <- renderText({
    if (is_valid_period1()) {
      paste("Période sélectionnée: ", format(ymd(input$startMonth1),format = "%B-%Y"), 
            "a",format(ymd(input$endMonth1),format = "%B-%Y"))
    } else {
      "Veuillez sélectionner une période valide"
    }
  })
  
## Filtered data
  
### Coverage by months
  
  filtered_data_region <- reactive({
    
      db_vaccination_regions_monthly <- db_vaccination_regions %>% 
        left_join(db_target_region %>% select(-cibles_annuelles),
                  by = c("orgunitlevel2" = "region")) %>% ungroup()
      
      db_vaccination_regions_monthly <- db_vaccination_regions_monthly %>% 
        mutate(
          cumulative_coverage_vap1 = round(total_vap1/cibles_mensuelles*100,1),
          cumulative_coverage_vap2 = round(total_vap2/cibles_mensuelles*100,1),
          cumulative_coverage_vap3 = round(total_vap3/cibles_mensuelles*100,1))%>%
        filter(orgunitlevel2 == input$choose_region,period_date >= ymd(input$startMonth),period_date <= ymd(input$endMonth))%>% 
        mutate(
          period_date = as.Date(period_date)
        )
    
  })
  
  ### Coverage by months cumulative 
  
  filtered_data_region_cum <- reactive({
    
    db_vaccination_regions_cum %>% 
        filter(orgunitlevel2 == input$choose_region,period_date >= ymd(input$startMonth),period_date <= ymd(input$endMonth)) %>% 
      mutate(
        period_date = as.Date(period_date)
      )
    
  })

  ### Coverage by month
  
  output$coverage_trend <- renderHighchart({
    
    highchart() %>%
      hc_chart(
        style = list(fontFamily = "Inter, Arial, sans-serif"), 
        backgroundColor = "#FFFFFF", 
        crosshair = list(
          color = 'rgba(48, 48, 48, 0.8)', # Semi-transparent crosshair
          dashStyle = 'Dash',
          width = 1,
          zIndex = 5
        ),
        animation = list(duration = 1000) # Smooth initial animation
      ) %>%
      hc_title(
        text = str_glue("Couverture vaccinale cumulée VAP dans le temps - {ifelse(input$choose_region == 'Toutes les régions','Niveau National',paste('Région','',input$choose_region))}"),
        style = list(
          fontSize = "24px", 
          fontWeight = "600",
          color = "#1A237E"  # Darker blue for better contrast
        ),
        align = "left"  # Left-aligned title for modern look
      ) %>%
      hc_subtitle(
        text = "Présentation des tendances de couverture VAP1, VAP2, et VAP3",
        style = list(
          fontSize = "16px",
          color = "#546E7A"  # Subtle gray for subtitle
        ),
        align = "left"
      ) %>%
      hc_xAxis(
        categories = filtered_data_region_cum()$period_date,
        title = list(
          text = "Mois",
          style = list(fontWeight = "500")
        ),
        gridLineWidth = 0.5,
        gridLineColor = "rgba(224, 224, 224, 0.5)",  # Lighter grid
        labels = list(
          style = list(fontSize = "12px", color = "#37474F"),
          rotation = -45,
          useHTML = TRUE
        ),
        crosshair = TRUE,
        tickmarkPlacement = "on"
      ) %>%
      hc_yAxis(
        title = list(
          text = "Couverture cumulée (%)",
          style = list(fontWeight = "500")
        ),
        gridLineWidth = 0.5,
        gridLineColor = "rgba(224, 224, 224, 0.5)",
        labels = list(
          format = "{value}%",
          style = list(color = "#37474F")
        ),
        min = 0,
        max = 150,
        tickInterval = 25  # Consistent tick intervals
      ) %>%
      hc_add_series(
        name = "Couverture VAP1",
        data = filtered_data_region_cum()$cumulative_coverage_vap1,
        type = "line",
        color = "#1976D2",  # More muted blue
        lineWidth = 2.5
      ) %>%
      hc_add_series(
        name = "Couverture VAP2",
        data = filtered_data_region_cum()$cumulative_coverage_vap2,
        type = "line",
        color = "#388E3C",  # More muted green
        lineWidth = 2.5
      ) %>%
      hc_add_series(
        name = "Couverture VAP3",
        data = filtered_data_region_cum()$cumulative_coverage_vap3,
        type = "line",
        color = "#FFA000",  
        lineWidth = 2.5
      ) %>%
      hc_tooltip(
        shared = TRUE,
        valueSuffix = "%",
        backgroundColor = "rgba(255, 255, 255, 0.98)",
        borderWidth = 0,
        borderRadius = 12,
        shadow = list(
          color= 'rgba(0, 0, 0, 0.1)',
          offsetX= 0,
          offsetY= 2,
          blur= 5
        ),
        style = list(fontSize = "14px"),
        headerFormat = '<div style="font-size: 14px; font-weight: 600; padding: 12px 12px 8px; min-width: 300px;">{point.x}</div>',
        pointFormat = paste0(
          '<div style="padding: 4px 12px; min-width: 100px;">',
          '<span style="color:{series.color}; font-size: 16px;">\u25CF </span>',
          '<span style="color:#263238; font-weight: 500;">{series.name}</span>: ',
          '<span style="color:#263238; font-weight: 600; float: right;">{point.y:.1f}%</span>',
          '</div>'
        ),
        useHTML = TRUE,
        followPointer = TRUE,
        animation = list(duration = 200)
      )%>%
      hc_plotOptions(
        line = list(
          marker = list(
            enabled = FALSE,
            symbol = "circle",
            radius = 4,
            states = list(
              hover = list(
                enabled = TRUE,
                lineWidth = 2,
                lineColor = "white",
                fillColor = "white",
                radius = 6
              )
            )
          ),
          states = list(
            hover = list(
              lineWidthPlus = 1,  
              halo = list(size =  0)  
            )
          )
        )
      ) %>%
      hc_legend(
        align = "center",
        verticalAlign = "bottom",
        layout = "horizontal",
        itemStyle = list(
          fontSize = "13px",
          fontWeight = "500",
          color = "#263238"
        ),
        itemHoverStyle = list(color = "#1A237E"),  # Darker on hover
        borderWidth = 0,
        backgroundColor = "rgba(255, 255, 255, 0.9)",
        padding = 16,
        symbolRadius = 2  # Squared legend markers
      ) %>%
      hc_credits(enabled = FALSE) %>%
      hc_exporting(
        enabled = TRUE,
        buttons = list(
          contextButton = list(
            menuItems = c("downloadPNG", "downloadJPEG", "downloadPDF", "downloadCSV"),
            symbol = 'menuball',  
            symbolStroke = '#263238'
          )
        ),
        chartOptions = list(
          plotOptions = list(
            series = list(
              animation = FALSE  # Disable animation in exported charts
            )
          )
        )
      ) %>%
      hc_boost(
        enabled = TRUE,
        useGPUTranslations = TRUE,
        usePreallocated = TRUE
      )
    
    
    
  })
  
  output$monthly_coverage <- renderHighchart({
    
    highchart() %>%
      hc_chart(
        style = list(fontFamily = "Inter, Arial, sans-serif"), 
        backgroundColor = "#FFFFFF", 
        crosshair = list(
          color = 'rgba(48, 48, 48, 0.8)', # Semi-transparent crosshair
          dashStyle = 'Dash',
          width = 1,
          zIndex = 5
        ),
        animation = list(duration = 1000) # Smooth initial animation
      ) %>%
      hc_title(
        text = str_glue("Couverture vaccinale mensuelle VAP - {ifelse(input$choose_region == 'Toutes les régions','Niveau National',paste('Région','',input$choose_region))}"),
        style = list(
          fontSize = "24px", 
          fontWeight = "600",
          color = "#1A237E"  # Darker blue for better contrast
        ),
        align = "left"  # Left-aligned title for modern look
      ) %>%
      hc_subtitle(
        text = "Présentation des tendances de couverture VAP1, VAP2, et VAP3",
        style = list(
          fontSize = "16px",
          color = "#546E7A"  # Subtle gray for subtitle
        ),
        align = "left"
      ) %>%
      hc_xAxis(
        categories = filtered_data_region()$period_date,
        title = list(
          text = "Mois",
          style = list(fontWeight = "500")
        ),
        gridLineWidth = 0.5,
        gridLineColor = "rgba(224, 224, 224, 0.5)",  # Lighter grid
        labels = list(
          style = list(fontSize = "12px", color = "#37474F"),
          rotation = -45,
          useHTML = TRUE
        ),
        crosshair = TRUE,
        tickmarkPlacement = "on"
      ) %>%
      hc_yAxis(
        title = list(
          text = "Couverture mensuelle (%)",
          style = list(fontWeight = "500")
        ),
        gridLineWidth = 0.5,
        gridLineColor = "rgba(224, 224, 224, 0.5)",
        labels = list(
          format = "{value}%",
          style = list(color = "#37474F")
        ),
        min = 0,
        max = 150,
        tickInterval = 25  # Consistent tick intervals
      ) %>%
      hc_add_series(
        name = "Couverture VAP1",
        data = filtered_data_region()$cumulative_coverage_vap1,
        type = "line",
        color = "#1976D2",  # More muted blue
        lineWidth = 2.5
      ) %>%
      hc_add_series(
        name = "Couverture VAP2",
        data = filtered_data_region()$cumulative_coverage_vap2,
        type = "line",
        color = "#388E3C",  # More muted green
        lineWidth = 2.5
      ) %>%
      hc_add_series(
        name = "Couverture VAP3",
        data = filtered_data_region()$cumulative_coverage_vap3,
        type = "line",
        color = "#FFA000",  
        lineWidth = 2.5
      ) %>%
      hc_tooltip(
        shared = TRUE,
        valueSuffix = "%",
        backgroundColor = "rgba(255, 255, 255, 0.98)",
        borderWidth = 0,
        borderRadius = 12,
        shadow = list(
          color= 'rgba(0, 0, 0, 0.1)',
          offsetX= 0,
          offsetY= 2,
          blur= 5
        ),
        style = list(fontSize = "14px"),
        headerFormat = '<div style="font-size: 14px; font-weight: 600; padding: 12px 12px 8px; min-width: 300px;">{point.x}</div>',
        pointFormat = paste0(
          '<div style="padding: 4px 12px; min-width: 100px;">',
          '<span style="color:{series.color}; font-size: 16px;">\u25CF </span>',
          '<span style="color:#263238; font-weight: 500;">{series.name}</span>: ',
          '<span style="color:#263238; font-weight: 600; float: right;">{point.y:.1f}%</span>',
          '</div>'
        ),
        useHTML = TRUE,
        followPointer = TRUE,
        animation = list(duration = 200)
      )%>%
      hc_plotOptions(
        line = list(
          marker = list(
            enabled = FALSE,
            symbol = "circle",
            radius = 4,
            states = list(
              hover = list(
                enabled = TRUE,
                lineWidth = 2,
                lineColor = "white",
                fillColor = "white",
                radius = 6
              )
            )
          ),
          states = list(
            hover = list(
              lineWidthPlus = 1,  
              halo = list(size =  0)  
            )
          )
        )
      ) %>%
      hc_legend(
        align = "center",
        verticalAlign = "bottom",
        layout = "horizontal",
        itemStyle = list(
          fontSize = "13px",
          fontWeight = "500",
          color = "#263238"
        ),
        itemHoverStyle = list(color = "#1A237E"),  # Darker on hover
        borderWidth = 0,
        backgroundColor = "rgba(255, 255, 255, 0.9)",
        padding = 16,
        symbolRadius = 2  # Squared legend markers
      ) %>%
      hc_credits(enabled = FALSE) %>%
      hc_exporting(
        enabled = TRUE,
        buttons = list(
          contextButton = list(
            menuItems = c("downloadPNG", "downloadJPEG", "downloadPDF", "downloadCSV"),
            symbol = 'menuball',  
            symbolStroke = '#263238'
          )
        ),
        chartOptions = list(
          plotOptions = list(
            series = list(
              animation = FALSE  # Disable animation in exported charts
            )
          )
        )
      ) %>%
      hc_boost(
        enabled = TRUE,
        useGPUTranslations = TRUE,
        usePreallocated = TRUE
      )
    
    
    
  })
  
  ### Coverage comparison by districts
  
  db_comparison_region <- reactive({
    
      db_vaccination_regions_cum %>%
        filter(period_date >= ymd(input$startMonth),period_date <= ymd(input$endMonth))%>% 
        mutate(
          period_date = as.Date(period_date)
        )
    
  })
  
   output$comparison_regions <- renderHighchart({
    
    data <- db_comparison_region() 
    
    # Get latest date
    latest_date <- max(data$period_date)
    
    # Filter data for latest date
    latest_data <- data %>%
      filter(period_date == latest_date) %>%
      select(orgunitlevel2, cumulative_coverage_vap1, cumulative_coverage_vap2, cumulative_coverage_vap3)
    
    highchart() %>%
      hc_chart(
        type = "bar",
        style = list(fontFamily = "Inter, Arial, sans-serif"),
        backgroundColor = "#FFFFFF",
        animation = list(duration = 1000)
      ) %>%
      hc_title(
        text = str_glue("Comparaison de la couverture vaccinale par régions"),
        style = list(
          fontSize = "24px",
          fontWeight = "600",
          color = "#1A237E"
        ),
        align = "left"
      ) %>%
      hc_subtitle(
        text = paste0("Situation au mois de ", format(latest_date, "%B %Y")),
        style = list(
          fontSize = "16px",
          color = "#546E7A"
        ),
        align = "left"
      ) %>%
      hc_xAxis(
        categories = latest_data$orgunitlevel2,
        title = list(
          text = "Région",
          style = list(fontWeight = "500")
        ),
        labels = list(
          style = list(
            fontSize = "12px",
            color = "#37474F"
          ),
          rotation = -45
        ),
        gridLineWidth = 0,
        lineWidth = 1,
        lineColor = "#E0E0E0"
      ) %>%
      hc_yAxis(
        title = list(
          text = "Couverture (%)",
          style = list(fontWeight = "500")
        ),
        gridLineWidth = 0.5,
        gridLineColor = "rgba(224, 224, 224, 0.5)",
        labels = list(
          format = "{value}%",
          style = list(color = "#37474F")
        )
      ) %>%
      hc_add_series(
        name = "VAP1",
        data = latest_data$cumulative_coverage_vap1,
        color = "#1976D2"
      ) %>%
      hc_add_series(
        name = "VAP2",
        data = latest_data$cumulative_coverage_vap2,
        color = "#388E3C"
      ) %>%
      hc_add_series(
        name = "VAP3",
        data = latest_data$cumulative_coverage_vap3,
        color = "#FFA000"
      ) %>%
      hc_tooltip(
        shared = TRUE,
        backgroundColor = "rgba(255, 255, 255, 0.98)",
        borderWidth = 0,
        borderRadius = 12,
        shadow = list(
          color = 'rgba(0, 0, 0, 0.1)',
          offsetX = 0,
          offsetY = 2,
          blur = 5
        ),
        style = list(fontSize = "14px"),
        headerFormat = '<div style="font-size: 14px; font-weight: 600; padding: 12px 12px 8px; min-width: 300px;">{point.x}</div>',
        pointFormat = paste0(
          '<div style="padding: 4px 12px; min-width: 100px;">',
          '<span style="color:{series.color}; font-size: 16px;">\u25CF </span>',
          '<span style="color:#263238; font-weight: 500;">{series.name}</span>: ',
          '<span style="color:#263238; font-weight: 600; float: right;">{point.y:.1f}%</span>',
          '</div>'
        ),
        useHTML = TRUE,
        followPointer = TRUE,
        animation = list(duration = 200)
      ) %>%
      hc_plotOptions(
        bar = list(
          groupPadding = 0.1,
          pointPadding = 0.05,
          borderRadius = 3,
          borderWidth = 0,
          states = list(
            hover = list(
              brightness = 0.1,
              halo = list(size = 0)
            )
          )
        )
      ) %>%
      hc_legend(
        align = "center",
        verticalAlign = "bottom",
        layout = "horizontal",
        itemStyle = list(
          fontSize = "13px",
          fontWeight = "500",
          color = "#263238"
        ),
        itemHoverStyle = list(color = "#1A237E"),
        borderWidth = 0,
        backgroundColor = "rgba(255, 255, 255, 0.9)",
        padding = 16,
        symbolRadius = 2
      ) %>%
      hc_credits(enabled = FALSE) %>%
      hc_exporting(
        enabled = TRUE,
        buttons = list(
          contextButton = list(
            menuItems = c("downloadPNG", "downloadJPEG", "downloadPDF", "downloadCSV"),
            symbol = 'menuball',
            symbolStroke = '#263238'
          )
        ),
        chartOptions = list(
          plotOptions = list(
            series = list(
              animation = FALSE
            )
          )
        )
      ) %>%
      hc_boost(
        enabled = TRUE,
        useGPUTranslations = TRUE,
        usePreallocated = TRUE
      )
  })
  
 ###  Dose administered by strategy
  
  db_vaccination_strategy <- reactive({
    
      db_vaccination_regions_cum %>%
        group_by(period_date,orgunitlevel2) %>%
        summarise(
          Fixe_Dose1 = sum(vap1_fixe),
          Fixe_Dose2 = sum(vap2_fixe),
          Fixe_Dose3 = sum(vap3_fixe),
          Avance_Dose1 = sum(vap1_avancee),
          Avance_Dose2 = sum(vap2_avancee),
          Avance_Dose3 = sum(vap3_avancee)
        ) %>%
        pivot_longer(
          cols = -c(period_date,orgunitlevel2),
          names_to = c("strategy", "dose"),
          names_pattern = "(.+)_Dose(.+)",
          values_to = "count") %>%   #%>% 
        #dplyr::filter(dose == input$doseid)
        mutate(
          period_date = as.Date(period_date)) %>% 
         dplyr::filter(orgunitlevel2 == input$choose_region,
                       period_date >= ymd(input$startMonth),period_date <= ymd(input$endMonth)
                       )
    
    
})
  
  output$vaccination_strategy <- renderHighchart({
    
    dose_choice <- as.numeric(str_extract(input$doseid,"1|2|3"))
    
    formatted_data <- db_vaccination_strategy() %>%
      mutate(
        period_date = as.Date(period_date),
        count = as.numeric(count)
      )
    
    highchart() %>%
      hc_chart(
        style = list(fontFamily = "Inter, Arial, sans-serif"),
        backgroundColor = "#FFFFFF",
        crosshair = list(
          color = 'rgba(48, 48, 48, 0.8)',
          dashStyle = 'Dash',
          width = 1,
          zIndex = 5
        ),
        animation = list(duration = 1000)
      ) %>%
      hc_title(
        text =str_glue("Doses administrées par stratégie de vaccination - {ifelse(input$choose_region == 'Toutes les régions','Niveau National',paste('Région','',input$choose_region))}"),
        style = list(
          fontSize = "24px",
          fontWeight = "600",
          color = "#1A237E"
        ),
        align = "left"
      ) %>%
      hc_subtitle(
        text = str_glue("Evolution des stratégies Fixe et Avancée dans le temps - {input$doseid}"),
        style = list(
          fontSize = "16px",
          color = "#546E7A"
        ),
        align = "left"
      ) %>%
      hc_xAxis(
        categories = unique(formatted_data$period_date),
        title = list(
          text = "Période",
          style = list(fontWeight = "500")
        ),
        gridLineWidth = 0.5,
        gridLineColor = "rgba(224, 224, 224, 0.5)",
        labels = list(
          style = list(fontSize = "12px", color = "#37474F"),
          rotation = -45,
          useHTML = TRUE
        ),
        crosshair = TRUE,
        tickmarkPlacement = "on"
      ) %>%
      hc_yAxis(
        title = list(
          text = "Nombre de vaccinations",
          style = list(fontWeight = "500")
        ),
        gridLineWidth = 0.5,
        gridLineColor = "rgba(224, 224, 224, 0.5)",
        labels = list(
          style = list(color = "#37474F")
        )
      ) %>%
      hc_tooltip(
        shared = TRUE,
        backgroundColor = "rgba(255, 255, 255, 0.98)",
        borderWidth = 0,
        borderRadius = 12,
        shadow = list(
          color = 'rgba(0, 0, 0, 0.1)',
          offsetX = 0,
          offsetY = 2,
          blur = 5
        ),
        style = list(fontSize = "14px"),
        headerFormat = '<div style="font-size: 14px; font-weight: 600; padding: 12px 12px 8px; min-width: 300px;">{point.x}</div>',
        pointFormat = paste0(
          '<div style="padding: 4px 12px; min-width: 100px;">',
          '<span style="color:{series.color}; font-size: 16px;">\u25CF </span>',
          '<span style="color:#263238; font-weight: 500;">{series.name}</span>: ',
          '<span style="color:#263238; font-weight: 600; float: right;">{point.y:,.0f}</span>',
          '</div>'
        ),
        useHTML = TRUE,
        followPointer = TRUE,
        animation = list(duration = 200)
      ) %>%
      hc_add_series(
        name = "Fixe",
        data = formatted_data %>% 
          filter(dose == dose_choice, strategy == "Fixe") %>% 
          arrange(period_date) %>%
          pull(count),
        type = "area",
        color = "#1976D2",
        fillColor = list(
          linearGradient = list(x1 = 0, y1 = 0, x2 = 0, y2 = 1),
          stops = list(
            list(0, "rgba(25, 118, 210, 0.7)"),
            list(1, "rgba(25, 118, 210, 0.1)")
          )
        )
      ) %>%
      hc_add_series(
        name = "Avance",
        data = formatted_data %>% 
          filter(dose == dose_choice, strategy == "Avance") %>% 
          arrange(period_date) %>%
          pull(count),
        type = "area",
        color = "#388E3C",
        fillColor = list(
          linearGradient = list(x1 = 0, y1 = 0, x2 = 0, y2 = 1),
          stops = list(
            list(0, "rgba(56, 142, 60, 0.7)"),
            list(1, "rgba(56, 142, 60, 0.1)")
          )
        )
      ) %>%
      hc_plotOptions(
        area = list(
          fillOpacity = 0.65,
          marker = list(
            enabled = FALSE,
            symbol = "circle",
            radius = 4,
            states = list(
              hover = list(
                enabled = TRUE,
                lineWidth = 2,
                lineColor = "white",
                fillColor = "white",
                radius = 6
              )
            )
          ),
          states = list(
            hover = list(
              lineWidthPlus = 1,
              halo = list(size = 0)
            )
          ),
          lineWidth = 2.5
        )
      ) %>%
      hc_legend(
        align = "center",
        verticalAlign = "bottom",
        layout = "horizontal",
        itemStyle = list(
          fontSize = "13px",
          fontWeight = "500",
          color = "#263238"
        ),
        itemHoverStyle = list(color = "#1A237E"),
        borderWidth = 0,
        backgroundColor = "rgba(255, 255, 255, 0.9)",
        padding = 16,
        symbolRadius = 2
      ) %>%
      hc_credits(enabled = FALSE) %>%
      hc_exporting(
        enabled = TRUE,
        buttons = list(
          contextButton = list(
            menuItems = c("downloadPNG", "downloadJPEG", "downloadPDF", "downloadCSV"),
            symbol = 'menuball',
            symbolStroke = '#263238'
          )
        ),
        chartOptions = list(
          plotOptions = list(
            series = list(
              animation = FALSE
            )
          )
        )
      ) %>%
      hc_boost(
        enabled = TRUE,
        useGPUTranslations = TRUE,
        usePreallocated = TRUE
      )
  })
  
  output$label_output_vap1 <- renderText({
    glue::glue("Doses administrées et couverture vaccinale VAP1 - {input$choose_region}")
  })
  
  output$label_output_vap2 <- renderText({
    glue::glue("Doses administrées et couverture vaccinale VAP2 - {input$choose_region}")
  })
  
  output$label_output_vap3 <- renderText({
    glue::glue("Doses administrées et couverture vaccinale VAP3 - {input$choose_region}")
  })
  
  vap_region_db <- reactive({
    
    db_vaccination_regions_cum %>% 
      filter(orgunitlevel2 == input$choose_region,period_date >= ymd(input$startMonth),period_date <= ymd(input$endMonth)) %>% 
      mutate(
        period_date = as.Date(period_date)) %>% 
      filter(period_date == max(period_date))
  })
  
  
  output$vap1_region <- renderText({
    
    adm_doses <- vap_region_db() %>% 
                  pull(total_vap1) %>% 
                  as.numeric()
    
    cvg_vap1 <- vap_region_db() %>% 
      pull(cumulative_coverage_vap1) %>% 
      as.numeric()
    
  paste(str_glue("{adm_doses} ({cvg_vap1}%)"))
    
  })
  
  output$vap2_region <- renderText({
    
    adm_doses <- vap_region_db() %>% 
      pull(total_vap2) %>% 
      as.numeric()
    
    cvg_vap2 <- vap_region_db() %>% 
      pull(cumulative_coverage_vap2) %>% 
      as.numeric()
    
    paste(str_glue("{adm_doses} ({cvg_vap2}%)"))
    
   
    
  })
  
  output$vap3_region <- renderText({
    
    adm_doses <- vap_region_db() %>% 
      pull(total_vap3) %>% 
      as.numeric()
    
    cvg_vap3 <- vap_region_db() %>% 
      pull(cumulative_coverage_vap3) %>% 
      as.numeric()
    
    paste(str_glue("{adm_doses} ({cvg_vap3}%)"))
    
    
    
  })

# District Level ----------------------------------------------------------

  filtered_data_district_cum <- reactive({

         db_vaccination_district_cum %>%
          filter(orgunitlevel4 == input$choose_district,period_date >= ymd(input$startMonth1),period_date <= ymd(input$endMonth1))%>% 
          mutate(
            period_date = as.Date(period_date)) %>% 
           filter(period_date >= ymd(input$startMonth1),period_date <= ymd(input$endMonth1))
    
})
  
  filtered_data_district <- reactive({
    
    db_vaccination_district %>% 
      filter(orgunitlevel4 == input$choose_district,period_date >= ymd(input$startMonth1),period_date <= ymd(input$endMonth1))%>% 
      mutate(
        period_date = as.Date(period_date)) %>% 
      filter(period_date >= ymd(input$startMonth1),period_date <= ymd(input$endMonth1))
   
  })
  
  coverage_comparison_by_districts <- reactive({
    
    districts_choices <- db_vaccination %>% 
      filter(orgunitlevel2 == input$choose_region1) %>% 
      pull(orgunitlevel4) %>% unique()
    
   db <-  db_vaccination_district_cum %>%
      filter(orgunitlevel4 %in% districts_choices ,period_date >= ymd(input$startMonth1),period_date <= ymd(input$endMonth1))%>% 
      mutate(period_date = as.Date(period_date)) %>% 
      filter(period_date >= ymd(input$startMonth1),period_date <= ymd(input$endMonth1))
    
   return(db)
   
  })
  
  vaccination_by_strategy_district <- reactive({
    
    db_vaccination_district_cum %>%
      group_by(period_date,orgunitlevel4) %>%
      summarise(
        Fixe_Dose1 = sum(vap1_fixe),
        Fixe_Dose2 = sum(vap2_fixe),
        Fixe_Dose3 = sum(vap3_fixe),
        Avance_Dose1 = sum(vap1_avancee),
        Avance_Dose2 = sum(vap2_avancee),
        Avance_Dose3 = sum(vap3_avancee)) %>%
      pivot_longer(
        cols = -c(period_date,orgunitlevel4),
        names_to = c("strategy", "dose"),
        names_pattern = "(.+)_Dose(.+)",
        values_to = "count") %>% 
      filter(orgunitlevel4 == input$choose_district,period_date == ymd(input$endMonth1))%>% 
      mutate(period_date = as.Date(period_date))
    
  })
  
  output$coverage_trend_district <- renderHighchart({
    
    highchart() %>%
      hc_chart(
        style = list(fontFamily = "Inter, Arial, sans-serif"), 
        backgroundColor = "#FFFFFF", 
        crosshair = list(
          color = 'rgba(48, 48, 48, 0.8)', # Semi-transparent crosshair
          dashStyle = 'Dash',
          width = 1,
          zIndex = 5
        ),
        animation = list(duration = 1000) # Smooth initial animation
      ) %>%
      hc_title(
        text = str_glue("Couverture vaccinale cumulée VAP dans le temps - District de {input$choose_district}"),
        style = list(
          fontSize = "24px", 
          fontWeight = "600",
          color = "#1A237E"  # Darker blue for better contrast
        ),
        align = "left"  # Left-aligned title for modern look
      ) %>%
      hc_subtitle(
        text = "Présentation des tendances de couverture VAP1, VAP2, et VAP3",
        style = list(
          fontSize = "16px",
          color = "#546E7A"  # Subtle gray for subtitle
        ),
        align = "left"
      ) %>%
      hc_xAxis(
        categories = filtered_data_district_cum()$period_date,
        title = list(
          text = "Mois",
          style = list(fontWeight = "500")
        ),
        gridLineWidth = 0.5,
        gridLineColor = "rgba(224, 224, 224, 0.5)",  # Lighter grid
        labels = list(
          style = list(fontSize = "12px", color = "#37474F"),
          rotation = -45,
          useHTML = TRUE
        ),
        crosshair = TRUE,
        tickmarkPlacement = "on"
      ) %>%
      hc_yAxis(
        title = list(
          text = "Couverture cumulée (%)",
          style = list(fontWeight = "500")
        ),
        gridLineWidth = 0.5,
        gridLineColor = "rgba(224, 224, 224, 0.5)",
        labels = list(
          format = "{value}%",
          style = list(color = "#37474F")
        ),
        min = 0,
        max = 150,
        tickInterval = 25  # Consistent tick intervals
      ) %>%
      hc_add_series(
        name = "Couverture VAP1",
        data = filtered_data_district_cum()$cumulative_coverage_vap1,
        type = "line",
        color = "#1976D2",  # More muted blue
        lineWidth = 2.5
      ) %>%
      hc_add_series(
        name = "Couverture VAP2",
        data = filtered_data_district_cum()$cumulative_coverage_vap2,
        type = "line",
        color = "#388E3C",  # More muted green
        lineWidth = 2.5
      ) %>%
      hc_add_series(
        name = "Couverture VAP3",
        data = filtered_data_district_cum()$cumulative_coverage_vap3,
        type = "line",
        color = "#FFA000",  
        lineWidth = 2.5
      ) %>%
      hc_tooltip(
        shared = TRUE,
        valueSuffix = "%",
        backgroundColor = "rgba(255, 255, 255, 0.98)",
        borderWidth = 0,
        borderRadius = 12,
        shadow = list(
          color= 'rgba(0, 0, 0, 0.1)',
          offsetX= 0,
          offsetY= 2,
          blur= 5
        ),
        style = list(fontSize = "14px"),
        headerFormat = '<div style="font-size: 14px; font-weight: 600; padding: 12px 12px 8px; min-width: 300px;">{point.x}</div>',
        pointFormat = paste0(
          '<div style="padding: 4px 12px; min-width: 100px;">',
          '<span style="color:{series.color}; font-size: 16px;">\u25CF </span>',
          '<span style="color:#263238; font-weight: 500;">{series.name}</span>: ',
          '<span style="color:#263238; font-weight: 600; float: right;">{point.y:.1f}%</span>',
          '</div>'
        ),
        useHTML = TRUE,
        followPointer = TRUE,
        animation = list(duration = 200)
      )%>%
      hc_plotOptions(
        line = list(
          marker = list(
            enabled = FALSE,
            symbol = "circle",
            radius = 4,
            states = list(
              hover = list(
                enabled = TRUE,
                lineWidth = 2,
                lineColor = "white",
                fillColor = "white",
                radius = 6
              )
            )
          ),
          states = list(
            hover = list(
              lineWidthPlus = 1,  
              halo = list(size =  0)  
            )
          )
        )
      ) %>%
      hc_legend(
        align = "center",
        verticalAlign = "bottom",
        layout = "horizontal",
        itemStyle = list(
          fontSize = "13px",
          fontWeight = "500",
          color = "#263238"
        ),
        itemHoverStyle = list(color = "#1A237E"),  # Darker on hover
        borderWidth = 0,
        backgroundColor = "rgba(255, 255, 255, 0.9)",
        padding = 16,
        symbolRadius = 2  # Squared legend markers
      ) %>%
      hc_credits(enabled = FALSE) %>%
      hc_exporting(
        enabled = TRUE,
        buttons = list(
          contextButton = list(
            menuItems = c("downloadPNG", "downloadJPEG", "downloadPDF", "downloadCSV"),
            symbol = 'menuball',  
            symbolStroke = '#263238'
          )
        ),
        chartOptions = list(
          plotOptions = list(
            series = list(
              animation = FALSE  # Disable animation in exported charts
            )
          )
        )
      ) %>%
      hc_boost(
        enabled = TRUE,
        useGPUTranslations = TRUE,
        usePreallocated = TRUE
      )
    
    
    
  })
  
  output$monthly_coverage_district <- renderHighchart({
    
    highchart() %>%
      hc_chart(
        style = list(fontFamily = "Inter, Arial, sans-serif"), 
        backgroundColor = "#FFFFFF", 
        crosshair = list(
          color = 'rgba(48, 48, 48, 0.8)', # Semi-transparent crosshair
          dashStyle = 'Dash',
          width = 1,
          zIndex = 5
        ),
        animation = list(duration = 1000) # Smooth initial animation
      ) %>%
      hc_title(
        text = str_glue("Couverture vaccinale mensuelle VAP - District de {input$choose_district}"),
        style = list(
          fontSize = "24px", 
          fontWeight = "600",
          color = "#1A237E"  # Darker blue for better contrast
        ),
        align = "left"  # Left-aligned title for modern look
      ) %>%
      hc_subtitle(
        text = "Présentation des tendances de couverture VAP1, VAP2, et VAP3",
        style = list(
          fontSize = "16px",
          color = "#546E7A"  # Subtle gray for subtitle
        ),
        align = "left"
      ) %>%
      hc_xAxis(
        categories = filtered_data_district()$period_date,
        title = list(
          text = "Mois",
          style = list(fontWeight = "500")
        ),
        gridLineWidth = 0.5,
        gridLineColor = "rgba(224, 224, 224, 0.5)",  # Lighter grid
        labels = list(
          style = list(fontSize = "12px", color = "#37474F"),
          rotation = -45,
          useHTML = TRUE
        ),
        crosshair = TRUE,
        tickmarkPlacement = "on"
      ) %>%
      hc_yAxis(
        title = list(
          text = "Couverture mensuelle (%)",
          style = list(fontWeight = "500")
        ),
        gridLineWidth = 0.5,
        gridLineColor = "rgba(224, 224, 224, 0.5)",
        labels = list(
          format = "{value}%",
          style = list(color = "#37474F")
        ),
        min = 0,
        max = 150,
        tickInterval = 25  # Consistent tick intervals
      ) %>%
      hc_add_series(
        name = "Couverture VAP1",
        data = filtered_data_district()$cumulative_coverage_vap1,
        type = "line",
        color = "#1976D2",  # More muted blue
        lineWidth = 2.5
      ) %>%
      hc_add_series(
        name = "Couverture VAP2",
        data = filtered_data_district()$cumulative_coverage_vap2,
        type = "line",
        color = "#388E3C",  # More muted green
        lineWidth = 2.5
      ) %>%
      hc_add_series(
        name = "Couverture VAP3",
        data = filtered_data_district()$cumulative_coverage_vap3,
        type = "line",
        color = "#FFA000",  
        lineWidth = 2.5
      ) %>%
      hc_tooltip(
        shared = TRUE,
        valueSuffix = "%",
        backgroundColor = "rgba(255, 255, 255, 0.98)",
        borderWidth = 0,
        borderRadius = 12,
        shadow = list(
          color= 'rgba(0, 0, 0, 0.1)',
          offsetX= 0,
          offsetY= 2,
          blur= 5
        ),
        style = list(fontSize = "14px"),
        headerFormat = '<div style="font-size: 14px; font-weight: 600; padding: 12px 12px 8px; min-width: 300px;">{point.x}</div>',
        pointFormat = paste0(
          '<div style="padding: 4px 12px; min-width: 100px;">',
          '<span style="color:{series.color}; font-size: 16px;">\u25CF </span>',
          '<span style="color:#263238; font-weight: 500;">{series.name}</span>: ',
          '<span style="color:#263238; font-weight: 600; float: right;">{point.y:.1f}%</span>',
          '</div>'
        ),
        useHTML = TRUE,
        followPointer = TRUE,
        animation = list(duration = 200)
      )%>%
      hc_plotOptions(
        line = list(
          marker = list(
            enabled = FALSE,
            symbol = "circle",
            radius = 4,
            states = list(
              hover = list(
                enabled = TRUE,
                lineWidth = 2,
                lineColor = "white",
                fillColor = "white",
                radius = 6
              )
            )
          ),
          states = list(
            hover = list(
              lineWidthPlus = 1,  
              halo = list(size =  0)  
            )
          )
        )
      ) %>%
      hc_legend(
        align = "center",
        verticalAlign = "bottom",
        layout = "horizontal",
        itemStyle = list(
          fontSize = "13px",
          fontWeight = "500",
          color = "#263238"
        ),
        itemHoverStyle = list(color = "#1A237E"),  # Darker on hover
        borderWidth = 0,
        backgroundColor = "rgba(255, 255, 255, 0.9)",
        padding = 16,
        symbolRadius = 2  # Squared legend markers
      ) %>%
      hc_credits(enabled = FALSE) %>%
      hc_exporting(
        enabled = TRUE,
        buttons = list(
          contextButton = list(
            menuItems = c("downloadPNG", "downloadJPEG", "downloadPDF", "downloadCSV"),
            symbol = 'menuball',  
            symbolStroke = '#263238'
          )
        ),
        chartOptions = list(
          plotOptions = list(
            series = list(
              animation = FALSE  # Disable animation in exported charts
            )
          )
        )
      ) %>%
      hc_boost(
        enabled = TRUE,
        useGPUTranslations = TRUE,
        usePreallocated = TRUE
      )
    
    
    
  })
  
  output$comparison_district  <- renderHighchart({
    
    data <- coverage_comparison_by_districts() 
    
    # Get latest date
    latest_date <- max(data$period_date)
    
    # Filter data for latest date
    latest_data <- data %>%
      filter(period_date == latest_date) %>%
      select(orgunitlevel4, cumulative_coverage_vap1, cumulative_coverage_vap2, cumulative_coverage_vap3)
    
    highchart() %>%
      hc_chart(
        type = "bar",
        style = list(fontFamily = "Inter, Arial, sans-serif"),
        backgroundColor = "#FFFFFF",
        animation = list(duration = 1000)
      ) %>%
      hc_title(
        text = "Comparaison de la couverture vaccinale par district et par région",
        style = list(
          fontSize = "24px",
          fontWeight = "600",
          color = "#1A237E"
        ),
        align = "left"
      ) %>%
      hc_subtitle(
        text = paste0("Situation au mois de ", format(latest_date, "%B %Y")),
        style = list(
          fontSize = "16px",
          color = "#546E7A"
        ),
        align = "left"
      ) %>%
      hc_xAxis(
        categories = latest_data$orgunitlevel4,
        title = list(
          text = "Districts",
          style = list(fontWeight = "500")
        ),
        labels = list(
          style = list(
            fontSize = "12px",
            color = "#37474F"
          ) ,
          rotation = -45
        ),
        gridLineWidth = 0,
        lineWidth = 1,
        lineColor = "#E0E0E0"
      ) %>%
      hc_yAxis(
        title = list(
          text = "Couverture (%)",
          style = list(fontWeight = "500")
        ),
        gridLineWidth = 0.5,
        gridLineColor = "rgba(224, 224, 224, 0.5)",
        labels = list(
          format = "{value}%",
          style = list(color = "#37474F")
        )
      ) %>%
      hc_add_series(
        name = "VAP1",
        data = latest_data$cumulative_coverage_vap1,
        color = "#1976D2"
      ) %>%
      hc_add_series(
        name = "VAP2",
        data = latest_data$cumulative_coverage_vap2,
        color = "#388E3C"
      ) %>%
      hc_add_series(
        name = "VAP3",
        data = latest_data$cumulative_coverage_vap3,
        color = "#FFA000"
      ) %>%
      hc_tooltip(
        shared = TRUE,
        backgroundColor = "rgba(255, 255, 255, 0.98)",
        borderWidth = 0,
        borderRadius = 12,
        shadow = list(
          color = 'rgba(0, 0, 0, 0.1)',
          offsetX = 0,
          offsetY = 2,
          blur = 5
        ),
        style = list(fontSize = "14px"),
        headerFormat = '<div style="font-size: 14px; font-weight: 600; padding: 12px 12px 8px; min-width: 300px;">{point.x}</div>',
        pointFormat = paste0(
          '<div style="padding: 4px 12px; min-width: 100px;">',
          '<span style="color:{series.color}; font-size: 16px;">\u25CF </span>',
          '<span style="color:#263238; font-weight: 500;">{series.name}</span>: ',
          '<span style="color:#263238; font-weight: 600; float: right;">{point.y:.1f}%</span>',
          '</div>'
        ),
        useHTML = TRUE,
        followPointer = TRUE,
        animation = list(duration = 200)
      ) %>%
      hc_plotOptions(
        bar = list(
          groupPadding = 0.1,
          pointPadding = 0.05,
          borderRadius = 3,
          borderWidth = 0,
          states = list(
            hover = list(
              brightness = 0.1,
              halo = list(size = 0)
            )
          )
        )
      ) %>%
      hc_legend(
        align = "center",
        verticalAlign = "bottom",
        layout = "horizontal",
        itemStyle = list(
          fontSize = "13px",
          fontWeight = "500",
          color = "#263238"
        ),
        itemHoverStyle = list(color = "#1A237E"),
        borderWidth = 0,
        backgroundColor = "rgba(255, 255, 255, 0.9)",
        padding = 16,
        symbolRadius = 2
      ) %>%
      hc_credits(enabled = FALSE) %>%
      hc_exporting(
        enabled = TRUE,
        buttons = list(
          contextButton = list(
            menuItems = c("downloadPNG", "downloadJPEG", "downloadPDF", "downloadCSV"),
            symbol = 'menuball',
            symbolStroke = '#263238'
          )
        ),
        chartOptions = list(
          plotOptions = list(
            series = list(
              animation = FALSE
            )
          )
        )
      ) %>%
      hc_boost(
        enabled = TRUE,
        useGPUTranslations = TRUE,
        usePreallocated = TRUE
      )
  })
  
  output$vap1_doses_strategy <- renderHighchart({
    
    data <- vaccination_by_strategy_district()
    
    data <- data %>% 
             filter(dose == 1)
    
    total <- sum(data$count)
    data$percentage <- round((data$count / total) * 100,1)
    
    highchart() %>%
      hc_chart(
        type = "pie",
        backgroundColor = "#ffffff"
      ) %>%
      hc_title(
        text = "VAP1",
        style = list(fontSize = "20px", fontWeight = "bold")
      ) %>%
      hc_subtitle(
        text = str_glue("% de doses de VAP1 administrées par stratégie, au mois de {format(ymd(input$endMonth1), '%B %Y')}")
      ) %>%
      hc_tooltip(
        valueSuffix = "%"
      ) %>%
      hc_plotOptions(
        series = list(
          allowPointSelect = TRUE,
          cursor = "pointer",
          dataLabels = list(
            list(
              enabled = TRUE,
              distance = 20,
              format = "{point.name}"
            ),
            list(
              enabled = TRUE,
              distance = -40,
              format = "{point.percentage:.1f}%",
              style = list(
                fontSize = "1.2em",
                textOutline = "none",
                opacity = 0.7
              ),
              filter = list(
                operator = ">",
                property = "percentage",
                value = 10
              )
            )
          )
        )
      ) %>%
      hc_add_series(
        name = "Pourcentage",
        colorByPoint = TRUE,
        data = list(
          list(
            name = "Fixe",
            y = data$percentage[1],
            selected = TRUE,
            sliced = TRUE
          ),
          list(
            name = "Avancé",
            y = data$percentage[2]
          )
        )
      ) %>%
      hc_colors(colors = c("#7cb5ec", "#f7a35c")) %>%
      hc_credits(enabled = FALSE)
    
    
    
    
    
  })
  
  vap2_by_strategy <- reactive({
    
    data <- vaccination_by_strategy_district()
    data <- data %>% 
      filter(dose == 2)
  })
  
  vap3_by_strategy <- reactive({
    
    data <- vaccination_by_strategy_district()
    data <- data %>% 
      filter(dose == 3)
  })
 
  
  output$vap2_doses_strategy <- renderHighchart({
    
   data <- vap2_by_strategy()
    
    # Check if data is empty
    shiny::validate(
      need(sum(data$count) > 0, "Aucune dose de VAP2 administrée pour le mois choisi")
    )
    
    # Proceed only if there is data
    total <- sum(data$count)
    data$percentage <- round((data$count / total) * 100, 1)
    
    highchart() %>%
      hc_chart(
        type = "pie",
        backgroundColor = "#ffffff"
      ) %>%
      hc_title(
        text = "VAP2",
        style = list(fontSize = "20px", fontWeight = "bold")
      ) %>%
      hc_subtitle(
        text = str_glue("% de doses de VAP2 administrées par stratégie, au mois de {format(ymd(input$endMonth1), '%B %Y')}")
      ) %>%
      hc_tooltip(
        valueSuffix = "%"
      ) %>%
      hc_plotOptions(
        series = list(
          allowPointSelect = TRUE,
          cursor = "pointer",
          dataLabels = list(
            list(
              enabled = TRUE,
              distance = 20,
              format = "{point.name}"
            ),
            list(
              enabled = TRUE,
              distance = -40,
              format = "{point.percentage:.1f}%",
              style = list(
                fontSize = "1.2em",
                textOutline = "none",
                opacity = 0.7
              ),
              filter = list(
                operator = ">",
                property = "percentage",
                value = 10
              )
            )
          )
        )
      ) %>%
      hc_add_series(
        name = "Pourcentage",
        colorByPoint = TRUE,
        data = list(
          list(
            name = "Fixe",
            y = data$percentage[1],
            selected = TRUE,
            sliced = TRUE
          ),
          list(
            name = "Avancé",
            y = data$percentage[2]
          )
        )
      ) %>%
      hc_colors(colors = c("#7cb5ec", "#f7a35c")) %>%
      hc_credits(enabled = FALSE)
    
  })
  
  output$vap3_doses_strategy <- renderHighchart({
    
    data <- vap3_by_strategy()
    
    # Check if data is empty
    shiny::validate(
      need(sum(data$count) > 0, "Aucune dose de VAP3 administrée pour le mois choisi")
    )
    
    # Proceed only if there is data
    total <- sum(data$count)
    data$percentage <- round((data$count / total) * 100, 1)
    
    highchart() %>%
      hc_chart(
        type = "pie",
        backgroundColor = "#ffffff"
      ) %>%
      hc_title(
        text = "VAP3",
        style = list(fontSize = "20px", fontWeight = "bold")
      ) %>%
      hc_subtitle(
        text = str_glue("% de doses de VAP3 administrées par stratégie, au mois de {format(ymd(input$endMonth1), '%B %Y')}")
      ) %>%
      hc_tooltip(
        valueSuffix = "%"
      ) %>%
      hc_plotOptions(
        series = list(
          allowPointSelect = TRUE,
          cursor = "pointer",
          dataLabels = list(
            list(
              enabled = TRUE,
              distance = 20,
              format = "{point.name}"
            ),
            list(
              enabled = TRUE,
              distance = -40,
              format = "{point.percentage:.1f}%",
              style = list(
                fontSize = "1.2em",
                textOutline = "none",
                opacity = 0.7
              ),
              filter = list(
                operator = ">",
                property = "percentage",
                value = 10
              )
            )
          )
        )
      ) %>%
      hc_add_series(
        name = "Pourcentage",
        colorByPoint = TRUE,
        data = list(
          list(
            name = "Fixe",
            y = data$percentage[1],
            selected = TRUE,
            sliced = TRUE
          ),
          list(
            name = "Avancé",
            y = data$percentage[2]
          )
        )
      ) %>%
      hc_colors(colors = c("#7cb5ec", "#f7a35c")) %>%
      hc_credits(enabled = FALSE)
    
  })

  vap_district_db <- reactive({
    
    db_vaccination_district_cum %>%
      filter(orgunitlevel4 == input$choose_district,period_date >= ymd(input$startMonth1),period_date <= ymd(input$endMonth1))%>% 
      mutate(
        period_date = as.Date(period_date)) %>% 
      filter(period_date >= ymd(input$startMonth1),period_date <= ymd(input$endMonth1)) %>% 
      filter(
        period_date == max(period_date)
      )
    
  })
  
  output$label_output_vap1_district <- renderText({
    glue::glue("Doses administrées et couverture vaccinale VAP1 - District de {input$choose_district}")
  })
  
  output$label_output_vap2_district <- renderText({
    glue::glue("Doses administrées et couverture vaccinale VAP2 - District de {input$choose_district}")
  })
  
  output$label_output_vap3_district <- renderText({
    glue::glue("Doses administrées et couverture vaccinale VAP3 - District de {input$choose_district}")
  })
  
  output$vap1_district <- renderText({
    
    adm_doses <- vap_district_db() %>% 
      pull(total_vap1) %>% 
      as.numeric()
    
    cvg_vap1 <- vap_district_db() %>% 
      pull(cumulative_coverage_vap1) %>% 
      as.numeric()
    
    paste(str_glue("{adm_doses} ({cvg_vap1}%)"))
    
  })
  
  output$vap2_district <- renderText({
    
    adm_doses <- vap_district_db() %>% 
      pull(total_vap2) %>% 
      as.numeric()
    
    cvg_vap1 <- vap_district_db() %>% 
      pull(cumulative_coverage_vap2) %>% 
      as.numeric()
    
    paste(str_glue("{adm_doses} ({cvg_vap1}%)"))
    
  })
  
  output$vap3_district <- renderText({
    
    adm_doses <- vap_district_db() %>% 
      pull(total_vap3) %>% 
      as.numeric()
    
    cvg_vap1 <- vap_district_db() %>% 
      pull(cumulative_coverage_vap3) %>% 
      as.numeric()
    
    paste(str_glue("{adm_doses} ({cvg_vap1}%)"))
    
  })
  
  
  
  # National Map 
  
  output$map_coverage <- renderLeaflet({
    
    leaflet::leaflet(bkfaso_shp, 
                     options = leafletOptions(
                       attributionControl = TRUE,
                       maxZoom = 10, 
                       minZoom = 2, 
                       noWrap = FALSE,
                       zoomControl = TRUE,
                       zoomSnap = 0.25,
                       zoomDelta = 0.25,
                       wheelPxPerZoomLevel = 100
                     )) %>% 
      # Add multiple base tile layers for more context
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Light") %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB Dark") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      
      setView(lng = lon_center, lat = lat_center, zoom = initial_zoom) %>%
      addMapPane("polygons", zIndex = 420) %>% 
      
      # Enhanced scale bar
      addScaleBar(
        position = "bottomleft",
        options = scaleBarOptions(
          maxWidth = 200,
          metric = TRUE,
          imperial = FALSE,
          updateWhenIdle = TRUE)
      ) %>% 
      
      # Add mini map for context
      addMiniMap(
        toggleDisplay = TRUE,
        tiles = providers$CartoDB.Positron,
        position = "topleft",
        width = 150,
        height = 150
      ) %>% 
      
      # VAP1 Layer with enhanced tooltip
      addPolygons(
        data = bkfaso_shp,
        stroke = TRUE,
        color = "white",
        weight = 2,
        smoothFactor = 0.3,
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        options = pathOptions(pane = "polygons"),
        group = "Couverture vaccinale VAP1",
        fillColor = ~colors_vap1,
        label = ~sprintf(
          "<div class='custom-popup' style='background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0 2px 5px rgba(0,0,0,0.2); min-width: 200px;'>
        <h4 style='margin:0 0 10px 0; color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 5px;'>%s</h4>
        <table style='width: 100%%; border-collapse: collapse;'>
          <tr style='border-bottom: 1px solid #eee;'>
            <td style='padding: 5px 0;'><i class='fa fa-syringe'></i> <b>Total doses VAP1:</b></td>
            <td style='padding: 5px 0; text-align: right;'>%s</td>
          </tr>
          <tr style='border-bottom: 1px solid #eee;'>
            <td style='padding: 5px 0;'><i class='fa fa-percentage'></i> <b>Couverture vaccinale:</b></td>
            <td style='padding: 5px 0; text-align: right;'>%s</td>
          </tr>
          <tr>
            <td style='padding: 5px 0;'><i class='fa fa-calendar'></i> <b>Mois des données:</b></td>
            <td style='padding: 5px 0; text-align: right;'>%s</td>
          </tr>
        </table>
      </div>",
          bkfaso_shp$adm1_altna,
          format(bkfaso_shp$total_vap1, big.mark = " "),
          sprintf("%.1f%%", bkfaso_shp$cumulative_coverage_vap1),
          format(ymd(max(db_vaccination$period_date)), "%B %Y")
        ) %>% lapply(htmltools::HTML),
        labelOptions = labelOptions(
          style = list(
            "font-family" = "Arial, sans-serif",
            "font-size" = "12px",
            "padding" = "0",
            "border" = "none"
          ),
          direction = "auto",
          noHide = FALSE,
          textOnly = FALSE,
          opacity = 1
        )
      ) %>% 
      addPolygons(
        data = bkfaso_shp,
        stroke = TRUE,
        color = "white",
        weight = 2,
        smoothFactor = 0.3,
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        options = pathOptions(pane = "polygons"),
        group = "Couverture vaccinale VAP2",
        fillColor = ~colors_vap2,
        label = ~sprintf(
          "<div class='custom-popup' style='background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0 2px 5px rgba(0,0,0,0.2); min-width: 200px;'>
        <h4 style='margin:0 0 10px 0; color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 5px;'>%s</h4>
        <table style='width: 100%%; border-collapse: collapse;'>
          <tr style='border-bottom: 1px solid #eee;'>
            <td style='padding: 5px 0;'><i class='fa fa-syringe'></i> <b>Total doses VAP2:</b></td>
            <td style='padding: 5px 0; text-align: right;'>%s</td>
          </tr>
          <tr style='border-bottom: 1px solid #eee;'>
            <td style='padding: 5px 0;'><i class='fa fa-percentage'></i> <b>Couverture vaccinale:</b></td>
            <td style='padding: 5px 0; text-align: right;'>%s</td>
          </tr>
          <tr>
            <td style='padding: 5px 0;'><i class='fa fa-calendar'></i> <b>Mois des données:</b></td>
            <td style='padding: 5px 0; text-align: right;'>%s</td>
          </tr>
        </table>
      </div>",
          bkfaso_shp$adm1_altna,
          format(bkfaso_shp$total_vap2, big.mark = " "),
          sprintf("%.1f%%", bkfaso_shp$cumulative_coverage_vap2),
          format(ymd(max(db_vaccination$period_date)), "%B %Y")
        ) %>% lapply(htmltools::HTML),
        labelOptions = labelOptions(
          style = list(
            "font-family" = "Arial, sans-serif",
            "font-size" = "12px",
            "padding" = "0",
            "border" = "none"
          ),
          direction = "auto",
          noHide = FALSE,
          textOnly = FALSE,
          opacity = 1
        )
      ) %>% 
      addPolygons(
        data = bkfaso_shp,
        stroke = TRUE,
        color = "white",
        weight = 2,
        smoothFactor = 0.3,
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        options = pathOptions(pane = "polygons"),
        group = "Couverture vaccinale VAP3",
        fillColor = ~colors_vap3,
        label = ~sprintf(
          "<div class='custom-popup' style='background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0 2px 5px rgba(0,0,0,0.2); min-width: 200px;'>
        <h4 style='margin:0 0 10px 0; color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 5px;'>%s</h4>
        <table style='width: 100%%; border-collapse: collapse;'>
          <tr style='border-bottom: 1px solid #eee;'>
            <td style='padding: 5px 0;'><i class='fa fa-syringe'></i> <b>Total doses VAP1:</b></td>
            <td style='padding: 5px 0; text-align: right;'>%s</td>
          </tr>
          <tr style='border-bottom: 1px solid #eee;'>
            <td style='padding: 5px 0;'><i class='fa fa-percentage'></i> <b>Couverture vaccinale:</b></td>
            <td style='padding: 5px 0; text-align: right;'>%s</td>
          </tr>
          <tr>
            <td style='padding: 5px 0;'><i class='fa fa-calendar'></i> <b>Mois des données:</b></td>
            <td style='padding: 5px 0; text-align: right;'>%s</td>
          </tr>
        </table>
      </div>",
          bkfaso_shp$adm1_altna,
          format(bkfaso_shp$total_vap3, big.mark = " "),
          sprintf("%.1f%%", bkfaso_shp$cumulative_coverage_vap3),
          format(ymd(max(db_vaccination$period_date)), "%B %Y")
        ) %>% lapply(htmltools::HTML),
        labelOptions = labelOptions(
          style = list(
            "font-family" = "Arial, sans-serif",
            "font-size" = "12px",
            "padding" = "0",
            "border" = "none"
          ),
          direction = "auto",
          noHide = FALSE,
          textOnly = FALSE,
          opacity = 1
        )
      ) %>% 
      # Moved legends to the left
      addLegend(
        position = "bottomright",
        colors = c("#ff0000", "#ffa500", "#ffff00", "#006400", "#a9a9a9"),
        opacity = 0.9,
        title = htmltools::HTML("
      <div style='
        background-color: white;
        padding: 10px;
        border-radius: 5px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.1);
      '>
        <strong style='color: #2c3e50;'>Couverture vaccinale VAP1</strong>
      </div>
    "),
        labels = c("< 30%", "30% - 49%", "50% - 79%", "≥ 80%", "Aucune donnée"),
        group = "Couverture vaccinale VAP1",
        className = "info legend custom-legend",
        layerId = "legend1"
      ) %>% 
      addLegend(
        position = "bottomright",
        colors = c("#ff0000", "#ffa500", "#ffff00", "#006400", "#a9a9a9"),
        opacity = 0.9,
        title = htmltools::HTML("
      <div style='
        background-color: white;
        padding: 10px;
        border-radius: 5px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.1);
      '>
        <strong style='color: #2c3e50;'>Couverture vaccinale VAP2</strong>
      </div>
    "),
        labels = c("< 30%", "30% - 49%", "50% - 79%", "≥ 80%", "Aucune donnée"),
        group = "Couverture vaccinale VAP2",
        className = "info legend custom-legend",
        layerId = "legend2"
      ) %>% 
      addLegend(
        position = "bottomright",
        colors = c("#ff0000", "#ffa500", "#ffff00", "#006400", "#a9a9a9"),
        opacity = 0.9,
        title = htmltools::HTML("
      <div style='
        background-color: white;
        padding: 10px;
        border-radius: 5px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.1);
      '>
        <strong style='color: #2c3e50;'>Couverture vaccinale VAP3</strong>
      </div>
    "),
        labels = c("< 30%", "30% - 49%", "50% - 79%", "≥ 80%", "Aucune donnée"),
        group = "Couverture vaccinale VAP3",
        className = "info legend custom-legend",
        layerId = "legend3"
      ) %>% 
      # Enhanced layers control
      addLayersControl(
        baseGroups = c("Couverture vaccinale VAP1", 
                       "Couverture vaccinale VAP2", 
                       "Couverture vaccinale VAP3"),
        overlayGroups = c("CartoDB Light", "CartoDB Dark", "Satellite"),
        position = "topright",
        options = layersControlOptions(
          collapsed = FALSE,
          autoZIndex = TRUE
        )) %>%
      htmlwidgets::onRender("
    function(el, x) {
      var legends = document.querySelectorAll('.legend');
      
      var updateLegend = function (layerName) {
        legends.forEach(function(legend) {
          legend.style.display = legend.innerText.includes(layerName) ? 'block' : 'none';
        });
      };

      updateLegend('Couverture vaccinale VAP1');  // Default legend

      this.on('baselayerchange', function(e) {
        updateLegend(e.name);
      });
    }")%>% 
      hideGroup(c("CartoDB Dark", "Satellite"))
    
    
    
  })
  
  output$evolution_national <- renderHighchart({
    
  data <-   db_vaccination_regions_cum %>% 
      select(-orgunitlevel2,-contains("coverage")) %>% 
      group_by(period_date) %>% 
      summarise_all(sum) %>% 
      ungroup() %>% 
      mutate(
        cumulative_coverage_vap1 = round(total_vap1/cumulative_target_this_month*100,1),
        cumulative_coverage_vap2 = round(total_vap2/cumulative_target_this_month*100,1),
        cumulative_coverage_vap3 = round(total_vap3/cumulative_target_this_month*100,1),
        orgunitlevel2 = "National") %>%
     # filter(period_date >= ymd(input$startMonth),period_date <= ymd(input$endMonth))%>% 
      mutate(
        period_date = as.Date(period_date))
  
  
  highchart() %>%
    hc_chart(
      style = list(fontFamily = "Inter, Arial, sans-serif"), 
      backgroundColor = "#FFFFFF", 
      crosshair = list(
        color = 'rgba(48, 48, 48, 0.8)', # Semi-transparent crosshair
        dashStyle = 'Dash',
        width = 1,
        zIndex = 5
      ),
      animation = list(duration = 1000) # Smooth initial animation
    ) %>%
    hc_title(
      text = str_glue("Couverture vaccinale cumulée VAP dans le temps - Niveau National"),
      style = list(
        fontSize = "24px", 
        fontWeight = "600",
        color = "#1A237E"  # Darker blue for better contrast
      ),
      align = "left"  # Left-aligned title for modern look
    ) %>%
    hc_subtitle(
      text = "Présentation des tendances de couverture VAP1, VAP2, et VAP3",
      style = list(
        fontSize = "16px",
        color = "#546E7A"  # Subtle gray for subtitle
      ),
      align = "left"
    ) %>%
    hc_xAxis(
      categories = data$period_date,
      title = list(
        text = "Mois",
        style = list(fontWeight = "500")
      ),
      gridLineWidth = 0.5,
      gridLineColor = "rgba(224, 224, 224, 0.5)",  # Lighter grid
      labels = list(
        style = list(fontSize = "12px", color = "#37474F"),
        rotation = -45,
        useHTML = TRUE
      ),
      crosshair = TRUE,
      tickmarkPlacement = "on"
    ) %>%
    hc_yAxis(
      title = list(
        text = "Couverture cumulée (%)",
        style = list(fontWeight = "500")
      ),
      gridLineWidth = 0.5,
      gridLineColor = "rgba(224, 224, 224, 0.5)",
      labels = list(
        format = "{value}%",
        style = list(color = "#37474F")
      ),
      min = 0,
      max = 150,
      tickInterval = 25  # Consistent tick intervals
    ) %>%
    hc_add_series(
      name = "Couverture VAP1",
      data = data$cumulative_coverage_vap1,
      type = "line",
      color = "#1976D2",  # More muted blue
      lineWidth = 2.5
    ) %>%
    hc_add_series(
      name = "Couverture VAP2",
      data = data$cumulative_coverage_vap2,
      type = "line",
      color = "#388E3C",  # More muted green
      lineWidth = 2.5
    ) %>%
    hc_add_series(
      name = "Couverture VAP3",
      data = data$cumulative_coverage_vap3,
      type = "line",
      color = "#FFA000",  
      lineWidth = 2.5
    ) %>%
    hc_tooltip(
      shared = TRUE,
      valueSuffix = "%",
      backgroundColor = "rgba(255, 255, 255, 0.98)",
      borderWidth = 0,
      borderRadius = 12,
      shadow = list(
        color= 'rgba(0, 0, 0, 0.1)',
        offsetX= 0,
        offsetY= 2,
        blur= 5
      ),
      style = list(fontSize = "14px"),
      headerFormat = '<div style="font-size: 14px; font-weight: 600; padding: 12px 12px 8px; min-width: 300px;">{point.x}</div>',
      pointFormat = paste0(
        '<div style="padding: 4px 12px; min-width: 100px;">',
        '<span style="color:{series.color}; font-size: 16px;">\u25CF </span>',
        '<span style="color:#263238; font-weight: 500;">{series.name}</span>: ',
        '<span style="color:#263238; font-weight: 600; float: right;">{point.y:.1f}%</span>',
        '</div>'
      ),
      useHTML = TRUE,
      followPointer = TRUE,
      animation = list(duration = 200)
    )%>%
    hc_plotOptions(
      line = list(
        marker = list(
          enabled = FALSE,
          symbol = "circle",
          radius = 4,
          states = list(
            hover = list(
              enabled = TRUE,
              lineWidth = 2,
              lineColor = "white",
              fillColor = "white",
              radius = 6
            )
          )
        ),
        states = list(
          hover = list(
            lineWidthPlus = 1,  
            halo = list(size =  0)  
          )
        )
      )
    ) %>%
    hc_legend(
      align = "center",
      verticalAlign = "bottom",
      layout = "horizontal",
      itemStyle = list(
        fontSize = "13px",
        fontWeight = "500",
        color = "#263238"
      ),
      itemHoverStyle = list(color = "#1A237E"),  # Darker on hover
      borderWidth = 0,
      backgroundColor = "rgba(255, 255, 255, 0.9)",
      padding = 16,
      symbolRadius = 2  # Squared legend markers
    ) %>%
    hc_credits(enabled = FALSE) %>%
    hc_exporting(
      enabled = TRUE,
      buttons = list(
        contextButton = list(
          menuItems = c("downloadPNG", "downloadJPEG", "downloadPDF", "downloadCSV"),
          symbol = 'menuball',  
          symbolStroke = '#263238'
        )
      ),
      chartOptions = list(
        plotOptions = list(
          series = list(
            animation = FALSE  # Disable animation in exported charts
          )
        )
      )
    ) %>%
    hc_boost(
      enabled = TRUE,
      useGPUTranslations = TRUE,
      usePreallocated = TRUE
    )
  
    
    
  })
  
  # Table overall
  
  output$table_overall <- renderDT({
    
    tab_join_overall <- tab_join_overall %>% 
                         mutate(period_date = as.Date(period_date)) %>% 
                         filter(period_date == ymd(input$choose_month)) %>% 
                        select(-period_date) 
    
    formatted_data <- tab_join_overall %>%
      mutate(
        evolution_vap1 = evolution_formatter(evolution_vap1),
        evolution_vap2 = evolution_formatter(evolution_vap2),
        evolution_vap3 = evolution_formatter(evolution_vap3),
        across(.cols = contains("coverage"),.fns = ~./100)
      ) %>% 
      rename(
        "Région" = "orgunitlevel2",
        "Doses VAP1 (Mois)" = "admin_doses_vap1_month",
        "Evolution VAP1" = "evolution_vap1",
        "Doses VAP1 (Cumul)" = "admin_doses_vap1_cumulative",
        "Couverture VAP1 (%)" = "cumulative_coverage_vap1",
        "Doses VAP2 (Mois)" = "admin_doses_vap2_month",
        "Evolution VAP2" = "evolution_vap2",
        "Doses VAP2 (Cumul)" = "admin_doses_vap2_cumulative",
        "Couverture VAP2 (%)" = "cumulative_coverage_vap2",
        "Doses VAP3 (Mois)" = "admin_doses_vap3_month",
        "Evolution VAP3" = "evolution_vap3",
        "Doses VAP3 (Cumul)" = "admin_doses_vap3_cumulative",
        "Couverture VAP3 (%)" = "cumulative_coverage_vap3"
      )
    
    
    # First identify which columns actually exist in the data
    datatable(
      formatted_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 't<"bottom"lip>', 
        columnDefs = list(
          list(className = 'dt-center', targets = "_all"),
          list(width = '120px', targets = "_all")
        ),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().container()).css({'font-size': '12px'});",
          "}"
        )
      ),
      class = 'cell-border stripe hover',
      escape = FALSE,
      rownames = FALSE,
      style = "bootstrap"
    ) %>% 
      formatStyle(
        c("Couverture VAP1 (%)", "Couverture VAP2 (%)", "Couverture VAP3 (%)"),
        backgroundColor = styleInterval(
          c(0.30, 0.50, 0.80),
          c('#FF4444', '#FFA500', '#FFD700', '#4CAF50')
        ),
        fontWeight = 'bold'
      ) %>% 
      formatPercentage(
        c("Couverture VAP1 (%)", "Couverture VAP2 (%)", "Couverture VAP3 (%)"),
        digits = 0
      ) %>% 
      formatStyle(
        c("Doses VAP1 (Cumul)", "Doses VAP2 (Cumul)", "Doses VAP3 (Cumul)"),
        background = styleColorBar(
          range(formatted_data$`Doses VAP1 (Cumul)`, na.rm = TRUE), 
          '#007bff'
        ),
        backgroundSize = '98% 88%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center',
        fontWeight = 'bold'
      ) %>%
      formatStyle(
        'Région',
        fontWeight = 'bold',
        backgroundColor = '#f8f9fa'
      ) %>%
      formatStyle(
        c("Evolution VAP1", "Evolution VAP2", "Evolution VAP3"),
        fontWeight = 'bold'
      ) %>%
      formatCurrency(
        c("Doses VAP1 (Mois)", "Doses VAP2 (Mois)", "Doses VAP3 (Mois)",
          "Doses VAP1 (Cumul)", "Doses VAP2 (Cumul)", "Doses VAP3 (Cumul)"),
        currency = "", 
        digits = 0
      )
    
    
  })
  
  
  # About 
  
  observeEvent(input$about_link, {
    showModal(
      modalDialog(
        title = "À propos",
        div(
          style = "padding: 20px;",
          h4("Introduction du vaccin antipaludique au Burkina Faso"),
          p("Cette application a été développée pour suivre le déploiement et la progression 
                  de la vaccination antipaludique au Burkina Faso."),
          br(),
          h5("Fonctionnalités:"),
          tags$ul(
            tags$li("Suivi en temps réel des couvertures vaccinales"),
            tags$li("Analyse comparative par région et district"),
            tags$li("Visualisation des tendances temporelles")
          ),
          br(),
          p(str_glue("Dernière mise à jour: {format(last_update, '%d %B %Y')}"))
        ),
        footer = modalButton("Fermer"),
        size = "m",
        easyClose = TRUE
      )
    )
  }) 
  
  
}
