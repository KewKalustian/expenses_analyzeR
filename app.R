library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(shinyjs)
library(shinyWidgets)



js_magic <- paste0(
  "table.on('key', function(e, datatable, key, cell, originalEvent){
    var targetName = originalEvent.target.localName;
    if(key == 13 && targetName == 'body'){
      $(cell.node()).trigger('dblclick.dt');
    }
  });
  table.on('keydown', function(e){
    var keys = [9,13,37,38,39,40];
    if(e.target.localName == 'input' && keys.indexOf(e.keyCode) > -1){
      $(e.target).trigger('blur');
    }
  });
  table.on('key-focus', function(e, datatable, cell, originalEvent){
    var targetName = originalEvent.target.localName;
    var type = originalEvent.type;
    if(type == 'keydown' && targetName == 'input'){
      if([9,37,38,39,40].indexOf(originalEvent.keyCode) > -1){
        $(cell.node()).trigger('dblclick.dt');
      }
    }
  });"
)




# Function to manipulate the table
initial_data <- data.frame(
  Ausgabenposition = as.character("Beispiel-Miete"),
  Kosten = 1000,
  stringsAsFactors = FALSE
)





ui <- dashboardPage(

  dashboardHeader(title = "Ausgabenrechner"),
  dashboardSidebar(
  #  fluidPage(   
    numericInput("gehalt_1", "Gehalt 1:", value = 2000, min = 0),
    numericInput("gehalt_2", "Gehalt 2:", value = 0, min = 0),
    numericInput("invest", "Investment-Anteil in %", min = 1, max = 100, value = 70), #fgColor = "#1c9e77",
            #  immediate = T, displayPrevious = F),
    numericInput("sparen", "Privater Spar-Anteil in %", min = 1, max = 100, value = 20), #fgColor = "#d95f02", 
            #  immediate = T, displayPrevious = F),
    numericInput("spaß", "Spaß-Geld-Anteil in %",  min = 1, max = 100, value = 10), #fgColor = "#7470b2", 
          #    immediate = T, displayPrevious = F),
    #),
    hr(),
    fluidPage(   
      downloadButton("download_data", "Tabelle runterladen")),
    fileInput(
      "upload_data",
      "Vorhandene Tabelle hochladen und bearbeiten.",
      buttonLabel = "Laden...",
      placeholder = "Keine Datei ausgewählt"
    )
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    fluidPage(
      tags$head(
        tags$meta(name = "viewport", content = "width=1920, initial-scale=1")
      ),
  
      
      tags$head(tags$style(
        HTML(
          "#download_data {background-color: #222d32; width: 100%;}
          td.focus {outline: 1px solid #222d32 !important;
         }"
        ))),
      
      box(
        width = 12,
        DTOutput("editable_table"),
        actionButton("add_row", "Position hinzufügen"),
        actionButton("delete_row", "Ausgewählte Position(en) löschen")
      ),
      box(
        width = 12,
        solidHeader = FALSE,
        title = "Ausgabenverteilung",
        collapsed = TRUE,
        collapsible = TRUE,
        plotOutput("ausgaben_plot"),
      )
    
    )
  )
)

server <- function(input, output, session) {
  
  
  plot_function_gehalt_1 <- reactive({
    
    req(input$gehalt_1)
    
    rechner_gehalt_1() %>%
      
      mutate(prop = Ausgaben / sum(Ausgaben),
             ymax = cumsum(prop),
             ymin = c(0, head(ymax, n = -1)),
             lab_pos =  ymax - (.5 * prop)) -> feats
    
    feats %>% 
      ggplot(aes(ymax = ymax,  ymin = ymin, 
                 xmax = 4, xmin = 3, fill = Position)) +
      
      geom_rect() +
      
      geom_label(aes(
        
        x = 2.,
        y = .5, 
        label = paste0("Verfügbares Einkommen\n(Netto ",
                       round(Gehalt) ," € abzgl. Fixkosten\nvon ",
                       round(Fixkosten/2,2), " €): \n" , 
                       round(`Verfügbares.Einkommen`,2), " €")), 
        col = "black", fill = "white",
        show.legend = F,size=3.1) +
      
      geom_label(aes(
        x = 3.3*1.03,
        y = lab_pos,
        label = paste0(round(prop * 100, 2), " %\n", 
                       "[",paste0(round(Ausgaben,2), " €]"))),
        col = "white", show.legend = F) +
      
      coord_polar(theta = "y", start = 0) + 
      
      xlim(c(2, 4)) +
      scale_fill_brewer("Position: ",palette = "Dark2") +
      labs(title = "Gehalt 1")+
      theme_void(base_size = 10
                 
      ) +
      theme(legend.position = "right") -> p
    
    return(p)
  })
  
  
  plot_function_gehalt_2 <- reactive({
    
    req(input$gehalt_2)
    
    rechner_gehalt_2() %>%
      
      mutate(prop = Ausgaben / sum(Ausgaben),
             ymax = cumsum(prop),
             ymin = c(0, head(ymax, n = -1)),
             lab_pos =  ymax - (.5 * prop)) -> feats
    
    feats %>% 
      ggplot(aes(ymax = ymax,  ymin = ymin, 
                 xmax = 4, xmin = 3, fill = Position)) +
      
      geom_rect() +
      
      geom_label(aes(
        
        x = 2.,
        y = .5, 
        label = paste0("Verfügbares Einkommen\n(Netto ",
                       round(Gehalt) ," € abzgl. Fixkosten\nvon ",
                       round(Fixkosten/2,2), " €): \n" , 
                       round(`Verfügbares.Einkommen`,2), " €")), 
        col = "black", fill = "white",
        show.legend = F,size=3.1) +
      
      geom_label(aes(
        x = 3.3*1.03,
        y = lab_pos,
        label = paste0(round(prop * 100, 2), " %\n", 
                       "[",paste0(round(Ausgaben,2), " €]"))),
        col = "white", show.legend = F) +
      
      coord_polar(theta = "y", start = 0) + 
      
      xlim(c(2, 4)) +
      scale_fill_brewer("Position: ",palette = "Dark2") +
      labs(title = "Gehalt 2")+
      theme_void(base_size = 10
                 
      ) +
      theme(legend.position = "right") -> p
    
    return(p)
  })
  
  
  
  
  
  
  plot_function_together <- reactive({
    
    req(input$gehalt_1 || input$gehalt_2)
    
    rechner() %>%
      
      mutate(prop = Ausgaben / sum(Ausgaben),
             ymax = cumsum(prop),
             ymin = c(0, head(ymax, n = -1)),
             lab_pos =  ymax - (.5 * prop)) -> feats
    
    feats %>% 
      ggplot(aes(ymax = ymax,  ymin = ymin, 
                 xmax = 4, xmin = 3, fill = Position)) +
      
      geom_rect() +
      
      geom_label(aes(
        
        x = 2.,
        y = .5, 
        label = paste0("Verfügbares Einkommen\n(Netto ",
                       round(Gehalt) ," € abzgl. Fixkosten\nvon ",
                      round(Fixkosten,2), " €): \n" , 
                       round(`Verfügbares.Einkommen`,2), " €")), 
        col = "black", fill = "white",
        show.legend = F,size=3.1) +
      
      geom_label(aes(
        x = 3.3*1.03,
        y = lab_pos,
        label = paste0(round(prop * 100, 2), " %\n", 
                       "[",paste0(round(Ausgaben,2), " €]"))),
        col = "white", show.legend = F) +
      
      coord_polar(theta = "y", start = 0) + 
      
      xlim(c(2, 4)) +
      scale_fill_brewer("Position: ",palette = "Dark2") +
      labs(title = "Gesamtes HHN")+
      theme_void(base_size = 10
                 
      ) +
      theme(legend.position = "right") -> p
    
    return(p)
  })
  
  
  
  
  
  rechner <- reactive({
    req(input$gehalt_1 , input$gehalt_2)
    
    x <- ifelse(input$gehalt_2==0, input$gehalt_1, input$gehalt_1 + input$gehalt_2)
    y <- sum(data()$Kosten)
    
    verfügbares_einkommen <- x - y
    invest <- verfügbares_einkommen * input$invest / 100
    privat_sparen <- verfügbares_einkommen * input$sparen / 100
    spaß_geld <- verfügbares_einkommen * input$spaß / 100
    
    data <- data.frame(
      Gehalt = x,
      `Verfügbares Einkommen` = verfügbares_einkommen,
      aufteilung_prop = abs((x - y) - x) / x * 100,
      `Position` = c("Invest", "Private Sparerei", "Spaß-Geld"),
      Ausgaben = c(invest, privat_sparen, spaß_geld),
      Fixkosten = y
    )
    return(data)
  })
  #
  
  
  
  rechner_gehalt_1 <- reactive({
    req(input$gehalt_1)
    
    x <- input$gehalt_1
    y <- sum(data()$Kosten)/2
    
    verfügbares_einkommen <- x - y
    invest <- verfügbares_einkommen * input$invest / 100
    privat_sparen <- verfügbares_einkommen * input$sparen / 100
    spaß_geld <- verfügbares_einkommen * input$spaß / 100
    
    data <- data.frame(
      Gehalt = x,
      `Verfügbares Einkommen` = verfügbares_einkommen,
      aufteilung_prop = abs((x - y) - x) / x * 100,
      `Position` = c("Invest", "Private Sparerei", "Spaß-Geld"),
      Ausgaben = c(invest, privat_sparen, spaß_geld),
      Fixkosten = y
    )
    return(data)
  })
  
  
  
  rechner_gehalt_2 <- reactive({
    
    req(input$gehalt_2)
    
    x <- input$gehalt_2
    y <- sum(data()$Kosten)/2
    
    verfügbares_einkommen <- x - y
    invest <- verfügbares_einkommen * input$invest / 100
    privat_sparen <- verfügbares_einkommen * input$sparen / 100
    spaß_geld <- verfügbares_einkommen * input$spaß / 100
    
    data <- data.frame(
      Gehalt = x,
      `Verfügbares Einkommen` = verfügbares_einkommen,
      aufteilung_prop = abs((x - y) - x) / x * 100,
      `Position` = c("Invest", "Private Sparerei", "Spaß-Geld"),
      Ausgaben = c(invest, privat_sparen, spaß_geld),
      Fixkosten = y
    )
    return(data)
  })
  
  
  
  
  observe({
    # Optimization logic
    optimize_sliders <- function(x) {
      penalty <- abs(sum(x) - 100)
      return(penalty)
    }
    
    # Perform optimization when any input changes
    observeEvent(c(input$invest, input$sparen, input$spaß), {
      # Stop optimization when the sum reaches or exceeds 100
      if (sum( c(input$invest, input$sparen, input$spaß) ) == 100) {
        return()
      }
      
      # Update reactive values
      values <- c(input$invest, input$sparen, input$spaß)
      optimized_values <- optim(values, optimize_sliders, method = "L-BFGS-B", lower = rep(1, 3), upper = rep(100, 3))$par
      
      # Update sliders with optimized values
      updateNumericInput(session, inputId = "invest", value = round(optimized_values[1]))
      updateNumericInput(session, inputId = "sparen", value = round(optimized_values[2]))
      updateNumericInput(session, inputId = "spaß", value = round(optimized_values[3]))
 
    })
  })
  
  data <- reactiveVal(initial_data)
  
  observeEvent(input$add_row, {
    new_row <- isolate({
      data() %>%
        add_row(Ausgabenposition = "", Kosten = 0) 
    })
    data(new_row)
  })
  
  output$editable_table <- renderDT({
    
    datatable(
      data(), 
      rownames = TRUE,
      editable = TRUE,
      callback = JS(js_magic),
      extensions = "KeyTable",
      escape = F,
      options = list(
      pageLength = 20,
      fillContainer = T,
      autoHideNavigation=T,
      keys=T
      )
    )
  })
  
  
  
  
      observeEvent(input$editable_table_cell_edit, {
          info <- input$editable_table_cell_edit
          data_edit <- isolate(data())
          data_edit[info$row, info$col] <- info$value
          data(data_edit)
      })
  
  
      observeEvent(input$delete_row, {
        selected_rows <- input$editable_table_rows_selected
        if (length(selected_rows) > 0) {
          data_edit <- data()
          data_edit <- data_edit[-selected_rows, ]
          data(data_edit)
        }  }) 
      
  output$ausgaben_plot <- renderPlot({

    if (input$gehalt_1>0 && input$gehalt_2 >0) {
    
      p_gehalt_1 <- plot_function_gehalt_1()  
      p_gehalt_2 <- plot_function_gehalt_2()  
      
      p_zusammen <-  plot_function_together() 
      
      
      ggpubr::ggarrange(p_gehalt_1, p_zusammen, p_gehalt_2, ncol =3, common.legend = TRUE, legend="bottom")
      
    } else {  plot_function_together()  }
  })

  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("Ausgabenliste_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      slider_values <- data.frame(
        Gehalt_1 = input$gehalt_1,
        Gehalt_2 = input$gehalt_2,
        Invest_Perc = input$invest,
        Sparen_Perc = input$sparen,
        Spaß_Perc = input$spaß
      )
      sliders_and_data <- cbind(slider_values, data())
      write.csv(sliders_and_data, file, row.names = FALSE)
    }
  )
  
  
  observeEvent(input$upload_data, {
    file_data <- read.csv(input$upload_data$datapath)
    updateNumericInput(session, "gehalt_1", value = file_data$Gehalt_1[1])
    updateNumericInput(session, "gehalt_2", value = file_data$Gehalt_2[1])
    updateSliderInput(session, "invest", value = file_data$Invest_Perc[1])
    updateSliderInput(session, "sparen", value = file_data$Sparen_Perc[1])
    updateSliderInput(session, "spaß", value = file_data$Spaß_Perc[1])
    data(file_data[, !(names(file_data) %in% c("Gehalt_1", "Gehalt_2", "Invest", "Sparen", "Spaß"))])
  })
}


shinyApp(ui = ui, server = server)
