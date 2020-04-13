library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
#library(viridis)
#library(plotly)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n] 
}

##Load Data
dataAbs = readRDS("dataAbs.RDS")
dataRel = readRDS("dataRel.RDS")
dataRelCapita = readRDS("dataRelCapita.RDS")

states = readRDS("states.RDS")
response = readRDS("response.RDS")
growth = readRDS("growth.RDS")

##policytext
policy_choices = c("Don't show", "Lockdown")
policy_text = c("-", "Lockdown                   ")

norm_choices = c("Nothing", "Population (millions)")
norm_text =  c( "Confirmed cases", "Confirmed cases per million people")
cases_text = c("Cases: ", "Cases (per million): ")

time_choices = c("Relative date", "Absolute date")



# Define UI ----
ui <- fluidPage(
  tags$head(
    tags$meta(property="og:image", content="preview.png")
  ),
  tags$head(tags$script(src = "https://kit.fontawesome.com/35fe32d798.js", crossorigin="anonymous")),
  theme = "style.css",

  ###UPDATE THIS
  titlePanel("Countries with emerging COVID-19 outbreaks"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("norm", "Normalize by:",
                   choices = norm_choices,
                   selected = norm_choices[1]),
      
      fluidRow(
        column(5, 
               radioButtons("scale", "Scale:", 
                            choices = c("Linear", "Log"), 
                            selected = "Linear")
        ),
        column(7,
               radioButtons("time", "Time:", 
                            choices = time_choices, 
                            selected = time_choices[1])     
        )
      ),

      conditionalPanel(
        condition = "input.time != \"Absolute Date\"",
        uiOutput("rangeControls")
      ),
      

      checkboxGroupInput("intl", "Compare: ", choices = c("Hubei", "Iran", "Italy", "South Korea", "US"), selected="Hubei"),
      
      ###UPDATE THIS
      selectizeInput("bin1", "Countries (over 10,000 cases)", 
                     states$bin1, 
                     multiple=TRUE, 
                     select = head(states$bin1, n=3)),
      selectizeInput("bin2", "Countries (over 5,000 cases)", 
                     states$bin2, 
                     multiple=TRUE),
      selectizeInput("bin3", "Countries (all other)", 
                     states$bin3, 
                     multiple=TRUE),
      
      radioButtons("policy", 
                         "Government responses: ", 
                         choices = policy_choices, 
                         selected = c("Lockdown"))),
    
    mainPanel(
      ###UPDATE THIS
      img(src='preview.png', style="display: none;"),
      p("Updated 2020-04-12 10:09"),
      tabsetPanel(type = "tabs",
        tabPanel("Cases", 
        br(),
        div(
          style = "position:relative",
          plotOutput(outputId = "main", hover = hoverOpts("plot_hover", delay = 0)),
          br(),
          textOutput("relative_date_exp"),
          br(),
          uiOutput("hover_info")
        )
        ),
        tabPanel("Growth rate (%)", 
          br(),
          DT::dataTableOutput("growthTable")
        )
      ),
      br(),
      h5("\n\nData sources:"),
      p("https://github.com/CSSEGISandData/COVID-19"),
      p("https://data.un.org/")
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  
  dataInput <- reactive({
    s = sort(c(input$bin1, input$bin2, input$bin3, input$intl))
    if (length(s) == 0) return()
    
    if (input$time == "Relative date") {
      curFrame = dataRel
    } else {
      curFrame = dataAbs
    }
    
    if (input$norm == "Nothing") {
        data.plot  = curFrame$noNorm
    } else {
      data.plot  = curFrame$normByPop
    }
    
    data.plot = data.plot[data.plot$state %in% s,]
    
    #print(data.plot)
    ##remove dates with no data
    tmp = select(data.plot, date, state, plot) %>% spread(key=state, value=plot)
    if (length(s)==1) {
      dates.keep = tmp[sapply(tmp[,2], function(x) sum(x!=0 & !is.na(x)) > 0),1]
    } else {
      dates.keep = tmp[apply(tmp[,2:ncol(tmp)], 1, function(x) sum(x!=0 & !is.na(x)) > 0),1]
    }
    data.plot = data.plot[data.plot$date %in% dates.keep,]
    
   
  
    return(data.plot)
  })
  
  output$rangeControls <- renderUI({
    data.plot = dataInput()
    dateRange = range(data.plot$date)
    
    ##min,max of non-intl regions
    s.noIntl = sort(c(input$bin1, input$bin2))
    
    ##remove dates with no data
    data.tmp = data.plot[data.plot$state %in% s.noIntl,]
    
    if (length(s.noIntl) > 1) {
      tmp = select(data.tmp, date, state, plot) %>% spread(key=state, value=plot)
      dates.noIntl = tmp[apply(tmp[,2:ncol(tmp)], 1, function(x) sum(x!=0 & !is.na(x)) > 0),1]
      sliderInput("relRange", "Relative date range: ", dateRange[1], 
                dateRange[2], step=1, value = c(min(dates.noIntl), min(max(dates.noIntl) + 2), dateRange[2]))
    } else if (length(s.noIntl) == 1) {
      ##remove dates with no data
      data.tmp = data.plot[data.plot$state %in% s.noIntl,]
      tmp = select(data.tmp, date, state, plot) %>% spread(key=state, value=plot)
      dates.noIntl = tmp[sapply(tmp[,2], function(x) sum(x!=0 & !is.na(x)) > 0),1]
      sliderInput("relRange", "Relative date range: ", dateRange[1], 
                  dateRange[2], step=1, value = c(min(dates.noIntl), min(max(dates.noIntl) + 2), dateRange[2]))
    } else {
      sliderInput("relRange", "Relative date range: ", dateRange[1], 
                  dateRange[2], step=1, value = dateRange)
    }
    
    
  })
  
  responseInput <- reactive({
    p = input$policy
    s = sort(c(input$bin1, input$bin2, input$bin3, input$intl))
    
    
    if (input$time == "Relative date") {
      if (input$norm == "Nothing") {
        response.plot = response$rel[response$rel$response == p,]
      } else {
        response.plot = response$relCapita[response$relCapita$response == p,]
      }
    } else {
      response.plot = response$abs[response$abs$response == p,]
    }

    rownames(response.plot) = response.plot$state
    response.plot = response.plot[as.character(s),]
    rownames(response.plot) = s
    response.plot$state = s
    return(response.plot)
  })
  
  output$main <- renderPlot({
    s = sort(c(input$bin1, input$bin2, input$bin3, input$intl))
    if (length(s) == 0) return()
    data.plot = dataInput()
    
    isSubset = FALSE

    if (input$time != "Absolute date" & !is.null(input$relRange)) {
      if (max(data.plot$date) > input$relRange[2]) isSubset = TRUE
      data.plot = data.plot[data.plot$date %in% seq(input$relRange[1], input$relRange[2]),]
    }
    
    #print(unique(data.plot$state))
    g = ggplot(data=data.plot, aes(x=date, y=plot, group=state)) +
      geom_line(aes(color=state)) +
      geom_point(aes(color=state)) +
      theme_classic(base_size = 17)

    if (input$time == time_choices[2]) {
      xlab = "Date"
      dates = unique(select(data.plot, date, date_label))[,1]
      labels = unique(select(data.plot, date, date_label))[,2]
      
      total = length(dates) - length(dates)%%5
      scale = total / 5
      
      g = g + scale_x_continuous(breaks = dates[seq(1, length(dates), scale)], labels = labels[seq(1, length(dates), scale)]) + theme(axis.text.x = element_text(angle = 90))
    } else if (input$time == time_choices[1]) {
        if (input$norm == norm_choices[1]) {
          xlab = "Date relative to 500 confirmed cases*"
        } else {
          xlab = "Date relative to confirmed cases in 0.003% of population*"
        }
    } 
    g = g + labs(x=xlab, y = norm_text[norm_choices == input$norm], color = "País")
    
  
    if (input$scale == "Log") {
      g = g + scale_y_log10()
    } 
    ##add annotations
    if (input$policy != "Don't show") {
      response.plot = responseInput()
      
      #print(response.plot)
      if (sum(!is.na(response.plot$date)) > 0) {
      
        annot_x = response.plot$date
        #print(annot_x)
        annot_y = c()
        #print(data.plot)
        for (i in 1:length(s)) {
          y = data.plot[data.plot$state == as.character(s[i]) & data.plot$date == annot_x[i],]$plot
          if (is.na(annot_x[i]) | length(y) == 0) {
            annot_y = c(annot_y, NA)
          } else  {
            #print((max(data.plot$plot) / 100))
            annot_y = c(annot_y, y)
          }
        }
        #print(annot_y)
        #print(sum(!is.na(annot_y)))
        if (sum(!is.na(annot_y)) > 0) {
          #print("inside")
          g = g + 
            annotate("text",
            x = annot_x, 
            y = annot_y + (max(data.plot$plot) / 40), 
            size=4, 
            color=gg_color_hue(length(annot_x)), 
            label=policy_text[policy_choices == input$policy]) + 
            annotate("segment", x = annot_x, xend = annot_x, y = rep(0, length(annot_x)), yend = annot_y, color=gg_color_hue(length(annot_x)), linetype="dashed")
        }
      }
    }
    
    if (isSubset) {
      g = g + annotate("text", x = max(data.plot$date) - (max(data.plot$date) - min(data.plot$date)) / 14, y = -2, size=4, label="Adjust range \nfor more dates >")
    }  
    
    g
  })
  
  output$hover_info <- renderUI({
    if (input$scale == "Log") return()
    s = sort(c(input$bin1, input$bin2, input$bin3, input$intl))
    if (length(s) == 0) return()

    cols = gg_color_hue(length(s))
    names(cols) = sort(s)
      
    data.plot = dataInput()
    casesText = cases_text[norm_choices == input$norm]
    
    hover <- input$plot_hover
    point <- nearPoints(data.plot, hover, threshold = 5, maxpoints = 1)
    if (nrow(point) == 0) return(NULL)
    
    #calculate point position INSIDE the image as percent of total dimensions
    #from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left) * .47
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top) * .47
    
    # create style property fot tooltip
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; padding:2px; font-size:80%; z-index:100; background-color: ", cols[as.character(point$state)], "; ",
                    "left:", left_px, "px; top:", top_px, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
        style = style,
        p(HTML(paste0("<b> Country: </b>", point$state, "<br/>",
                      "<b>", casesText, "</b>", point$cases, "<br/>")))
      )
  })
  output$relative_date_exp <- renderText({
    if (input$time == time_choices[1]) {
      if (input$norm == norm_choices[1]) {
        "*For countries with less than 500 cases, day 0 is set to most recent date."
      } else if (input$norm == norm_choices[2]) {
        "*For countries with less than 0.003% cases per capita, day 0 is set to most recent date."
      }
    }
  })
  
  output$growthTable<- DT::renderDataTable({
    s = sort(c(input$bin1, input$bin2, input$bin3, input$intl))
    growthTable = growth[,s]
    
    if(length(s) == 1) {
      
      growthTableFt = as.data.frame(growthTable[!is.na(growthTable)])
      
      colnames(growthTableFt) = s
      rownames(growthTableFt) = rownames(growth)[!is.na(growthTable)]
      
    } else{
      growthTableFt = growthTable[apply(growthTable, 1, function(x) sum(!is.na(x)) > 0),]
    }
    
    DT::datatable(growthTableFt, options = list(
      lengthChange = FALSE,
      lengthMenu = -1,
      searching = F,
      ordering = F
    )
    )
  })
  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
