library(shiny)
library(ggplot2)
#library(viridis)
#library(plotly)
library(dplyr)
library(tidyr)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

##Load Data
dataAbs = readRDS("dataAbs.RDS")
dataRel = readRDS("dataRel.RDS")

states = readRDS("states.RDS")
response = readRDS("response.RDS")

growth = readRDS("growth.RDS")

##policytext
policy_choices = c("Don't show", "Lockdown")
policy_text = c("-", "Lockdown                   ")

norm_choices = c("Nothing", "Population (thousands)")
norm_text =  c( "Confirmed cases", "Confirmed cases per thousand people")

time_choices = c("Relative date", "Absolute date")

cases_text = c("Cases: ", "Cases (per thousand): ")

# Define UI ----
ui <- fluidPage(
  tags$head(
    tags$meta(property="og:image", content="preview.png")
  ),
  tags$head(tags$script(src = "https://kit.fontawesome.com/35fe32d798.js", crossorigin="anonymous")),
  theme = "style.css",

  ###UPDATE THIS
  titlePanel("COVID-19 Growth in Europe"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("norm", "Normalize by:",
                   choices = norm_choices,
                   selected = norm_choices[1]),
      fluidRow(
        column(4, 
         radioButtons("scale", "Scale:", 
         choices = c("Linear", "Log"), 
         selected = "Linear")
        ),
        column(8,
          radioButtons("time", "Time:", 
          choices = time_choices, 
          selected = time_choices[1])     
        )
      ),
      conditionalPanel(
        condition = "input.time != \"Absolute date\"",
        uiOutput("rangeControls")
      ),
      

      checkboxGroupInput("intl", "International regions:", choices = c("Hubei", "US")),
      
      ###UPDATE THIS
      selectizeInput("bin1", "Countries (over 10,000 cases):", 
                     states$bin1, 
                     multiple=TRUE, 
                     select = head(states$bin1, n=4)),
      selectizeInput("bin2", "Countries (over 2,000 cases):", 
                     states$bin2, 
                     multiple=TRUE),
      selectizeInput("bin3", "Countries (all other):", 
                     states$bin3, 
                     multiple=TRUE),
      
      radioButtons("policy", 
                         "Government response:", 
                         choices = policy_choices, 
                         selected = c("Don't show"))),
    
    mainPanel(
      ###UPDATE THIS
      img(src='preview.png', style="display: none;"),
      p("Updated 2020-04-12 at 10:16 PM"),
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
        tabPanel("Growth Rate (%)", 
           br(),
           DT::dataTableOutput("growthTable")
        )  
      ),

      br(),
      h5("\n\nData sources:"),
      p("https://github.com/CSSEGISandData/COVID-19\n")
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
    
    if (input$norm == ("Nothing")) {
      data.plot = curFrame$noNorm
    } else {
      data.plot = curFrame$normByPop
    }


    data.plot = data.plot[data.plot$state %in% s,]
    
    return(data.plot)
  })
  
  output$rangeControls <- renderUI({
    data.plot = dataInput()
    dateRange = range(data.plot$date)
    sliderInput("relRange", "Relative date range: ", dateRange[1], 
                dateRange[2], value = dateRange, step=1)
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
    
    #head(data.plot)
    if (input$time != "Absolute date" & !is.null(input$relRange)) {
      data.plot = data.plot[data.plot$date %in% seq(input$relRange[1], input$relRange[2]),]
    }
    
    #print(data.plot)
    g = ggplot(data=data.plot, aes(x=date, y=plot, group=state)) +
      geom_line(aes(color=state)) +
      geom_point(aes(color=state)) +
      theme_classic(base_size = 17) 

    if (input$time == "Absolute date") {
      xlab = "Date"
      dates = unique(select(data.plot, date, date_label))[,1]
      labels = unique(select(data.plot, date, date_label))[,2]
      #print(head(dates))
      
      total = length(dates) - length(dates)%%5
      scale = total / 5
      
      g = g + scale_x_continuous(breaks = dates[seq(1, length(dates), scale)], labels = labels[seq(1, length(dates), scale)]) + theme(axis.text.x = element_text(angle = 90))
    } else if (input$time == "Relative date") {
      if (input$norm == "Nothing") {
        xlab = "Date relative to 500 confirmed cases*"
      } else {
        xlab = "Date relative to confirmed cases in 0.003% of population*"
      }
    } 
    
    
    g = g + labs(x=xlab, y = norm_text[norm_choices == input$norm], color = "Country")
    
    ##reformat y axis for log scale
    if (input$scale == "Log") {
      g = g + scale_y_log10()
    } 
    ##add annotations
    if (input$policy != "Don't show") {
      response.plot = responseInput()
      
      if (sum(!is.na(response.plot$date)) > 0) {
      
        annot_x = response.plot$date
        annot_y = c()
        for (i in 1:length(s)) {
          y = data.plot[data.plot$state == as.character(s[i]) & data.plot$date == annot_x[i],]$plot
          if (is.na(annot_x[i]) | length(y) == 0) {
            annot_y = c(annot_y, NA)
          } else  {
            annot_y = c(annot_y, y)
          }
        }
        #print(annot_x)
        #print(annot_y)
        
        if (sum(!is.na(annot_y)) == 0) return()
        g = g + annotate("text", 
          x = annot_x, 
          y = annot_y, 
          size=4, 
          color=gg_color_hue(length(annot_x)), 
          label=policy_text[policy_choices == input$policy]) +
          annotate("segment", x = annot_x, xend = annot_x, y = rep(0, length(annot_x)), yend = annot_y, color=gg_color_hue(length(annot_x)), linetype="dashed")
        
      }
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
