library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

##Load Data
dataAbs = readRDS("dataAbs.RDS")
dataRel = readRDS("dataRel.RDS")
dataDaily = readRDS("daily.RDS")

states = readRDS("states.RDS")
response = readRDS("response.RDS")

tests = readRDS("tests.RDS")
growth = readRDS("growth.RDS")

##policytext
policy_choices = c("Don't show", "Lockdown", "Schools closed")
policy_text = c("-", "Lockdown                   ", "Schools closed                          ")

norm_choices = c("Nothing", "Population (thousands)", "Total tests performed")
norm_text =  c( "Confirmed cases", "Confirmed cases per thousand people", "Confirmed cases / Total tests performed")

time_choices = c("Relative date",
                 "Absolute date")

cases_text = c("Cases: ", "Cases (per thousand): ", "%Cases: ")

# Define UI ----
ui <- fluidPage(
  tags$head(tags$meta(property="og:image", content="preview.png")),
  tags$head(tags$script(src = "https://kit.fontawesome.com/35fe32d798.js", crossorigin="anonymous")),
  theme = "style.css",
  titlePanel("COVID-19 Growth in U.S."),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("norm", "Normalize by:",
                   choices = norm_choices,
                   selected = norm_choices[1]),
      fluidRow(
      column(4, conditionalPanel(
        condition = "input.norm != \"Total tests performed\"",
        radioButtons("scale", "Scale:", 
                   choices = c("Linear", "Log"), 
                   selected = "Linear")
      )),
      
      column(8,conditionalPanel(
        condition = "input.norm != \"Total tests performed\"",
        radioButtons("time", "Time:", 
                     choices = time_choices, 
                     selected = time_choices[1])
      ))),
      
      conditionalPanel(
        condition = "input.time != \"Absolute date\" && input.norm != \"Total tests performed\"",
        uiOutput("rangeControls")
      ),
      
      conditionalPanel(
        condition = "input.norm != \"Total tests performed\"",
        checkboxGroupInput("intl", "International regions:", choices = c("Hubei", "Italy"))
      ),
      selectizeInput("states1", "States (over 10,000 cases):", 
                     states$bin1, 
                     multiple=TRUE, 
                     select = c(head(states$bin1, n=3), "MA")),
      selectizeInput("states2", "States (over 3,000 cases):", 
                     states$bin2, 
                     multiple=TRUE),
      selectizeInput("states3", "States (over 1,000 cases):", 
                     states$bin3, 
                     multiple=TRUE),
      selectizeInput("states4", "States (all other):", 
                     states$bin4, 
                     multiple=TRUE),
      
      radioButtons("policy", 
                         "Government response:", 
                         choices = policy_choices, 
                         selected = c("Don't show"))),
    
    mainPanel(
      
      img(src='preview.png', style="display: none;"),
      p("Updated 2020-05-02 at 2:31 PM"),
      tabsetPanel(type = "tabs",
        tabPanel("Cases (cumulative)", 
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
        tabPanel("Cases (daily) †", 
          br(),
          div(
            style = "position:relative",
            plotOutput(outputId = "daily", hover = hoverOpts("plot_hover", delay = 0)),
            br(),
            textOutput("relative_date_exp_daily"),
            br(),
            p("† Daily counts have been averaged by previous 7 days to smooth out weekday/weekend effects."),
            uiOutput("hover_daily")
          )
        ),
        tabPanel("Growth Rate (%)", 
                 br(),
                 DT::dataTableOutput("growthTable")
        ),
        tabPanel("Total tests", 
          br(),
          DT::dataTableOutput("testsTable")
        )
      ),
      
      br(),
      h5("\n\nData sources:"),
      p("https://covidtracking.com/\n
        https://github.com/CSSEGISandData/COVID-19\n
        https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States_by_population\n
        https://www.edweek.org/ew/section/multimedia/map-coronavirus-and-school-closures.html\n
        https://www.wsj.com/articles/a-state-by-state-guide-to-coronavirus-lockdowns-11584749351")
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  
  dataInput <- reactive({
    if (input$norm == norm_choices[3]) {
      s = sort(c(input$states1, input$states2, input$states3, input$states4))
    } else {
      s = sort(c(input$states1, input$states2, input$states3, input$states4, input$intl))
    }
    if (length(s) == 0) return()

    if(input$norm == norm_choices[3]) {
      data.plot  = dataAbs$normByTotalCases
    } else {
      if (input$time == "Relative date") {
        if (input$norm == "Nothing") {
          data.plot  = dataRel$noNorm
        } else {
          data.plot = dataRel$normByPop
        }
      } else {
        if (input$norm == "Nothing") {
          data.plot  = dataAbs$noNorm
        } else {
        data.plot  = dataAbs$normByPop
      }
      }
    }

    data.plot = data.plot[data.plot$state %in% s,]
    return(data.plot)
  })
  
  dailyInput <- reactive({
    if (input$norm == norm_choices[3]) {
      s = sort(c(input$states1, input$states2, input$states3, input$states4))
    } else {
      s = sort(c(input$states1, input$states2, input$states3, input$states4, input$intl))
    }
    if (length(s) == 0) return()
    
    if(input$norm == norm_choices[3]) {
      data.plot  = dataDaily$abs$normByTotalCases
    } else {
      if (input$time == "Relative date") {
        if (input$norm == "Nothing") {
          data.plot  = dataDaily$rel$noNorm
        } else {
          data.plot = dataDaily$rel$normByPop
        }
      } else {
        if (input$norm == "Nothing") {
          data.plot  = dataDaily$abs$noNorm
        } else {
          data.plot  = dataDaily$abs$normByPop
        }
      }
    }
    
    data.plot = data.plot[data.plot$state %in% s,]
    
    ##remove dates with no data
    tmp = select(data.plot, date, state, plot) %>% spread(key=state, value=plot)
    #print(tail(tmp))
    if (length(s)==1) {
      dates.keep = tmp[sapply(tmp[,2], function(x) sum(x!=0 & !is.na(x)) > 0),1]
    } else {
      dates.keep = tmp[apply(tmp[,2:ncol(tmp)], 1, function(x) sum(x!=0 & !is.na(x)) > 0),1]
      #print(tail(dates.keep))
    }
    data.plot = inner_join(data.plot, dates.keep)

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
    if (input$norm == norm_choices[3]) {
      s = sort(c(input$states1, input$states2, input$states3, input$states4))
    } else {
      s = sort(c(input$states1, input$states2, input$states3, input$states4, input$intl))
    }

    if (input$norm == norm_choices[3] || input$time == "Absolute date") {
      response.plot = response$abs[response$abs$response == p,]
    } else {
      if (input$norm == "Nothing") {
        response.plot = response$rel[response$rel$response == p,]
      } else {
        response.plot = response$relCapita[response$relCapita$response == p,]
      }
    }
      

    rownames(response.plot) = response.plot$state
    response.plot = response.plot[as.character(s),]
    rownames(response.plot) = s
    response.plot$state = s
    return(response.plot)
    
    
  })
  
  output$main <- renderPlot({
    if (input$norm == norm_choices[3]) {
      s = sort(c(input$states1, input$states2, input$states3, input$states4))
    } else {
      s = sort(c(input$states1, input$states2, input$states3, input$states4, input$intl))
    }
    if (length(s) == 0) return()
    data.plot = dataInput()
    
    #head(data.plot)
    if (input$time != "Absolute date" & !is.null(input$relRange)) {
      data.plot = data.plot[data.plot$date %in% seq(input$relRange[1], input$relRange[2]),]
    }
    
    g = ggplot(data=data.plot, aes(x=date, y=plot, group=state)) +
      geom_line(aes(color=state)) +
      geom_point(aes(color=state)) +
      theme_classic(base_size = 17) 
    
    
    ##reformt x axis for absolute dates
    
    if (input$norm == norm_choices[3] | input$time == "Absolute date") {
      xlab = "Date"
      dates = sort(unique(data.plot$date))
      labels = sort(unique(data.plot$date_label))
      
      if (length(dates)> 0) {
        total = length(dates) - length(dates)%%5
        scale = total / 5
      
        g = g + 
          scale_x_continuous(breaks = dates[seq(1, length(dates), scale)], labels = labels[seq(1, length(dates), scale)]) + 
          theme(axis.text.x = element_text(angle = 90))
      }
    } else if (input$time == "Relative date") {
      if (input$norm == "Nothing") {
        xlab = "Date relative to 500 confirmed cases*"
      } else {
        xlab = "Date relative to confirmed cases in 0.003% of population*"
      }
    } 
    
    ##reformat y axis for log scale
    if (input$scale == "Log") {
      g = g + scale_y_log10()
    } 
    
    g = g + labs(x=xlab, y = norm_text[norm_choices == input$norm], color = "State")
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
        
        #print(head(response.plot))
        #print(annot_x)
        #print(annot_y)
        
        if (sum(!is.na(annot_y)) == 0) return()
        g = g + annotate("text", 
          x = annot_x, 
          y = annot_y + (max(data.plot$plot) / 40), 
          size=4, 
          color=gg_color_hue(length(annot_x)), 
          label=policy_text[policy_choices == input$policy]) + 
          annotate("segment", x = annot_x, xend = annot_x, y = rep(min(data.plot$plot), length(annot_x)), yend = annot_y, color=gg_color_hue(length(annot_x)), linetype="dashed")
        
      }
    }
    
    g
  })
  
  output$hover_info <- renderUI({
    
    if (input$scale == "Log") return()
    if (input$norm == norm_choices[3]) {
      s = sort(c(input$states1, input$states2, input$states3, input$states4))
    } else {
      s = sort(c(input$states1, input$states2, input$states3, input$states4, input$intl))
    }
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
        p(HTML(paste0("<b> State: </b>", point$state, "<br/>",
                      "<b>", casesText, "</b>", point$cases, "<br/>")))
      )
  })
  
  output$hover_daily <- renderUI({
    
    if (input$scale == "Log") return()
    if (input$norm == norm_choices[3]) {
      s = sort(c(input$states1, input$states2, input$states3, input$states4))
    } else {
      s = sort(c(input$states1, input$states2, input$states3, input$states4, input$intl))
    }
    if (length(s) == 0) return()
    
    cols = gg_color_hue(length(s))
    names(cols) = sort(s)
    
    data.plot = dailyInput()
    casesText = cases_text[norm_choices == input$norm]
    
    hover <- input$plot_hover
    point <- nearPoints(data.plot, hover, threshold = 5, maxpoints = 1)
    if (nrow(point) == 0) return(NULL)
    
    #calculate point position INSIDE the image as percent of total dimensions
    #from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left) * .43
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top) * .55
    
    # create style property fot tooltip
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; padding:2px; font-size:80%; z-index:100; background-color: ", cols[as.character(point$state)], "; ",
                    "left:", left_px, "px; top:", top_px, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> State: </b>", point$state, "<br/>",
                    "<b>", casesText, "</b>", point$cases, "<br/>")))
    )
  })
  
  
  output$relative_date_exp <- renderText({
    if (input$time == time_choices[1]) {
      if (input$norm == norm_choices[1]) {
        "*For states with less than 500 cases, day 0 is set to most recent date."
      } else if (input$norm == norm_choices[2]) {
        "*For states with less than 0.003% cases per capita, day 0 is set to most recent date."
      }
    }
    
  })
  output$relative_date_exp_daily <- renderText({
    if (input$time == time_choices[1]) {
      if (input$norm == norm_choices[1]) {
        "*For states with less than 500 cases, day 0 is set to most recent date."
      } else if (input$norm == norm_choices[2]) {
        "*For states with less than 0.003% cases per capita, day 0 is set to most recent date."
      }
    }
    
  })
  
  
  
  output$daily <- renderPlot({
    if (input$norm == norm_choices[3]) {
      s = sort(c(input$states1, input$states2, input$states3, input$states4))
    } else {
      s = sort(c(input$states1, input$states2, input$states3, input$states4, input$intl))
    }
    if (length(s) == 0) return()
    data.plot = dailyInput()
    
    if (input$time != "Absolute date" & !is.null(input$relRange)) {
      data.plot = data.plot[data.plot$date %in% seq(input$relRange[1], input$relRange[2]),]
    }
    
    g = ggplot(data=data.plot, aes(x=date, y=plot, group=state)) +
      geom_line(aes(color=state)) +
      geom_point(aes(color=state)) +
      theme_classic(base_size = 17) 
    
    
    ##reformt x axis for absolute dates
    
    if (input$norm == norm_choices[3] | input$time == "Absolute date") {
      xlab = "Date"
      dates = sort(unique(data.plot$date))
      labels = sort(unique(data.plot$date_label))
      
      if (length(dates)> 0) {
        total = length(dates) - length(dates)%%5
        scale = total / 5
        
        g = g + 
          scale_x_continuous(breaks = dates[seq(1, length(dates), scale)], labels = labels[seq(1, length(dates), scale)]) + 
          theme(axis.text.x = element_text(angle = 90))
      }
    } else if (input$time == "Relative date") {
      if (input$norm == "Nothing") {
        xlab = "Date relative to 500 confirmed cases*"
      } else {
        xlab = "Date relative to confirmed cases in 0.003% of population*"
      }
    }
    ##reformat y axis for log scale
    if (input$scale == "Log") {
      g = g + scale_y_log10()
    } 
    
    ##add annotations
    if (input$policy != "Don't show") {
      response.plot = responseInput()
      #print(response.plot)
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
        
        #print(head(response.plot))
        #print(annot_x)
        #print(annot_y)
        
        if (sum(!is.na(annot_y)) == 0) return()
        g = g + annotate("text", 
                         x = annot_x, 
                         y = annot_y   + (max(data.plot$plot, na.rm=T) / 40), 
                         size=4, 
                         color=gg_color_hue(length(annot_x)), 
                         label=policy_text[policy_choices == input$policy]) + 
          annotate("segment", x = annot_x, xend = annot_x, y = rep(min(data.plot$plot, na.rm=T), length(annot_x)), yend = annot_y, color=gg_color_hue(length(annot_x)), linetype="dashed")
        
      }
    }
    
    
    g = g + labs(x=xlab, y = norm_text[norm_choices == input$norm], color = "State")

    
    g
  })
  
  
  output$growthPlot <- renderPlot({
    s = sort(c(input$states1, input$states2, input$states3, input$states4, input$intl))
    
    data.plot = dataAbs$growth
    data.plot = data.plot[data.plot$state %in% s,]
    keep = unique(data.plot[!is.na(data.plot$plot),2])
    
    data.plot = data.plot[data.plot$date %in% seq(60, max(keep)),]
    g = ggplot(data=data.plot, aes(x=date, y=plot, group=state)) +
      geom_line(aes(color=state)) +
      geom_point(aes(color=state)) +
      theme_classic(base_size = 17) 
    g
    
  })
  output$growthTable<- DT::renderDataTable({
    s = sort(c(input$states1, input$states2, input$states3, input$states4, input$intl))
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
  
  
  output$testsTable<- DT::renderDataTable({
    s = sort(c(input$states1, input$states2, input$states3, input$states4))
    curTable = tests[s,]
    DT::datatable(curTable, options = list(
      lengthChange = FALSE,
      lengthMenu = -1,
      searching = F
      )
    )
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
