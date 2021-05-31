#####################################################################################
#
# APP (server +ui) file for the visualisation of the Eurofound COVID-19 e-survey data
#
#####################################################################################
library(shiny) 
library(plotly) 
library(shinyWidgets) 
library(shinycssloaders) 
library(shinythemes)
library(dplyr)  #1.0.4
library(purrr)
library(leaflet)
library(leafem)
library(htmlwidgets)
library(htmltools)
library(rclipboard)
library(rlist)
library(shinyjs)
library(lubridate)

###------------------------------PREPARATION--------------------------------###
#Minimun number of cases for a category to be shown (effective sample size)
threshold <- 100
threshold_flag <- 200

# This is the full dataset that is loaded into the app
load("./data/ds.Rda")


#=========================================================varinfo=============================================================================================================================
# Loading the list with all the variable info
load("./data/varinfo.Rda")

####start the app##########################################################################################################
#
# Creating vector of the names of factor variables
factors <- names(list.filter(varinfo, "factor" %in% class))

breakdown_list <- list("Country" = "B001",
                       "Gender" = "B002",
                       "Age" = "age_group",
                       "Employment status" = "emp_stat"
                       )
# Load the shapefile
load("data/shp_20.Rda")

#Function that calculates the dataset for the plot
source("make_data.R", local=TRUE)

#Function that creates the plot
source("make_plot.R", local=TRUE)

#Function that creates the map
source("make_map.R", local=TRUE)

#Function that creates the description under and above the figure
source("make_description.R", local=TRUE)

# colour scheme- insert colors for your color scheme
EF_colours <- list("#....,'#.....")

#=========================================== sections ============================
#Creating lists of variable names and labels for the question selection
sections <- c("Quality of life during COVID-19","Democracy and trust during COVID-19","Working during COVID-19","Financial situation and security during COVID-19","Support measures during COVID-19", "Quality of public services during COVID-19","Vaccination during COVID-19")

# Function to make a list of variables for the dropdown 'question' menu. 
# This also excludes variables not meant in the benchmark if benchmark mode is active
make_variable_list <- function(sections, benchmark) {
  
  variables <- lapply(sections, function(s) {
    
    labels <- list.filter(varinfo, section==s) %>%  
      # {if (benchmark==1) list.filter(.,benchmark==1) else .} %>%
      list.mapv(label, use.names = FALSE)
    
    names <- as.list(names(list.filter(varinfo, section==s))) #%>%
                             #{if (benchmark==1) list.filter(.,benchmark==1) else .}))
    
    names(names) <- labels
    
    return(names)
    
  })
  
  names(variables) <- sections
  
  return(variables)
  
}

# Because a vector is required to mark the number of round the questions was asked in. the label, and the subtexts, we need to extract these list elements
# from the varinfo list given a particular section.
# this is a function for that which also excludes any variables not meant for the 
# benchmark if benchmark mode is enabled

get_wicons <- function(sections, benchmark) {

  # if (benchmark==1) {
  #   varinfo <- list.filter(varinfo, benchmark==1)
  # }

  lapply(sections, function(s) {

    list.filter(varinfo, section==s) %>%  list.mapv(c(paste(w_icon," ",label," ",HTML("<FONT color='gray'>",subtext,"</FONT>"))), use.names = FALSE)
    }) %>%  unlist()

}

# JS function for detecting mobile
# https://g3rv4.com/2017/08/shiny-detect-mobile-browsers
mobileDetect <- function(inputId, value = 0) {
  tagList(
    singleton(tags$head(tags$script(
      '
      var isMobileBinding = new Shiny.InputBinding();
      $.extend(isMobileBinding, {
        find: function(scope) {
          return $(scope).find(".mobile-element");
          callback();
        },
        getValue: function(el) {
          return /((iPhone)|(iPod)|(iPad)|(Android)|(BlackBerry))/.test(navigator.userAgent)
        },
        setValue: function(el, value) {
        },
        subscribe: function(el, callback) {
        },
        unsubscribe: function(el) {
        }
      });
      
      Shiny.inputBindings.register(isMobileBinding);
      '
    ))),
    tags$input(id = inputId,
               class = "mobile-element",
               type = "hidden")
  )
}




###----------------------------------UI----------------------------------###
ui <- fluidPage(
  title = "Eurofound Living, working and COVID-19 survey data visualisation",
  theme = shinytheme("cerulean"), #The app fills the entire page. 
  
  # Activate shiny javascript
  useShinyjs(),
  
  #Activating the mobile detect function
  mobileDetect('isMobile'),
  
  tags$head( # refers to the head of the html document
    #This CSS code is required to change the position and
    #formatting of the messsage box you get when you click 'copy link'. 
    #Also it makes sure that the variable selection is on top of the leaflet legend
    tags$style(
      HTML("
             .shiny-notification {
                position:fixed;
                top: calc(50%);
                right: calc(0%);
                background-color: #00b462;
                color: #ffffff;
             }
            
             .leaflet-top, .leaflet-bottom {
               z-index: unset !important;
             }
             
             .leaflet-touch .leaflet-control-layers, .leaflet-touch .leaflet-bar .leaflet-control-zoom {
               z-index: 10000000000 !important;
             }")
    )),                
  
  uiOutput("leaflet_legend_font"),
  
  # Necessary for the clipboard button
  rclipboardSetup(),
  
  fluidRow( #Top fluid row
    
    #Question selection
    column(width=9, #Spanning across, which is the width of the window

                      uiOutput("var_selector")
           
    ),
    
    column(width=3,
           
           # Breakdown widget
           pickerInput(inputId = "breakdown", 
                       label = "By",
                       choices = breakdown_list[1:3],
                       width = "100%")
           
    )
    
    
  ),
  
  fluidRow(
    column(width=3,
           hidden(
             pickerInput(inputId = "time", 
                         label = "Period",
                         choices = list("2020 Apr/May"="1", "2020 Jun/Jul"="2", "2021 Feb/Mar"="3", "All (compare)"="0"),
                         selected = "3",
                         width = "100%")
           )
           
    ),
    
    column(width=6,
           
           # This dropdown only shows if its a factor variable. 
           # The user is supposed to select a category belonging 
           # to the variable selected. 
           uiOutput("cat_selector")
           
    ),
    
    column(width=3,
           
           #Conditional panel that lets you choose map or bar
           #It only shows up if country is selected as breakdown
           conditionalPanel(
             condition = "input.breakdown == 'B001' & input.time != '0'",
             pickerInput(inputId = "chart_type", label = "Chart type", 
                         choices = c("Map","Bar"),
                         selected = "Map",
                         width = "100%")
           )
           
    )
    
  ),
  
  fluidRow(
    column(width=12,
           
           #Description of the plot
           p(htmlOutput("text_above")),
           
           #benchmark text
           p(htmlOutput('benchmark_text')),
           
           #This shows the plot or map (see server)  
           uiOutput('plot_ui')
           
    )
  ),
  
  fluidRow(style="margin-top: 10px",
           
           column(width=8,
                  
                  #Description under the plot
                  textOutput("text_below")
                  
           ),
           
           column(width=4, align="right",
                  
                  #Download button and link button 
                  div(style="display:inline-block",uiOutput("downloadbutton"),width=6),
                  div(style="display:inline-block",uiOutput("clip"))
                  
           )
           
  ),
  
  #Section under the plot for filtering data
  h3("Filter data"),
  
  fluidRow(
    
    column(5,
           pickerInput(inputId = "country_filter", label = "Country", 
                       # Choices are all the levels of the country factor
                       choices = levels(ds$B001)[1:27],
                       # Selected are the EU27 by default
                       selected = levels(ds$B001)[1:27],
                       options = list(`actions-box` = TRUE,
                                      title = "Select at least one country"),
                       multiple=TRUE,
                       width = "100%")
    ),
    
    column(7,
           #splitlayout evenly spreads the elements within the column    
           splitLayout(
             
             awesomeRadio(
               inputId = "gender_filter",
               label = "Gender", 
               choices = c("All","Male","Female"),
               selected = "All"),
             
             awesomeCheckboxGroup(
               inputId = "age_filter",
               label = "Age", 
               choices = levels(ds$age_group),
               selected = levels(ds$age_group)
             )
          ),
           
           a(target="_blank", href="https://github.com/eurofound/covid_survey/",
             img(style="position: absolute; right: 0px; bottom: 0px",
                 src="https://github.githubassets.com/images/modules/logos_page/GitHub-Mark.png",
                 align="right",
                 height=20))
    )
  )
  
)


###----------------------------------SERVER----------------------------------###
server <- function(input, output, session) {
  
  output$leaflet_legend_font <- renderUI({
    
    if (input$isMobile==TRUE) {
      
      tagList(
        tags$head(
          tags$style(
            HTML('.leaflet .legend {
    
                      font-size: 8px !important;
                      
                    }')
          )
        )
      )
    }
    
  })
  
  
  # Querying the URL parameters: these are generated when a user clicks 'copy link'
  # The parameters can set the inputs so that the same chart with the same inputs
  # can be reproduced
  query <- reactive(parseQueryString(session$clientData$url_search))
  

  variables <- reactive({
    
    if (is.null(query()[["id"]])) {
      
      make_variable_list(sections, benchmark=0)
      
    } else {#But if in benchmark mode, select only the variables with benchmark=1 in varinfo
      
      make_variable_list(sections, benchmark=1)
      
    }
    
  })
  
  #same for icons
  w_icons <- reactive({

    if (is.null(query()[["id"]])) {

      get_wicons(sections, 0)

    } else {

      get_wicons(sections, 1)

   }

  })
  output$var_selector <- renderUI({
#================================== pickerinput1 ===============================================   
 #the 'content=' in pickerInput option allows to use cusom images in the dropdown menu, the image are stored in the www folder and
#the name of each image is an element in varinfo file
    pickerInput(inputId = "var", #the id the server uses to refer to this input
                label = HTML('<FONT color="black">Select question </FONT> <FONT color="gray">(The numbers indicate in how many rounds the question was asked)</FONT>'),
                choices = variables(), 
                choicesOpt = list(content=(w_icons())),
                options = list(size = 25),
                width = "100%")
                })
  
  # Creating the dropdown for selecting categories.
  # This dropdown only shows if its a factor variable. 
  # The user is supposed to select a category belonging 
  # to the variable selected and therefore only the categories belonging to 
  # that particular question should be shown. 
  output$cat_selector <-  renderUI({
    
    #require that this output is rendered only if there is an input variable
    #we are creating the input for the question at the server end so this variable is
    #not available initially. So this is to prevent a temporary error
    req(input$var)
    
    #First, check if any parameters have been defined in the URL because
    #they should supersede anything else.
    if (!is.null(query()[["cat_sel"]])) {
      
      # Get it and get rid of '_'.
      cat_sel <- query()[["cat_sel"]] %>%
        strsplit("_")
      
      #Selected categories are the ones in the URL parameters
      selected <- cat_sel[[1]]
      
      
      #If in benchmark mode, show all categories  
    } else if (!is.null(query()[["id"]])) {
      
      selected <- varinfo[[input$var]]$levels
      
    } else {#If not get the default preferred levels    
      
      selected <- varinfo[[input$var]]$default_levels
      
    }
    
    # Only do this if the selected variable is a factor variable
    # 'factors' is defined
    if (input$var %in% factors) {
      
      pickerInput(inputId = "cat_sel", 
                  label = "Select category", 
                  choices = varinfo[[input$var]]$levels,
                  selected = selected,
                  multiple = TRUE,
                  width = "100%",
                  options = list(title = "Select at least one category")) 
    }
    
  })
  
  # Force the choice of plot to be bar when comparing multiple categories
  # We cannot show a map when comparing time periods
  observeEvent(input$time, {
    
    if (input$time=="0") {
      
      updatePickerInput(session, "chart_type", selected = "Bar")
      
    } 
    
  })
  
  # Because the available categories are dependent on the inputvariable, R shiny does:
  # inputvar change -> run data + plot + update categories -> run data + plot 
  # The first step is triggered by the inputvariable change and the second step
  # by the category change. Therefore we have to stop it from running data and plotting
  # when the catogories have not been updated yet. Note: this only is an issue with
  # categorical data. Running twice doesn't really matter that much because in a split
  # second it is replaced by the second plot but still it's nicer to avoid it.
  
  #Reactive value saying that the categories have not been updated
  #This variable is used later on by requiring that it is TRUE for the data and plot
  #functions to be run.
  updated <- reactiveVal(FALSE)
  #In case of a change in the selected categories, flag the variable to TRUE
  observeEvent(input$cat_sel, updated(TRUE))
  #In case of an input variable change...
  observeEvent(input$var, {
    
    #... and if the variable is numeric ...
    if (("numeric" %in% varinfo[[input$var]]$class) |
        # ... or at least one of the selected categories are the same
        # as the categories of the last variable selected ...
        sum(input$cat_sel %in% varinfo[[input$var]]$levels)>0 ) 
      # Set updated variable to true, and if not keep it to false
    {updated(TRUE)} else {updated(FALSE)}}
    #In the second round the condition will apply and it will be set to TRUE
    
  )
  
  #Same idea but then for a switch in the time period selected
  #When you go from map in one time period to multiple time periods it will
  #automatically switch to bar chart. However, it briefly shows a map first because of the loop
  #this avoids that problem. 
  observeEvent(input$chart_type, if(input$time=="0") {updated(TRUE)})
  observeEvent(input$time,{
    
    #in case map is selected and also both time periods
    if(input$chart_type=="Map" & input$time=="0") {updated(FALSE)}
    
  })
  
  
  #Reactive value to show that the data has been updated. 
  #If the data has not been updated the dowload button and the copy link button should
  #not appear.
  data_updated <- reactiveVal(FALSE)
  
  # Here the make_data function is called ('make_data.R') for each panel
  # It returns a list of a dataframe and a data class (numeric or factor)
  # These variables are used for the plot as well as for the download data function
  data <- reactive({
    
    #Requires the the category selection has been updated
    req(updated())
    #Sets the data updated variable to false
    data_updated(FALSE)
    #Runs the make data function
    data <- make_data(input$var, input$breakdown, input$cat_sel, 
                      input$gender_filter, input$age_filter, #input$education_filter, 
                      input$country_filter, input$empstat_filter, input$sector_filter, threshold, input$time)
    #Sets the data updated to TRUE. This step does not occur if one of the conditions
    #specified in the make_data function are not met (see validates in make_data)
    data_updated(TRUE)
    
    return(data)
    
  })
  
  # Calling the make_plot and make_map function for each tab
  # Both functions are in a seperate R file
  output$map  <- renderLeaflet({
    
    #Also require the categories to be updated
    req(updated())
    #Call the make_map function with the data created
    make_map(data(), input$isMobile)
    
  })
  

 output$plot <- renderPlotly({
    req(updated())                                  
    make_plot(data(), input$isMobile)
    
  })
  
  
  # This function writes a ui part. Had to do this server side because it needs to choose
  # between a leafletOutput and a plotlyOutput
  make_plot_ui <- function(breakdown, chart_type, mapoutput, plotoutput) {    
    
    #If breakdown is country and chart type is map
    if (breakdown=="B001" &
        chart_type=="Map") {
      
      #Make the map
      leafletOutput(mapoutput, height="600px")  %>% withSpinner() 
      
      # in all other cases make the plotly with different heights
      # depending on the breakdown used
    } else {
      
      if (breakdown=="B001") {
        
        height <- "500px"
        
      } else if (breakdown!="emp_stat") {
        
        height <- "300px"
      } else if (breakdown!="F236") { 
        
        height <- "300px"
        
      } else {
        
        height <- "400px"
        
      }
      
      #and make the plotly
      plotlyOutput(plotoutput, height=height)  %>% withSpinner() 
      
    }
    
  }
  
  # Calling the plot ui functions for each tab
  # renderUI ensures its result is rendered as as ui element.
  output$plot_ui <-  renderUI(make_plot_ui(input$breakdown, input$chart_type ,"map","plot"))
  
  # Applying the make_description and make_excluded_text function to each tab
  # make_description is in 'make_description.R' and creates the little description 
  # of what is shown under the plot. This is rendered as text.
  # Storing them as reactive variables first so they can be used for the csv as well 
  text_above <- reactive({
    
    #Only appears if there is data
    validate(need(data_updated(), message=""))
    
    paste(make_description(input$cat_sel, input$var), 
          make_filter_description(input$country_filter, input$gender_filter, 
                                  input$age_filter),  
          make_question_description(input$var),
          varinfo[[input$var]]$extra_text
    )
    
  })
  
  text_below <- reactive({
    
    #Only appears if there is data
    validate(need(data_updated(), message=""))
    
    paste(make_excluded_text(data()),
          make_low_reliability_description(data()))
    
  })
  
  output$text_above <- renderText(text_above())
  
  output$text_below <- renderText(text_below())
  
  # The user has the option to download the data that was used to
  # create the plot. The make_data function that was called above prepares the data. 
  # This is used to create the plot and separately its used by the download button.
  
  output$downloadData <-  downloadHandler(
    
    filename = 'EF_data.csv',
    content = function(con) {
      
      #Determine the number of periods in the data
      periods <- length(data())
      
      if (periods==1) {#If only 1 time period
        
        #Data is the first element of the first and only list
        df <- data()[[1]][[1]]
        
      } else {#If multiple time periods
        
        data_1 <- data()[[1]][[1]]
        data_1$period <- data()[[1]][[7]]
        data_2 <- data()[[2]][[1]]
        data_2$period <- data()[[2]][[7]]
        data_3 <- data()[[3]][[1]]
        data_3$period <- data()[[3]][[7]]
        
        #this merges the datasets by adding rows
        df <- rbind(data_1, data_2, data_3) %>%
          droplevels()
        
        
      }
      
      
      # Removing commas from the data for the download data function
      colnames(df) <- gsub(",", "_", colnames(df))
      
      df <- df %>%
        # And rounding to 1 decimals
        mutate_if(is.numeric,round,digits=1)
      
      breakdowns_inverted <- split(rep(names(breakdown_list), 
                                       lengths(breakdown_list)), 
                                   unlist(breakdown_list))
      
      title <- paste0('"',"'",varinfo[[input$var]]$label, "' by ", 
                      tolower(breakdowns_inverted[[input$breakdown]]),'"')
      
      description <- 
        paste0('"',text_above(),'"') %>%
        #Removing double spaces
        gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", . , perl=TRUE) %>%
        #removing any html tags
        gsub("<.*?>", "", .) %>%
        #Change some wording
        gsub("The figure shows", "The data show", .)
      
      #Writing the data plus surrounding text to the file
      write(title, file=con)
      
      write(description,file=con, append=TRUE)
      
      write("",file=con, append=TRUE)
      
      write.table(df, con, row.names=FALSE, append=TRUE, sep=",", col.names = TRUE)
      
      write("",file=con, append=TRUE)
      
      write(paste0('"',text_below(),'"'),con,append=TRUE)
      
      write("",file=con, append=TRUE)
      
      write(paste0('"',"Cite as: Eurofound (2021), Living, working and COVID-19 dataset, Dublin, http://eurofound.link/covid19data",'"'),
            file=con, append=TRUE)
      
    }
  )
  
  #Creating the UI for the downloadbutton. 
  output$downloadbutton <- renderUI({
    
    #Only show it if there is data
    validate(
      need(data_updated(), message="")
    )
    
    downloadButton('downloadData', label = "Download data")
    
    
  })
  
  #Creating the UI for the copy link button
  output$clip <- renderUI({
    
    #Only show it if there is data
    validate(
      need(data_updated(), message="")
    )
    
    rclipButton("clipbtn", "Copy link", make_url(), icon("clipboard"))
    
  })
  
  #Message that link has been copied
  #Observe event ensures the message is shown if the button has been clicked.
  observeEvent(input$clipbtn, {
    showNotification("Link copied to clipboard",type="warning")
  })
  
  
  # What this part also does is it reads the parameters from the url, e.g. ?tab=qol
  # paramaters added to the url overrule any selections made in the app and 
  # automatically adjust them. This functionality is added so that links can be made
  # to specific plot configurations.
  
#==================================== shinyjs::show ================================== 
  # Its triggered only by a change in the URL
  observeEvent(session$clientData$url_search, {
    
    query <- query()  
    
    #Enables extra filters and breakdowns
    if (!is.null(query[["unhide"]])) {
      
      if (query[["unhide"]]=="true") {
        
        shinyjs::show("education_filter")
        shinyjs::show("empstat_filter")
        shinyjs::show("sector_filter")

        updatePickerInput(session,inputId="breakdown",
                          choices = breakdown_list)
        
      }
    }
    
    # Limit the number of variables shown to those in the section 
    # if section parameter is specified
    if (!is.null(query[["section"]])) {
      
      choices_var <- variables()[[query[["section"]]]]
      
      
      if (is.null(query()[["id"]])) {
        
        subtexts <- get_subtexts(query[["section"]], 0)
        w_icons <- get_wicons(query[["section"]], 0)
      } else {
        
        subtexts <- get_subtexts(query[["section"]], 1)
        w_icons <- get_wicons(query[["section"]], 1)
      }
      
      
    } else {
      
      choices_var <- variables()
      subtexts <- subtexts()
      w_icons <- w_icons()
      
    }
#=================================== updatepickerinput ========================    
    #Update other fields according to parameters in the URL
    #the 'content=' option in updatePickerInput allows to use custom images in the dropdwon menu
    updatePickerInput(session, "var", selected = query[["var"]],
                      choices = choices_var,
                      choicesOpt = list(content=(w_icons())))
    updatePickerInput(session, "breakdown", selected = query[["breakdown"]])
    
    if (is.null(query[["id"]])) {
      
      shinyjs::show("time")
      
    }
    
    # Always show bar if in benchmark mode
    # And always show april / may data
    # NOTE: THIS MAY NEED TO BE UPDATED IN CASE OF BENCHMARKING WAVE 2
    
    if (!is.null(query[["id"]])) {
      
      updatePickerInput(session, "chart_type", selected = "Bar")
      updatePickerInput(session, "time", selected = "1")
      
      #If not in benchmark mode, select what it in the parameter
    } else { 
      
      updatePickerInput(session, "chart_type", selected = query[["chart_type"]])
      
    }
    
    if (!is.null(query[["time"]])) {
      
      updatePickerInput(session, "time", selected = query[["time"]])
      
    }
    
    
    #Update filters. These are not dependent on the tabs
    for (filter in c("country_filter","empstat_filter")) {
      
      if (!is.null(query[[filter]])) {
        
        #parameter value must come in the form of country_country
        selection <- query[[filter]] %>%
          
          strsplit("_")
        
        updatePickerInput(session, filter, selected = selection[[1]] )
      }    
      
    }
    
    for (filter in c("gender_filter","age_filter")) {
      
      if (!is.null(query[[filter]])) {    
        
        #parameter value must come in the form of country_country
        selection <- query[[filter]] %>%
          strsplit("_")
        
        updateAwesomeCheckboxGroup(session, filter, selected = selection[[1]] )
      }    
      
    }
    
  })
  
  #This part constructs a URL with parameters that can be copied by the user 
  #to go back to the same exact plot configuration later
  
  #Function to create a parameter string
  make_parameter <- function(input,inputstring) {
    
    if (!is.null(input)) {
      
      paste0("&",inputstring,"=",input)
      
    }
    
  }
  
  # Function to create parameter string with multiple values.
  # it places an underscore between each value.
  # e.g. '&country_filter=Austria_Germany'
  make_multiple_parameter <- function(input, inputstring) {
    
    if (!is.null(input)) {
      
      text <- paste0("&",inputstring,"=")
      
      for (i in input) {
        
        text <- paste0(text,i,"_")
        
      }
      
      #Removing final underscore 
      return(substr(text,1,nchar(text)-1))
      
    }
    
  }
  
  #Function for pasting the different sets of parameters together
  #The if statemets are there to prevent a parameter from being 
  #added if the default inputs are chosen. 
  paste_parameters <- function(inputvar,inputvar_label,
                               cat_sel,cat_sel_label,
                               breakdown,breakdown_label,
                               chart_type,chart_type_label,
                               time,time_label) {
    
    paste0(
      
      make_parameter(inputvar,inputvar_label),
      
      make_multiple_parameter(cat_sel,cat_sel_label),
      
      #if selected time is not the default (3: February/March)
      {if (time!=3)
        #Then make the parameter
        make_parameter(time,time_label)
      },
      { if (breakdown!="B001") 
        make_parameter(breakdown,breakdown_label)
      },
      { if (chart_type!="Map" & time!=0)
        
        make_parameter(chart_type,chart_type_label)
        
      }
      
    )
    
  }

}

###----------------------------------RUN----------------------------------###
shinyApp(ui = ui, server = server)