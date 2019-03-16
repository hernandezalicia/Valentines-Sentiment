library(shiny)
library(shinyWidgets)

# Defining UI for shinyApp.


fluidPage(
  
  #App Title ---
  tags$head(HTML("<title>Valentine's Day Twitter Analysis</title>")), #Changes title of tab on browser.
  
  # Application title
  titlePanel(h1(div(strong("Valentine's Day Tweet Analysis"),
                    style="color: #CC0000; font-family: monospace;"))),
  
  
  
  
  # Creating tabs for app navigation.
  tabsetPanel(
    
    
    

    ## First tab containing read-me file and app information.
    tabPanel("README", fluid = TRUE,
             
             fluidRow(
               column(10, align = "center",
                      h3(strong("Project Information"), style ="font-family: Arial;"), br()),
               
               fluidRow(
                column(6, align = "left", offset = 1, style = "height:400px;",
                       #Adding already formatted html file that was created for readMe section.
                       includeHTML("README.txt")
                       
                ),
                column(3, offset = 1, align = "center",
                       HTML('This application was created by
                              <a href="https://www.linkedin.com/in/ahernandez93/">Alicia Hernandez</a>.
                              <br><br>
                              <b>To view code for this project and view additional projects:</b><br><br>
                            <a href="https://github.com/hernandezalicia">github.com/hernandezalicia</a>'),
                       br(), br(), br(),
                       
                       h4(strong("Random Tweet from Data Set")),
                       
                       #Creating output for random tweet each time the app is used.
                       verbatimTextOutput('randomTweet', placeholder = TRUE), br(),
                       
                       strong("Note:"), "Tweet displayed in above output has already been cleaned.",
                       "Output will only change when application is reloaded.", br(), br(),
                       
                       strong("Warning:"), "Some content in this application may not be appropriate for",
                       "all users."
                       
                  )
               )
              )
             ), #END OF FIRST TAB PANEL
    
    
    
    
    
    #Second tab containing various word clouds.
    tabPanel("Word Clouds", fluid = TRUE, br(),
             
             #Creating sidebar panel for word cloud inputs.
             sidebarPanel(
               
                              #Radio buttons for selection of type of word cloud.
               radioButtons('cloud_length', label = h4(strong("Wordcloud Type")),
                            choices = c("Unigram" = 1L,
                                        "Bigram" = 2L,
                                        "Trigram" = 3L)), br(),
               
               
               #Action button to update settings after changed.
               fluidRow(align = "center",
                 actionButton("cloud_update", strong("Generate"),
                            style='padding:14px; font-size: 16px; color:#CC0000; font-family: monospace;')), br(),
               
               
               #Setting color of all slider inputs.
               setSliderColor(c("#CC0000", "#CC0000"), c(1, 2)),   #Changes color of sliders to match layout.
               
               #Input for number of words.
               sliderInput("cloud_number", label = h4(strong("Maximum Number of Words")), min = 100, 
                           max = 500, value = 200, step = 25, width = '100%'), br(),
               
               
               #Input for minumum frequency.
               sliderInput("cloud_freq", label = h4(strong("Minimum Word Frequency")), min = 0, 
                           max = 5000, value = 500, step = 250, width = '100%'), br(),
               
               
               #Switch for TF-IDF setting.
               checkboxGroupInput("tfswitch", label = h4(strong("TF-IDF Word Cloud")), choices = c("Yes"),
                                                    selected = NULL),br(),
               strong("Note: "), "TF-IDF cloud only available in Unigram format.", br(),
               
               
               #Width of sidebar.
               width = 3),
             
             
             
             # Main panel for word cloud display.
             mainPanel(
               
               conditionalPanel(condition = "input.tfswitch != 'Yes'",
                                
                                #Adding word cloud plot to main panel.
                                plotOutput('cloudPlot', height = 650, width = 700)
                                
                                ),
               
               conditionalPanel(condition = "input.tfswitch == 'Yes'",
                                
                                #Adding conditional TF-IDF plot that will only appear when box is checked.
                                plotOutput('IDFplot', height = 650, width = 700)
                                
                                )
               
             ) #End of main panel.
             
    ), #END OF SECOND TAB PANEL
    
    
    
    
    #Third tab for commonality/comparison analysis.
    tabPanel("Comparison/Commonality", fuid = TRUE, br(),
             
             #Creating sidebar panel to distinguish between comparison/commonality.
             sidebarPanel(
               
               radioButtons("comclouds", label = h4(strong("Select Word Cloud Type")),
                            choices = c('Commonality Cloud' = 1, 'Comparison Cloud' = 2), 
                            selected = 1),
               
               
               #Adding conditional panels for explanation of each word cloud.
               conditionalPanel(condition = "input.comclouds == 1", align = 'center',
                                
                                br(), br(), h4(strong("Commonality Cloud")),
                                "This output showcases the most common words between",
                                " tweets categorized with either a positive or negative sentiment.", br()
                                
               ),
               
               
               conditionalPanel(condition = "input.comclouds == 2", align = 'center',
                                
                                br(), br(), h4(strong("Comparison Cloud")),
                                "This output showcases a comparison of the most popular words from",
                                " tweets categorized with either a positive or negative sentiment.", br()
                                
               ),
               
               
               
             width = 3), #End of sidebar panel.
             
             
             
             #Main panel for word cloud outputs and possibly basic stats.
             mainPanel(
               
               
               #Adding conditional panel for when commonality cloud is selected.
               conditionalPanel(condition = "input.comclouds == 1",
                                
                                #Commonality cloud output.
                                plotOutput('commonCloud', height = 650, width = 700)
                                
               ),
               
               
               #Adding conditional panel for when comparison cloud is selected.
               conditionalPanel(condition = "input.comclouds == 2",
                                
                                #Comparison cloud output.
                                plotOutput('compareCloud', height = 700, width = 600)
                                
               )
               
               
               ) #End of main panel.
             
             ), #END OF THIRD TAB PANEL
    
    
    
    #Fourth tab for sentiment analysis.
    tabPanel("Sentiment Analysis", fluid = TRUE, br(),
             
             
             #Creating sidebar panel for input of various sentiment features.
             sidebarPanel(
               
               selectInput("sis_type", label = h4(strong("Select Type of Analysis")), 
                           choices = list("Overall Analysis" = 1, "Sentiment Trends" = 2), 
                           selected = 1),

               
               radioButtons("sentitype", label = h4(strong("Select Sentiment Type")),
                           choices = c("Overall",
                                       "Positive" = "positive",
                                       "Negative" = "negative",
                                       "Neutral" = "neutral")),
               
               
             width = 3),
             
             
             #Main panel for sentiment outputs.
             mainPanel(
               
               conditionalPanel(condition = "input.sis_type == 2",
                                plotOutput('sentitrendPlot', height = 650, width = '100%')),
               
               conditionalPanel(condition = "input.sis_type == 1",
                                plotOutput('sentimenttotals', height = 650, width = '100%'))
               
             ) #End of main panel.
             
             ), #END OF FOURTH TAB PANEL
    
    
    
    
    #Fifth tab for emotional analysis.
    tabPanel("Emotion Analysis", fluid = TRUE, br(),
             
             #Creating sidebar panel which will be used to filter date, possibly emotions.
             sidebarPanel(
               
               selectInput('emotype', 'Select Type of Analysis',
                           choices = c("Radar Chart" = 1, "Emotion Trends" = 2), selected = 1),
               
               
               conditionalPanel(condition = "input.emotype == 1", align = 'center',
                                
                                br(), h4(strong("Radar Chart")),
                                "This output displays the frequency of emotions in the entire data set",
                                " represented on an axis starting from a single point.", br()
                                
                                ),
               
               
               conditionalPanel(condition = "input.emotype == 2",
                                
                                pickerInput('emotionSelect', 'Choose Emotions to Include',
                                            options = list(`actions-box` = TRUE), multiple = TRUE,
                                            choices = c('Anger', 'Anticipation', 'Disgust', 'Fear',
                                                        'Joy', 'Sadness', 'Surprise', 'Trust'),
                                            selected = 'Joy'), br(),
                                
                                #Using HTML function here so that only the description is centered.
                                HTML('<center><h4><b>Emotion Trends</h4></b>
                                This output showcases the trend of the percentage of daily tweets
                                which was found to represent each type of emotion.</center>'), br()
                                
                                ),
               
               width = 3),
             
             
             
             
             #Creating main panel where radar plot will be located.
             mainPanel(
               
               #Creating conditional panel to display radar chart.
               conditionalPanel(condition = "input.emotype == 1",
                                
                                #Radar Chart output.
                                chartJSRadarOutput('emoradar', height = '300',  width = '300')
                                
               ),
               
               
               #Creating conditional panel to display emotion trends.
               conditionalPanel(condition = "input.emotype == 2",
                                
                                #Plot output for emotion trends.
                                plotOutput('emotrends', height = 600, width = '100%')
                                
               )
               
             ) #End of main panel.
             
             
    ) #END OF FIFTH TAB PANEL
    
    
  ) #END OF TABSET PANEL
  
  
)