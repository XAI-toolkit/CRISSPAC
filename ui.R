shinyUI(
  # Home item navigation page ----
  dashboardPage(
    skin = "red",
    dashboardHeader(title = 'CRISSPAC', titleWidth = 310),
    dashboardSidebar(
      width = 310,
      sidebarMenu(
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem(
          "Descriptive",
          tabName = "descriptive",
          icon = icon("bar-chart"),
          menuSubItem("History", tabName = "historical"),
          menuSubItem("Entry", tabName = "entry"),
          menuSubItem("Biochemical", tabName = "biochemical"),
          menuSubItem("Complete Blood Count", tabName = "blood"),
          menuSubItem("Differential", tabName = "differential")
        ),
        menuItem(
          "Exploratory",
          tabName = "exploratory",
          icon = icon("search-plus"),
          menuSubItem("History", tabName = "historicalExp"),
          menuSubItem("Entry", tabName = "entryExp"),
          menuSubItem("Biochemical", tabName = "biochemicalExp"),
          menuSubItem("Complete Blood Count", tabName = "bloodExp"),
          menuSubItem("Differential", tabName = "differentialExp")
        ),
        menuItem(
          "Inferential",
          tabName = "hypothesis",
          icon = icon("users"),
          menuSubItem("History", tabName = "historicalHyp"),
          menuSubItem("Entry", tabName = "entryHyp"),
          menuSubItem("Biochemical", tabName = "biochemicalHyp"),
          menuSubItem("Complete Blood Count", tabName = "bloodHyp"),
          menuSubItem("Differential", tabName = "differentialHyp")
        ),
        menuItem(
          "Predictive",
          tabName = "prediction",
          icon = icon("line-chart"),
          menuSubItem("Feature Selection", tabName = "featureselectiongess"),
          menuSubItem("Prediction", tabName = "predictiongess")
        )
      )
    ),
    # dashboard body  ----
    dashboardBody(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
      useShinyjs(),
      tabItems(
        # welcome page   ----
        tabItem(tabName = "home",
                fluidRow(align = "justify",
                         column(1,br()),
                         column(10,
                                br(),
                                wellPanel(
                                  HTML("<p>The <b>C</b>oronary artery disease <b>RI</b>sk-stratification <b>S</b>yntax <b>S</b>core <b>P</b>redictive <b>A</b>lgorithm <b>C</b>alculator (<b>CRISSPAC</b>) is  
                                         an open-source and academic on-going research software dedicated to a risk-stratification Machine Learning Framework for the prediction 
                                         of patients with suspected <b>Coronary Artery Disease (CAD)</b>. 
                                         The aim of <b>CRISSPAC </b> is to provide a unified data-driven ML solution  
                                         (i.e. <b>Descriptive</b>, <b>Exploratory</b>, <b>Inferential</b> and <b>Predictive Analytics</b>) 
                                         aggregating information and 
                                         extracting knowledge from clinical records collected from patients in Greece.</p>"))),
                         column(1,br())),
                fluidRow(align = "justify",
                         column(1, br()),
                         column(5,
                                wellPanel(
                                  HTML("<h3><b>Modules</b></h3>
                                       <p><b>CRISSPAC</b> is ready to perform
                                       all the data analysis with a fully automated manner offering the following modules for
                                       the clinical records of patients:
                                       <ul> 
                                       <li>Descriptive Analytics</li>
                                       <li>Exploratory Analytics </li> 
                                       <li>Inferential Analytics </li>
                                       <li>Predictive Analytics </li>
                                       </ul>
                                       </p>"))),
                         column(5,
                                wellPanel(
                                  HTML("<h3><b>Development Team</b></h3><p> <b>Developer</b>: Dr. Nikolaos Mittas</p>
                                  <h3><b>Contact Info</b></h3>
                                  <p> <b>Dr. Nikolaos Mittas</b>
                                  <br>
                                  e-mail: <b>nmittas@chem.ihu.gr</b></p>
                                  <br>"))))),
        
        # Descriptive Item (ui-side) ----
        
        # Descriptive results for History records ----
        tabItem(
          tabName = "historical",
          fluidRow(
            box(
              title = "Info",
              width = 12,
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              HTML(
                "<blockquote><h3>Description</h3></blockquote>
            <p> The Descriptive <b> History </b> panel provides a descriptive statistics report
            for the history records of patients.</p>"
              )
            )
          ),
          fluidRow(plotOutput('HistoryBarplots')),
          fluidRow(plotOutput('HistoryHistogram')),
          fluidRow(width = NULL,
                   DT::dataTableOutput('HistoryDescriptiveTable'))
        ),
        
        # Descriptive results for Entry records ----
        tabItem(
          tabName = "entry",
          fluidRow(
            box(
              title = "Info",
              width = 12,
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              HTML("<blockquote><h3>Description</h3></blockquote> 
            <p> The Descriptive <b> Entry </b> panel provides a descriptive statistics report
            for the entry records of patients.</p>")
            )
          ),
          fluidRow(
            plotOutput('EntryBarplots')
          ),
          fluidRow(
            plotOutput('EntryHistogram')
          ),
          fluidRow(width = NULL,
                   DT::dataTableOutput('EntryDescriptiveTable'))
        ), 
        
        # Descriptive results for Biochemical records ----
        tabItem(
          tabName = "biochemical",
          fluidRow(
            box(
              title = "Info",
              width = 12,
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              HTML(
                "<blockquote><h3>Description</h3></blockquote>
            <p> The Descriptive <b> Biochemical </b> panel provides a descriptive statistics report
            for the biochemical records of patients.</p>"
              )
            )
          ),
          fluidRow(plotOutput('BiochemicalHistogram')),
          fluidRow(width = NULL,
                   DT::dataTableOutput('BiochemicalDescriptiveTable'))
        ),
        
        # Descriptive results for Complete Blood Count records ----
        tabItem(
          tabName = "blood",
          fluidRow(
            box(
              title = "Info",
              width = 12,
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              HTML(
                "<blockquote><h3>Description</h3></blockquote>
            <p> The Descriptive <b> Complete Blood Count </b> panel provides a descriptive statistics report
            for the complete blood count records of patients.</p>"
              )
            )
          ),
          fluidRow(plotOutput('BloodHistogram')),
          fluidRow(width = NULL,
                   DT::dataTableOutput('BloodDescriptiveTable'))
        ),
        
        # Descriptive results for Differential records ----
        tabItem(tabName = "differential",
                fluidRow(
                  box(
                    title = "Info",
                    width = 12,
                    status = "info",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    HTML(
                      "<blockquote><h3>Description</h3></blockquote>
            <p> The  Descriptive <b> Differential </b> panel provides a descriptive statistics report
            for the differential records of patients.</p>"
                    )
                  )
                ),
                fluidRow(plotOutput('DifferentialBarplots'))),   

        # Exploratory Item (server-side) ----
        
        # Exploratory results for History records ----
        tabItem(
          tabName = "historicalExp",
          fluidRow(
            box(
              title = "Info",
              width = 12,
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              HTML(
                "<blockquote><h3>Description</h3></blockquote>
          <p> The  Exploratory <b> History </b> panel provides an exploratory analysis report
          for SYNTAX score regarding the history records of patients.</p>"
              )
            )
          ),
          fluidRow(plotOutput('HistoryExpBoxplots')),
          fluidRow(width = NULL,
                   DT::dataTableOutput('HistoryExploratoryTable'))
        ),  
        
        # Exploratory results for Entry records ----
        tabItem(
          tabName = "entryExp",
          fluidRow(
            box(
              title = "Info",
              width = 12,
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              HTML(
                "<blockquote><h3>Description</h3></blockquote>
          <p> The  Exploratory <b> Entry </b> panel provides an exploratory analysis report
          for SYNTAX score regarding the entry records of patients.</p>"
              )
            )
          ),
          
          fluidRow(plotOutput('EntryExpBoxplots')),
          fluidRow(plotOutput('EntryExpCorrplot')),
          fluidRow(width = NULL,
                   DT::dataTableOutput('EntryExploratoryTable'))
          
        ), 
        
        # Exploratory results for Biochemical records ----
        tabItem(tabName = "biochemicalExp",
                fluidRow(
                  box(
                    title = "Info",
                    width = 12,
                    status = "info",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    HTML(
                      "<blockquote><h3>Description</h3></blockquote>
          <p> The  Exploratory <b> Biochemical </b> panel provides an exploratory analysis report
          for SYNTAX score regarding the biochemical records of patients.</p>"
                    )
                  )
                ),
                fluidRow(plotOutput('BiochemicalExpCorrplot'))), 
        
        # Exploratory results for Complete Blood Count records ----
        tabItem(tabName = "bloodExp",
                fluidRow(
                  box(
                    title = "Info",
                    width = 12,
                    status = "info",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    HTML(
                      "<blockquote><h3>Description</h3></blockquote>
          <p> The  Exploratory <b> Complete Blood Count </b> panel provides an exploratory analysis report
          for SYNTAX score regarding the complete blood count records of patients.</p>"
                    )
                  )
                ),
                
                fluidRow(plotOutput('CBCExpCorrplot'))),
        
        # Exploratory results for Differential records ----
        tabItem(
          tabName = "differentialExp",
          fluidRow(
            box(
              title = "Info",
              width = 12,
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              HTML(
                "<blockquote><h3>Description</h3></blockquote>
          <p> The  Exploratory <b> Differential </b> panel provides an exploratory analysis report
          for SYNTAX score regarding the differential records of patients.</p>"
              )
            )
          ),
          
          fluidRow(plotOutput('DifferentialExpBoxplots')),
          
          fluidRow(width = NULL,
                   DT::dataTableOutput('DifferentialExploratoryTable'))
        ), 
        
        # Inferential Item (ui-side) ----
        
        # Inferential results for History records ----
        tabItem(tabName = "historicalHyp",
                fluidRow(
                  box(
                    title = "Info",
                    width = 12,
                    status = "info",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    HTML(
                      "<blockquote><h3>Description</h3></blockquote>
          <p> The  Inferential <b> History </b> panel provides an inferential analysis report
          for SYNTAX score regarding the historical records of patients.</p>"
                    )
                  )
                ),
                
                fluidRow(width = NULL,
                         DT::dataTableOutput('HistoryHypTable'))),
        
        # Inferential results for Entry records ----
        tabItem(tabName = "entryHyp",
                fluidRow(
                  box(
                    title = "Info",
                    width = 12,
                    status = "info",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    HTML(
                      "<blockquote><h3>Description</h3></blockquote>
          <p> The  Inferential <b> Entry </b> panel provides an inferential analysis report
          for SYNTAX score regarding the entry records of patients.</p>"
                    )
                  )
                ),
                
                fluidRow(width = NULL,
                         DT::dataTableOutput('EntryHypTable'))), 
        
        # Inferential results for Biochemical records ----
        tabItem(tabName = "biochemicalHyp",
                fluidRow(
                  box(
                    title = "Info",
                    width = 12,
                    status = "info",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    HTML(
                      "<blockquote><h3>Description</h3></blockquote>
          <p> The  Inferential <b> Biochemical </b> panel provides an inferential analysis report
          for SYNTAX score regarding the biochemical records of patients.</p>"
                    )
                  )
                ),
                
                fluidRow(width = NULL,
                         DT::dataTableOutput('BiochemicalHypTable'))),
        
        # Inferential results for Complete Blood Count records ----
        tabItem(tabName = "bloodHyp",
                fluidRow(
                  box(
                    title = "Info",
                    width = 12,
                    status = "info",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    HTML(
                      "<blockquote><h3>Description</h3></blockquote>
          <p> The  Inferential <b> Complete Blood Count </b> panel provides an inferential analysis report
          for SYNTAX score regarding the complete blood count records of patients.</p>"
                    )
                  )
                ),
                
                fluidRow(width = NULL,
                         DT::dataTableOutput('BloodHypTable'))), 
        
        # Inferential results for Differential records ----
        tabItem(tabName = "differentialHyp",
                fluidRow(
                  box(
                    title = "Info",
                    width = 12,
                    status = "info",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    HTML(
                      "<blockquote><h3>Description</h3></blockquote>
          <p> The  Inferential <b> Differential </b> panel provides an inferential analysis report
          for SYNTAX score regarding the differential records of patients.</p>"
                    )
                  )
                ),
                
                fluidRow(width = NULL,
                         DT::dataTableOutput('DifferentialHypTable'))), 
        
        # Predictive Item (ui-side) ----
        # Results for selection of predictors for the zero- and count-parts of the model ----
        tabItem(tabName = "featureselectiongess",
                fluidRow(
                  box(
                    title = "Info",
                    width = 12,
                    status = "info",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    HTML(
                      "<blockquote><h3>Description</h3></blockquote> <p> The  Predictive <b> Feature Selection </b> panel provides a report regarding the selection of
                         the predictors inserted into the zero-part and the count-part of the final model after the execution of the Boruta algorithm (100 repetitions).</p>
                      <p><b>Notes</b>:
                      <ul> 
                      <li><b>Ratio 1</b> is defined as the monocyte-to-HDL cholesterol ratio (MONO% x WBC)/HDL</li>
                      <li><b>Ratio 2</b> is defined as the lymphocyte-to-monocyte ratio</li>
                      <li><b>Ratio 3</b> is defined as the Atherogenic index of plasma levels log(TG/HDL)</li>
                      </ul>
                      </p>"
                    )
                  )
                ),
                
                fluidRow(width = NULL,),
                
                fluidRow(
                  box(
                    title = "Feature Selection (Zero-Part Model)",
                    status = "success",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 6,
                    plotOutput('BorutalFeaturesBinaryBoxplots')
                  )
                  ,
                  box(
                    title = "Feature Selection (Count-Part Model)",
                    status = "success",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 6,
                    plotOutput('BorutalFeaturesCountBoxplots')
                  )
                )),
        
        # Inputs for selected predictors of the model) ----
        tabItem(
          tabName = "predictiongess",
          fluidRow(
            box(
              title = "Info",
              width = 12,
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              HTML(
                "<blockquote><h3>Description</h3></blockquote>
          <p> In the <b> Prediction </b> panel, the user can insert the values for the predictors participating into the zero- and count-part of the final model
          for a new patient and CRISSPAC will estimate (a) the probability of presenting a non-zero SYNTAX score value and (b) an expectation of the SYNTAX score given
                         that the patient presents a non-zero SYNTAX score.</p>
                <p><b>Notes</b>:
                <ul> 
                <li><b>Ratio 1</b> is defined as the monocyte-to-HDL cholesterol ratio (MONO% x WBC)/HDL</li>
                <li><b>Ratio 3</b> is defined as the Atherogenic index of plasma levels log(TG/HDL)</li>
                </ul>
                </p>"
              )
            )
          ),
          
          fluidRow(width = NULL,
                   fluidRow(
                     box(
                       title = "Select/type in the values of Clinical Predictors (Common in Zero/Count-Part Models)",
                       status = "warning",
                       solidHeader = TRUE,
                       collapsible = TRUE,
                       width = 12,
                       fluidRow(column(
                         2,
                         textInput(
                           "inCheckboxGRACESCOREBothPart",
                           value = "112",
                           label = "GRACE SCORE"
                         )
                       ),
                       column(
                         2,
                         textInput("inCheckboxGLUBothPart",
                                   value = "120",
                                   label = "GLU")
                       ))
                     )
                   ),
                   
                   fluidRow(
                     box(
                       title = "Select/type in the values of Clinical Predictors (Zero-Part Model)",
                       status = "warning",
                       solidHeader = TRUE,
                       collapsible = TRUE,
                       width = 12,
                       fluidRow(
                         column(
                           2,
                           radioButtons(
                             "inCheckboxCHESTPAINZeroPart",
                             "CHEST PAIN",
                             choices = list("No" = 1, "Yes" = 2),
                             selected = 1
                           )
                         ),
                         column(
                           2,
                           radioButtons(
                             "inCheckboxSTCHANGESZeroPart",
                             "ST-T CHANGES",
                             choices = list("No" = 1, "Yes" = 2),
                             selected = 1
                           )
                         ),
                         column(
                           2,
                           radioButtons(
                             "inCheckboxATRIALFIBRILLATIONZeroPart",
                             "ATRIAL FIBRILLATION",
                             choices = list("No" = 1, "Yes" = 2),
                             selected = 1
                           )
                         ),
                         column(
                           2,
                           radioButtons(
                             "inCheckboxGENDERZeroPart",
                             "GENDER",
                             choices = list("Female" = 1, "Male" = 2),
                             selected = 1
                           )
                         ),
                         column(
                           2,
                           radioButtons(
                             "inCheckboxEASYFATIGUEZeroPart",
                             "EASY FATIGUE",
                             choices = list("No" = 1, "Yes" = 2),
                             selected = 1
                           )
                         ),
                         column(
                           2,
                           radioButtons(
                             "inCheckboxAORTICANEURYSMSZeroPart",
                             "AORTIC ANEURYSMS",
                             choices = list("No" = 1, "Yes" = 2),
                             selected = 1
                           )
                         ),
                         column(
                           2,
                           textInput(
                             "inCheckboxRATIO1ZeroPart",
                             value = "1.4",
                             label = "RATIO 1"
                           )
                         ),
                         column(
                           2,
                           textInput("inCheckboxNEUZeroPart",
                                     value = "64.2",
                                     label = "NEU(%)")
                         ),
                         column(
                           2,
                           textInput(
                             "inCheckboxRATIO3ZeroPart",
                             value = "0.5",
                             label = "RATIO 3"
                           )
                         ),
                         column(
                           2,
                           textInput("inCheckboxSGOTZeroPart",
                                     value = "23",
                                     label = "SGOT")
                         ),
                         column(
                           2,
                           textInput("inCheckboxEOSZeroPart",
                                     value = "2.8",
                                     label = "EOS(%)")
                         )
                       )
                     )
                   ),
                   
                   fluidRow(
                     box(
                       title = "Select/type in the values of Clinical Predictors (Count-Part Model)",
                       status = "warning",
                       solidHeader = TRUE,
                       collapsible = TRUE,
                       width = 12,
                       fluidRow(
                         column(
                           2,
                           textInput(
                             "inCheckboxCRUSADESCORECountPart",
                             value = "22",
                             label = "CRUSADE SCORE"
                           )
                         ),
                         column(
                           2,
                           textInput("inCheckboxGFRCountPart",
                                     value = "98.3",
                                     label = "GFR")
                         ),
                         column(
                           2,
                           textInput("inCheckboxAGECountPart",
                                     value = "56",
                                     label = "AGE")
                         ),
                         column(
                           2,
                           textInput(
                             "inCheckboxQRSDURATIONCountPart",
                             value = "102",
                             label = "QRS DURATION ms"
                           )
                         ),
                         column(
                           2,
                           textInput("inCheckboxHCTCountPart",
                                     value = "35.9",
                                     label = "HCT")
                         ),
                         column(
                           2,
                           textInput("inCheckboxHGBCountPart",
                                     value = "13.2",
                                     label = "HGB")
                         ),
                         column(
                           2,
                           textInput(
                             "inCheckboxUREACountPart",
                             value = "31",
                             label = "UREA"
                           )
                         ),
                         column(
                           2,
                           radioButtons(
                             "inCheckboxDIABETESMELLITUSCountPart",
                             "DIABETES MELLITUS",
                             choices = list("No" = 1, "Yes" = 2),
                             selected = 1
                           )
                         )
                       )
                     )
                   ),
                   fluidRow(
                     width = NULL,
                     box(
                       title = "Perform",
                       status = "danger",
                       solidHeader = TRUE,
                       collapsible = TRUE,
                       width = 2,
                       withBusyIndicatorUI(
                         actionButton(
                           "PREDICTIONRun",
                           label = "Run",
                           icon = icon("refresh"),
                           class = "btn-primary"
                         )
                         
                       )
                       
                     ),
                     infoBoxOutput("PredictedClass", width = 5),
                     infoBoxOutput("PredictedCount", width = 5)
                     )
                   ))
        )
      )
    )
)




  
                       