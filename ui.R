# ui script

library(markdown)

shinyUI(navbarPage("Appify",
                   tabPanel("Occurrence data",
                            sidebarLayout(
                              sidebarPanel(
                                width = 3,
                                h2("Occurrences"),
                                uiOutput('occurrence_text'),
                                br(),
                                downloadButton('downloadOccData', 'Download')
                              ),
                              mainPanel(
                                uiOutput("occurrence_tabs")
                              )
                            )
                   ),
                   tabPanel("Covariate data",
                            sidebarLayout(
                              sidebarPanel(
                                h2("Covariates"),
                                uiOutput('covariate_text'),
                                br(),
                                downloadButton('downloadCovData', 'Download')
                              ),
                              mainPanel(
                                uiOutput("covariate_tabs")
                              )
                            )
                   ),
                   tabPanel("Model",
                            tags$head(
                              tags$style(HTML("
                                #model_out {
                                  padding: 19px;
                                  background-color: #f5f5f5;
                                  border: 1px solid #e3e3e3;
                                  border-radius: 4px;
                                }
                              "))
                            ),
                            sidebarLayout(
                              sidebarPanel(
                                h2("Model"),
                                uiOutput('model_text'),
                                br(),
                                downloadButton('downloadModelData', 'Download')
                              ),
                              mainPanel(
                                uiOutput("model_tabs")
                              )
                            )
                   ),
                   tabPanel("Predictions",
                            sidebarLayout(
                              sidebarPanel(
                                h2("Predictions"),
                                uiOutput('pred_text')
                              ),
                              mainPanel(
                                uiOutput("pred_tabs")
                              )
                            )
                   )
))