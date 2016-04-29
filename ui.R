# ui script

library(markdown)

shinyUI(navbarPage("Appify",
                   tabPanel("Overview",
                            sidebarLayout(
                              sidebarPanel(
                                width = 3,
                                h2("Overview"),
                                p(HTML('These pages show you the output from a ZOON workflow. <a href="https://zoonproject.wordpress.com/" target="_blank">Zoon</a> is a tool for reproducible species distribution modelling in R. Here you can find the results for the workflow detailed to the right. You’ll find the occurrence data used as well as the covariate data and the model output. Enjoy!'))
                              ),
                              mainPanel(
                                div(img(src = 'https://github.com/zoonproject/blog/raw/master/zoon_top.png',
                                        width = '100%',
                                        alt = 'ZOON')),
                                #h4('Workflow Structure'),
                                br(),
                                div(htmlOutput('org_chart'))
                              )
                            )
                   ),
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
                                uiOutput("occurrence_tabs"),
                                br()
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
                                uiOutput("covariate_tabs"),
                                br()
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
                                uiOutput("model_tabs"),
                                br()
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
                                uiOutput("pred_tabs"),
                                br()
                              )
                            )
                   )
))