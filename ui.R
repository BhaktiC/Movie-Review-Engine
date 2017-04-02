
shinyUI(fluidPage(
  titlePanel("Movie Review Engine"),
  
  sidebarLayout(
    sidebarPanel(textInput("text", label = h3("Movie name:"), 
                           value = ""),submitButton("Submit")
                 ),
    mainPanel(h3("Response"), plotOutput("plot1"))
  )
))