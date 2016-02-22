library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Pop Music Chord Generator"),
  
  sidebarPanel(
    selectInput("artist", "Choose an artist:", 
                choices = c("Taylor Swift", "Nicki Minaj", "Katy Perry")),
    selectInput("part", "Choose a song part:", 
                choices = c("Verse", "Chorus")),
    selectInput("key", "Choose a key:", 
                choices = c("A", "A#", "B", "C", "C#", "D", "D#","E", "F", "F#", "G", "G#"), selected = "C"),
    numericInput("nChords", "Choose a number of Chords:", value = 4, min = 3, max = 6),
    submitButton("Submit")
  ),
  
  mainPanel(
      h3(textOutput("songs"), align = "center"),
      br(),
      h4(textOutput("chords"),style= "font-style:italic", align = "center"),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      h5(textOutput("description"), align="center")
  )
))