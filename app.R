### Cunningham & HAley (2020)

library(shiny)
library(tibble)
library(qdap) 
library(koRpus)

set.kRp.env(lang="en")
koRpus.lang.en::lang.support.en()
txt <- as.character('Cunningham (2020): "We extracted an orthographic transcript that included no chat codes. We excluded unintelligible words, but all other verbal productions were included, such as whole-wordrepetitions, filler words, and so forth."')

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Word Information Measure and Moving Average Type Token Ratio"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textAreaInput("transcr",
                        "Paste Transcription Here:",
                        value = "Young boy is practicing playing soccer. Kicking the ball up and keeping it in the air. He miskicks. and and it fall goes and breaks the window of his house. of the living room actually. nd bounces into the living room knocking a lamp over where his father is sitting. the father picks up the soccer ball. Looks out the window. And calls for the little boy &t to come and explain. [AphasiaBank]",
                       width = '100%',
                       height = '400px'),
            sliderInput("mattr_w", "MATTR WINDOW:", value = 5, min = 5, max = 50),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tableOutput("summaryStats"),
           textOutput("description")
        ) 
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
    output$description <- renderText({txt})
        
    output$summaryStats <- renderTable({
        
        # save input as a variable
        transcript <- as.character(input$transcr)
        # WIM
        ld <- diversity(transcript)$shannon
        #MATTR
        tokenized.obj <- tokenize(transcript, lang = "en", format = 'obj') #using the tokenize function in koRpus
        m <- MATTR(tokenized.obj, window = input$mattr_w) #this is the analysis window, currently set to 5 words
        m <- m@MATTR
        m <-m$MATTR
        
        df <- tibble(
           MATTR = m,
           LexicalDiversity = ld
        )

        return(df)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


# deployment: 

# library(rsconnect)
# rsconnect::deployApp("/Users/robcavanaugh/Dropbox/discourse_analysis")

