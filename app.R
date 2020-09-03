### Cunningham & HAley (2020)

library(shiny)
library(tibble)
library(qdap) 
library(koRpus)

set.kRp.env(lang="en")
koRpus.lang.en::lang.support.en()
txt <- as.character("Cunningham (2020): We extracted an orthographic transcript that included no chat codes. We excluded unintelligible words, but all other verbal productions were included, such as whole-wordrepetitions, filler words, and so forth.")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Word Information Measure and Moving Average Type Token Ratio"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textAreaInput("transcr",
                        "Paste Transcription Here:",
                        value = "Cinderella was a pleasant young girl. Her father married a nasty woman who had two nasty daughters and they treated her like dirt and the prince of the area was planning a ball. She was uh sisters, they ain’t real. They ain’t married really but they’re. Anyway, she uh they wanted the the ball and the prince yeah they wanna go see him",
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

