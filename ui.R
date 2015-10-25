library(shiny)

# Define UI for random distribution application 
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Distribution of Sample Means"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(
    sidebarPanel(
      radioButtons("dist", "Choose distribution",
                   c("Uniform" = "unif",
                     "Log-normal" = "lnorm",
                     "Exponential" = "exp")),
      br(),br(), 
      
            sliderInput("n", 
                "Sample size", 
                value = 100,
                min = 1, 
                max = 4000),
      br(),
      sliderInput("reps", 
                  "Number of repetitions", 
                  value = 200,
                  min = 1, 
                  max = 500),
      br(),
      h5("This application is inspired by the statistical inference course.
        First you choose from the three distributions. Then you choose
        the sample size and the number of repetitions. When
        you make a choice, the graphs on the right hand side are updated.
        The graphs are self-explanatory. Please note that the distribution of
        the sample means is normal, and its variance gets smaller or larger as the number of
        samples or repetitions increase or decrease."),
      br(),br(),br()
      
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
))
