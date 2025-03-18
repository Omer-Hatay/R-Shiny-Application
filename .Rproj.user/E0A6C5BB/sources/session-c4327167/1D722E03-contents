#install libraries
#install.packages("shiny")
#install.packages("shinythemes")
#install.packages("tidyverse")
#install.packages("ggplot2")

# Load the libraries
library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)

# Load the data
library(readr)
Salary_Data <- read_csv("Salary_Data.csv")

View(Salary_Data)
# Data manipulation
Salary_Data<- Salary_Data %>%
  filter(Gender=="Male" | Gender=="Female") %>% 
  rename(Job_Title=`Job Title`,Education_lvl=`Education Level`,
         years_of_experience=`Years of Experience`) %>%
  mutate(Education_lvl=replace(Education_lvl,Education_lvl=="Bachelor's","Bachelor's Degree")) %>%
  mutate(Education_lvl=replace(Education_lvl,Education_lvl=="Master's","Master's Degree")) %>% 
  mutate(Education_lvl=replace(Education_lvl,Education_lvl=="phD","PhD")) %>% 
  filter(Education_lvl=="Bachelor's Degree" |Education_lvl=="Master's Degree"|
           Education_lvl=="PhD") %>%
  mutate(Job_Title=replace(Job_Title,Job_Title=="Customer Service Rep",
                           "Customer Service Representative"))%>% 
  mutate(Job_Title=replace(Job_Title,Job_Title=="Front end Developer",
                           "Front End Developer")) %>% 
  mutate(Job_Title=replace(Job_Title,Job_Title=="HR Manager",
                           "Human Resources Manager")) %>% 
  mutate(Job_Title=replace(Job_Title,Job_Title=="Senior HR Manager"
                           ,"Senior Human Resources Manager")) %>% 
  mutate(Job_Title=replace(Job_Title,Job_Title=="Social Media Man",
                           "Social Media Manager")) %>% 
  
  na.omit() 



# I filtered job types with more than 10 job type data 

meslek_freq <- Salary_Data %>% count(Job_Title)
meslek_filtre <- meslek_freq %>% 
  filter(n > 10)%>% 
  pull(Job_Title)

Salary_Data <- Salary_Data %>%
  filter(Job_Title %in% meslek_filtre)


# User Interface
ui <- fluidPage( theme = shinytheme("cyborg"),
      
  titlePanel("Salary Analysis of IT Sector"),

                                                  
  sidebarLayout(
    sidebarPanel(
      # input widgets
      selectInput("Job_t",
                  label = "Choose job type",
                  choices =sort( c( unique(Salary_Data$Job_Title))),
                  selected = "Data Scientist"),
      
      sliderInput("Sal_range",
                  label = "Range of salary ($)",
                  min = 0,
                  max = 250000,
                  value = c(0, 250000)),
      radioButtons("edulevel","Education Level",
                         choices = c("All", unique(Salary_Data$Education_lvl)),
                         selected = "All"),
     
      sliderInput("exp_range","Years of experience",
                  min=0,
                  max=34,
                  value=c(0,34)),
   
    ),
    
    
    
    
    
    mainPanel(
      
      plotOutput("Scatterplot")
    )
  )
)

# Server 
server <- function(input, output) {
  
  # Filter the data from user inputs
  filtered_data <- reactive({
    Salary_Data %>%
      filter(Job_Title == input$Job_t) %>%
      filter(Salary >= input$Sal_range[1] & Salary <= input$Sal_range[2]) %>%
      filter(if (input$edulevel == "All") TRUE else Education_lvl %in% input$edulevel) %>%
      filter(years_of_experience >= input$exp_range[1] & years_of_experience <= input$exp_range[2])
  })
  
  # Render the plot of salary vs years of experience
  output$Scatterplot <- renderPlot({
    ggplot(filtered_data(), aes(x = years_of_experience, y = Salary, color = Education_lvl)) +
      geom_point(alpha=0.5) +
      labs(title = paste("Salary vs Years of Experience for", input$Job_t),
           x = "Years of Experience",
           y = "Salary ($)",
           color="Education Level") +facet_wrap(~Gender)+
    
      
      scale_color_manual(values = c("Bachelor's Degree" = "red",
                                    "Master's Degree" = "darkgreen", 
                                    "PhD" = "purple")) 
      
    
    },res=100
  )
  
  
  
}
shinyApp(ui = ui, server = server)
