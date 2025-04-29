library(arules)# For apriori algorithm
library(gridExtra)
library(dplyr)
library(tidyverse)
library(cluster)
library(shiny)
# Load the data
data <- read.csv("D:/grc.csv")
#explore the data
data
dim(data)
summary(data)
str(data)
head(data)
tail(data)
#Clean the data if needed
duplicated(data) #check duplicated data
data[duplicated(data),] #show duplicated rows
data[!duplicated(data),] #clean the duplicated rows
#check Nulls (Missing values)in the data
is.na(data)
sum(is.na(data)) #sum of Nulls
na.omit(data) #Remove Nulls

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Visualize the data (all shapes)
#put them in one plot
#par(mfrow=c(3,3))
#barplot
#Compare cash and credit totals.
# Use barplot to compare cash and credit totals
plot_cash_credit<-barplot(table(data$paymentType), main = "Cash vs Credit Totals", xlab = "Payment Method", ylab = "Frequency", col = c("skyblue", "green"))

pie(x = table(data$paymentType), main = "Compare Cash And Credit" ,col = c("blue","red"))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Compare each age and sum of total spending. 
# Create a bar plot to compare each age and sum of total spending
plot_age_spending<-  barplot(tapply(data$total, data$age, sum), 
                             xlab = "Age", ylab = "Total Spending",
                             main = "Total Spending by Age" ,col ="blue")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#Show each city total spending and arrange it by total descending. 
# Show each city total spending and arrange it by total descending
city_totals <- tapply(data$total, data$city, sum)
ordered_city_totals <- sort(city_totals, decreasing = TRUE)
plot_city_spending <- barplot(ordered_city_totals, main = "City Total Spending", xlab = "City",
                              ylab = "Total Spending", col = "green")
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#Display the distribution of total spending. 
# Display the distribution of total spending
plot_spending_distribution <- hist(data$total, main = "Distribution of Total Spending", xlab = "Total Spending",
                                   col = "orange")
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

# Create a dashboard with all plots
par(mfrow = c(3, 3))  # 3x3 grid

plot_cash_credit<-barplot(table(data$paymentType), main = "Cash vs Credit Totals", xlab = "Payment Method", ylab = "Frequency", col = c("skyblue", "green"))


pie(x = table(data$paymentType), main = "Compare Cash And Credit" ,col = c("blue","red"))
plot_age_spending<-  barplot(tapply(data$total, data$age, sum), 
                             xlab = "Age", ylab = "Total Spending",
                             main = "Total Spending by Age",col = "blue")
plot_city_spending <- barplot(ordered_city_totals, main = "City Total Spending", xlab = "City",
                              ylab = "Total Spending", col = "green")
plot_spending_distribution <- hist(data$total, main = "Distribution of Total Spending", xlab = "Total Spending",
                                   col = "orange")



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

GroupedNames <- data %>%
  group_by(customer) %>%
  summarise (TotalSpending = sum(total), AgeAvg = mean(age)) #This part basically calculates the 
# total spending and average age of each name 


#Now grouped names data frame has 2 columns, each name and its total spending 
#Since we want to apply kmeans with respect to age, we're gonna add age as well

datatest <- GroupedNames[, c(2, 3)]

clusters = 3

if (clusters >= 2 && clusters <= 7) { # We made this condition to force the user to input a value
  # between 2 aand 4 (inclusive)
  
  clustering <- kmeans(datatest, centers = clusters)
  GroupedNames$cluster <- clustering$cluster
  print(GroupedNames)
} else {
  while (clusters < 2 || clusters > 7) { 
    clusters = readline('Number of groups must be between 2 and 7!')
  }
  clustering <- kmeans(datatest, centers = clusters)
  GroupedNames$cluster <- clustering$cluster
  print(GroupedNames)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

# Define UI
ui <- fluidPage(
  titlePanel("Data Analysis and Clustering"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("centers", "Number of clusters:", min = 2, max = 7, value = 3),
      actionButton("go", "Update Clusters"),
      br(),
      radioButtons("plotType", "Select plot type:",
                   choices = c("Cash vs Credit Totals", 
                               "Total Spending by Age",
                               "City Total Spending",
                               "Distribution of Total Spending",
                               "Compare Cash And Credit"),  # Add this line
                   selected = "Cash vs Credit Totals"),
      actionButton("updatePlot", "Update Plot")
    ),
    
    mainPanel(
      plotOutput("plot"),
      br(),
      h3("Clustered Data"),
      tableOutput("clusteredTable")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Perform clustering and update data
  clusteredData <- reactive({
    clus <- kmeans(data[,c("age","total")], centers = input$centers)
    data$Cluster <- as.factor(clus$cluster)
    data
  })
  
  # Render plot based on selected plot type
  output$plot <- renderPlot({
    req(clusteredData())  # Ensure data is available
    
    if (input$updatePlot > 0) {
      if (input$plotType == "Cash vs Credit Totals") {
        barplot(table(clusteredData()$paymentType), 
                main = "Cash vs Credit Totals", 
                xlab = "Payment Method", 
                ylab = "Frequency", 
                col = c("skyblue", "green"))
      } else if (input$plotType == "Total Spending by Age") {
        barplot(tapply(clusteredData()$total, clusteredData()$age, sum), 
                xlab = "Age", ylab = "Total Spending", 
                main = "Total Spending by Age", col = "blue")
      } else if (input$plotType == "City Total Spending") {
        city_totals <- tapply(clusteredData()$total, clusteredData()$city, sum)
        ordered_city_totals <- sort(city_totals, decreasing = TRUE)
        barplot(ordered_city_totals, 
                main = "City Total Spending", 
                xlab = "City", ylab = "Total Spending", 
                col = "green")
      } else if (input$plotType == "Distribution of Total Spending") {
        hist(clusteredData()$total, 
             main = "Distribution of Total Spending", 
             xlab = "Total Spending", 
             col = "orange")
      } else if (input$plotType == "Compare Cash And Credit") {  # Add this block
        pie(x = table(clusteredData()$paymentType), 
            main = "Compare Cash And Credit" ,
            col = c("blue","red"))
      }
    }
  })
  
  # Display clustered data table
  output$clusteredTable <- renderTable({
    req(clusteredData())  # Ensure data is available
    clusteredData()[, c("customer", "age", "total", "paymentType", "Cluster")]
  })
}

# Run the application
shinyApp(ui = ui, server = server)

