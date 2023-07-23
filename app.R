#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(randomForest)

# load the data-set
data <- read.csv("adult.csv")
# map income values to numerical (>50k->1) (<=50k->0)
data$income_binary <- ifelse(data$income == ">50K", 1, 0)

# set default value for ntrees hyperparam
default_ntree <- 100

# random forest classifier with default ntree
rf_model <- randomForest(income_binary ~ age + education.num, data = data, ntree = default_ntree )


ui <- fluidPage(
  navbarPage(
    "Adult Census Income - Analysis With Random Forests",
    tabPanel("Description",
             fluidRow(
               column(width = 12,
                      h2("Application Description"),
                      h3("Data"),
                      p("This application uses data from the  “Adult Census Income” data-set. This data-set has been extracted from the US Census bureau database for the year 1994. The focal dependent variable in this dataset is “income” for which each record has either an income labeled “<=50k” or “>50k”. In total, this dataset has 32562 records and 15 different columns. "),
                      p("Other columns include: age: num, workclass: class, fnlwgt: num, education: class, education.num: num, marital.status: class, occupation: class, relationship: class, race: class, sex: class, capital.gain: num, capital.loss: num, hours.per.week: num, native.country: class"),
                      p("For features, we chose to focus on age and education.num which is an ordinal measure of educational level represented as an integer 1-16, where a 1 is minimal education background (preschool) and a 16 is a high level of education (doctorate)."),
                      p("For data-cleaning there were no missing entries for either of our features of interest (age and education.num). In order to perform the classification it was necessary to map the values of the “income” column, where “<=50k” is represented as 0 and “>50k” is represented as 1."),
                      h3("Choosing a Machine Learning Method"),
                      p("For this application I chose to use a Random Forest Classifier which looks at age and education.num in order to predict whether an individual would be making over 50K a year or not. Here, I wanted to be able to visualize the decision boundary in the prediction space for the classification. Further I chose to use Random forests as it is suitable for non-linear relationships."),
                      h3("Random Forests Classifier Summary"),
                      p("The random forests classifier is an extension of decision trees. In decision trees we aim to split up the prediction space by creating a hierarchical set of feature-based decisions in a tree-like structure. Random forests extends this idea by creating a composite model based off of many trees (aka forest)."),
                      p("In random forest Classification we first perform bootstrap sampling (random sampling with replacement) on our dataset in order to get datasets for each of the trees that will be created. Next, only a subset of the features are randomly chosen to be used on a tree by tree basis. In our case we only have 2 features (age and education.num), so some trees would use age, some would use education.num, and some would use both. Next, the trees are generated. The amount of trees generated for the random forest is specified by the ntrees hyperparameter, which in this application the user is able to control and can affect the accuracy of the model. This process works typically by maximizing the purity of classes in each split by either gini or entropy to find what feature is best to split on and the optimal split point. For example a split point in our case could be “age<=45” and “age>45”. This then repeats recursively until we reach a stopping point which can be specified by a max depth or minimum samples number at a node. Finally, the terminal nodes are assigned a class label (either <=50k or >50k) based on the majority class of the samples in the “leaf” node. The overall prediction is then just determined by combining these trees through majority voting."),
                      p("The significance of the green line on the predictor space is the decision boundary line created by the Random forest classification. Where the model tries to separate the data points such that one class (<=50k) is on one side, and (>50k) is on the other side of the line. In other words it is a visual projection of our composite decision tree projected onto the 2d feature space, which is the combined decisions of all the decision trees in the Random Forest model ."),
                      h3("Application Overview"),
                      p("Overall, this application intends to help visualize using random forests classifiers to predict whether an individual's income is greater than $50,000 or not based on their age and education level, using the Adult census Income dataset. A scatterplot shows the data points and color codes the labels where red represents income “>50k” and blue represents data points which have income “<=50k”. Then shown in green,  we visually overlay the non-linear boundary over our classified data points, and we can visually observe how the Random Forest Classification algorithm tries to partition the predictor space. The user can use the 'Number of Trees' input in the sidebar to adjust the number of trees hyperparameter in the Random Forest classifier model."),
               )
             )
    ),
    tabPanel("Analysis",
             sidebarLayout(
               sidebarPanel(
                 numericInput("ntree", "Number of Trees:", value = default_ntree, min = 1, max = 1000 )
               ),
               mainPanel(plotOutput("scatter_plot"))
             )
    )
  )
)


server <- function(input, output) {
  # scatter plot
  output$scatter_plot <- renderPlot({
    # create scatter plot
    p <- ggplot(data, aes(x = age, y = education.num, color = factor(income_binary) )) +
      geom_point() +
      scale_color_manual( values = c("blue", "red")) +
      labs(color = "Income", x = "Age", y = "Education (Numerical)" ) +
      theme_minimal()
    
    # get prediction grid
    x_grid <- seq( min(data$age), max(data$age), length.out = 100 )
    y_grid <- seq( min(data$education.num), max(data$education.num), length.out = 100 )
    xy_grid <- expand.grid( age = x_grid, education.num = y_grid)
    
    # make preds using the RFC model on the prediction grid
    predictions <- data.frame(xy_grid, prediction = predict(rf_model, newdata = xy_grid, type = "response"))
    
    # overlay decision boundary
    p + geom_contour(data = predictions, aes(x = age, y = education.num, z = prediction),
                     breaks = 0.5, color = "green", linetype = "solid", size = 1.5 )
  })
  
  # update random forest model and boundary when user changes number of trees
  observeEvent(input$ntree, {
    rf_model <<- randomForest(income_binary ~ age + education.num, data = data, ntree = input$ntree )
  })
}

# run app
shinyApp(ui = ui, server = server)
