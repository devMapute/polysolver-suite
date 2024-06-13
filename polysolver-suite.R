# MAPUTE ANDRAE M
# 202109264
# CMSC150


# Load R packages
library(shiny)
library(shinythemes)

options(max.print = 99999)

# For Checkboxes in Diet Food Solver
foodlist <- c("Frozen Broccoli","Carrots, Raw", "Celery, Raw","Frozen Corn","Lettuce, Iceberg, Raw","Peppers, Sweet, Raw",
              "Potatoes, Baked","Tofu","Roasted Chicken","Spaghetti W/ Sauce","Tomato, Red, Ripe, Raw","Apple, Raw, W/ Skin","Banana",
              "Grapes","Kiwifruit, Raw, Fresh","Oranges","Bagels","Wheat Bread","White Bread","Oatmeal Cookies","Apple Pie","Chocolate Chip Cookies",
              "Butter, Regular","Cheddar Cheese","3.3% Fat, Whole Milk","2% Lowfat Milk","Skim Milk","Poached Eggs","Scrambled Eggs","Bologna, Turkey",
              "Frankfurter, Beef","Ham, Sliced, Extra lean","Kielbasa, Pork","Cap'N Crunch","Cheerios","Corn Flks, Kellogg'S","Raisin Brn, Kellg'S",
              "Rice Krispies","Special K","Oatmeal","Malt-O- Meal, Choc","Pizza W/ Pepperoni","Taco","Hamburger W/ Toppings","Hotdog, Plain","Couscous",
              "White Rice","Macaroni, Ckd","Peanut Butter","Pork","Sardines in Oil","White Tuna in Water","Popcorn, Air- Popped","Potato Chips, Bbqflvr",
              "Pretzels","Tortilla Chip","Chicknoodl Soup","Splt Pea&Hamsoup","Vegetbeef Soup","Neweng Clamchwd","Tomato Soup","New E Clamchwd, W/ Mlk",
              "Crm Mshrm Soup, W/ Mlk","Beanbacn Soup, W/ Watr")


#----------------------------------------------------
#---------------------- UI --------------------------
#----------------------------------------------------

ui <- fluidPage(theme = shinytheme("sandstone"),
                navbarPage(
                  "PolySolver Suite",
                  tabPanel("Polynomial Regression",
                           sidebarPanel( # inputs
                             tags$h3("Input:"),
                             checkboxInput("header", "First row contains labels", value = TRUE),
                             fileInput("file", "Choose CSV File", accept = c(".csv")),
                             sliderInput("degree", "Enter Degree:", 2, min = 0, max = 10),
                             numericInput("evalx", label = "Enter X Value", value = 1),
                             actionButton(inputId = "submitButton2", label = "Submit"),
                             
                             
                           ), 
                           mainPanel( # outputs
                             h3("Output"),
                             h4("Initial Table"),
                             tableOutput("coeffMatrixTable"),
                             h4("Coefficients"),
                             textOutput("coefficientsText"), 
                             h4("Function"),
                             textOutput("polynomialStringText"),  
                             h4("F(x)"),
                             textOutput("outputfx"),
                             h4("User File"),
                             tableOutput("inputTable"),
                             
                             
                           ) 
                           
                  ), 
                  tabPanel("QSI",
                           sidebarPanel( # inputs
                             tags$h3("Input:"),
                             checkboxInput("header2", "First row contains labels", value = TRUE),
                             fileInput("file2", "Choose CSV File", accept = c(".csv")),
                             numericInput("xtoeval", label = "Enter X Value", value = 1),
                             actionButton(inputId = "submitButton3", label = "Submit"),
                             
                           ), 
                           mainPanel( # outputs
                             h3("Output"),
                             h4("Unknowns"),
                             tableOutput("UnknownsValue"),
                             h4("f(x) per interval"),
                             verbatimTextOutput("fxperinterval"),
                             h4("Function used"),
                             verbatimTextOutput("fxused"),
                             h4("f(x) Value"),
                             textOutput("fxeval"),
                             h4("User File"),
                             tableOutput("inputTable2")
                           ) 
                  ),
                  tabPanel("Diet Problem Solver", 
                           wellPanel( # inputs
                             h3("Food Selector"),
                             checkboxGroupInput(inputId = "checkboxes", label = "Please select your food.", choices = foodlist, inline = TRUE),
                             actionButton(inputId = "uncheckButton", label = "Uncheck All"),
                             actionButton(inputId = "checkAllButton", label = "Check All"),
                             actionButton(inputId = "submitButton", label = "Submit Selection"),
                             
                           ), 
                           wellPanel( # outputs
                             h3("Final Solution"),
                             textOutput("resultText"),
                             tableOutput("solTableSimplex")),
                           wellPanel(
                             h3("Basic Solution Per Iteration"),
                             verbatimTextOutput("consoleOutput")
                           ),
                           wellPanel(
                             h3("Tableau"),
                             verbatimTextOutput("tableau")
                           )
                          
                  ),
                  tabPanel("About",
                           wellPanel(
                             h3("About"),
                             h4("Made by Mapute, A.M."),
                             h5("Final Project for CMSC 150"),
                             
                           )
                    
                  )
                ) # navbarPage
) # fluidPage

#------------------------------------------------------------
#----------------------- SERVER -----------------------------
#------------------------------------------------------------



server <- function(input, output, session) {
  
  #---------------- REGRESSION--------------------------
  # handle the input csv file
  data <- reactive({
    req(input$file)
    suppressWarnings(read.csv(input$file$datapath, header = input$header))
    
  })
  
  # observe how many data points 
  observe({
    req(input$file)
    suppressWarnings(data <- read.csv(input$file$datapath, header = input$header))
    max_rows <- nrow(data) -1
      updateSliderInput(session, "degree", max = max_rows)
  })
  
  # render the table from the csv file 
  output$inputTable <-renderTable({
    x <- data()[, 1]
    y <- data()[, 2]
    matrix_xy <- cbind(x, y)
    # arrange
    matrix_xy <- matrix_xy[order(matrix_xy[, 1]), ]
    matrix_xy
    
  })
  
  # submit button
  observeEvent(input$submitButton2, {
  # render the table of coefficients
  output$coeffMatrixTable <- renderTable({
    x <- data()[, 1]
    y <- data()[, 2]
    matrix_xy <- cbind(x, y)
    # arrange
    matrix_xy <- matrix_xy[order(matrix_xy[, 1]), ] 
    
    inputs = list(matrix_xy[,1],matrix_xy[,2])
    result <- PolynomialRegression(input$degree, inputs)

    if (is.character(result)) {
      return(NULL)  # Return NULL if there is an error
    }
    
    # Display the Augmented Coefficient Matrix
    result$Augcoeffmatrix
  })
  
  # render the list of coefficients
  output$coefficientsText <- renderText({
    x <- data()[, 1]
    y <- data()[, 2]
    matrix_xy <- cbind(x, y)
    # arrange
    matrix_xy <- matrix_xy[order(matrix_xy[, 1]), ] 
    
    inputs = list(matrix_xy[,1],matrix_xy[,2])
    result <- PolynomialRegression(input$degree, inputs)

    if (is.character(result)) {
      return(NULL)  # Return NULL if there is an error
    }
    
    # Display the Coefficients with proper formatting
    formatted_coefficients <- sprintf("%.6f", result$Coefficients)
    paste(formatted_coefficients, collapse = "\n")
  })
  
  # render the function
  output$polynomialStringText <- renderText({
    x <- data()[, 1]
    y <- data()[, 2]
    matrix_xy <- cbind(x, y)
    # arrange
    matrix_xy <- matrix_xy[order(matrix_xy[, 1]), ] 
    
    inputs = list(matrix_xy[,1],matrix_xy[,2])
    result <- PolynomialRegression(input$degree, inputs)

    if (is.character(result)) {
      return(NULL)  # Return NULL if there is an error
    }
    
    # Display the Polynomial String
    paste(result$Polynomial_string)
  })
  
  # render f(x)
  output$outputfx <- renderText({
    x <- data()[, 1]
    y <- data()[, 2]
    matrix_xy <- cbind(x, y)
    # arrange
    matrix_xy <- matrix_xy[order(matrix_xy[, 1]), ] 
    
    inputs = list(matrix_xy[,1],matrix_xy[,2])
    result <- PolynomialRegression(input$degree, inputs)
    xtofind <- input$evalx

    
    if (is.character(result)) {
      return(NULL)  # Return NULL if there is an error
    }
    paste(result$Polynomial_function(xtofind))
  })
  
  })
  #--------------QSI--------------------
  
  # handles the csv file input
  data2 <- reactive({
    req(input$file2)
    suppressWarnings(read.csv(input$file2$datapath, header = input$header2, fill = TRUE))
  })
  
  
  # renders the table from the input file
  output$inputTable2 <- renderTable({
    x <- data2()[, 1]
    y <- data2()[, 2]
    matrix_xy <- cbind(x, y)
    
    # arrange
    matrix_xy <- matrix_xy[order(matrix_xy[, 1]), ]
    
    
    matrix_xy
    
  })
  
  # submit button
  observeEvent(input$submitButton3, {

    # renders the unknown values in a table
    output$UnknownsValue <- renderTable({
      x <- data2()[, 1]
      y <- data2()[, 2]
      matrix_xy <- cbind(x, y)
      
      # arrange
      matrix_xy <- matrix_xy[order(matrix_xy[, 1]), ]
      
      xtofind <- input$xtoeval
      list <- list(matrix_xy[,1],matrix_xy[,2],xtofind)
      result <- QSI(list)
      
      if (is.character(result)) {
        return(NULL)  # Return NULL if there is an error
      }
      
      result$UnknownsValue
    })
    
    # prints the functions for each interval
    output$fxperinterval <- renderPrint({
      x <- data2()[, 1]
      y <- data2()[, 2] 
      matrix_xy <- cbind(x, y)
      
      # arrange
      matrix_xy <- matrix_xy[order(matrix_xy[, 1]), ]
      
      xtofind <- input$xtoeval
      list <- list(matrix_xy[,1],matrix_xy[,2],xtofind)
      result <- QSI(list)
      
      if (is.character(result)) {
        return(NULL)  # Return NULL if there is an error
      }
      
      print(result$Functions)
    })
    
    # prints the function used
    output$fxused <- renderPrint({
      x <- data2()[, 1]
      y <- data2()[, 2] 
      matrix_xy <- cbind(x, y)
      
      # arrange
      matrix_xy <- matrix_xy[order(matrix_xy[, 1]), ]
      
      xtofind <- input$xtoeval
      list <- list(matrix_xy[,1],matrix_xy[,2],xtofind)
      result <- QSI(list)
      
      if (is.character(result)) {
        return(NULL)  # Return NULL if there is an error
      }
      
      print(result$FxUsed)
    })
    
    
    # prints the f(x) 
    output$fxeval <- renderText({
      x <- data2()[, 1]
      y <- data2()[, 2]
      matrix_xy <- cbind(x, y)
      
      # arrange
      matrix_xy <- matrix_xy[order(matrix_xy[, 1]), ]
      
      xtofind <- input$xtoeval
      list <- list(matrix_xy[,1],matrix_xy[,2],xtofind)
      result <- QSI(list)
      
      error <- "X is not in the intervals"
      # Check if 'f_x' exists in 'result'
      if (!is.null(result) && "f_x" %in% names(result)) {
        # Access 'f_x' from 'result'
        f_x <- result$f_x
      } else {
        # Set 'f_x' to NULL if 'f_x' does not exist in 'result'
        f_x <- NULL
      }
      
      if (is.null(f_x)) {
        return(error)  # Return NULL if 'f_x' is not found
      }
      
      # Return the value of 'f_x'
      f_x
    })
  })
  #---------------SIMPLEX----------------
  
  
  # Create reactive expression to track selected checkboxes
  selectedCheckboxes <- reactiveVal(foodlist)
  
  # Update selectedCheckboxes when checkboxGroupInput is changed
  observeEvent(input$checkboxes, {
    selectedCheckboxes(input$checkboxes)
  })
  
  # Uncheck all checkboxes when uncheckButton is clicked
  observeEvent(input$uncheckButton, {
    updateCheckboxGroupInput(session, "checkboxes", selected = character(0))
    selectedCheckboxes(character(0))
  })
  
  # Check all checkboxes when uncheckButton is clicked
  observeEvent(input$checkAllButton, {
    updateCheckboxGroupInput(session, "checkboxes", selected = foodlist)
  })
  
  # file that contains the data for the foods // initialize as a matrix
  data3 <- read.csv("nutrional_values.csv", header=FALSE)
  data_matrix <- as.matrix(data3)
  data_matrix <- cbind(data_matrix[, -1], data_matrix[, 1])
  
  # submit button
  observeEvent(input$submitButton, {
    checked_foods <- input$checkboxes
    checked_index <- match(checked_foods, foodlist)
    
    # Check if the user has selected at least two checkboxes
    if (length(checked_index) < 2) {
      # Display a warning or take other actions
      output$resultText <- renderText({
        paste("Please select at least two checkboxes.\n")})
      return()
    }
    
    
    # Extract the corresponding row from the data_matrix
    selected_food <- get_selected_rows(data_matrix,checked_index)
    foodNames <- get_food_names(checked_index, foodlist)
    servPrice <- get_servingPrice(servingPrice, checked_index)
    
    
    # Use the selected checkboxes as a parameter to the Simplex function
    result <- Simplex(selected_food, foodNames, servPrice)
    
    output$resultText <- renderText({
      if (is.character(result[1])) {
        return("INFEASIBLE!")  # Return NULL if there is an error
      }else{
      
        optCost <- sprintf("%.2f", result[1])
        paste("The cost of this optimal diet is $", optCost, "per day.")}
    
    })
    
    # render table of Final Solution
    output$solTableSimplex <- renderTable({
      if (is.character(result[2])) {
        print("INFEASIBLE!")  # Return NULL if there is an error
      } else{
      
     
       # result[[2]][length(result[[2]])]
        result[[3]]
      }
    })
    
    # render iterations
    output$consoleOutput <- renderPrint({
      
      if (is.character(result[2])) {
        print("INFEASIBLE!")  # Return NULL if there is an error
      }else{
        print(result[2])
        
      }
      
  
    })
    
    output$tableau <- renderPrint({
      
      if (is.character(result[3])) {
        print("INFEASIBLE!")  # Return NULL if there is an error
      }else{
      
      print(result[4])
      print(result[5])
      }
      
      
    })
    
    
  })
  
  
  #------------------------------------------------------------
  
  
  
  
  
} # server












# ----------------------------------------------------------------
# --------------------- MATH FUNCTIONS ---------------------------
#-----------------------------------------------------------------


################# GAUSS JORDAN #####################

GaussJordanMethod <- function(mat){
  
  n_rows <- nrow(mat)
  
  
  
  for (i in 1:n_rows){
    if (i != n_rows){
      pivotRow <- i
      
      # Find the row with the largest absolute value in the current column
      for (j in (i + 1):n_rows) {
        if (abs(mat[j, i]) > abs(mat[pivotRow, i])) {
          pivotRow <- j
        }
      }
      
      # Swap the current row with the pivot row
      if (pivotRow != i) {
        mat[c(i, pivotRow), ] <- mat[c(pivotRow, i), ]
      }
    }
    mat[i,] = mat[i,]/mat[i,i]
    for (j in 1:n_rows){
      if (i==j){
        next
      }
      normRow = mat[j,i]*mat[i,]
      mat[j,] = mat[j,] - normRow
      
    }
    # print(mat)
    
  }
  solution <- as.vector(mat[,ncol(mat)])
  return(solution)
}

################ POLYNOMIAL REGRESSION ####################

PolynomialRegression <- function(degree, inputs){
  
  # Catch error
  if (  length(inputs[[1]]) == 1 || length(inputs[[1]]) != length(inputs[[2]]) || !is.numeric(inputs[[1]]) || !is.numeric(inputs[[2]])){
    return("NA")
  }
  
  # Variables
  independent = inputs[[1]]
  dependent = inputs[[2]]
  
  nrows = degree + 1
  ncols = degree + 2
  
  # Initialize matrix
  augcoeffmatrix =   matrix(0, nrow = nrows, ncol = ncols)
  
  exponent = 0
  
  # Assign values on the matrix
  for (i in 1:nrows){
    exponent = i-1
    for (j in 1:ncols){
      element = independent^exponent
      print = element
      augcoeffmatrix[i,j] =  sum(element)
      
      if (j == ncols){
        element = ((independent)^(i-1))*dependent
        augcoeffmatrix[i,ncols] = sum(element)
      }
      exponent = exponent+1
    }
  }
  # print(augcoeffmatrix)
  coefficients = GaussJordanMethod(augcoeffmatrix)
  # print(coefficients)
  
  # Create the polynomial string using the coefficients from the matrix
  polynomial_string = ""
  exponent = 0
  for (i in 1:length(coefficients)){
    
    if (exponent == 0){
      polynomial_string = paste("function(x) ", polynomial_string, coefficients[[i]] , sep= "")
    }else{
      coeff_withexp = paste(coefficients[[i]],"* x ^", exponent, sep = " ")
      polynomial_string = paste(polynomial_string,coeff_withexp, sep = " + ")
    }
    
    exponent = exponent+1
    
  }
  # print(polynomial_string)
  polynomial_function = eval(parse(text = polynomial_string))
  # print(polynomial_function)
  
  output = list(Augcoeffmatrix = augcoeffmatrix, Coefficients = coefficients, Polynomial_string = polynomial_string, Polynomial_function = polynomial_function )
  
  
  
  return(output)
}


################## QSI ########################

QSI <- function(list){
  
  # Catch error
  if ( length(list[[1]]) < 3 || !is.numeric(list[[1]]) || !is.numeric(list[[2]])){
    return("NA")
  }
  
  
  x_values <- list[[1]]
  y_values <- list[[2]]
  x <- list[[3]]
  
  
  num_of_splines <- length(x_values)-1  #number of segments
  unknowns <- 3*num_of_splines          #number of unknown values
  
  mat <- matrix(0, nrow = unknowns, ncol = unknowns+1)
  temp <- 0
  for (i in 1:num_of_splines) {
    
    
    # each quadratic spline passes through two consecutive data points 
    ai_1 <- x_values[i]^2
    ai_2 <- x_values[i+1]^2
    bi_1 <- x_values[i]
    bi_2 <- x_values[i+1]
    ci_1 <- 1
    ci_2 <- 1
    
    # matrix setup 
    mat[i+temp,(3*i-2):(3*i)] <- c(ai_1, bi_1, ci_1)
    mat[i+(temp + 1),(3*i-2):(3*i)] <- c(ai_2, bi_2, ci_2)
    mat[i+temp,(3*num_of_splines+1)] = y_values[i]
    mat[i+(temp + 1),(3*num_of_splines+1)] = y_values[i+1]
    temp = temp + 1
    
  }
  # continuation of the matrix
  j <- 1
  for (i in (2*(num_of_splines)+1):(3*(num_of_splines)-1)) {
    ai_1 <- x_values[j+1]*2
    ai_2 <- x_values[j+1]*-2
    bi_1 <- 1
    bi_2 <- -1
    ci_1 <- 0
    ci_2 <- 0
    
    
    mat[i,(3*j-2):(3*j+3)] <- c(ai_1, bi_1, ci_1,ai_2, bi_2, ci_2)
    j<- j+1
    
  }
  
  mat[(3*num_of_splines), (1:3)] <- c(1,0,0)
  
  # get unknown values using Gauss Jordan and convert into a matrix
  unknownsvalue = GaussJordanMethod(mat)
  unknownsvalue_mat = matrix(unknownsvalue, nrow = num_of_splines, ncol = 3,dimnames = list(1:num_of_splines, c("a", "b", "c")),byrow = TRUE)
  
  
  # write the functions using the unknowns
  functionlist <- list()
  for (i in 1:num_of_splines) {
    
    functionstring <- paste("function(x) ", unknownsvalue_mat[i,1], " * x ^ 2 + ", unknownsvalue_mat[i,2], " * x + ", unknownsvalue_mat[i,3] , sep= "")
    functionfx <- eval(parse(text = functionstring))
    
    functionlist <- append(functionlist, functionfx)
    
    
  }
  

  # get the function value
  for (i in 1:num_of_splines) {
    if(x >= x_values[i] & x <= x_values[i+1]){
      f_x <- functionlist[[i]](x)
      fxUsed <- functionlist[[i]]
      break
    } else{
      f_x <- NULL
      fxUsed <- NULL
    }
    
  }

  
  output <- list(UnknownsValue = unknownsvalue_mat, Functions = functionlist ,f_x = f_x, FxUsed = fxUsed)
  return(output)
}

############### SIMPLEX #################


servingPrice <- c(0.16, 0.07, 0.04, 0.18, 0.02, 0.53, 0.06, 0.31, 0.84, 0.78, 0.27, 0.24, 0.15, 0.32, 0.49, 0.15,
                  0.16, 0.05, 0.06, 0.09, 0.16, 0.03, 0.05, 0.25, 0.16, 0.23, 0.13, 0.08, 0.11, 0.15, 0.27, 0.33,
                  0.15, 0.31, 0.28, 0.28, 0.34, 0.32, 0.38, 0.82, 0.52, 0.44, 0.59, 0.83, 0.31, 0.39, 0.08, 0.17,
                  0.07, 0.81, 0.45, 0.69, 0.04, 0.22, 0.12, 0.19, 0.39, 0.67, 0.71, 0.75, 0.39, 0.99, 0.65, 0.67)

Simplex <- function(foods, foodNames, servPrice){
  num_of_foods <- length(foods)
  # constraints
  max_constraint = c(2250, 300, 65, 2400, 300, 100, 100, 50000, 20000, 1600, 30)
  min_constraint = c(2000, 0, 0, 0, 0, 25, 50, 5000, 50, 800, 10)
  serving_constraint <- rep(10, each = 64)
  
  # set up the matrix
  foodmax <- do.call(cbind, foods)
  colnames(foodmax) <- paste0("food_", 1:num_of_foods)
  nutmat_max <- cbind(foodmax[1:(nrow(foodmax)-1),],max_constraint)
  colnames(nutmat_max)[ncol(nutmat_max)] <- "Soln"
  
  nutmat_max <- nutmat_max *(-1)
  foodmin <- do.call(cbind, foods)
  nutmat_min <- cbind(foodmin[1:(nrow(foodmin)-1),], min_constraint)
  nutmat <- rbind(nutmat_max, nutmat_min)
  rownames(nutmat) <- paste0("S", 1:nrow(nutmat))
  
  
  # add the the serving constraints
  temp <- matrix(0, nrow = num_of_foods, ncol = 1 + num_of_foods )
  rownames(temp) <- paste0("Serving_", 1:nrow(temp))
  
  for (i in 1:num_of_foods) {
    for (j in 1: (num_of_foods+1)) {
      if (i==j){
        temp[i,j] = -1
      }
      if (j== ncol(temp)){
        temp[i,j] = -10
      }
    }
  }
  
  nutmat <- rbind(nutmat, temp)
  
  
  # add the objective function
  Cost <- matrix(0, nrow = 1, ncol = 1 + num_of_foods )
  Cost[,1:num_of_foods] = foodmax[nrow(foodmax), 1:num_of_foods]
  
  nutmat <- rbind(nutmat, Cost)
  
  #transpose the matrix
  t_nutmat <- t(nutmat)
  
  # flip the signs of the last row
  t_nutmat[nrow(t_nutmat),] = t_nutmat[nrow(t_nutmat),] * (-1)
  
  foodNames <- c(foodNames,"Z")
  
  # add the slack variables to the matrix
  slack_vars <- matrix(0, nrow = nrow(t_nutmat), ncol = (num_of_foods + 1), dimnames = list(NULL, foodNames))
  
  
  for (i in 1: (num_of_foods+1)) {
    slack_vars[i,i] = 1
  }
  
  
  
  start_indexFoods = ncol(t_nutmat)
  end_indexFoods = start_indexFoods + (ncol(slack_vars)-2)
  
  # iteration 1 of the tableau
  t_nutmat <- cbind(t_nutmat[, 1:(ncol(t_nutmat)-1)], slack_vars, t_nutmat[, ncol(t_nutmat)])
  initialTableau <- t_nutmat
  
  
  ######################## SIMPLEX [GAUSSJORDAN] ##############################
  
  n_rows <- nrow(t_nutmat)
  iterationList = c()
  basicSolList = c()
  tableau <- c()
  iteration <- 1
  
  basicSolution <- t(t_nutmat[1, 1:(ncol(t_nutmat)-1)])
  colnames(basicSolution) <- colnames(t_nutmat)[1:(length(colnames(t_nutmat))-1)]
  dimnames(basicSolution)[[1]][1] <- ""
  while (any(t_nutmat[nrow(t_nutmat),] < 0)) {
    iteration <- iteration +1
    # pivot column => highest magnitude value that is negative
    PC_index <- which.min(t_nutmat[nrow(t_nutmat),])
    # test ratio vector
    TR <- t_nutmat[,ncol(t_nutmat)] /t_nutmat[,PC_index]
    #   # call the getMinimum function to get the pivot row
    PR_index <- getMinimum(TR)
    if (PR_index == -1){
      print("INFEASIBLE: Solutions are all negative in the last iteration. ")
      return("INFEASIBLE: Solutions are all negative in the last iteration. ")
    }
    
    PR <- t_nutmat[PR_index,]
    
    # pivot element
    PE <- t_nutmat[PR_index, PC_index]
    
    if (PE == 0){
      print("INFEASIBLE: Pivot Element is zero.")
      return("INFEASIBLE: Pivot Element is zero.")
    }
    
    # normalized pivot row
    nPR <- PR/PE
    
    for (j in 1:nrow(t_nutmat)){
      if (j == PR_index){
        t_nutmat[j,] = nPR
      }else{
        t_nutmat[j,] = t_nutmat[j,] - (nPR * t_nutmat[j,PC_index])
      }
    }
    
    # build the table of solutions
    Servings <- c()
    Food <- c()
    cost <- c()
    for (x in start_indexFoods:end_indexFoods) {
      if (t_nutmat[nrow(t_nutmat), x] > 0) {
        Servings <- c(Servings, t_nutmat[nrow(t_nutmat), x])
        Food <- c(Food, colnames(t_nutmat)[x]) 
        
        cost <- c(cost, servPrice[(x-start_indexFoods)+1])
      }
    }
    
    for (x in 1:(ncol(t_nutmat)-1)) {
      for (y in 1:nrow(t_nutmat)) {
        if (t_nutmat[y, x] == 1) {
          basicSolution[x] <- t_nutmat[y, ncol(t_nutmat)]
        }
      }
    }
    
    
    cost <- Servings * cost
    results <- data.frame(Food, Servings, cost)
    colnames(results)[colnames(results) == "cost"] <- "Cost"
    iterationList <- c(iterationList, list(results))
    basicSolList <- c(basicSolList, list(basicSolution))
    tableau <- c(tableau, list(t_nutmat))
    
    
    # Check if there are no more negative numbers in the last row
    if (!any(t_nutmat[nrow(t_nutmat),] < 0)) {
      break
    }
    
  }
  
  
  # output <- list(Solution = t_nutmat[nrow(t_nutmat), ncol(t_nutmat)], Iterations = iterationList, Tableau = tableau)
  output <- list(Solution = t_nutmat[nrow(t_nutmat), ncol(t_nutmat)], BasicSolution = basicSolList, SolCostBreakdown = iterationList[length(iterationList)], InitialTableau = initialTableau ,Iterations = tableau)
  
  return(output)
  
}

#################### SIMPLEX [OTHER FX] ######################

get_servingPrice <- function(servingPrice, indices){
  prices <- servingPrice[indices]
  return(prices)
}



get_selected_rows <- function(matrix, indices) {
  selected_rows <- lapply(indices, function(i) matrix[i, ])
  return(selected_rows)
}


get_food_names <- function(indices, foodlist) {
  food_names <- foodlist[indices]
  return(food_names)
}

getMinimum <- function(vector){
  
  vec <- vector
  min <- max(vec)
  
  for (i in 1:length(vec)) {
    if (vec[i] < min && vec[i] > 0){
      min <- vec[i]
    }
  }
  
  min_index <- as.vector(which(vec == min,arr.ind = TRUE))
  
  if (min <= 0){
    return(-1)
  }
  return(min_index[1])
}

# Create Shiny object
shinyApp(ui = ui, server = server)