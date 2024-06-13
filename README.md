# PolySolver Suite

## Overview

PolySolver Suite is a versatile program designed to address various mathematical optimization and curve fitting challenges. It seamlessly handles polynomial regression, quadratic spline interpolation, and the simplex method, providing powerful solutions for a range of applications.

## Features

### A. Generic Solvers

1. **CSV Input**  
   Import data directly from CSV files for analysis.

2. **Quadratic Spline Interpolation (QSI)**  
   Perform quadratic spline interpolation to obtain smooth curve fitting through a series of data points.

   **Expected Outputs:**
   - \( f(x) \) per interval
   - Correct \( f(x) \) for the estimate

3. **Polynomial Regression (PR)**  
   Execute polynomial regression to determine the polynomial function that best fits the given data.

   **Expected Outputs:**
   - \( f(x) \)
   - Estimate of \( f(x) \)

### B. Simplex Implementation

1. **Diet Problem Solver**  
   This feature is designed to find the cheapest and most nutritious combination of foods that satisfy all daily nutritional requirements. The diet problem is formulated as a linear program aiming to minimize cost while meeting nutritional constraints.

   **Key Elements:**
   - **Objective:** Minimize the total cost of the diet.
   - **Constraints:** Ensure the diet meets specified nutritional needs within the allowed range of servings for each food item.
   - **Inputs:** Food items, nutritional values, costs, and serving constraints.
   - **Outputs:** Optimal combination of food items, number of servings, total cost, tableau, and basic solution for each iteration. The final solution and objective function value are also provided.

   **Specifications:**
   - User-friendly interface for selecting food items and adjusting inputs.
   - Option to select any or all food items.
   - Reset option to easily modify inputs.
   - A button to start solving the problem.
   - Final output includes a table with the optimal combination of food items, number of servings, and total cost.
   - Display of the tableau and basic solution for each iteration.
   - Identification of the final solution and objective function value.

   **Programming Languages:** R, JavaScript, or Python.

## Assumptions

- Any food item may be selected for the Diet Problem Solver.
- Some solutions may be infeasible due to the non-satisfaction of constraints.

## User Manual for PolySolver Suite

### Packages to Install

To run the PolySolver Suite, ensure you have the necessary R packages installed. Follow these steps to install the required packages:

1. **Shiny Package**
   
   Shiny is an R package that makes it easy to build interactive web applications straight from R.

2. **Shiny Themes Package**
   
   Shiny themes provide a variety of visual themes for Shiny applications to enhance the user interface.

### Installation Steps

1. Open an R session on your computer.
2. Ensure you are connected to the internet.
3. Run the following commands to install the required packages:

   ```R
   install.packages("shiny")
   install.packages("shinythemes")
   ```

These commands will download and install the Shiny and Shiny Themes packages from the Comprehensive R Archive Network (CRAN). Once installed, you will be able to run the PolySolver Suite with interactive web applications and enhanced themes.

### Running the Application

After installing the necessary packages, you can launch the PolySolver Suite by opening your R session and running the main script of the application. Ensure all dependencies are loaded and start solving your polynomial regression, quadratic spline interpolation, and simplex method problems with ease.

## Usage

PolySolver Suite is implemented using R. It is designed to be intuitive and user-friendly, making it accessible for users with various levels of expertise in mathematical optimization and curve fitting.

Enjoy using PolySolver Suite to explore and solve your polynomial regression, quadratic spline interpolation, and linear programming challenges effectively!
