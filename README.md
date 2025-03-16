# Miltsim
Miltefosine Dose Optimization and Population Analysis

## Shiny Application

This project includes a Shiny application designed for Miltefosine dose optimization and population analysis. The application provides an interactive interface for users to:

- Input patient-specific data
- Visualize dose-response relationships
- Simulate different dosing regimens
- Analyze population-level pharmacokinetic and pharmacodynamic data

### Features

- **Patient Data Input**: Users can enter patient-specific information such as age, weight, height.
- **Dose-Response Visualization**: Interactive plots to visualize how different doses affect patient outcomes.
- **Dosing Simulations**: Simulate various dosing regimens to find the optimal dose for different patient groups.
- **Population Analysis**: Analyze data across a population to identify trends and optimize dosing strategies.

### Getting Started

To run the Shiny application, follow these steps:

1. Ensure you have R and Shiny installed on your system.
2. Clone this repository to your local machine.
3. Open the R project file in RStudio.
4. Run the Shiny application using the following command in the R console:

    ```R
    shiny::runApp('path/to/shiny/app')
    ```

Replace `'path/to/shiny/app'` with the actual path to the Shiny application directory.

### Dependencies

The Shiny application relies on the following R packages:

- `shiny`
- `ggplot2`
- `dplyr`
- `tidyr`

Make sure to install these packages before running the application.

### Contributing

We welcome contributions to improve the Shiny application. Please fork the repository and submit pull requests with your changes.
