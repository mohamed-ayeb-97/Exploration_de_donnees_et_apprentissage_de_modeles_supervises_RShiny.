## Exploration and Supervised Learning Models with RShiny

### Overview

This project is an interactive web application developed using Shiny, designed for data exploration and the application of supervised learning models. The application facilitates the loading and analysis of various datasets, offering a user-friendly interface for both data preprocessing and model evaluation.

### Features

- **Data Preprocessing**: Imputation, normalization, dummification, and handling class imbalance.
- **Exploratory Data Analysis (EDA)**: Univariate, bivariate, and multivariate analysis with visualization tools.
- **Model Training and Evaluation**: Supports multiple supervised learning algorithms with performance metrics like accuracy, recall, F-score, ROC curves, and AUC.
- **Interactive Interface**: Users can upload their own datasets for analysis and model training.

### Usage

1. **Clone the Repository**:
   ```sh
   git clone https://github.com/mohamed-ayeb-97/Exploration_de_donnees_et_apprentissage_de_modeles_supervises_RShiny.git
   ```

2. **Install Required Packages**:
   ```r
   install.packages(c("shiny", "ggplot2", "dplyr", "caret", "randomForest"))
   ```

3. **Run the Application**:
   ```r
   library(shiny)
   runApp("path_to_your_cloned_repo")
   ```

### Getting Started

The application guides users through the following steps:

1. **Data Loading**: Upload a dataset in CSV format.
2. **Data Exploration**: Perform EDA with various visualization options.
3. **Model Training**: Select and train supervised learning models (e.g., logistic regression, decision trees, random forests).
4. **Model Evaluation**: View performance metrics and visualize important features.

### Example Datasets

The application is designed to work with several recommended datasets such as Dermatology, Statlog-Heart, Hepatitis, Breast Cancer Wisconsin, and more.

### Live Demo

Check out the live application [here](https://mohamed-elayeb.shinyapps.io/Projet_Prog_Web/).

### Contributions

Contributions are welcome. Please fork the repository and create a pull request to contribute to the project.

---
