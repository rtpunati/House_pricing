
# House Pricing

This project implements a classification model to predict house prices based on various features using machine learning algorithms. The dataset contains various features such as the number of bedrooms, bathrooms, square footage, and more. The project evaluates different models including Decision Trees, Random Forests, and Bagging, and compares their performance on both training and validation data.

## Table of Contents

- [Project Description](#project-description)
- [Data Description](#data-description)
- [Modeling](#modeling)
  - [Decision Tree](#decision-tree)
  - [Random Forest](#random-forest)
  - [Bagging](#bagging)
- [Evaluation](#evaluation)
- [Installation](#installation)
- [Usage](#usage)
- [License](#license)

## Project Description

The objective of this project is to predict whether a house's price falls into one of two categories (0 or 1) based on several features. The dataset includes information such as:

- Bedrooms
- Bathrooms
- Square footage (living area, lot size)
- Condition, view, and more

We apply different machine learning models to predict the price category, and evaluate their accuracy, sensitivity, and specificity.

## Data Description

The dataset consists of 4,549 observations across 18 variables:

- **price**: House price (numerical)
- **bedrooms**: Number of bedrooms
- **bathrooms**: Number of bathrooms
- **sqft_living**: Square footage of the living space
- **sqft_lot**: Square footage of the lot
- **floors**: Number of floors in the house
- **waterfront**: Whether the house is near the waterfront (binary: 0 = No, 1 = Yes)
- **view**: Quality of the view (integer scale)
- **condition**: Condition of the house (integer scale)
- **sqft_above**: Square footage of the house above ground
- **sqft_basement**: Square footage of the basement
- **yr_built**: Year the house was built
- **yr_renovated**: Year the house was renovated
- **street**: Street address (string)
- **city**: City (string)
- **statezip**: State and zip code (string)
- **country**: Country (string)

## Modeling

### Decision Tree

A Decision Tree classifier was trained on the dataset to predict the house price category based on the features provided. The importance of the features was calculated, and the classification matrix for both training and validation data was evaluated.

### Random Forest

Random Forests, an ensemble method, were used for classification. It helps to improve accuracy by aggregating the predictions from multiple decision trees. Performance was evaluated with classification matrices for both the training and validation sets.

### Bagging

Bagging (Bootstrap Aggregating) is another ensemble method that combines multiple models to reduce variance and improve accuracy. This method was implemented and evaluated using the classification matrix for both training and validation data.

## Evaluation

The models were evaluated based on the following metrics:

- **Accuracy**: The proportion of correctly predicted instances.
- **Sensitivity (True Positive Rate)**: The proportion of actual positive cases correctly identified.
- **Specificity (True Negative Rate)**: The proportion of actual negative cases correctly identified.

### Decision Tree Evaluation

- **Training Accuracy**: 80.29%
- **Validation Accuracy**: 79.18%
- **Sensitivity**: 66.57%
- **Specificity**: 88.47%

### Random Forest Evaluation

- **Training Accuracy**: 97.11%
- **Validation Accuracy**: 80.11%
- **Sensitivity**: 93.53%
- **Specificity**: 99.24%

### Bagging Evaluation

- **Training Accuracy**: 99.60%
- **Validation Accuracy**: 80.19%
- **Sensitivity**: 99.41%
- **Specificity**: 99.71%

## Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/rtpunati/house-price-classification.git
   ```

2. Install dependencies:
   ```bash
   pip install -r requirements.txt
   ```

## Usage

1. Load the dataset and preprocess it.
2. Train and evaluate models (Decision Tree, Random Forest, Bagging).
3. Generate performance metrics such as accuracy, sensitivity, and specificity.
4. Visualize the results and compare model performance.
