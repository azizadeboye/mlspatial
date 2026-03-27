---
title: 'mlspatial: Machine Learning and Mapping for Spatial Epidemiology'
tags:
- R
- Spatial epidemiology
- Machine Learning
- Disease Mapping
- Oncology
date: "26 March 2026"
output:
  word_document: default
  html_document:
    df_print: paged
  pdf_document: default
authors:
- name: Adeboye Azeez
  affiliation: 1
  orcid: "0000-0001-9427-7374"
- name: Colin Noel
  affiliation: 1
  orcid: "0000-0003-2344-415X"
bibliography: paper.bib
affiliations:
- name: Gastrointestinal Research Unit, University of the Free State, Bloemfontein
    9301, South Africa
  index: 1
editor_options:
  markdown:
    wrap: sentence
---

# Summary

`mlspatial` provides a unified framework for the integration, visualization, and modelling of spatial epidemiological data, combining geospatial analysis with modern machine learning techniques. The package is designed to support the workflow to facilitate the exploration and prediction of disease patterns across geographic regions. It enables users to import, clean, and preprocess spatial datasets, including shapefiles and associated health or demographic data, and to generate high-quality thematic maps for exploratory analysis. The core functionality integrates machine learning algorithms, such as Random Forest, XGBoost, and Support Vector Regression to model spatial variation, identify key risk factors, and produce predictive risk surfaces.

The framework supports end-to-end spatial analysis, from data preparation and feature engineering to model training, validation, and visualization of outputs. It also allows users to customize modelling workflows and adapt them to different diseases, regions, and data structures. Although motivated by applications in Pancreatic cancer epidemiology, `mlspatial` is domain-flexible and can be applied broadly across public health and spatial data science contexts. It is particularly suited for spatial epidemiologists, public health researchers, and GIS analysts seeking to uncover geographic patterns, improve predictive accuracy, and inform targeted, evidence-based interventions.


# Statement of need

Spatial epidemiology is essential for understanding the geographic distribution of disease and informing targeted public health interventions. However, many existing analytical workflows remain fragmented, requiring separate tools for geospatial processing, statistical modelling, and machine learning (ML). While datasets such as the Global Burden of Disease (GBD) provide rich epidemiological information across regions and time [@GBD2021], current approaches are often limited to descriptive mapping or traditional statistical models that may not capture complex, non-linear relationships in health data [@Spurna2008].

Recent studies have demonstrated that ML algorithms can significantly improve predictive performance in epidemiological research [@Arkoudis2025, @Rahimi2024, @Afrash2023, @Harison2021, @Rajkomar2019, @Rajula2020], particularly for modelling disease incidence, prevalence, and mortality. Despite this, there is a lack of unified, reproducible frameworks that integrate ML with spatial analysis and mapping. Existing R packages typically focus on either geospatial analysis or machine learning, requiring users to manually combine multiple tools, which can reduce reproducibility and increase implementation complexity.

The mlspatial package addresses this gap by providing an integrated framework for spatial epidemiology that combines geospatial data processing, ML-based predictive modelling, and spatial statistical diagnostics within a single workflow. It enables users to perform end-to-end analyses, including data preprocessing, model training (Random Forest, XGBoost, Support Vector Regression), spatial autocorrelation analysis (Moran’s I, LISA), and visualisation of predictive risk maps [@Spurna2008, @Juliani2024]. By bridging these components, mlspatial improves accessibility, reproducibility, and scalability of spatial epidemiological modelling, particularly in data-limited settings such as Africa.

# Software Architecture and Workflow

`mlspatial` is designed as a modular and extensible R package that supports the full lifecycle of spatial epidemiological analysis. The workflow is organised into four main components:

## Data Input and Preprocessing

Functions are provided to import and harmonise spatial (e.g., shapefiles) and non-spatial epidemiological datasets. The package supports data cleaning, transformation, feature engineering, and merging of geographic and health data.

## Spatial Analysis and Visualisation

The package enables exploratory spatial data analysis through thematic mapping and geospatial visualisation. It includes tools for computing spatial weights matrices and assessing spatial autocorrelation using Global Moran’s I and Local Indicators of Spatial Association (LISA) [@Spurna2008, @Juliani2024].

## Machine Learning Modelling

`mlspatial` integrates multiple supervised ML algorithms, including Random Forest, XGBoost, and Support Vector Regression. These models are implemented within a consistent interface, allowing users to train, tune, and validate predictive models using cross-validation techniques.

## Model Evaluation and Mapping Outputs

The framework provides standard performance metrics such as RMSE, MAE, and R² [@Willmott2005, @Hodson2022], along with diagnostic plots and residual analysis. Predicted outcomes and residuals can be visualised spatially, enabling users to assess geographic patterns in model performance.

This modular design ensures reproducibility and flexibility, allowing users to adapt workflows to different datasets, diseases, and geographic contexts.


# Methods

The `mlspatial` framework combines machine learning modelling with spatial statistical analysis to support predictive mapping of epidemiological outcomes.

## Machine Learning Models

The package implements three supervised regression approaches:

- Random Forest (RF): An ensemble method that aggregates multiple decision trees to improve predictive accuracy and robustness.
- Extreme Gradient Boosting (XGBoost): A gradient boosting technique that efficiently captures complex non-linear relationships in structured data [@Chen2021, @Chakraborty2023, @Khan2024].
- Support Vector Regression (SVR): A kernel-based approach suitable for modelling non-linear continuous outcomes.

## Model Training and Validation

Models are trained using demographic and temporal predictors (e.g., age, sex, year). A temporal train-test split is applied, and five-fold cross-validation is used for hyperparameter tuning and model selection.

## Performance Evaluation

Model performance is assessed using:

- Root Mean Square Error (RMSE) [@Willmott2005, @Hodson2022]
- Mean Absolute Error (MAE) [@Willmott2005, @Hodson2022]
- Coefficient of Determination (R²)

These metrics are computed across training and validation datasets to ensure generalisability.

## Spatial Autocorrelation Analysis

To account for spatial dependence, mlspatial incorporates:

- Global Moran’s I for measuring overall spatial autocorrelation [@Spurna2008]
- Local Indicators of Spatial Association (LISA) for detecting local clusters and spatial outliers [@Juliani2024]

Spatial weights are defined using contiguity-based approaches, enabling the identification of geographic clustering in both observed and predicted outcomes.

## Integrated Spatial Prediction

The framework produces spatially explicit predictions and residual maps, allowing users to evaluate geographic variation in disease burden and model performance. This integration of ML and spatial statistics supports more accurate and interpretable epidemiological analyses.



# Acknowledgements

The development and research of the `mlspatial` package was made possible through generous support from GIT Research Unit, Department of Surgery, University of the Free State.
The authors would like to sincerely thank these organizations for their financial assistance as well as all of the individuals who participated in the project.


# References

