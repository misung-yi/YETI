# YETI: Pet MBTI based on PCA

YETI is a statistical framework for classifying pet dog personalities using an MBTI-inspired structure based on C-BARQ behavioral data.

## Overview
This project constructs four dichotomous personality axes (Extraversion, Behavior, Stability, Intelligence) and derives scoring functions using Principal Component Analysis (PCA).

## Documentation
- [Methodology](Methodology.md)

## Data and Reproducibility

To reproduce the YETI scoring formula exactly as implemented in this study, the same data and preprocessing conditions must be applied. Specifically, the dataset used in this study consists of records collected between March 7, 2008, and May 30, 2025. The data were randomly split into training and test sets using a 70/30 ratio, with a fixed random seed of `set.seed(20251216)` to ensure reproducibility.

The YETI scoring model (including PCA loadings, scaling parameters, and beta coefficients) was derived exclusively from the training dataset. Therefore, to obtain identical scoring formulas and results, the model must be reconstructed under the same conditions, including the same data range, random seed, and train–test split procedure. Any deviation from these settings may lead to differences in the resulting principal components and derived coefficients, and consequently, the YETI scores.

The data used in this study are based on the C-BARQ database. Researchers who wish to replicate or extend this work are encouraged to obtain access to the dataset directly from the data owners through the official C-BARQ website:

👉 https://vetapps.vet.upenn.edu/cbarq/
