## Methodology

### Overview
The YETI framework was designed to statistically classify pet dog personalities using an MBTI-inspired structure. The methodology focuses on constructing interpretable personality axes and deriving scoring functions through data-driven techniques.

### Personality Axis Construction
Behavioral variables were derived from the C-BARQ (Canine Behavioral Assessment and Research Questionnaire). Based on these items, four dichotomous personality axes were defined:
- **Extraverted vs. Introverted** (social openness)
- **Troublemaker vs. Angel** (problem-behavior propensity)
- **Stable vs. Unstable** (emotional stability)
- **Bright vs. Clumsy** (learning ability)

Each axis was constructed using a subset of behavior variables corresponding to its conceptual domain.

### Principal Component Analysis (PCA)
For each personality axis, Principal Component Analysis (PCA) was conducted using the training dataset only:
- Input variables were standardized prior to PCA
- Principal components were retained until the cumulative explained variance approached **70%**.

### Composite Score Derivation
For each axis, a composite score was calculated as a weighted sum of the retained principal components:
- Standardized component scores were used
- Weights correspond to PCA loadings and derived coefficients from the training data

These composite scores represent continuous latent traits for each personality dimension.

### Data Preprocessing
- Outliers were removed prior to model construction
- All preprocessing steps were applied consistently within the training dataset

### Dichotomization Strategy
Each continuous composite score was converted into a binary classification:
- Three candidate cutoff values were evaluated
- Performance was assessed using both training and validation datasets

### Final Classification Rule
A cutoff value of **0** was selected for all four axes, as it showed the most consistent performance across datasets.  
This choice ensures both interpretability and stability of the classification results.
