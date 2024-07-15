# Criminal-Case-Data-Analysis-R
This project involves a comprehensive examination of legal case outcomes, including data pre-processing, exploratory analysis, and predictive modeling. The primary goals are to combine different data sources, clean and validate the data, examine missing values, conduct descriptive analytics, and research prescriptive analytics, clustering, and classification approaches.

## Dataset

The dataset in this report is supported by The Crown Prosecution Service Case Outcomes by Principal Offense Category from the data.gov.uk website. The outcomes fall into two categories: convictions and unsuccessful outcomes. Convictions include pleas of guilty, sentencing after trial, and cases proven in the absence of the accused. Unsuccessful outcomes represent any consequences other than conviction, including dismissal, withdrawal, dismissal and acquittal, and administrative amendment.

Offenses data are partitioned into the following categories:
- Homicide
- Offenses against the individual
- Sexual offenses
- Theft
- Robbery
- Extortion and fraud
- Criminal harm
- Drugs offenses
- Criminal case committed to public places and automobiles

All other offenses but motoring-related offenses are included within these categories.

## Key Features

- **Data Integration:** Combining several datasets from various sources into a unified dataset for analysis.
- **Data Cleaning:** Performing techniques to solve inconsistencies and check for missing values.
- **Descriptive Analytics:** Using correlation analysis to study the links between variables and quantify the direction of connections. Visualizations such as heatmaps are used to graphically display patterns and connections in the data.
- **Prescriptive Analytics:** Utilizing linear regression and multiple regression to model connections between dependent and independent variables, identifying important predictors, and analyzing their influence on the target variable. Hypothesis testing is conducted to provide evidence supporting or rejecting proposed correlations.
- **Clustering Analysis:** Implementing k-means clustering with different cluster numbers (3, 4, 5, and 6) to find unique groups within the dataset. Hierarchical clustering is also used to show hierarchical connections and generate dendrogram visualizations.
- **Classification Approaches:** Employing k-nearest neighbors (KNN) to categorize cases and measure predictive accuracy.

## Analysis Process

The project includes the following steps, all performed within a single R script:

1. **Data Integration:**
   - Combining datasets from various sources into a unified dataset.

2. **Data Cleaning:**
   - Solving inconsistencies and handling missing values.

3. **Descriptive Analytics:**
   - Using correlation analysis and visualizations (e.g., heatmaps) to study variable relationships.

4. **Prescriptive Analytics:**
   - Conducting linear and multiple regression analyses to model variable connections and test hypotheses.

5. **Clustering Analysis:**
   - Performing k-means clustering with different cluster numbers.
   - Using hierarchical clustering and dendrogram visualizations.

6. **Classification Approaches:**
   - Applying k-nearest neighbors (KNN) to classify cases and measure predictive accuracy.
