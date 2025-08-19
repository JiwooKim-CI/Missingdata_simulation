# Missing Data Simulation

This repository contains the source code for the **Missing Data Simulation Shiny App**, an interactive tool for assessing the criteria under which intercept and slope parameters can be estimated without bias using the listwise deletion method, given specified m-DAGs.  
The app is deployed at: [https://8sezpc-jiwookim0ci.shinyapps.io/Aexam/](https://8sezpc-jiwookim0ci.shinyapps.io/Aexam/)
The code for generating the Shiny App is also available in this GitHub repository [app.R](app.R).

---

## Overview

The **Missing Data Simulation Shiny App** allows users to:
- Visualize and manipulate **m-DAGs** (missingness-directed acyclic graphs).
- Explore **recoverability** of intercept and slope (causal effects) in a given model.
- Examine the impact of **different missing data mechanisms** on statistical estimation.

This tool was developed as part of the preparation for the PhD A-exam at Cornell University, with a focus on **listwise deletion under missing data**.

---

## Using the App

1. **Access the app**  
   Go to [Missing Data Shiny App](https://8sezpc-jiwookim0ci.shinyapps.io/Aexam/).

2. **Navigate between tabs**  
   - **Model Setup (Missingdata Proportion Setting, Path Coefficients)** – Specify variables, missingness indicators, and structural assumptions via the m-DAG editor. Missingness can be assumed in both \(X\) and \(Y\). Relationships between $X$, \(Y\), \(R_X\), and \(R_Y\) can be modified by adjusting:
     1. Proportion and direction of missingness
     2. Path coefficients  
     m-DAGs are generated under the assumption that a missingness indicator cannot affect its own latent variable, following prior research. The DAG will update automatically based on the selected settings.
   - **Run Simulation** – run Monte Carlo simulations to examine bias, variance, and coverage under different mechanisms.
   - **DAGs** – DAGs generated following the model setup can be checked.
   - **D-Separation Criterion** – Apply *d-separation* to test recoverability of intercept and slope.
   - **Results (Intercept Plot, Slope Plot, Scatter Plot, Regression Results)** – View numerical summaries and plots. In the intercept and slope plot windows, density plots are shown for both datasets—with missing values (generated according to the model setup) and without missing values—separately for the assumed intercept and slope. The scatter plot displays data points from both datasets, with and without missingness.

---

## Features

- **Interactive DAG Editor** – Build and modify m-DAGs to represent missing data structures.
- **Parameter Control** – Adjust path coefficients, missingness probabilities, and sample size with sliders.
- **Recoverability Check** – Apply *d-separation* to evaluate identifiability.
- **Simulation Engine** – Run repeated simulations to evaluate estimator performance.
- **Visualization** – Display DAGs, parameter paths, and simulation results.

---

## Recommended Workflow

1. **Specify the m-DAG**  
   Define the relationships between \(X\), \(Y\), \(R_X\), and \(R_Y\) based on theoretical knowledge.

2. **Modify relationships as needed**  
   Adjust path coefficients, missingness proportions, and (if applicable) add interaction terms.  
   *Note:* Interaction is only allowed between \(X\) and the unobserved confounder of \(Y\) and \(R_Y\), following Ross et al. (2020).

3. **Check recoverability**  
   The app automatically estimates recoverability of intercept and slope from the assumed m-DAG.

4. **Simulate data (optional)**  
   Run simulations to evaluate performance under hypothetical data-generating processes.


---

## Background

The app draws on:
- **m-DAG theory** for representing missingness mechanisms.
- **Causal inference** methods, including *d-separation*.

**Reference:**  
- Bhattacharya, R., Nabi, R., Shpitser, I., & Robins, J. M. (2020). Identification in missing data models represented by directed acyclic graphs. *Proceedings of the 35th Uncertainty in Artificial Intelligence Conference (UAI)*. *Proceedings of Machine Learning Research*, 115, 1149–1158. https://proceedings.mlr.press/v115/bhattacharya20b.html

- Ross, R., Breskin, A., & Westreich, D. (2020). When is a complete case approach to missing data valid? The importance of effect measure modification. *American Journal of Epidemiology*, *189*(11), 1331–1339. https://doi.org/10.1093/aje/kwaa124

- Rubin, D. B. (1976). Inference and missing data. *Biometrika*, *63*(3), 581–592. https://doi.org/10.1093/biomet/63.3.581

- Mohan, K., Pearl, J., & Tian, J. (2013). Graphical models for inference with missing data. In C. J. Burges, L. Bottou, M. Welling, Z. Ghahramani, & K. Q. Weinberger (Eds.), *Advances in Neural Information Processing Systems* (Vol. 26, pp. 1277–1285). Curran Associates, Inc. https://proceedings.neurips.cc/paper_files/paper/2013/file/0ff8033cf9437c213ee13937b1c4c455-Paper.pdf



