# ISSS608 Group 10 Project

This repository contains the Group 10 project website for **ISSS608 Visual Analytics**.

The home page is [`index.qmd`](index.qmd), which introduces **COFINFAD: Colombian Fintech Financial Analysis**, a visual analytics project examining how socio-demographic and behavioural factors shape customer financial activity, digital engagement, and satisfaction in the Colombian fintech ecosystem.

## Project Overview

The project uses the COFINFAD dataset to study customer transaction behaviour in a fintech environment. It combines:

- Exploratory analysis
- Customer segmentation
- Predictive modelling

The analysis focuses on identifying meaningful customer patterns, understanding behavioural differences across demographic groups, and interpreting factors associated with fintech user behaviour.

## Repository Structure

- [`index.qmd`](index.qmd) - Home page
- [`proposal.qmd`](proposal.qmd) - Project proposal
- [`findings.qmd`](findings.qmd) - Consolidated findings
- [`poster.qmd`](poster.qmd) - Project poster page
- [`meeting-minutes.qmd`](meeting-minutes.qmd) - Meeting minutes index
- [`team.qmd`](team.qmd) - Team page
- [`styles.css`](styles.css) - Website styling
- [`ISSS608_group10_shinyapp/`](ISSS608_group10_shinyapp/) - Shiny application source
- [`_quarto.yml`](_quarto.yml) - Quarto website configuration

## Dataset

The analysis is based on the **COFINFAD dataset**, which contains customer-level and transaction-level information from a Colombian fintech environment.

Dataset source: [COFINFAD Dataset on Mendeley](https://data.mendeley.com/datasets/mhb4zn3258/1)

## Shiny App

The deployed Shiny application is available at:

https://colombianfintechfinancialanalytics.shinyapps.io/isss608_group10_shinyapp/

## Preview Locally

To preview the Quarto website locally:

```bash
quarto preview
```

To render the website:

```bash
quarto render
```

## Sync Status

As of the latest verification, the local `main` branch is aligned with `origin/main`; it is not behind the GitHub repository.
