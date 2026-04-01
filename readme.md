# Keywords on mainframes
This reposiroty contains code and data for the paper "Keywords on Mainframes: Reproducing Geir Kjetsaa's Early Experiments in Computational Poetics".

## Overview
The repository contains reproduced corpus of poems (based on Kjetsaa's bibliography) as well as R code scripts to replicate and reinvestigate Kjetsaa's original studies.

## Repository Structure
```
.
├── data/                     # Core datasets and replication materials
│   ├── cntrds/               # Keyword lists derived from ruBERT centroids
│   ├── kjetsaa_corpus/       # Replicated Kjetsaa corpus
│   ├── kjetsaa_lists/        # Original keyword lists
│   └── results/              # Replication results
├── plots/                    # Figures and visualizations for the paper
│                              # *Note: Remove centroids before finalizing*
├── renv/                     # R environment snapshots for reproducibility
├── scr/                      # Analysis: Quarto notebooks (.qmd & .md)
│   ├── 00_data_preparation.qmd  # Prepares PoeTree & Kjetsaa corpora
│   ├── 00_lemmatization.ipynb   # Lemmatizes the corpora
│   ├── 01_TTR.qmd               # Replicated TTR experiments (Part I)
│   └── 02_keywords.qmd          # Replicated keywords experiments (Part II)
├── renv.lock                 # Locked R package dependencies
├── environment.yml           # Conda env definition for Python dependencies
├── 				          # 
└── 		                  # 

```