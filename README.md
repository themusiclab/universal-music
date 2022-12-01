# Mutual intelligibility in musical communication

> **Warning**  
> This repository is currently being populated and validated. It *should* contain everything you need, but we're not 100% sure yet. We will remove this message when all code and materials have been uploaded. In the meantime, please contact us if you have any questions or run into issues with our code.

This is the repository for Yurdum et al. (2022) "Mutual intelligibility in musical communication". The manuscript is publicly available at https://psyarxiv.com/4kdx6.

You can find the following here:

* an R Markdown file that generates the manuscript
* data, analysis, and visualisation code to produce the results reported in the manuscript
* xml files for the listener experiments run via Qualtrics in industrialised societies
* E-Prime code for the listener experiments run in smaller-scale societies
* supplementary data and materials

Further data and information are available elsewhere:

* the audio excerpts can be downloaded from https://doi.org/10.5281/zenodo.7265514
* the preregistration is at https://osf.io/msvwz

**For assistance, please contact the corresponding authors: Lidya Yurdum (lidya.yurdum@yale.edu) and Samuel Mehr (sam@yale.edu).**

## Anatomy of the repo

To render the paper, run the code in `/writing/manuscript.Rmd`. <!-- add a note about the builder script here -->

## Data and analysis code

All raw data files are in `/data`. Identifiable information (i.e., IP addresses) have been removed. 

Scripts for preprocessing the data are in `/analysis`. 

Preprocessed data, interim datasets and the like are in `/results`.

### Visualisations

Visualisation code is in `/viz`, along with images and static data used for non-dynamic visualisations.

### Materials

Research materials are in `/materials`, and include E-Prime code to run the listener experiment in the smaller-scale societies, and .xml files to reproduce the industrialised society Qualtrics surveys. These files can also be referred to for the translations of all materials into the 31 languages featured in the study.
