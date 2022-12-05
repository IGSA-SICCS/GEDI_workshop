# GEDI workshop

To launch the interactive coding environment click here:


:arrow_down_small::arrow_down_small::arrow_down_small:
<!-- badges: start -->
[![Launch Rstudio Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/IGSA-SICCS/GEDI_workshop/main?urlpath=rstudio)
<!-- badges: end --> :arrow_up_small::arrow_up_small::arrow_up_small:

For a html-based code walkthrough, see https://igsa-siccs.github.io/workshops/

## Studying forest structure from space: An introduction to GEDI lidar data pre-processing and analysis in ecological applications

The purpose of this workshop is to assist interested Ecologists, Biologists, and Geologists in accessing NASA’s Global Ecosystem Dynamics Investigation (GEDI) near-global lidar data and applying it to their research. We will review recent applications and discuss future directions for GEDI data. Using open-access R software, we will demonstrate how participants may easily access, manipulate, explore, assess, and apply GEDI data in their study areas. We will develop modules (download > cleaning > visualization, etc) and share code, inputs, and outputs to prevent bottlenecks in case of technical issues. This strategy allows attendees to participate in every module with products derived from previous stages even if previous steps/results weren’t completed. Participants may be required to install open-source software prior to the workshop. This workshop is organized primarily by PhD students in the Ecological and Environmental Informatics program at Northern Arizona University.

## Objectives

1. The primary objective of this workshop is to disseminate information on the use and functionality of spaceborne lidar (light detection and ranging) from GEDI, including: how the instrument functions, spatial resolution and coverage, available data products, where and how to access the data, and uses of lidar data in a variety of earth science subjects.

2. Participants will learn how to download GEDI data for an area of interest, process the data, and apply it to an ecological question in R.

3. Presenters will provide example code and intermediate data products so that participants can follow along and explore results in the event of technical challenges.

## How to use this repository

### 1. Presentation
The presentation contains (1) a summary of workshop goals and expected outcomes, (2) an overview of the GEDI instrument, data types, and data products, (3) examples of GEDI data applications in biological and ecological research, and (4) instructions for using the rGEDI package API to download data in R

### 2. Scripts
The scripts for workshop can be launched from the binder (badge above). Binder is an open-source web service which hosts interactive and reproducible environments. You do not need to install any programs or dependencies to use this.
If you prefer, you can also run workshop scripts in Rstudio. AGU_GEDI_Workshop.Rmd contains the workflow.

Section 1: How to download GEDI data for a region of interest.
Section 2: How to filter and clip GEDI data products to your region of interest and visualize GEDI footprints
Section 3: How to visualize GEDI data including waveforms, height metrics, and derived indices
Section 4: How to compute descriptive statistics and create raster layers from GEDI footprints

### 3. Data
We provide small sample datasets which are compatible with the scripts. These allow users to use the scripts without waiting for data download (a significant commitment).

## Additional resources
