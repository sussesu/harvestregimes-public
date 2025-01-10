# European forest harvesting regimes

Susanne Suvanto

Codes for running the analysis for the 'Understanding Europe's Forest Harvesting Regimes' manuscript, which has been accepted for publication in the journal Earth's Future (details updated here when available). The latest update of this repository (10/1/2025) contains the updates to the codes during the review process, now corresponding to the results presented in the accepted version of the paper.

Preprint of the manuscript available here: https://eartharxiv.org/repository/view/5858/ (pre-review version).

The resulting gridded maps (figures 2, 3, 4 and S3 in the final version) will be archieved in Zenodo at the time of the publication of the paper, with DOI: https://doi.org/10.5281/zenodo.8382920

### Explanations of files (the main workflow)

* 02_Tmt_data_processing.R - The main data processing file converting the forest inventory data to a format needed for in this analysis
* 02d_prepare_RF_data.R - Additional processing for the outputs from the previous file to prepare it for the random forest models, extracting data from external data sources etc.
* 03b_basic_plots.R - Creating exploratory plots, including e.g. some supplementary figures.
* 03e_harvest_maps_MS_weights.R - Codes for creating figures 1 and 3
* 03f_freq_vs_intensity_MSonly_weights.R - Codes for creating figure 2
* 04a_RF_harvest01.R - Codes for training the harvest probability RF, incl. cross-validation (this can take a bit of time and was typically run on an external HPC cluster)
* 04b_RF_intensityBA.R - Codes for training the harvest probability RF, incl. cross-validation (this can take a bit of time and was typically run on an external HPC cluster)
* 05a_results_harvest01.R - Codes for processing the results of the harvest probability RF: variable importance and PDP plots (the PDPs can take a bit of time and this was typically run on an external HPC cluster)
* 05a_results_intensityBA.R - Codes for processing the results of the harvest intensity RF: variable importance and PDP plots (the PDPs can take a bit of time and this was typically run on an external HPC cluster)
* 06_MS_figures - Creating the final figures for cross-validation, variable impotance and the PDPs (figures 4-6)
* 07_harvest_year_test_SUPP.R - Codes for running additional tests presented in the supplement figures S1 and S2 (added in revision).
* 08_process_data_toZenodo.R - Codes for processing the results files to be archieved in Zenodo.
* create_table1.r - Code for extracting the information for Table 1.

### Explanations of folders

* preprocessing_scripts/ - Scripts for preprocessing different data sets
* src/ - Source files for defining custom functions used the the workflows
