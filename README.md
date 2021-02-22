README
================

-   [Title](#title)
-   [Author](#author)
-   [File descriptions](#file-descriptions)
    -   [Main directory](#main-directory)
    -   [Directory `theory`](#directory-theory)
    -   [Directory `empirical`](#directory-empirical)

# Title

Ecosystem size and complexity dictate riverine biodiversity

# Author

Akira Terui, Seoghyun Kim, Christine L. Dolph, Taku Kadoya, Yusuke
Miyazaki

# File descriptions

See below or `metadata.html` (to view `metadata.html`, clone the
repository and open `metadata.html` from your local computer). **Note
that original empirical data sets were not included in this repository
because they were provided by third parties**.

## Main directory

Main directory for ‘Ecosystem size and complexity dictate riverine
biodiversity’

-   `manuscript` - Rmarkdown for main text
-   `supplementary_information` - Rmarkdown for Supplementary
    Information
-   `figure` - Rmarkdown for main figures
-   `table` - Rmarkdown for main tables

## Directory `theory`

Directory for theoretical data analysis.

-   `analysis_sensitivity` - regressions for simulation results of
    sensitivity analysis
-   `figure_alpha_pattern` - figure for spatial patterns of local
    species richness (for Supplementary Information)
-   `figure_n_patch` - figure for relationships between diversity
    metrics and ecosystem size
-   `figure_n_patch_si` - figure for relationships between diversity
    metrics and ecosystem size (for Supplementary Information)
-   `figure_p_branch` - figure for relationships between diversity
    metrics and ecosystem complexity
-   `figure_p_branch_si` - figure for relationships between diversity
    metrics and ecosystem complexity (for Supplementary Information)
-   `run_sensitivity_analysis` - run metacommunity simulations for
    sensitivity analysis
-   `run_simulaiton_alpha_pattern` - run metacommunity simulations for
    spatial patterns of local species richness (for Supplementary
    Information)
-   `run_simulation` - run metacommunity simulations
-   `table_sensitivity_analysis` - table for the results of sensitivity
    analysis
-   `table_sim_parameter_main` - table for parameters used in main
    simulations
-   `table_sim_parameter_sensitivity` - table for parameters used in
    sensitivity analysis
-   `/result` - subdirectory for simulation results
    -   `result_sim2020-11-10.csv` - results of main metacommunity
        simulation
        </summary>
    -   `result_sim_alpha_pattern2021-01-20.csv` - results of
        supplementary simulation (local diversity patterns)
        </summary>
    -   `result_sensitivity.csv` - results of sensitivity analysis
        </summary>

## Directory `empirical`

Directory for empirical data analysis.

-   `analysis_iNEXT_sim` - sensitivity simulation for the iNEXT function
    (Chao 2 estimator)
-   `analysis_lm_bf` - main analysis for influences of environmental
    variables on diversity metrics
-   `data_merge_hkd` - merge fish and GIS data in Hokkaido, Japan
-   `data_merge_mw` - merge fish and GIS data in Midwest, US
-   `figure_correlation` - correlation structure for potential
    explanatory variable (Supplementary Information)
-   `figure_diversity` - figure for relationships between biodiversity
    and ecosystem properties
-   `figure_iNEXT_sim` - figure for sensitivity analysis of the iNEXT
    function
-   `figure_map` - figure for maps
-   `table_sim_parameter_main` - table for parameter values of main
    simulations
-   `table_splist_hkd`; table for a fish species list in Hokkaido, Japan
-   `table_splist_mw` - table for a fish species list in Midwest, US
-   `/data_gis` - subdirectory for raw gis data
    -   `albers_channel_hkd` - channel network for Hokkaido
    -   `albers_channel_mw` - channel network for Midwest
    -   `albers_hkd_shape` - polygon for Hokkaido
    -   `albers_mw_shape` - polygon for Midest (HUC)
    -   `albers_point_subset_hkd` - subset of sampling points for
        Hokkaido
    -   `albers_point_subset_mw` - subset of sampling points for Midwest
    -   `albers_watershed_hkd_final` - watershed polygons for Hokkaido
    -   `albers_watershed_mw_final` - watershed polygons for Midwest
    -   `albers_wsd_subset_hkd` - subset of watershed polygons for
        Hokkaido
    -   `albers_wsd_subset_mw` - subset of watershed polygons for
        Midwest
-   `/data_org_hkd` - subdirectory for raw fish data in Hokkaido
    -   `data_fmt_hkd_latest.csv`
-   `/data_org_mw` - subdirectory for raw fish data in Midwest
    -   `data_fmt_STATE_latest.csv`
-   `/data_out`: subdirectory for formatted fish data
    -   `dat_hkd_site.csv` - site-level fish data in Hokkaido
        </summary>
    -   `dat_mw_site.csv` - site-level fish data in Midwest
        </summary>
    -   `data_REGION.csv` - watershed-level data in Hokkaido (`REGION` =
        `hkd`) or Midwest (`mw`)
        </summary>
    -   `data_hkd_splist.csv` - list of fish species in Hokkaido
        </summary>
    -   `data_mw_splist.csv` - list of fish species in Midwest
        </summary>
    -   `data_mw_splist_latin.csv` - correspondence table for common and
        latin names of fish species in Midwest
        </summary>
-   `/result` - subdirectory for simulation output of the iNEXT
    sensitivity analysis
