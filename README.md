README
================

## Main directory

Main directory for ‘Ecosystem size and complexity dictate riverine
biodiversity’

  - `manuscript`: Rmarkdown for main text
  - `supplementary_information`: Rmarkdown for Supplementary Information
  - `figure`: Rmarkdown for main figures
  - `table`: Rmarkdown for main tables

## `/theory`

A directory for theoretical data analysis.

  - `analysis_sensitivity`: regressions for simulation results of
    sensitivity analysis
  - `figure_alpha_pattern`: figure for spatial patterns of local species
    richness (for Supplementary Information)
  - `figure_n_patch`: figure for relationships between diversity metrics
    and ecosystem size
  - `figure_n_patch_si`: figure for relationships between diversity
    metrics and ecosystem size (for Supplementary Information)
  - `figure_p_branch`: figure for relationships between diversity
    metrics and ecosystem complexity
  - `figure_p_branch_si`: figure for relationships between diversity
    metrics and ecosystem complexity (for Supplementary Information)
  - `run_sensitivity_analysis`: run metacommunity simulations for
    sensitivity analysis
  - `run_simulaiton_alpha_pattern`: run metacommunity simulations for
    spatial patterns of local species richness (for Supplementary
    Information)
  - `run_simulation`: run metacommunity simulations
  - `/result`: subdirectory for simulation results

## `/empirical`

A directory for empirical data analysis.

  - `analysis_iNEXT_sim`: sensitivity simulation for the iNEXT function
    (Chao 2 estimator)
  - `analysis_lm_bf`: main analysis for influences of environmental
    variables on diversity metrics
  - `data_merge_hkd`: merge fish and GIS data in Hokkaido, Japan
  - `data_merge_mw`: merge fish and GIS data in Midwest, US
  - `figure_correlation`: correlation structure for potential
    explanatory variable (Supplementary Information)
  - `figure_diversity`: figure for relationships between biodiversity
    and ecosystem properties
  - `figure_iNEXT_sim`: figure for sensitivity analysis of the iNEXT
    function
  - `figure_map`: figure for maps
  - `table_sim_parameter_main`: table for parameter values of main
    simulations
  - `table_splist_hkd`; table for a fish species list in Hokkaido, Japan
  - `table_splist_mw`: table for a fish species list in Midwest, US
  - `/data_gis`: subdirectory for raw gis data
      - `albers_channel_hkd`: channel network for Hokkaido
      - `albers_channel_mw`: channel network for Midwest
      - `albers_hkd_shape`: polygon for Hokkaido
      - `albers_mw_shape`: polygon for Midest (HUC)
      - `albers_point_subset_hkd`: subset of sampling points for
        Hokkaido
      - `albers_point_subset_mw`: subset of sampling points for Midwest
      - `albers_watershed_hkd_final`: watershed polygons for Hokkaido
      - `albers_watershed_mw_final`: watershed polygons for Midwest
      - `albers_wsd_subset_hkd`: subset of watershed polygons for
        Hokkaido
      - `albers_wsd_subset_mw`: subset of watershed polygons for Midwest
  - `/data_org_hkd`: subdirectory for raw fish data in Hokkaido
      - `data_fmt_hkd_latest`
          - SiteID: unique siteID. Defined at 3 decimal degrees of
            latitude and longitude.
          - VisitID: unique visitID. Each sampling site should have one
            visitID as the data have been screed to the latest data.
          - Year: sampling year
          - OTU: operational taxonomic unit (some species treated as
            species groups)
          - Abundance: abundance uncorrected for sampling efforts. `NA`
            indicates species present but abundance information is
            unavailable
          - Sample\_area: areal area of sampling (m<sup>2</sup>)
          - Source: ID of data sources
          - Lon: longitude (decimal, crs - WGS84)
          - Lat: latitude (decimal, crs - WGS84)
          - n\_obs: number of observations at a site. disregard
          - Presence: 1 if present. disregard
  - `/data_org_mw`: subdirectory for raw fish data in Midwest
      - `data_fmt_STATE_latest`
          - State: state
          - SiteID: unique siteID.
          - VisitID: unique visitID. Each sampling site should have one
            visitID as the data have been screed to the latest data.
          - Date: sampling date
          - Lat: longitude (decimal, crs - IA: wgs84; MN, WI, IL: NAD83)
          - Lon: longitude (decimal, crs - IA: wgs84; MN, WI, IL: NAD83)
          - Species: common name of species
          - Abundance: abundance uncorrected for sampling efforts
  - `/result`: subdirectory for simulation output of the iNEXT
    sensitivity analysis
