README
================

  - [Main directory](#main-directory)
  - [`/theory`](#theory)
  - [`/empirical`](#empirical)

## Main directory

Main directory for ‘Ecosystem size and complexity dictate riverine
biodiversity’

  - `manuscript`: Rmarkdown for main text
  - `supplementary_information`: Rmarkdown for Supplementary Information
  - `figure`: Rmarkdown for main figures
  - `table`: Rmarkdown for main tables

## `/theory`

Directory for theoretical data analysis.

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
  - `table_sensitivity_analysis`: table for the results of sensitivity
    analysis
  - `table_sim_parameter_main`: table for parameters used in main
    simulations
  - `table_sim_parameter_sensitivity`: table for parameters used in
    sensitivity analysis
  - `/result`: subdirectory for simulation results
      - `result_sim2020-11-10`: results of main simulation
          - n\_rep: replicate ID
          - mc\_capacity: sum of carrying capacities across habitat
            patches
          - n\_patch: number of habitat patches \(N_{p}\)
          - p\_branch: branching probability \(P_{b}\)
          - n\_species: number of species simulated
          - mean\_env\_source: mean environmental value at headwaters
          - sd\_env\_source: sd of environmental value at headwaters
            \(\sigma_{h}\)
          - rho: spatial autocorrelation in mean environment \(\rho\)
          - asymmetry\_factor: degree of asymentry in distance matrix
          - min\_optim: minimum value of niche optimum
          - max\_optim: maximum value of niche optimum
          - sd\_env\_lon: sd of local environmental noise \(\sigma_{l}\)
          - theta: rate parameter of dispersal distance \(\theta\)
          - K0: carrying capacity at headwaters
          - sd\_env: temporal sd of environmental values \(\sigma_{z}\)
          - spatial\_env\_cor: logical value indicating whether spatial
            autocorrelation in temporal environmental variation is
            considered
          - phi: degree of spatial autocorrelation in temporal
            environmental variation \(\phi\)
          - min\_niche\_width: minimum value of niche width
          - max\_niche\_width: maximum value of niche width
          - niche\_cost: niche cost \(\nu\)
          - p\_dispersal: dispersal probability \(p_{d}\)
          - min\_alpha: minimum value of interspecific competition
            coefficient
          - max\_alpha: maximum value of interspecific competition
            coefficient
          - alpha\_div: predicted alpha diversity
          - beta\_div: predicted beta diversity (additive)
          - gamma\_div: predicted gamma diversity
      - `result_sim_alpha_pattern2021-01-20`: results of supplementary
        simulation (local diversity patterns)
      - `result_sensitivity`: results of sensitivity analysis

## `/empirical`

Directory for empirical data analysis.

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
  - `/data_out`: subdirectory for formatted fish data
      - `dat_hkd_site`: site-level fish data in Hokkaido
          - SiteID: unique site ID
          - VisitID: unique visit ID
          - Year: sampling year
          - Species: species
          - Abundance: abundance uncorrected for sampling efforts
          - Sample\_area: aerial area of sampling (m<sup>2</sup>)
          - Source: literature source
          - n\_obs: number of observation years per site
          - Presence: 1 if present, disregard
          - geometry: geometry coordinates for watershed polygons
          - watershed\_id: unique watershed id
          - area: watershed area (km<sup>2</sup>)
          - frac\_forest: fraction of forest land use
          - frac\_urban: fraction of urban land use
          - frac\_agri: fraction of agricultural land use
          - mean\_temp: spatial average of annual mean air temperature
            for watershed w
          - mean\_ppt: spatial average of cumulative precipitation for
            watershed w
          - mean\_dem: spatial average of degital elevation map for
            watershed w
          - rate: rate parameter of an exponential distribution for
            watershed w (branch length \~ exp(lambda))
          - n\_branch: number of branch for watershed w
          - p\_branch: branching probability for watershed w
          - n\_dam: number of dams for watershed w
      - `dat_mw_site`: site-level fish data in Midwest
          - State: state
          - SiteID: unique site ID
          - VisitID: unique visit ID
          - Date: sampling date
          - Species: species
          - Abundance: abundance uncorrected for sampling efforts
          - geometry: geometry coordinates for watershed polygons
          - watershed\_id: unique watershed id
          - area: watershed area (km<sup>2</sup>)
          - frac\_forest: fraction of forest land use
          - frac\_urban: fraction of urban land use
          - frac\_agri: fraction of agricultural land use
          - mean\_temp: spatial average of annual mean air temperature
            for watershed w
          - mean\_ppt: spatial average of cumulative precipitation for
            watershed w
          - mean\_dem: spatial average of degital elevation map for
            watershed w
          - rate: rate parameter of an exponential distribution for
            watershed w (branch length \~ exp(lambda))
          - n\_branch: number of branch for watershed w
          - p\_branch: branching probability for watershed w
          - n\_dam: number of dams for watershed w
      - `data_REGION`: watershed-level data in Hokkaido (`hkd`) or
        Midwest (`mw`)
          - Site: site ID output from iNEXT function. Equivalent to
            watershed\_id
          - Diversity: diversity metric
          - Observed: observed species richness
          - Estimator: asymptotic species richness
          - s.e.: SE of asymptotic species richness
          - LCL: lower 95% CI of asymptotic species richness
          - UCL: upper 95% CI of asymptotic species richness
          - watershed\_id: unique watershed id
          - area: watershed area (km<sup>2</sup>)
          - frac\_forest: fraction of forest land use
          - frac\_urban: fraction of urban land use
          - frac\_agri: fraction of agricultural land use
          - mean\_temp: spatial average of annual mean air temperature
            for watershed w
          - mean\_ppt: spatial average of cumulative precipitation for
            watershed w
          - mean\_dem: spatial average of degital elevation map for
            watershed w
          - rate: rate parameter of an exponential distribution for
            watershed w (branch length \~ exp(lambda))
          - n\_branch: number of branch for watershed w
          - p\_branch: branching probability for watershed w
          - n\_dam: number of dams for watershed w
      - `data_hkd_splist`: list of fish species in Hokkaido
      - `data_mw_splist`: list of fish species in Midwest
      - `data_mw_splist_latin`: correspondence table for common and
        latin names of fish species in Midwest
  - `/result`: subdirectory for simulation output of the iNEXT
    sensitivity analysis
