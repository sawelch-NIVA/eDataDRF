library(dm)
library(DiagrammeR)

eData_dm <- dm(
  campaign = initialise_campaign_tibble(),
  references = initialise_references_tibble(),
  sites = initialise_sites_tibble(),
  parameters = initialise_parameters_tibble(),
  # compartments = initialise_compartments_tibble(),
  methods = initialise_methods_tibble(),
  # samples = initialise_samples_tibble(),
  biota = initialise_biota_tibble(),
  measurements = initialise_measurements_tibble(),
  CREED_scores = initialise_CREED_scores_tibble()
) |>
  # Primary keys
  dm_add_pk(campaign, CAMPAIGN_NAME_SHORT) |>
  dm_add_pk(references, REFERENCE_ID) |>
  dm_add_pk(sites, SITE_CODE) |>
  dm_add_pk(parameters, PARAMETER_NAME) |>
  # dm_add_pk(compartments, c(ENVIRON_COMPARTMENT, ENVIRON_COMPARTMENT_SUB)) |>
  dm_add_pk(methods, PROTOCOL_ID) |>
  # dm_add_pk(samples, SAMPLE_ID) |>
  dm_add_pk(biota, SAMPLE_ID) |>
  # Foreign keys
  dm_add_fk(measurements, CAMPAIGN_NAME_SHORT, campaign) |>
  dm_add_fk(measurements, REFERENCE_ID, references) |>
  dm_add_fk(measurements, SITE_CODE, sites) |>
  dm_add_fk(measurements, PARAMETER_NAME, parameters) |>
  # dm_add_fk(
  #   measurements,
  #   c(ENVIRON_COMPARTMENT, ENVIRON_COMPARTMENT_SUB),
  #   compartments
  # ) |>
  # dm_add_fk(measurements, SAMPLE_ID, samples) |>
  # dm_add_fk(samples, SITE_CODE, sites) |>
  # dm_add_fk(samples, PARAMETER_NAME, parameters) |>
  # dm_add_fk(
  #   samples,
  #   c(ENVIRON_COMPARTMENT, ENVIRON_COMPARTMENT_SUB),
  #   compartments
  # ) |>
  dm_add_fk(biota, SITE_CODE, sites) |>
  dm_add_fk(biota, PARAMETER_NAME, parameters) |>
  # dm_add_fk(
  #   biota,
  #   c(ENVIRON_COMPARTMENT, ENVIRON_COMPARTMENT_SUB),
  #   compartments
  # ) |>
  # dm_add_fk(biota, SAMPLE_ID, samples) |>
  dm_add_fk(methods, CAMPAIGN_NAME, campaign, CAMPAIGN_NAME_SHORT) |>
  dm_add_fk(CREED_scores, REFERENCE_ID, references) |>
  # Colors
  dm_set_colors(
    lightblue = campaign,
    lightcoral = references,
    lightgreen = sites,
    lightyellow = parameters,
    # lightcyan = compartments,
    lavender = methods,
    # peachpuff = samples,
    plum = biota,
    wheat = measurements,
    lightpink = CREED_scores
  )

# basic diagram
eData_dm |>
  dm_draw(
    rankdir = "TB",
    view_type = "all"
  )
