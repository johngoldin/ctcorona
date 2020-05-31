#' The `fetch_acs` function fetches ACS data summarized by GEOID for an
#' estimate and also returns moe and percentage of the total.
#' @description Results are summed across all the variables. The moe
#' is based on `moe_sum` and the moe of the percentage is based on
#' `moe_ratio`.
#'
#' @param variables List of ACS variables
#' @param var_for_summary Variable to be used for denominator of percentage
#' @param var_name Base name for returned columns
#' @param as_pct Should percentage be included
#' @param geography Geography: "state", "county" or "county subdivision"
#'
#' @return data frame with GEOID, NAME, and ACS data (but no geometry)
#' @export
#'
#' @examples
#'   poverty_state_acs <- fetch_acs(variables = "B17001_002", var_for_summary = "B17001_001",
#'                           geography = "state", var_name = poverty)
#'   vars_under_25 <- c(paste0("B01001_00", c(3:9)), paste0("B01001_0", c(10, 27:34)))
#' young_state_acs <- fetch_acs(variables = vars_under_25, var_for_summary = "B01001_001",
#'                              geography = "state", var_name = under_25)
#' young_county_acs <- fetch_acs(variables = vars_under_25, var_for_summary = "B01001_001",
#'                               geography = "county", var_name = under_25)

fetch_acs <- function(variables, var_for_summary, var_name = item, as_pct = TRUE, geography) {
  get_acs(geography = geography,
          state = "CT",
          geometry = "FALSE", # no map at this time
          survey = "acs5",
          variables = variables,
          summary_var = var_for_summary) %>%
    filter(estimate > 0) %>%
    group_by(NAME, GEOID)  %>%
    summarize("{{ var_name }}" := sum(estimate),
              moe := moe_sum(moe, estimate),
              total_pop = max(summary_est),
              total_pop_moe = max(summary_moe),
              .groups = "drop"
    ) %>%
    mutate(NAME = str_replace(NAME, " town, .* County, Connecticut", "")) %>%
    mutate(NAME = str_replace(NAME, " County, Connecticut", "")) %>%
    # mutate("{{var_name}}_pct" := {{ var_name }} / total_pop) %>%
    mutate(item_pct := {{ var_name }} / total_pop,
           item_pct_moe := moe_ratio({{var_name}}, total_pop, moe, total_pop_moe)) %>%
    select(GEOID, NAME, {{var_name}}, moe, item_pct, item_pct_moe, total_pop) %>%
    rename("{{var_name}}_moe" := moe,
           "{{var_name}}_pct" := item_pct, "{{var_name}}_pct_moe" := item_pct_moe)
}
