
getFullTableData <- function(groupBy) {
  padding_left <- max(str_length(data_evolution$value_new), na.rm = TRUE)
  data         <- data_evolution %>%
    dplyr::filter(date == current_date) %>%
    pivot_wider(names_from = var, values_from = c(value, value_new)) %>%
    select(-date, -Lat, -Long) %>%
    add_row(
      "Province.State"      = "World",
      "Country.Region"      = "World",
      "population"          = 7800000000,
      "value_confirmed"     = sum(.$value_confirmed, na.rm = T),
      "value_new_confirmed" = sum(.$value_new_confirmed, na.rm = T),
      "value_recovered"     = sum(.$value_recovered, na.rm = T),
      "value_new_recovered" = sum(.$value_new_recovered, na.rm = T),
      "value_deceased"      = sum(.$value_deceased, na.rm = T),
      "value_new_deceased"  = sum(.$value_new_deceased, na.rm = T),
      "value_active"        = sum(.$value_active, na.rm = T),
      "value_new_active"    = sum(.$value_new_active, na.rm = T)
    ) %>%
    group_by(!!sym(groupBy), population) %>%
    summarise(
      confirmed_total     = sum(value_confirmed, na.rm = T),
      confirmed_new       = sum(value_new_confirmed, na.rm = T),
      recovered_total     = sum(value_recovered, na.rm = T),
      recovered_new       = sum(value_new_recovered, na.rm = T),
      deceased_total      = sum(value_deceased, na.rm = T),
      deceased_new        = sum(value_new_deceased, na.rm = T),
      active_total        = sum(value_active, na.rm = T),
      active_new          = sum(value_new_active, na.rm = T)
    ) %>%
    mutate(
      "confirmed_newPer" = confirmed_new / (confirmed_total - confirmed_new) * 100,
      "recovered_newPer" = recovered_new / (recovered_total - recovered_new) * 100,
      "deceased_newPer"  = deceased_new / (deceased_total - deceased_new) * 100,
      "active_newPer"    = active_new / (active_total - active_new) * 100
    ) %>%
    mutate_at(vars(contains('_newPer')), list(~na_if(., Inf))) %>%
    mutate_at(vars(contains('_newPer')), list(~na_if(., 0))) %>%
    mutate(
      confirmed_new = str_c(str_pad(confirmed_new, width = padding_left, side = "left", pad = "0"), "|",
                            confirmed_new, if_else(!is.na(confirmed_newPer), sprintf(" (%+.2f %%)", confirmed_newPer), "")),
      recovered_new = str_c(str_pad(recovered_new, width = padding_left, side = "left", pad = "0"), "|",
                            recovered_new, if_else(!is.na(recovered_newPer), sprintf(" (%+.2f %%)", recovered_newPer), "")),
      deceased_new  = str_c(str_pad(deceased_new, width = padding_left, side = "left", pad = "0"), "|",
                            deceased_new, if_else(!is.na(deceased_newPer), sprintf(" (%+.2f %%)", deceased_newPer), "")),
      active_new    = str_c(str_pad(active_new, width = padding_left, side = "left", pad = "0"), "|",
                            active_new, if_else(!is.na(active_newPer), sprintf(" (%+.2f %%)", active_newPer), ""))
    ) %>%
    select(-population) %>%
    distinct(Country.Region, .keep_all = TRUE) %>%
    as.data.frame()
}


