
summariseData <- function(df, groupBy) {
  df %>%
    group_by(!!sym(groupBy)) %>%
    summarise(
      "Confirmed"          = (sum(confirmed, na.rm = T)/2),
      "Recovery"           = (sum(recovered, na.rm = T)/2),
      "Deaths"             = (sum(deceased, na.rm = T)/2),
      "Current Confirmed"  = (sum(active, na.rm = T)/2)
    ) %>%
    as.data.frame()
}

getSummaryDT <- function(data, groupBy, selectable = FALSE) {
  datatable(
    na.omit(summariseData(data, groupBy)),
    rownames  = FALSE,
    options   = list(
      order          = list(1, "desc"),
      scrollX        = TRUE,
      scrollY        = "37vh",
      scrollCollapse = T,
      dom            = 'ft',
      paging         = FALSE
    ),
    selection = ifelse(selectable, "single", "none")
  )
}