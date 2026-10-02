get_stat <- function(model, term, stat) {
  all_tab %>%
    filter(Model == model, Term == term) %>%
    pull({{ stat }})
}
