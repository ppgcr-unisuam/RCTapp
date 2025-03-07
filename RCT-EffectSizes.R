esize_inter <- function(rules) {
  value <- seq(0,5,0.01)
  interpretations <- data.frame(
    value = test,
    cohen1988 = effectsize::interpret_cohens_d(value, , rules = "cohen1988"),
    sawilowsky2009 = effectsize::interpret_hedges_g(value, rules = "sawilowsky2009"),
    gignac2016 = effectsize::interpret_hedges_g(value, rules = "gignac2016"),
    lovakov2021 = effectsize::interpret_hedges_g(value, rules = "lovakov2021")
  )
  
  # get first and last value of each interpretation
  # convert to paragraph
  cohen <- interpretations %>%
    dplyr::group_by(cohen1988) %>%
    dplyr::summarise(
      first = dplyr::first(value),
      last = dplyr::last(value)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(cohen1988, first, last) %>%
    dplyr::mutate(
      paragraph = paste0(
        cohen1988,
        " between ",
        first,
        " and ",
        last,
        ","
      )
    ) %>%
    dplyr::pull(paragraph) %>%
    paste(collapse = " ")
  
  # remove last dot
  cohen1988 <- substr(cohen, 1, nchar(cohen) - 1)
  
  sawilowsky <- interpretations %>%
    dplyr::group_by(sawilowsky2009) %>%
    dplyr::summarise(
      first = dplyr::first(value),
      last = dplyr::last(value)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(sawilowsky2009, first, last) %>%
    dplyr::mutate(
      paragraph = paste0(
        sawilowsky2009,
        " between ",
        first,
        " and ",
        last,
        ","
      )
    ) %>%
    dplyr::pull(paragraph) %>%
    paste(collapse = " ")
  
  sawilowsky2009 <- substr(sawilowsky, 1, nchar(sawilowsky) - 1)
  
  gignac <- interpretations %>%
    dplyr::group_by(gignac2016) %>%
    dplyr::summarise(
      first = dplyr::first(value),
      last = dplyr::last(value)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(gignac2016, first, last) %>%
    dplyr::mutate(
      paragraph = paste0(
        gignac2016,
        " between ",
        first,
        " and ",
        last,
        ","
      )
    ) %>%
    dplyr::pull(paragraph) %>%
    paste(collapse = " ")
  
  gignac2016 <- substr(gignac, 1, nchar(gignac) - 1)
  
  lovakov <- interpretations %>%
    dplyr::group_by(lovakov2021) %>%
    dplyr::summarise(
      first = dplyr::first(value),
      last = dplyr::last(value)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(lovakov2021, first, last) %>%
    dplyr::mutate(
      paragraph = paste0(
        lovakov2021,
        " between ",
        first,
        " and ",
        last,
        ","
      )
    ) %>%
    dplyr::pull(paragraph) %>%
    paste(collapse = " ")
  
  lovakov2021 <- substr(lovakov, 1, nchar(lovakov) - 1)
  
  result <- if(rules == "cohen1988") {
    cohen1988
  } else if(rules == "sawilowsky2009") {
    sawilowsky2009
  } else if(rules == "gignac2016") {
    gignac2016
  } else if(rules == "lovakov2021") {
    lovakov2021
  }
  
  return(result)
}
