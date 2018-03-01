# monthIndex --------------------------------------------------------------
#' Create a month index for analysis and plotting
#'
#' @param df Data Frame: Data set you want an index for
#' @param SLS_YR Bare Column Name: Name of column containing the year as integer
#' @param SLS_MNTH_INT Bare Column Name: Name of column containing the month as integer
#'
#' @return Data Frame containing the columns provided as SLS_YR, SLS_MNTH_INT and a index
#' @export
#' @importFrom magrittr '%>%'
#'
#' @examples monthIndex(sales_by_month, SLS_YR = SLS_YR, SLS_MNTH_INT = SLS_MNTH_INT)
monthIndex = function(df, SLS_YR, SLS_MNTH_INT) {

  # quo the col names
  quo_SLS_YR = rlang::quo(SLS_YR)
  quo_SLS_MNTH_INT = rlang::quo(SLS_MNTH_INT)

  # shrink the data set
  monthIndex = dplyr::select(df, !!quo_SLS_YR, !!quo_SLS_MNTH_INT) %>%
    dplyr::distinct()

  # setup date boundaries
  minYr = dplyr::select(monthIndex, !!quo_SLS_YR) %>%
    min()

  minMnth = dplyr::filter(monthIndex, rlang::UQE(quo_SLS_YR) == minYr) %>%
    min()

  maxYr = dplyr::select(monthIndex, !!quo_SLS_YR) %>%
    max()

  maxMnth = dplyr::filter(monthIndex, rlang::UQE(quo_SLS_YR) == maxYr) %>%
    dplyr::select(!!quo_SLS_MNTH_INT) %>%
    max()

  # make sure all months have at least one observation
  fillObs = data.frame(yr = rep(maxYr, 12), mnth = seq(12))
  colnames(fillObs) = c(rlang::UQE(quo_SLS_YR), rlang::UQE(quo_SLS_MNTH_INT))

  monthIndex = dplyr::bind_rows(monthIndex, fillObs) %>%
    dplyr::distinct()

  # create the index
  monthIndex = tidyr::complete(monthIndex, !!quo_SLS_YR, !!quo_SLS_MNTH_INT) %>%
    dplyr::filter(rlang::UQE(quo_SLS_YR) == minYr & rlang::UQE(quo_SLS_MNTH_INT) >= minMnth
                  | rlang::UQE(quo_SLS_YR) > minYr & rlang::UQE(quo_SLS_YR) < maxYr
                  | rlang::UQE(quo_SLS_YR) == maxYr & rlang::UQE(quo_SLS_MNTH_INT) <= maxMnth) %>%
    dplyr::arrange(!!quo_SLS_YR, !!quo_SLS_MNTH_INT) %>%
    dplyr::mutate(index = 1:n()) %>%
    data.frame()

  # remove extra months when there is just one year of data
  if(minYr == maxYr) {
    monthIndex = dplyr::filter(monthIndex, rlang::UQE(quo_SLS_MNTH_INT) >= minMnth,
                               rlang::UQE(quo_SLS_MNTH_INT) <= maxMnth)
  }

  return(monthIndex)
}
