#' @export
get_df_hp <- function() {
  df_hp <- tibble::tibble(Level = 1:30,
                          hp_level=c(1:10+10, (1:10+10)*2, (1:10+10)*4)) %>%
    dplyr::mutate(HP1=(2*hp_level) %>% floor(),
                  HP2=(2.5*hp_level) %>% floor(),
                  HP3=(3*hp_level) %>% floor(),
                  Surge1 = (HP1/4) %>% floor(),
                  Surge2 = (HP2/4) %>% floor(),
                  Surge3 = (HP3/4) %>% floor()) %>%
    #floor() %>% # TODO: Apply floor to all columns
    dplyr::select(Level,
                  HP1, Surge1,
                  HP2, Surge2,
                  HP3, Surge3
                  )
  df_hp
}

#' @export
get_df_hp_html <- function(df_hp=NULL) {

  if(df_hp %>% is.null()) {df_hp = get_df_hp()}

  df_hp_html <- df_hp %>%
    knitr::kable(format='html',
                 col.names=c('Level',rep(c('HP','Surge'),3))) %>%
    kableExtra::add_header_above(
      c(" ", "Low HP"=2, "Medium HP"=2, "High HP"=2))

  df_hp_html
}

