context("test_utils")

test_that("test_llply.parallel.ab works", {
    # Test 1 - simple input
    l1 = list(
      a1=list(b1='a',b2='b'),
      a2=list(b1='a',b2='b')
    )
    
    get_l_result = function(l1,l2) {list(list1=l1, list2=l2)}
    
    l_result = llply.parallel.ab(l1,l1, 2,.fun=get_l_result)
    # Alt implementation: with map2
    l_result2 = purrr::map2(l1,l1, ~purrr::map2(.x, .y, get_l_result))
    

    
    verify_output(
      test_path('output','test_llply.parallel.ab.txt'),
      l_result %>% qstr(4),
    )
    
    verify_output(
      test_path('output','test_llply.parallel.ab2.txt'),
      l_result2 %>% qstr(4)
    )
    
    succeed()
  }
)

test_that("llply.parallel.multilist works", {
  # Test 1 - simple input
  l1 = list(
    a1=list(b1='a',b2='b'),
    a2=list(b1='a',b2='b')
  )
  l_in = list(l1, l1, l1)
  
  get_l_result = function(l_in) {l_in}
  
  l_result = llply.parallel.multilist(l1,l_in, 2,.fun=get_l_result)
  
  verify_output(
    test_path('output','test_llply.parallel.multilist.txt'),
     l_result %>% qstr(4)
  )

  # Alt implementation: with map2 - haven't managed it yet.
  get_l_result = function(...) {list(...)}
  #l_result2 = map2(l1,l1, ~map2(.x, .y, get_l_result))
  #l_result2 = purrr::pmap(l_in, get_l_result)
  #l_result2 = purrr::pmap(l_in, ~pmap(., get_l_result))
  #l_result2 = purrr::pmap(l_in, ~pmap(.,  ~pmap(., get_l_result)))
  #l_result2 %>% qstr(4)
  
  succeed()
}
)

test_that("trans_df works", {
  df_in = tibble::tibble(x=c('hello','world'),y=c('a', 'o'))
  df_result = df_in %>% trans_df()
  df_result_null = NULL %>% trans_df()
  
  succeed()
}
)