# qstr provides a more convenient interface to 'str'
qstr <- function(object, strSize = 3) {
  str(object, strSize, vec.len = strSize, list.len = strSize)
} 
