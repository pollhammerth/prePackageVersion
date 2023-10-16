#### 
# pmt.select() is used to create an object containing an expression, that can be evaluated to filter the data to desired polygons
# required argument is data
# the filter arguments can be delineated interactively
# e.g. HochTerrasse <- pmt.select(data)
# the output should be assigned to an object, which can then be evaluated within further pmt functions
# e.g. plot.terrace(eval(HighTerrace))
# pmt.filter() is a required function, used in the expression afterwards.
# pmt.it() is a little helper, to get polygon ids from the expression. e.g. pmt.id(eval(expression))
# The filter expression can be edited, to filter by polygon ids (expr <- edit(expr)). This way, polygons can be added or removed easily.
####
# -> IMPORTANT: if you want to keep your expression, don't forget to save it to a file before quitting!


pmt.select <- function(data.name = "data", 
                       regard.column = NA, 
                       regard.elements = NA, 
                       disregard.column = NA, 
                       disregard.elements = NA, 
                       thresh.column = NA, 
                       threshold = NA,
                       x = NA, y = NA, z = NA) {
  #### get the original data object
  data <- eval(parse(text = data.name))
  
  
  #### Interactive filtering guide
  if (is.na(regard.column) == TRUE) {
    cat("> Please choose one of these column names, to regard elements from:\n> (Or just press enter, if you don't want to.) \n")
    cat(names(data), fill = 1)
    cat("> Enter colname and press enter:\n")
    regard.column = as.character(readLines(n = 1))
    if (regard.column == "") { regard.column = NA }
#    if (!is.na(regard.column) == TRUE) { cat("> Thx. Available elements to be regarded in this column are:\n") }
  } else {}
  if(!is.na(regard.column) == TRUE) {
  if (is.na(regard.elements) == TRUE) {
    cat("> Thx for supplying a regard column name. Available elements to regard in this column are:\n")
    cat(unique(data[[regard.column]])[is.na(unique(data[[regard.column]])) == FALSE], fill = 1)
    cat("> Please enter an element to be regarded and press enter:\n> (You may add more than one element as vector: c('element1', 'element2', ... ))")
    regard.elements = as.character(readLines(n = 1))
  } else {}} else {}
  
  if (is.na(disregard.column) == TRUE) {
    cat("> Please choose one of these column names, to disregard elements from:\n> (Or just press enter, if you don't want to.)\n")
    cat(names(data), fill = 1)
    cat("> Enter colname and press enter:\n")
    disregard.column = as.character(readLines(n = 1))
    if (disregard.column == "") { disregard.column = NA }
#    if (!is.na(disregard.column) == TRUE) { cat("> Thx. Available elements to be disregarded in this column are:\n") }
  } else {}
  if(!is.na(disregard.column) == TRUE) {
  if (is.na(disregard.elements) == TRUE) {
    cat("> Thx for supplying a disregard column name. Available elements to be disregarded in this column are:\n")
    cat(unique(data[[disregard.column]])[is.na(unique(data[[disregard.column]])) == FALSE], fill = 1)
    cat("> Please enter an element to be disregarded and press enter:\n> (You may add more than one element as vector: c('element1', 'element2', ... ))")
    disregard.elements = as.character(readLines(n = 1))
  } else {}} else {}
  
  if (is.na(thresh.column) == TRUE) {
    cat("> Please choose one of these column names, to filter values by threshold:\n> (Or just press enter, if you don't want to.)\n")
    cat(names(data), fill = 1)
    cat("> Enter colname and press enter:\n")
    thresh.column = as.character(readLines(n = 1))
    if (thresh.column == "") { thresh.column = NA }
#    if (!is.na(thresh.column) == TRUE) { cat("> Thx. The value range in this column is:\n") }
  } else {}
  if(!is.na(thresh.column) == TRUE) {
    if (is.na(threshold) == TRUE) {
      cat("> Thx for supplying a threshold column name. The value range in this column is:\n")
      cat(c(min(data[[thresh.column]]), max(data[[thresh.column]])))
      cat("> Please enter a threshold value and press enter:\n> Only values below this threshold will be regarded.\n> (Useful e.g. for slope or distance values.)")
      threshold = as.double(readLines(n = 1))
    } else {}} else {}
  
  
  # rename and/or drop columns (optional)
  cat("> Do you want to rename columns to x, y and z? (y/n) \n> (Enter = No) \n")
  test = as.character(readLines(n = 1))
  if (test == "y") {
    cat("> Please enter the name of the column that should be named x:\n")
    cat(names(data), fill = 1)
    x = as.character(readLines(n = 1))
    cat("> Same for y:\n")
    y = as.character(readLines(n = 1))
    cat("> And same for z:\n")
    z = as.character(readLines(n = 1))
    cat("> Thank you\n")
  }
  cat("> Do you want to drop all columns other than x, y and z? (y/n)\n> (Enter = No)\n")
  test2 = as.character(readLines(n = 1))
  if (test2 == "y") {drop.unused = TRUE} else {drop.unused = FALSE}
  cat("> Thank you, all needed information has been ingested now.\n")
      
  #### create an expression for filter executing
  return(as.expression(as.call(str2lang(paste0("pmt.filter(",
                                 data.name,
                                 if(is.na(regard.column) == FALSE){paste0(", '",
                                                                          regard.column,
                                                                          "', ",
                                                                          paste0("c('", 
                                                                                 regard.elements, 
                                                                                 "')", 
                                                                                 collapse = "', '")
                                 )} else {paste0(", regard.column = NA, regard.elements = NA")},
                                 if(is.na(disregard.column) == FALSE){paste0(", ",
                                                                             disregard.column,
                                                                             ", ",
                                                                             paste0("c('",
                                                                                    disregard.elements, 
                                                                                    "')", 
                                                                                    collapse = "', '")
                                 )} else {paste0(", disregard.column = NA, disregard.elements = NA")},
                                 ", ",
                                 if (is.na(thresh.column) == FALSE) {
                                   paste0("thresh.column = '",thresh.column,"', threshold = ",threshold)
                                 } else {paste0("thresh.column = NA, threshold = NA")},
                                 ", ",
                                 if (!is.na(x) == TRUE){paste0("x = '",x,"'")}else{paste0("x = NA")},
                                 ", ",
                                 if (!is.na(y) == TRUE){paste0("y = '",y,"'")}else{paste0("y = NA")},
                                 ", ",
                                 if (!is.na(z) == TRUE){paste0("z = '",z,"'")}else{paste0("z = NA")},
                                 ", ",
                                 paste0("drop.unused = ", drop.unused),
                                 ")"
  )))))
  cat("> The expression has been created and can be executed by eval(expression). \n Don't forget to save your expression. e.g. save(list = c('expression'), file = 'expression.Rdata'); load('expression.Rdata')")
}




pmt.id <- function(expression, field = "map.terrace") {
  as.character(unique(eval(expression)[[field]]))
}




pmt.filter <- function(data, regard.column = NA, regard.elements = NA, disregard.column = NA, disregard.elements = NA, thresh.column = NA, threshold = NA, x = NA, y = NA, z = NA, drop.unused = FALSE){
  # execute filter
#  cat("The data will now be filtered for elements to be regarded and/or disregarded.\n")
  if (is.na(regard.column) == TRUE) {} else if (is.na(regard.elements[1]) == FALSE) {data <- data[which(data[[regard.column]] %in% regard.elements),]}
  if (is.na(disregard.column) == TRUE) {} else if (is.na(disregard.elements[1]) == FALSE) {
  # create the opposite of %in%
  '%!in%' <- Negate('%in%')
  data <- data[which(data[[disregard.column]] %!in% disregard.elements),]
  } else {}
  # filter slope above threshold
  if (is.na(thresh.column[1]) == FALSE) {
    data <- data[which(data[[thresh.column[1]]] <= threshold[1]),]
    if (length(thresh.column) > 1) {
      for (i in 2:length(thresh.column)) {
        data <- data[which(data[[thresh.column[i]]] <= threshold[i]),]
      }
    }
  }
  # rename columns (optional)
  if (is.na(x) == FALSE) {names(data)[names(data) == x] <- "x"}
  if (is.na(y) == FALSE) {names(data)[names(data) == y] <- "y"}
  if (is.na(z) == FALSE) {names(data)[names(data) == z] <- "z"}
  # drop all columns other than xyz (optional)
  if (drop.unused == TRUE) {data <- data[names(data) %in% c("x","y","z")]}
  # function output
  return(data)
}




################### OLD ######
# 
# #### 
# # pmt.select() is used to create an object containing an expression, that can be evaluated to filter the data to desired polygons
# # required argument is data
# # the filter arguments can be delineated interactively
# # e.g. HochTerrasse(pmt.select(data))
# # the output should be assigned to an object, which can then be evaluated within furter pmt functions
# # e.g. plot.terrace(eval(HighTerrace))
# # pmt.filter() is a required function, used in the expression afterwards.
# ####
# # -> if you want to keep your expression, don't forget to save it to a file before quitting!
# 
# 
# pmt.select <- function(data, regard.column = NA, regard.elements = NA, disregard.column = NA, disregard.elements = NA) {
#   #### Interactive filtering guide
#   if (is.na(regard.column) == TRUE) {
#     cat("> Please choose one of these column names, to regard elements from:\n")
#     cat(names(data), fill = 1)
#     cat("> Enter colname and press enter:\n")
#     regard.column = as.character(readLines(n = 1))
#     if (regard.column == "") { regard.column = NA }
#     if (!is.na(regard.column) == TRUE) { cat("> Thx. Available elements to be regarded in this column are:\n") }
#   } else {}
#   if(!is.na(regard.column) == TRUE) {
#     if (is.na(regard.elements) == TRUE) {
#       cat("> Thx for supplying a column name. Available elements to regard in this column are:\n")
#       cat(unique(data[[regard.column]])[is.na(unique(data[[regard.column]])) == FALSE], fill = 1)
#       cat("> Please enter an element to be regarded and press enter:\n > (You may add more than one element as vector: c('element1', 'element2', ... ))")
#       regard.elements = as.character(readLines(n = 1))
#     } else {}} else {}
#   if (is.na(disregard.column) == TRUE) {
#     cat("> Please choose one of these column names, to disregard elements from:\n")
#     cat(names(data), fill = 1)
#     cat("> Enter colname and press enter:\n")
#     disregard.column = as.character(readLines(n = 1))
#     if (disregard.column == "") { disregard.column = NA }
#     if (!is.na(disregard.column) == TRUE) { cat("> Thx. Available elements to be disregarded in this column are:\n") }
#   } else {}
#   if(!is.na(disregard.column) == TRUE) {
#     if (is.na(disregard.elements) == TRUE) {
#       cat("> Thx for supplying a column name. Available elements to be disregarded in this column are:\n")
#       cat(unique(data[[disregard.column]])[is.na(unique(data[[disregard.column]])) == FALSE], fill = 1)
#       cat("> Please enter an element to be disregarded and press enter:\n (You may add more than one element as vector: c('element1', 'element2', ... ))")
#       disregard.elements = as.character(readLines(n = 1))
#     } else {}} else {}
#   
#   #### create an expression for filter executing
#   return(as.call(str2lang(paste0("pmt.filter(",
#                                  quote(data),
#                                  if(is.na(regard.column) == FALSE){paste0(", '",
#                                                                           regard.column,
#                                                                           "', ",
#                                                                           paste0("c('", regard.elements, "')", collapse = "', '")
#                                  )} else {paste0(", regard.column = NA, regard.elements = NA")},
#                                  if(is.na(disregard.column) == FALSE){paste0(",",
#                                                                              disregard.column,
#                                                                              ",",
#                                                                              paste0("c('",disregard.elements, "')", collapse = "', '")
#                                  )} else {paste0(", disregard.column = NA, disregard.elements = NA")},")"
#   ))))
#   cat("> The expression has been created and can be executed by eval(expression). \n Don't forget to save your expression. e.g. save(list = c('expression'), file = 'expression.Rdata'); load('expression.Rdata')")
# }
# 
# 
# 
# 
# pmt.filter <- function(data, regard.column = NA, regard.elements = NA, disregard.column = NA, disregard.elements = NA){
#   #### Executing filter
#   cat("The data will now be filtered for elements to be regarded and/or disregarded.\n")
#   if (is.na(regard.column) == FALSE) {
#     data <- data[which(data[regard.column] == regard.elements),]
#   } else {}
#   if (is.na(disregard.column) == FALSE) {
#     # create the opposite of %in%
#     #  '%!in%' <- Negate('%in%')
#     data <- data[which(data[disregard.column] != disregard.elements),]
#   } else {}
#   return(data)
# }
# 
# 
# 
# 
# 
