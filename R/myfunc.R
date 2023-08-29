myfunc = function(data, criteria){

  # compute scale
  data$row_scale = rowMeans(x,na.rm=TRUE)

  # Count NA row-wise
  for(i in 1:nrow(data)){
    data$na_row[i]=sum(is.na(data[i, 1:ncol(data)]))
  }

  # Compute result
  data$outcome = ifelse(data$na_row>criteria,NA,data$row_scale)

  return(data$outcome)

}
