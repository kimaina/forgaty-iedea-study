describeMissing= function (x, html = TRUE, number_first = TRUE, percentage_sign = TRUE, 
                           language = "en", useNA.digits = 1, ...) {
  
  if (!any(is.na(x))) 
    return(invisible())
  df_arg_list <- list(x = is.na(x), html = html, number_first = number_first, 
                      percentage_sign = percentage_sign, language = language, 
                      digits = useNA.digits)
  dot_args <- list(...)
  for (n in names(dot_args)) {
    if (!n %in% names(df_arg_list)) {
      df_arg_list[[n]] <- dot_args[[n]]
    }
  }
  missing <- fastDoCall(describeFactors, df_arg_list)
  rownames(missing)<-c("FALSE","Missing")
  return(missing["Missing", ])
}

desc_both <- function(x, ...) {
  result <- c(
    describeMean(x, useNA="no"),
    describeMedian(x, useNA="no"),
    describeMissing(x)
  )
  return(result)
}

desc_mean <- function(x, ...) {
  result <- c(
    describeMean(x, useNA="no"),
    describeMissing(x)
  )
  return(result)
}


desc_median <- function(x, ...) {
  result <- c(
    describeMedian(x, useNA="no"),
    describeMissing(x)
  )
  return(result)
}


MainX<<-NULL
# Creating a wrapper for getting descriptive statistics
getTable1Stats <- function(x, y, cont_fx=desc_both, data=dataset, digits = 1,statistics = T,na.rm=na.rm.var){
  MainX <<- append(MainX, c(x))
  data=data%>%drop_na(all_of(y))
  if(na.rm){
    data=data%>%drop_na(all_of(x))
  } 
  getDescriptionStatsBy(x = data[[x]], 
                        by = data[[y]],
                        digits = digits,
                        statistics = statistics,
                        continuous_fn = cont_fx,
                        hrzl_prop = T,
                        total_col_show_perc = T,
                        header_count = TRUE)
  
}

relevelBy <- function(varList, data){
  for (factorName in names(varList)) {
    for(var in varList[[factorName]]){
      # print(var)
      data[[var]] =relevel(factor(data[[var]]),factorName)
    }
  }
  return(data)
}


getMultipleTable1Stats <- function(x, y,y2,y3, cont_fx=desc_both, data=dataset, digits = 1,statistics = T,hrzl_prop = F,na.rm=na.rm.var){
  # data=data%>%drop_na(all_of(y))
  if(na.rm){
    data=data%>%drop_na(all_of(x))
  } 
  yy1= getDescriptionStatsBy(x = data[[x]], 
                             by = data[[y]],
                             digits = digits,
                             statistics = statistics,
                             continuous_fn = cont_fx,
                             hrzl_prop = hrzl_prop,
                             total_col_show_perc = T,
                             header_count = TRUE)
  
  yy2= getDescriptionStatsBy(x = data[[x]], 
                             by = data[[y2]],
                             digits = digits,
                             statistics = statistics,
                             continuous_fn = cont_fx,
                             hrzl_prop = hrzl_prop,
                             total_col_show_perc = T,
                             header_count = TRUE)
  
  yy3= getDescriptionStatsBy(x = data[[x]], 
                             by = data[[y3]],
                             digits = digits,
                             statistics = statistics,
                             continuous_fn = cont_fx,
                             hrzl_prop = hrzl_prop,
                             total_col_show_perc = T,
                             header_count = TRUE)
  
  
  result <- tryCatch({
    return(cbind(yy1,yy2,yy3))
    
  }, warning = function(war) {
    
    # warning handler picks up where error was generated
    print(paste(x,"ERROR"))
    print(paste("MY_WARNING:  ",war))
    
    
  }, error = function(err) {
    
    # error handler picks up where error was generated
    print(paste(x,"ERROR"))
    print(paste("MY_ERROR:  ",err))
    #return(f)
    
  }, finally = {
    
    # print(paste(x,"Done"))
    
  }) # END tryCatch
  
}

stargazer2 <- function(model, odd.ratio = F, ...) {
  if(!("list" %in% class(model))) model <- list(model)
  
  if (odd.ratio) {
    #exponentiate <- function(x) exp(x)
    coefOR2 <- lapply(model, function(x) exp(coef(x)))
    CIOR2 <- lapply(model, function(x) exp(confint(x)))
    seOR2 <- lapply(model, function(x) exp(coef(x)) * summary(x)$coef[, 2])
    p2 <- lapply(model, function(x) summary(x)$coefficients[, 4])
    stargazer(model, coef = coefOR2, se = seOR2, p = p2,  ci.custom =CIOR2, ...)
    
  } else {
    stargazer(model, ...)
  }
}

generateInterpretation= function(outcome,topn,model.df){
  
  model.df%>%mutate(
    interpretation=ifelse( term!="(Intercept)",paste("Fixing all else constant, a unit increase in ",tolower(variable_name),", changes the odds of ",outcome," by: ", estimate_or, " (",conf.low_or, "-",conf.high_or,") on average. In other words, the odds of ",outcome," changes by", 100 * (estimate_or - 1), "% due to each unit increase in ",tolower(variable_name)),paste("Not meaningful but can be roughly translated to the odds of ",outcome," which is ",estimate_or, " (",conf.low_or, "-",conf.high_or,") ,fixing all else constant")),
    
    interpretation=ifelse(categorical==T,paste("The odds of ",outcome," in ", tolower(variable_name), " is ", estimate_or, " (",conf.low_or, "-",conf.high_or,") times higher than the odds of ", outcome," in the reference group, fixing all else constant." ),interpretation )
  )%>%select(variable_name,interpretation )%>%
    kable( "html", booktabs = T, longtable = F,  digits=2,
           caption = paste("Model Interpretation | ",capitalize(outcome)) ) %>%
    kable_styling(bootstrap_options = c("striped","hold_position","condensed","responsive"))
  
  
}

generateLogitPlot= function(outcome,topn,model.df){
  ggplot(model.df%>%top_n(topn,p.value), aes(estimate, variable_name, color = variable_name )) +
    geom_point() + geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) + geom_vline(xintercept = 1, color = "blue", lty = 2) +
    ggtitle( sprintf( paste("Precision Comparison | ",capitalize(outcome)) ) )+  xlab("Log Odds Ratio") + ylab("Covariate")+
    theme_economist()+ guides(fill=FALSE, color=FALSE)+theme(plot.title = element_text(size=14, face="bold"))
}
