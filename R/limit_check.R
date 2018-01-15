#' limit_check
#' 
#' NOT CURRENTLY FUNCTIONING
#' 
#' Checks for rate limit issues. For each token, prints all rate limits that have been used
#' in the current time frame.
#' @param tokenstocheck a numeric vector specifying which tokens to check. Default is all of them .
#' @details Since this function uses the "application/rate_status" rate limit,
#' this rate limit will always be printed for each token. If this rate limit is exhausted, the function prints
#' a notification of this rather than the standard output for that token. 
#' @export


limit_check= function(){
  for (i in 1:length(get_tokens())){
    limit= suppressWarnings(try(rate_limit(get_tokens()[tokenstocheck[i]]), silent=TRUE))
    cat(c("\n", "\n", "Token Number:", i), fill = T)
    if(class(limit)[1]!="try-error"){
      print(as.data.frame(limit[which(limit$limit!=limit$remaining),]))
    }
    else{print("Rate limit for checking rate limit exceeded!")}
  }
}
