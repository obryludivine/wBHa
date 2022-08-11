#' wBHa Procedure
#'
#' wBHa is an R package that implements a covariate multiple testing procedure specifically adapted in a Genome-Wide Association Studies context.
#' wBHa allows better detection of rare variants (which where difficult to detect since the current existing procedures are not powerful enough) by better integrating external information while allowing the optimization of the overall power.
#'
#'
#' @importFrom stats p.adjust
#'
#'
#' @param pvalues Numeric vector of pvalues.
#' @param covariates Numeric vector of covariates independent under the H0 of the pvalue.
#' @param alpha Numeric in \code{[}0,1\code{]}, significance level of the multiple testing procedure. Default \code{alpha=0.05}.
#' @param K Integer, number of bootstrap samples. Default \code{K=100}.
#' @param size Integer, size of bootstrap samples. Default \code{size=length(pvalues)/100}
#'
#' @return
#' \describe{
#'   \item{\code{final_a_opt}}{Numeric value of the parameter a.}
#'   \item{\code{adjusted_covariates}}{Numeric vector of adjusted covariates.}
#'   \item{\code{adjusted_pvalues}}{Numeric vector of adjusted pvalues.}
#' }
#'
#' @examples
#' set.seed(123)
#'
#' pvalues <- c(runif(100,0,0.1), runif(100,0,1))
#' covariates <- runif(200,0.05,0.5)
#' wBHa_object <- wBHa(pvalues, covariates, alpha=0.05, K=60)
#'
#' data("GSE90102_01")
#' pvalues <- GSE90102_01$rawp
#' covariates <- GSE90102_01$MAF
#' wBHa_object <- wBHa(pvalues, covariates)
#'
#'
#' @author Ludivine Obry
#'
#' @export

wBHa <- function(pvalues, covariates, alpha=0.05, K=100, size=length(pvalues)/K){

  a_opt_bootstraps <- c()
  for (i in c(1:K)) {
    random_bootstraps <-sample(length(pvalues),size,replace=TRUE)

    pvalues_in_sample <- pvalues[random_bootstraps]
    covariates_in_sample <- covariates[random_bootstraps]

    R_wBHa_all <- c()
    FDP_wBHa_all <- c()
    min_a <-0
    max_a <-10
    pas_a <-0.1
    a_values <- seq(min_a,max_a,pas_a)
    for (a in a_values) {
      covariates_in_sample_a <- (covariates_in_sample^a)
      wBHa_rejects <- which(p.adjust(pvalues_in_sample/
                                       ((length(pvalues_in_sample)/sum(1/covariates_in_sample_a))*(1/covariates_in_sample_a)),
                                     method = "BH")<=alpha)
      R_wBHa <- length(wBHa_rejects)
      R_wBHa_all <- c(R_wBHa_all, R_wBHa)
    }

    a_opt <- a_values[which(R_wBHa_all==max(R_wBHa_all))]

    if(!FALSE%in%(diff(a_opt)==pas_a)){
      a_opt <- max(a_opt)
    }else{
      a_opt_split<-split(a_opt, cumsum(c(1, diff(a_opt)!=pas_a)))
      length_a_opt_split<-unlist(lapply(a_opt_split,length))
      index_longest_interval<-which(length_a_opt_split==max(length_a_opt_split))
      if(length(index_longest_interval)==1){
        a_opt <-max(a_opt_split[[index_longest_interval]])
      }else{
        a_opt_longest_interval <- a_opt_split[index_longest_interval]
        if(length(a_opt_longest_interval[[1]])==1){
          a_opt_closest_interval <- unlist(a_opt_longest_interval[[
            which.min(unlist(lapply(a_opt_longest_interval, function(vect){return(sum(abs(1-vect)))})))]])
          a_opt<-max(a_opt_closest_interval)
        }else{
          a_opt_closest_interval <- unlist(a_opt_longest_interval[
            which.min(unlist(lapply(a_opt_longest_interval, function(vect){return(sum(abs(1-vect)))})))])
          a_opt<-max(a_opt_closest_interval)
        }
      }
    }
    a_opt_bootstraps <- c(a_opt_bootstraps,a_opt)
  }
  final_a_opt <- mean(a_opt_bootstraps)

  adjusted_covariates <- (covariates^final_a_opt)
  adjusted_pvalues <- p.adjust(pvalues/((length(pvalues)/sum(1/adjusted_covariates))*(1/adjusted_covariates)),method = "BH")

  res <- list(final_a_opt=final_a_opt,
              adjusted_covariates=adjusted_covariates,
              adjusted_pvalues=adjusted_pvalues
  )
}
