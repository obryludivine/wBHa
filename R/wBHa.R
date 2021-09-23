#' wBHa Procedure
#'
#' wBHa is an R package that implements a covariate multiple testing procedure specifically adapted in a Genome-Wide Association Studies context.
#' wBHa allows better detection of rare variants (which where difficult to detect since the current existing procedures are not powerful enough) by better integrating external information while allowing the optimization of the overall power.
#'
#'
#' @importFrom stats p.adjust spline
#'
#'
#' @param pvalues Numeric vector of pvalues.
#' @param covariates Numeric vector of covariates independent under the H0 of the pvalue.
#' @param alpha Numeric in \code{[}0,1\code{]}, significance level of the multiple testing procedure. Default \code{alpha=0.05}.
#' @param K Integer, number of bootstrap samples. Default \code{K=60}.
#'
#' @return
#' \describe{
#'   \item{\code{final_a_opt}}{Numeric value of the parameter a.}
#'   \item{\code{adjusted_covariates}}{Numeric vector of adjusted covariates.}
#'   \item{\code{adjusted_pvalues}}{Numeric vector of adjusted pvalues.}
#' }
#'
#' @examples
#' pvalues<-runif(100)
#' covariates<-runif(100,1,2)
#' wBHa(pvalues,covariates,alpha=0.05,K=60)
#'
#' @author Ludivine Obry
#'
#' @export

wBHa <- function(pvalues, covariates, alpha=0.05, K=60){

  #Bootstraps
  random_bootstraps_list<-list()
  for (i in c(1:K)) {
    random_bootstraps_list[[i]]<-sample(length(pvalues),length(pvalues)/K,replace=TRUE)
  }

  a_opt_bootstraps <- list()
  for(i in 1:K){
    random_bootstraps <- random_bootstraps_list[[i]]

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
    res_spline <- spline(a_values, R_wBHa_all)
    a_opt <- res_spline$x[which.max(res_spline$y)]

    a_opt_bootstraps[[i]] <- a_opt
  }

  final_a_opt <- mean(unlist(a_opt_bootstraps))
  adjusted_covariates <- (covariates^final_a_opt)
  adjusted_pvalues <- p.adjust(pvalues/((length(pvalues)/sum(1/adjusted_covariates))*(1/adjusted_covariates)),method = "BH")

  res<- list(final_a_opt=final_a_opt,
             adjusted_covariates=adjusted_covariates,
             adjusted_pvalues=adjusted_pvalues
             )

  return(res)
}
