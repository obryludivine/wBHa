## ---- message=FALSE---------------------------------------------------------------------------------
library("ggplot2")
library("cowplot")
library("wBHa")


## ----function---------------------------------------------------------------------------------------
get_legend<-function(myggplot){
  # Allows to extract the legend of the graph "myggplot"
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

FDR_graph_design_ind <- function(design_tab,tag){
  # Allows to make FDR graphics of "design_tab" 
  # "design_tab" comes from the "simulation_data" of the wBHa package
  # "design_tab" contains the results obtained for the independent case
  graph_FDR_ind <- ggplot(data=design_tab, aes(x=m, y=FDR2, colour=Procedure,shape=Procedure))+ 
  geom_line(stat="identity",size=0.5)+
    geom_point(size=rep(rep(c(2.5,2,1.75,1.75,2,1.5,1.75,1.75),8),3))+
  scale_shape_manual(values=c(15,16,17,18,19,20,15,17))+
  geom_errorbar(aes(min=FDR2-se.FDR2,max=FDR2+se.FDR2),width=500)+
    labs(tag=tag)+
    scale_x_continuous(breaks=c(unique(design_tab$m)),
                       labels=c(as.character(unique(design_tab$m))))+
    geom_hline(yintercept=5, linetype="dashed", color = "red") +
    scale_colour_manual(values=c("#A6CEE3","#1F78B4","#7570B3","#1B9E77",
                                 "#DADAEB","#E69F00","#D9D9D9","#E31A1C"))+
    labs(y=c("FDR (%)")) +
    facet_wrap(~m1,labeller=label_both,scales="free",ncol=4) +
    theme(axis.title.x=element_blank(),
          legend.position="bottom",
          panel.background=element_rect(fill="#F0F0F0", colour="#F0F0F0", 
                                        size=0.5, linetype="solid"),
          panel.grid.major=element_line(size=0.5, linetype="solid", 
                                        colour="white"),
          panel.grid.minor=element_line(size=0.25, linetype="solid", 
                                        colour="white") )
  
  return(graph_FDR_ind)
}

FDR_graph_design_ind_large_m <- function(design_tab,tag){
  # Allows to make FDR graphics of "design_tab" 
  # "design_tab" comes from the "simulation_data" of the wBHa package
  # "design_tab" contains the results obtained for the independent case
  graph_FDR_ind <- ggplot(data=design_tab, aes(x=m, y=FDR2, colour=Procedure,shape=Procedure))+ 
    geom_line(stat="identity",size=0.5)+
    geom_point(size=rep(rep(c(2.5,2,1.75,1.75,2,1.5,1.75,1.75),3),3))+
    scale_shape_manual(values=c(15,16,17,18,19,20,15,17))+
    geom_errorbar(aes(min=FDR2-se.FDR2,max=FDR2+se.FDR2),width=500)+
    labs(tag=tag)+
    scale_x_continuous(breaks=c(unique(design_tab$m)),
                       labels=c(as.character(unique(design_tab$m))))+
    geom_hline(yintercept=5, linetype="dashed", color = "red") +
    scale_colour_manual(values=c("#A6CEE3","#1F78B4","#7570B3","#1B9E77",
                                 "#DADAEB","#E69F00","#D9D9D9","#E31A1C"))+
    labs(y=c("FDR (%)")) +
    facet_wrap(~m1,labeller=label_both,scales="free",ncol=4) +
    theme(axis.title.x=element_blank(),
          legend.position="bottom",
          panel.background=element_rect(fill="#F0F0F0", colour="#F0F0F0", 
                                        size=0.5, linetype="solid"),
          panel.grid.major=element_line(size=0.5, linetype="solid", 
                                        colour="white"),
          panel.grid.minor=element_line(size=0.25, linetype="solid", 
                                        colour="white") )
  
  return(graph_FDR_ind)
}

FDR_graph_design_corr <- function(design_tab,tag){
  # Allows to make FDR graphics of "design_tab" 
  # "design_tab" comes from the "simulation_data" of the wBHa package
  # "design_tab" contains the results obtained for the correlation case
  graph_FDR_corr <- ggplot(data=design_tab, aes(x=rho, y=FDR2, colour=Procedure,shape=Procedure))+ 
  geom_line(stat="identity",size=0.5)+
    geom_point(size=rep(rep(c(2.5,2,1.75,1.75,2,1.5,1.75,1.75),8),5))+
  scale_shape_manual(values=c(15,16,17,18,19,20,15,17))+
  geom_errorbar(aes(min=FDR2-se.FDR2,max=FDR2+se.FDR2),width=0.035)+
    labs(tag=tag)+
    scale_x_continuous(breaks=c(unique(design_tab$rho)),
                       labels=c(as.character(unique(design_tab$rho))))+
    geom_hline(yintercept=5, linetype="dashed", color = "red") +
    scale_colour_manual(values=c("#A6CEE3","#1F78B4","#7570B3","#1B9E77",
                                 "#DADAEB","#E69F00","#D9D9D9","#E31A1C"))+
    labs(y=c("FDR (%)")) +
    facet_wrap(~m1,labeller=label_both,scales="free",ncol=4) +
    theme(axis.title.x=element_blank(),
          legend.position="bottom",
          panel.background=element_rect(fill="#F0F0F0", colour="#F0F0F0", 
                                        size=0.5, linetype="solid"),
          panel.grid.major=element_line(size=0.5, linetype="solid", 
                                        colour="white"),
          panel.grid.minor=element_line(size=0.25, linetype="solid", 
                                        colour="white") )
  
  return(graph_FDR_corr)
}

FDR_graph_semisimu <- function(design_tab){
  # Allows to make FDR graphics of "design_tab" 
  # "design_tab" comes from the "simulation_data" of the wBHa package
  # "design_tab" contains the results obtained for the simulation based on a real dataset
  graph_FDR_semisimu <- ggplot(data=design_tab,aes(x=Procedure, y=FDR2, fill=Procedure))+ 
    geom_bar(stat="identity")+
    scale_fill_manual(values=c("#A6CEE3","#1F78B4","#7570B3","#1B9E77",
                               "#DADAEB","#E69F00","#D9D9D9","#E31A1C"))+
    facet_wrap(~m1,labeller=label_both,scales="free",ncol=5) +
    geom_errorbar(aes(min=FDR2-se.FDR2,max=FDR2+se.FDR2),color="#666666")+
    labs(y = c("FDR (%)")) +
    theme(legend.position = "none",
          axis.text.x = element_text(angle=90,hjust=1,vjust=0.5), 
          axis.title.x=element_blank(), 
          panel.background=element_rect(fill="#F0F0F0",colour="#F0F0F0",
                                        size=0.5,linetype="solid"), 
          panel.grid.major=element_line(size=0.5,linetype="solid",
                                        colour = "white"),
          panel.grid.minor=element_line(size=0.25,linetype="solid",
                                        colour = "white") ) 
  return(graph_FDR_semisimu)
}

power_graph_design_ind_8procedures <- function(design_tab,tag){
  # Allows to make power graphics of "design_tab" 
  # "design_tab" comes from the "simulation_data" of the wBHa package
  # "design_tab" contains the results obtained for the independent case
  graph_power_ind <- ggplot(data=design_tab,aes(x=m, y=Power2, colour=Procedure,shape=Procedure))+ 
  geom_line(stat="identity",size=0.5)+
    geom_point(size=rep(rep(c(2.5,2,1.75,1.75,2,1.5,1.75,1.75),8),3))+
  scale_shape_manual(values=c(15,16,17,18,19,20,15,17))+
    geom_errorbar(aes(min=Power2-se.Power2,max=Power2+se.Power2),width=500)+
    labs(tag=tag)+
    scale_x_continuous(breaks=c(unique(design_tab$m)),
                       labels=c(as.character(unique(design_tab$m))))+
    scale_colour_manual(values=c("#A6CEE3","#1F78B4","#7570B3","#1B9E77",
                                 "#DADAEB","#E69F00","#D9D9D9","#E31A1C"))+
    labs(y = c("Power (%)")) +
    facet_wrap(~m1,labeller=label_both,scales="free",ncol=4) +
    
    theme(axis.title.x=element_blank(),
          legend.position="bottom",
          panel.background=element_rect(fill="#F0F0F0", colour="#F0F0F0", 
                                        size=0.5, linetype="solid"),
          panel.grid.major=element_line(size=0.5, linetype="solid",
                                        colour="white"),
          panel.grid.minor=element_line(size=0.25, linetype="solid",
                                        colour="white") )
  
  return(graph_power_ind)
}

power_graph_design_ind_5procedures <- function(design_tab,tag){
  # Allows to make power graphics of "design_tab" 
  # "design_tab" comes from the "simulation_data" of the wBHa package
  # "design_tab" contains the results obtained for the independent case
  graph_power_ind <- ggplot(data=design_tab,aes(x=m, y=Power2, colour=Procedure,shape=Procedure))+ 
    geom_line(stat="identity",size=0.5)+
    geom_point(size=rep(rep(c(2,1.75,1.75,1.5,1.75),8),3))+
    scale_shape_manual(values=c(16,17,18,20,17))+
    geom_hline(yintercept=0,size=0.75)+
    labs(tag=tag)+
    scale_x_continuous(breaks=c(unique(design_tab$m)),
                       labels=c(as.character(unique(design_tab$m))))+
    scale_y_continuous(expression(paste(Delta, "Power (uninformative-BH) (%)")))+
    scale_colour_manual(values=c("#1F78B4","#7570B3","#1B9E77","#E69F00","#E31A1C"))+
    facet_wrap(~m1,labeller=label_both,scales="free",ncol=4) +
    
    theme(axis.title.x=element_blank(),
          legend.position="bottom",
          panel.background=element_rect(fill="#F0F0F0", colour="#F0F0F0", 
                                        size=0.5, linetype="solid"),
          panel.grid.major=element_line(size=0.5, linetype="solid",
                                        colour="white"),
          panel.grid.minor=element_line(size=0.25, linetype="solid",
                                        colour="white") )
  
  return(graph_power_ind)
}

power_graph_design_ind_large_m <- function(design_tab,tag){
  # Allows to make power graphics of "design_tab" 
  # "design_tab" comes from the "simulation_data" of the wBHa package
  # "design_tab" contains the results obtained for the independent case
  graph_power_ind <- ggplot(data=design_tab,aes(x=m, y=Power2, colour=Procedure,shape=Procedure))+ 
    geom_line(stat="identity",size=0.5)+
    geom_point(size=rep(rep(c(2.5,2,1.75,1.75,2,1.5,1.75,1.75),3),3))+
    scale_shape_manual(values=c(15,16,17,18,19,20,15,17))+
    geom_errorbar(aes(min=Power2-se.Power2,max=Power2+se.Power2),width=500)+
    labs(tag=tag)+
    scale_x_continuous(breaks=c(unique(design_tab$m)),
                       labels=c(as.character(unique(design_tab$m))))+
    scale_colour_manual(values=c("#A6CEE3","#1F78B4","#7570B3","#1B9E77",
                                 "#DADAEB","#E69F00","#D9D9D9","#E31A1C"))+
    labs(y = c("Power (%)")) +
    facet_wrap(~m1,labeller=label_both,scales="free",ncol=4) +
    
    theme(axis.title.x=element_blank(),
          legend.position="bottom",
          panel.background=element_rect(fill="#F0F0F0", colour="#F0F0F0", 
                                        size=0.5, linetype="solid"),
          panel.grid.major=element_line(size=0.5, linetype="solid",
                                        colour="white"),
          panel.grid.minor=element_line(size=0.25, linetype="solid",
                                        colour="white") )
  
  return(graph_power_ind)
}

power_graph_design_corr <- function(design_tab,tag){
  # Allows to make power graphics of "design_tab" 
  # "design_tab" comes from the "simulation_data" of the wBHa package
  # "design_tab" contains the results obtained for the correlation case
  graph_power_corr <- ggplot(data=design_tab,aes(x=rho, y=Power2, colour=Procedure,shape=Procedure))+ 
  geom_line(stat="identity",size=0.5)+
    geom_point(size=rep(rep(c(2.5,2,1.75,1.75,2,1.5,1.75,1.75),8),5))+
  scale_shape_manual(values=c(15,16,17,18,19,20,15,17))+
  geom_errorbar(aes(min=Power2-se.Power2,max=Power2+se.Power2),width=0.035)+
    labs(tag=tag)+
    scale_x_continuous(breaks=c(unique(design_tab$rho)),
                       labels=c(as.character(unique(design_tab$rho))))+
    scale_colour_manual(values=c("#A6CEE3","#1F78B4","#7570B3","#1B9E77",
                                 "#DADAEB","#E69F00","#D9D9D9","#E31A1C"))+
    labs(y = c("Power (%)")) +
    facet_wrap(~m1,labeller=label_both,scales="free",ncol=4) +
    
    theme(axis.title.x=element_blank(),
          legend.position="bottom",
          panel.background=element_rect(fill="#F0F0F0", colour="#F0F0F0", 
                                        size=0.5, linetype="solid"),
          panel.grid.major=element_line(size=0.5, linetype="solid",
                                        colour="white"),
          panel.grid.minor=element_line(size=0.25, linetype="solid",
                                        colour="white") )
  
  return(graph_power_corr)
}

power_graph_semisimu <- function(design_tab){
  # Allows to make power graphics of "design_tab" 
  # "design_tab" comes from the "simulation_data" of the wBHa package
  # "design_tab" contains the results obtained for the simulation based on a real dataset
  graph_power_semisimu <- ggplot(data=design_tab,aes(x=Procedure, y=Power2, fill=Procedure))+ 
    geom_bar(stat="identity")+
    scale_fill_manual(values=c("#A6CEE3","#1F78B4","#7570B3","#1B9E77",
                               "#DADAEB","#E69F00","#D9D9D9","#E31A1C"))+
    facet_wrap(~m1,labeller=label_both,scales="free",ncol=5) +
    geom_errorbar(aes(min=Power2-se.Power2,max=Power2+se.Power2),color="#666666")+
    labs(y = c("Power (%)")) +
    theme(legend.position = "none",
          axis.text.x = element_text(angle=90,hjust=1,vjust=0.5), 
          axis.title.x=element_blank(), 
          panel.background=element_rect(fill="#F0F0F0",colour="#F0F0F0",
                                        size=0.5,linetype="solid"), 
          panel.grid.major=element_line(size=0.5,linetype="solid",
                                        colour = "white"),
          panel.grid.minor=element_line(size=0.25,linetype="solid",
                                        colour = "white") ) 
  return(graph_power_semisimu)
}

subpower_graph_design_ind <- function(design_tab,tag){
  # Allows to make subpower graphics of "design_tab" 
  # "design_tab" comes from the "simulation_data" of the wBHa package
  # "design_tab" contains the results obtained for the independent case
  subpower_graph <- ggplot(data=design_tab, aes(x=m, y=Power2, colour=Procedure,shape=Procedure))+ 
  geom_line(stat="identity",size=0.5)+
    geom_point(size=rep(rep(c(2.5,2,1.75,1.75,2,1.5,1.75,1.75),8),3))+
  scale_shape_manual(values=c(15,16,17,18,19,20,15,17))+
  geom_errorbar(aes(min=Power2-se.Power2,max=Power2+se.Power2),width=500)+
    labs(tag=tag)+
    labs(y = c("Power (%)")) +
    scale_x_continuous(breaks=c(unique(design_tab$m)),
                       labels=c(as.character(unique(design_tab$m))))+
    scale_colour_manual(values=c("#A6CEE3","#1F78B4","#7570B3","#1B9E77",
                                 "#DADAEB","#E69F00","#D9D9D9","#E31A1C"))+
    facet_wrap(~m1,labeller=label_both,scales="free",ncol=4) +
    
    theme(axis.title.x=element_blank(),
          legend.position="bottom",
          panel.background=element_rect(fill="#F0F0F0", colour="#F0F0F0", 
                                        size=0.5, linetype="solid"),
          panel.grid.major=element_line(size=0.5, linetype="solid", 
                                        colour="white"),
          panel.grid.minor=element_line(size=0.25, linetype="solid", 
                                        colour="white") )
  
  return(subpower_graph)
}

subpower_graph_design_ind_large_m <- function(design_tab,tag){
  # Allows to make subpower graphics of "design_tab" 
  # "design_tab" comes from the "simulation_data" of the wBHa package
  # "design_tab" contains the results obtained for the independent case
  subpower_graph <- ggplot(data=design_tab, aes(x=m, y=Power2, colour=Procedure,shape=Procedure))+ 
    geom_line(stat="identity",size=0.5)+
    geom_point(size=rep(rep(c(2.5,2,1.75,1.75,2,1.5,1.75,1.75),3),3))+
    scale_shape_manual(values=c(15,16,17,18,19,20,15,17))+
    geom_errorbar(aes(min=Power2-se.Power2,max=Power2+se.Power2),width=500)+
    labs(tag=tag)+
    labs(y = c("Power (%)")) +
    scale_x_continuous(breaks=c(unique(design_tab$m)),
                       labels=c(as.character(unique(design_tab$m))))+
    scale_colour_manual(values=c("#A6CEE3","#1F78B4","#7570B3","#1B9E77",
                                 "#DADAEB","#E69F00","#D9D9D9","#E31A1C"))+
    facet_wrap(~m1,labeller=label_both,scales="free",ncol=4) +
    
    theme(axis.title.x=element_blank(),
          legend.position="bottom",
          panel.background=element_rect(fill="#F0F0F0", colour="#F0F0F0", 
                                        size=0.5, linetype="solid"),
          panel.grid.major=element_line(size=0.5, linetype="solid", 
                                        colour="white"),
          panel.grid.minor=element_line(size=0.25, linetype="solid", 
                                        colour="white") )
  
  return(subpower_graph)
}

subpower_graph_design_corr <- function(design_tab,tag){
  # Allows to make subpower graphics of "design_tab" 
  # "design_tab" comes from the "simulation_data" of the wBHa package
  # "design_tab" contains the results obtained for the correlation case
  subpower_graph <- ggplot(data=design_tab, aes(x=rho, y=Power2, colour=Procedure,shape=Procedure))+ 
  geom_line(stat="identity",size=0.5)+
    geom_point(size=rep(rep(c(2.5,2,1.75,1.75,2,1.5,1.75,1.75),8),5))+
  scale_shape_manual(values=c(15,16,17,18,19,20,15,17))+
  geom_errorbar(aes(min=Power2-se.Power2,max=Power2+se.Power2),width=0.035)+
    labs(tag=tag)+
    labs(y = c("Power (%)")) +
    scale_x_continuous(breaks=c(unique(design_tab$rho)),
                       labels=c(as.character(unique(design_tab$rho))))+
    scale_colour_manual(values=c("#A6CEE3","#1F78B4","#7570B3","#1B9E77",
                                 "#DADAEB","#E69F00","#D9D9D9","#E31A1C"))+
    facet_wrap(~m1,labeller=label_both,scales="free",ncol=4) +
    
    theme(axis.title.x=element_blank(),
          legend.position="bottom",
          panel.background=element_rect(fill="#F0F0F0", colour="#F0F0F0", 
                                        size=0.5, linetype="solid"),
          panel.grid.major=element_line(size=0.5, linetype="solid", 
                                        colour="white"),
          panel.grid.minor=element_line(size=0.25, linetype="solid", 
                                        colour="white") )
  
  return(subpower_graph)
}


## ----Initialization---------------------------------------------------------------------------------
data("simulation_data")
size_n <- 2000 #number of individuals


## ----Crea_Graphs------------------------------------------------------------------------------------
for (Case in c("independent","large_m_and_m1","correlation","semisimulation")) {
  if(Case==c("independent")){ #case without correlations between SNPs
    vrho <- 0 #rho value
    blsize <- 0 #bloc size 
    r2 <- 0.2 #coefficient of determination
    graph_FDR_ind_maf <- list(reference="", inverse="", constant="")
    graph_FDR_ind_1bymaf <- list(reference="", inverse="", constant="")
    graph_FDR_ind_uninfo <- list(reference="", inverse="", constant="")
    
    graph_power_ind_maf <- list(reference="", inverse="", constant="")
    graph_power_ind_1bymaf <- list(reference="", inverse="", constant="")
    graph_power_ind_diff_uninfo_BH <- list(reference="", inverse="", constant="")
    
    graph_power_rare_ind_maf <- list(reference="", inverse="", constant="")
    graph_power_common_ind_1bymaf <- list(reference="", inverse="", constant="")
    
    for (scenario in c("reference","inverse","constant")) {
    
      if(scenario=="reference"){ #corresponding to scenario 1
        binary_beta=c("B_2.2_1.8_1.5_1.3_1.rda")
        quantitative_beta=c("B43210.rda")
      }
      if(scenario=="inverse"){ #corresponding to scenario 2
        binary_beta=c("B_1.3_1.5_1.8_2.2_1.rda")
        quantitative_beta=c("B12340.rda")
      }
      if(scenario=="constant"){ #corresponding to scenario 3
        binary_beta=c("B_1.5_1.5_1.5_1.5_1.rda")
        quantitative_beta=c("B22220.rda")
      }
      
      quantitative_tab_maf <- list()
      quantitative_tab_1bymaf <- list()
      quantitative_tab_uninfo <- list()
      quantitative_tab_Diff_uninfo_BH <- list()
      binary_tab_maf <- list()
      binary_tab_1bymaf <- list()
      binary_tab_uninfo <- list()
      binary_tab_Diff_uninfo_BH <- list()
      
      quantitative_tab_subgp_maf <- list()
      quantitative_tab_subgp_1bymaf <- list()
      binary_tab_subgp_maf <- list()
      binary_tab_subgp_1bymaf <- list()

      
      for (size_m in c(8000,14000,20000)) { 
        #size_m is the total number of null hypotheses tested
        for(m1 in c(5,10,15,20,25,50,100,150)){
          #m1 is the number of causal variants
          linear_file_in=paste("Res_LineR","n",size_n,"m",size_m,"m1",m1,"rho",vrho,
                               "tb",blsize,"r2",r2,quantitative_beta,sep="_")
          res_FDR_power <- simulation_data[[linear_file_in]]$res_FDR_power
          res_Subpower <- simulation_data[[linear_file_in]]$res_Subpower
          name_procedures <- c("BH","wBH","wBHa","IHW","qvalue","swfdr",
                               "FDRreg","CAMT", "wBH_1/MAF", 
                               "wBHa_other_weight_fun", "IHW_1/MAF",
                               "swfdr_1/MAF", "fdrreg_1/MAF", "CAMT_log_1/MAF",
                               "wBH_un_cov", "wBHa_un_cov", "IHW_un_cov", 
                               "swfdr_un_cov", "fdrreg_un_cov", "CAMT_un_cov")
            
          # Quantitative dataframe containing power and FDR 
          # with MAF was used as the informative covariate
          quantitative_tab_maf[[(length(quantitative_tab_maf)+1)]] <- data.frame(
            name_procedures[1:8], unlist(c(res_FDR_power["Power",c(1:8)], use.names=F)),
            c(rep(size_m,8)), c(rep(m1,8)), unlist(c(res_FDR_power["FDR",c(1:8)],
            use.names=F)), unlist(c(res_FDR_power["se.FDR",c(1:8)],use.names=F)),
            unlist(c(res_FDR_power["se.Power",c(1:8)],use.names=F)) )
          colnames(quantitative_tab_maf[[(length(quantitative_tab_maf))]]) <- c("Procedure",
                                                                        "Power","m",
                                                                        "m1","FDR",
                                                                        "se.FDR",
                                                                        "se.Power")
          
          # Quantitative dataframe containing power and FDR 
          # with 1/MAF was used as the covariate
          quantitative_tab_1bymaf[[(length(quantitative_tab_1bymaf)+1)]] <- data.frame(
            name_procedures[c(1,9:10,5,11:14)], 
            unlist(c(res_FDR_power["Power",c(1,9:10,5,11:14)], use.names=F)),
            c(rep(size_m,8)), c(rep(m1,8)), 
            unlist(c(res_FDR_power["FDR",c(1,9:10,5,11:14)],use.names=F)), 
            unlist(c(res_FDR_power["se.FDR",c(1,9:10,5,11:14)],use.names=F)),
            unlist(c(res_FDR_power["se.Power",c(1,9:10,5,11:14)],use.names=F)) )
          colnames(quantitative_tab_1bymaf[[(length(quantitative_tab_1bymaf))]]) <- c("Procedure",
                                                                        "Power","m",
                                                                        "m1","FDR",
                                                                        "se.FDR",
                                                                        "se.Power")
          # Quantitative dataframe containing power and FDR 
          # with an uninformative covariate
          quantitative_tab_uninfo[[(length(quantitative_tab_uninfo)+1)]] <- data.frame(
            name_procedures[c(1,15:17,5,18:20)], 
            unlist(c(res_FDR_power["Power",c(1,15:17,5,18:20)], use.names=F)),
            c(rep(size_m,8)), c(rep(m1,8)), 
            unlist(c(res_FDR_power["FDR",c(1,15:17,5,18:20)],use.names=F)), 
            unlist(c(res_FDR_power["se.FDR",c(1,15:17,5,18:20)],use.names=F)),
            unlist(c(res_FDR_power["se.Power",c(1,15:17,5,18:20)],use.names=F)) )
          colnames(quantitative_tab_uninfo[[(length(quantitative_tab_uninfo))]]) <- c("Procedure",
                                                                        "Power","m",
                                                                        "m1","FDR",
                                                                        "se.FDR",
                                                                        "se.Power")
          
          # Quantitative dataframe containing power and FDR 
          # with an uninformative covariate - BH
          quantitative_tab_Diff_uninfo_BH[[(length(quantitative_tab_Diff_uninfo_BH)+1)]] <- data.frame(
            name_procedures[c(16:20)], 
            c(unlist(c(res_FDR_power["Power",c(16:20)], use.names=F))-res_FDR_power["Power",1]),
            c(rep(size_m,5)), c(rep(m1,5)), 
            c(unlist(c(res_FDR_power["FDR",c(16:20)],use.names=F))-res_FDR_power["FDR",1]) )
          colnames(quantitative_tab_Diff_uninfo_BH[[(length(quantitative_tab_Diff_uninfo_BH))]]) <- c("Procedure",
                                                                        "Power","m",
                                                                        "m1","FDR")
          
          
          
            
          # Quantitative dataframe containing subpower
          # with MAF was used as the informative covariate
          colnames(res_Subpower) <- c("X1","X2","X3","X4",
                                      "se.X1","se.X2","se.X3","se.X4")
          
          quantitative_tab_subgp_maf[[(length(quantitative_tab_subgp_maf)+1)]] <- data.frame(
            Procedure=name_procedures[c(1:8)],
            Power=c(res_Subpower$X1[c(1:8)], res_Subpower$X2[c(1:8)],
                    res_Subpower$X3[c(1:8)], res_Subpower$X4[c(1:8)]),
            Subgroup=c(rep("Rare",length(res_Subpower[c(1:8),1])),
                       rep("Medium-Rare",length(res_Subpower[c(1:8),1])),
                       rep("Medium",length(res_Subpower[c(1:8),1])),
                       rep("Common",length(res_Subpower[c(1:8),1]))),
            m=rep(size_m,32), m1=rep(m1,32),
            se.Power=c(res_Subpower$se.X1[c(1:8)], res_Subpower$se.X2[c(1:8)],
                       res_Subpower$se.X3[c(1:8)], res_Subpower$se.X4[c(1:8)]) )
          
          # Quantitative dataframe containing subpower
          # with 1/MAF was used as the covariate
          quantitative_tab_subgp_1bymaf[[(length(quantitative_tab_subgp_1bymaf)+1)]] <- data.frame(
            Procedure=name_procedures[c(1,9:10,5,11:14)],
            Power=c(res_Subpower$X1[c(1,9:10,5,11:14)], 
                    res_Subpower$X2[c(1,9:10,5,11:14)],
                    res_Subpower$X3[c(1,9:10,5,11:14)], 
                    res_Subpower$X4[c(1,9:10,5,11:14)]),
            Subgroup=c(rep("Rare",length(res_Subpower[c(1,9:10,5,11:14),1])),
                       rep("Medium-Rare",length(res_Subpower[c(1,9:10,5,11:14),1])),
                       rep("Medium",length(res_Subpower[c(1,9:10,5,11:14),1])),
                       rep("Common",length(res_Subpower[c(1,9:10,5,11:14),1]))),
            m=rep(size_m,32), m1=rep(m1,32),
            se.Power=c(res_Subpower$se.X1[c(1,9:10,5,11:14)], 
                       res_Subpower$se.X2[c(1,9:10,5,11:14)],
                       res_Subpower$se.X3[c(1,9:10,5,11:14)], 
                       res_Subpower$se.X4[c(1,9:10,5,11:14)]) )
          
          
          
          
          
          
          
          logit_file_in=paste("Res_Logit","n",size_n,"m",size_m,"m1",m1,"rho",vrho,
                              "tb",blsize,binary_beta,sep="_")
          
          res_FDR_power <- simulation_data[[logit_file_in]]$res_FDR_power
          res_Subpower <- simulation_data[[logit_file_in]]$res_Subpower
          name_procedures <- c("BH","wBH","wBHa","IHW","qvalue","swfdr",
                               "FDRreg","CAMT", "wBH_1/MAF", 
                               "wBHa_other_weight_fun", "IHW_1/MAF",
                               "swfdr_1/MAF", "fdrreg_1/MAF", "CAMT_log_1/MAF",
                               "wBH_un_cov", "wBHa_un_cov", "IHW_un_cov", 
                               "swfdr_un_cov", "fdrreg_un_cov", "CAMT_un_cov")
            
          # Binary dataframe containing power and FDR 
          # with MAF was used as the informative covariate
          binary_tab_maf[[(length(binary_tab_maf)+1)]] <- data.frame(
            name_procedures[1:8], unlist(c(res_FDR_power["Power",c(1:8)], use.names=F)),
            c(rep(size_m,8)), c(rep(m1,8)), unlist(c(res_FDR_power["FDR",c(1:8)],
            use.names=F)), unlist(c(res_FDR_power["se.FDR",c(1:8)],use.names=F)),
            unlist(c(res_FDR_power["se.Power",c(1:8)],use.names=F)) )
          colnames(binary_tab_maf[[(length(binary_tab_maf))]]) <- c("Procedure",
                                                                        "Power","m",
                                                                        "m1","FDR",
                                                                        "se.FDR",
                                                                        "se.Power")
          
          # Binary dataframe containing power and FDR 
          # with 1/MAF was used as the covariate
          binary_tab_1bymaf[[(length(binary_tab_1bymaf)+1)]] <- data.frame(
            name_procedures[c(1,9:10,5,11:14)], 
            unlist(c(res_FDR_power["Power",c(1,9:10,5,11:14)], use.names=F)),
            c(rep(size_m,8)), c(rep(m1,8)), 
            unlist(c(res_FDR_power["FDR",c(1,9:10,5,11:14)],use.names=F)), 
            unlist(c(res_FDR_power["se.FDR",c(1,9:10,5,11:14)],use.names=F)),
            unlist(c(res_FDR_power["se.Power",c(1,9:10,5,11:14)],use.names=F)) )
          colnames(binary_tab_1bymaf[[(length(binary_tab_1bymaf))]]) <- c("Procedure",
                                                                        "Power","m",
                                                                        "m1","FDR",
                                                                        "se.FDR",
                                                                        "se.Power")
          # Binary dataframe containing power and FDR 
          # with an uninformative covariate
          binary_tab_uninfo[[(length(binary_tab_uninfo)+1)]] <- data.frame(
            name_procedures[c(1,15:17,5,18:20)], 
            unlist(c(res_FDR_power["Power",c(1,15:17,5,18:20)], use.names=F)),
            c(rep(size_m,8)), c(rep(m1,8)), 
            unlist(c(res_FDR_power["FDR",c(1,15:17,5,18:20)],use.names=F)), 
            unlist(c(res_FDR_power["se.FDR",c(1,15:17,5,18:20)],use.names=F)),
            unlist(c(res_FDR_power["se.Power",c(1,15:17,5,18:20)],use.names=F)) )
          colnames(binary_tab_uninfo[[(length(binary_tab_uninfo))]]) <- c("Procedure",
                                                                        "Power","m",
                                                                        "m1","FDR",
                                                                        "se.FDR",
                                                                        "se.Power")
          
          # Binary dataframe containing power and FDR 
          # with an uninformative covariate - BH
          binary_tab_Diff_uninfo_BH[[(length(binary_tab_Diff_uninfo_BH)+1)]] <- data.frame(
            name_procedures[c(16:20)], 
            c(unlist(c(res_FDR_power["Power",c(16:20)], use.names=F))-res_FDR_power["Power",1]),
            c(rep(size_m,5)), c(rep(m1,5)), 
            c(unlist(c(res_FDR_power["FDR",c(16:20)],use.names=F))-res_FDR_power["FDR",1]) )
          colnames(binary_tab_Diff_uninfo_BH[[(length(binary_tab_Diff_uninfo_BH))]]) <- c("Procedure",
                                                                        "Power","m",
                                                                        "m1","FDR")
          
            
          # Binary dataframe containing subpower
          # with MAF was used as the informative covariate
          colnames(res_Subpower) <- c("X1","X2","X3","X4",
                                      "se.X1","se.X2","se.X3","se.X4")
          
          binary_tab_subgp_maf[[(length(binary_tab_subgp_maf)+1)]] <- data.frame(
            Procedure=name_procedures[c(1:8)],
            Power=c(res_Subpower$X1[c(1:8)], res_Subpower$X2[c(1:8)],
                    res_Subpower$X3[c(1:8)], res_Subpower$X4[c(1:8)]),
            Subgroup=c(rep("Rare",length(res_Subpower[c(1:8),1])),
                       rep("Medium-Rare",length(res_Subpower[c(1:8),1])),
                       rep("Medium",length(res_Subpower[c(1:8),1])),
                       rep("Common",length(res_Subpower[c(1:8),1]))),
            m=rep(size_m,32), m1=rep(m1,32),
            se.Power=c(res_Subpower$se.X1[c(1:8)], res_Subpower$se.X2[c(1:8)],
                       res_Subpower$se.X3[c(1:8)], res_Subpower$se.X4[c(1:8)]) )
          
          # Binary dataframe containing subpower
          # with 1/MAF was used as the covariate
          binary_tab_subgp_1bymaf[[(length(binary_tab_subgp_1bymaf)+1)]] <- data.frame(
            Procedure=name_procedures[c(1,9:10,5,11:14)],
            Power=c(res_Subpower$X1[c(1,9:10,5,11:14)], 
                    res_Subpower$X2[c(1,9:10,5,11:14)],
                    res_Subpower$X3[c(1,9:10,5,11:14)], 
                    res_Subpower$X4[c(1,9:10,5,11:14)]),
            Subgroup=c(rep("Rare",length(res_Subpower[c(1,9:10,5,11:14),1])),
                       rep("Medium-Rare",length(res_Subpower[c(1,9:10,5,11:14),1])),
                       rep("Medium",length(res_Subpower[c(1,9:10,5,11:14),1])),
                       rep("Common",length(res_Subpower[c(1,9:10,5,11:14),1]))),
            m=rep(size_m,32), m1=rep(m1,32),
            se.Power=c(res_Subpower$se.X1[c(1,9:10,5,11:14)], 
                       res_Subpower$se.X2[c(1,9:10,5,11:14)],
                       res_Subpower$se.X3[c(1,9:10,5,11:14)], 
                       res_Subpower$se.X4[c(1,9:10,5,11:14)]) )
          
        }
      }
      # Final dataframes formatting
      quantitative_tab_maf <- data.frame(Reduce(rbind, quantitative_tab_maf))
      quantitative_tab_maf$FDR2=quantitative_tab_maf$FDR*100
      quantitative_tab_maf$Power2=quantitative_tab_maf$Power*100
      quantitative_tab_maf$se.FDR2=quantitative_tab_maf$se.FDR*100
      quantitative_tab_maf$se.Power2=quantitative_tab_maf$se.Power*100
      
      quantitative_tab_1bymaf <- data.frame(Reduce(rbind, quantitative_tab_1bymaf))
      quantitative_tab_1bymaf$FDR2=quantitative_tab_1bymaf$FDR*100
      quantitative_tab_1bymaf$Power2=quantitative_tab_1bymaf$Power*100
      quantitative_tab_1bymaf$se.FDR2=quantitative_tab_1bymaf$se.FDR*100
      quantitative_tab_1bymaf$se.Power2=quantitative_tab_1bymaf$se.Power*100
      
      quantitative_tab_uninfo <- data.frame(Reduce(rbind, quantitative_tab_uninfo))
      quantitative_tab_uninfo$FDR2=quantitative_tab_uninfo$FDR*100
      quantitative_tab_uninfo$Power2=quantitative_tab_uninfo$Power*100
      quantitative_tab_uninfo$se.FDR2=quantitative_tab_uninfo$se.FDR*100
      quantitative_tab_uninfo$se.Power2=quantitative_tab_uninfo$se.Power*100
      
      quantitative_tab_Diff_uninfo_BH <- data.frame(Reduce(rbind, quantitative_tab_Diff_uninfo_BH))
      quantitative_tab_Diff_uninfo_BH$FDR2=quantitative_tab_Diff_uninfo_BH$FDR*100
      quantitative_tab_Diff_uninfo_BH$Power2=quantitative_tab_Diff_uninfo_BH$Power*100
      
      binary_tab_maf <- data.frame(Reduce(rbind, binary_tab_maf))
      binary_tab_maf$FDR2=binary_tab_maf$FDR*100
      binary_tab_maf$Power2=binary_tab_maf$Power*100
      binary_tab_maf$se.FDR2=binary_tab_maf$se.FDR*100
      binary_tab_maf$se.Power2=binary_tab_maf$se.Power*100
      
      binary_tab_1bymaf <- data.frame(Reduce(rbind, binary_tab_1bymaf))
      binary_tab_1bymaf$FDR2=binary_tab_1bymaf$FDR*100
      binary_tab_1bymaf$Power2=binary_tab_1bymaf$Power*100
      binary_tab_1bymaf$se.FDR2=binary_tab_1bymaf$se.FDR*100
      binary_tab_1bymaf$se.Power2=binary_tab_1bymaf$se.Power*100
      
      binary_tab_uninfo <- data.frame(Reduce(rbind, binary_tab_uninfo))
      binary_tab_uninfo$FDR2=binary_tab_uninfo$FDR*100
      binary_tab_uninfo$Power2=binary_tab_uninfo$Power*100
      binary_tab_uninfo$se.FDR2=binary_tab_uninfo$se.FDR*100
      binary_tab_uninfo$se.Power2=binary_tab_uninfo$se.Power*100
      
      binary_tab_Diff_uninfo_BH <- data.frame(Reduce(rbind, binary_tab_Diff_uninfo_BH))
      binary_tab_Diff_uninfo_BH$FDR2=binary_tab_Diff_uninfo_BH$FDR*100
      binary_tab_Diff_uninfo_BH$Power2=binary_tab_Diff_uninfo_BH$Power*100
      
      quantitative_tab_subgp_maf <- data.frame(Reduce(rbind, quantitative_tab_subgp_maf))
      quantitative_tab_rare_maf <- subset(quantitative_tab_subgp_maf, 
                                      quantitative_tab_subgp_maf$Subgroup=="Rare")
      quantitative_tab_rare_maf$Power2=quantitative_tab_rare_maf$Power*100
      quantitative_tab_rare_maf$se.Power2=quantitative_tab_rare_maf$se.Power*100
      
      
      quantitative_tab_subgp_1bymaf <- data.frame(Reduce(rbind, quantitative_tab_subgp_1bymaf))
      quantitative_tab_common_1bymaf <- subset(quantitative_tab_subgp_1bymaf, 
                                      quantitative_tab_subgp_1bymaf$Subgroup=="Common")
      quantitative_tab_common_1bymaf$Power2=quantitative_tab_common_1bymaf$Power*100
      quantitative_tab_common_1bymaf$se.Power2=quantitative_tab_common_1bymaf$se.Power*100
      
      binary_tab_subgp_maf <- data.frame(Reduce(rbind, binary_tab_subgp_maf))
      binary_tab_rare_maf <- subset(binary_tab_subgp_maf, 
                                      binary_tab_subgp_maf$Subgroup=="Rare")
      binary_tab_rare_maf$Power2=binary_tab_rare_maf$Power*100
      binary_tab_rare_maf$se.Power2=binary_tab_rare_maf$se.Power*100
      
      
      binary_tab_subgp_1bymaf <- data.frame(Reduce(rbind, binary_tab_subgp_1bymaf))
      binary_tab_common_1bymaf <- subset(binary_tab_subgp_1bymaf, 
                                      binary_tab_subgp_1bymaf$Subgroup=="Common")
      binary_tab_common_1bymaf$Power2=binary_tab_common_1bymaf$Power*100
      binary_tab_common_1bymaf$se.Power2=binary_tab_common_1bymaf$se.Power*100
      
      # Creation of FDR graphics for independent case
      # with MAF was used as the informative covariate
      FDR_lineR_maf <- FDR_graph_design_ind(quantitative_tab_maf, "A")
      FDR_logit_maf <- FDR_graph_design_ind(binary_tab_maf, "B")
      legende <- get_legend(FDR_lineR_maf)
      FDR_lineR_maf <- FDR_lineR_maf+theme(legend.position="none")
      FDR_logit_maf <- FDR_logit_maf+theme(legend.position="none")
      graph_FDR_ind_maf[[scenario]] <- plot_grid(FDR_lineR_maf, FDR_logit_maf, legende, 
                                         ncol=1, rel_heights=c(2.3,2.3,0.5) )
      
      # with 1/MAF was used as the covariate
      FDR_lineR_1bymaf <- FDR_graph_design_ind(quantitative_tab_1bymaf, "A")
      FDR_logit_1bymaf <- FDR_graph_design_ind(binary_tab_1bymaf, "B")
      legende <- get_legend(FDR_lineR_1bymaf)
      FDR_lineR_1bymaf <- FDR_lineR_1bymaf+theme(legend.position="none")
      FDR_logit_1bymaf <- FDR_logit_1bymaf+theme(legend.position="none")
      graph_FDR_ind_1bymaf[[scenario]] <- plot_grid(FDR_lineR_1bymaf, FDR_logit_1bymaf, legende, 
                                         ncol=1, rel_heights=c(2.3,2.3,0.5) )
      
      
      # with an uninformative covariate
      FDR_lineR_uninfo <- FDR_graph_design_ind(quantitative_tab_uninfo, "A")
      FDR_logit_uninfo <- FDR_graph_design_ind(binary_tab_uninfo, "B")
      legende <- get_legend(FDR_lineR_uninfo)
      FDR_lineR_uninfo <- FDR_lineR_uninfo+theme(legend.position="none")
      FDR_logit_uninfo <- FDR_logit_uninfo+theme(legend.position="none")
      graph_FDR_ind_uninfo[[scenario]] <- plot_grid(FDR_lineR_uninfo, FDR_logit_uninfo, legende, 
                                         ncol=1, rel_heights=c(2.3,2.3,0.5) )
      
      # Creation of power graphics for independent case
      # with MAF was used as the informative covariate
      power_lineR_maf <- power_graph_design_ind_8procedures(quantitative_tab_maf, "A")
      power_logit_maf <- power_graph_design_ind_8procedures(binary_tab_maf, "B")
      legende <- get_legend(power_lineR_maf)
      power_lineR_maf <- power_lineR_maf+theme(legend.position="none")
      power_logit_maf <- power_logit_maf+theme(legend.position="none")
      graph_power_ind_maf[[scenario]] <- plot_grid(power_lineR_maf, power_logit_maf, legende,
                                           ncol=1, rel_heights=c(2.3,2.3,0.5) )
      # with 1/MAF was used as the covariate
      power_lineR_1bymaf <- power_graph_design_ind_8procedures(quantitative_tab_1bymaf, "A")
      power_logit_1bymaf <- power_graph_design_ind_8procedures(binary_tab_1bymaf, "B")
      legende <- get_legend(power_lineR_1bymaf)
      power_lineR_1bymaf <- power_lineR_1bymaf+theme(legend.position="none")
      power_logit_1bymaf <- power_logit_1bymaf+theme(legend.position="none")
      graph_power_ind_1bymaf[[scenario]] <- plot_grid(power_lineR_1bymaf, power_logit_1bymaf, legende,
                                           ncol=1, rel_heights=c(2.3,2.3,0.5) )
      # difference between uninformative covariates and BH procedure
      power_lineR_diff_uninfo_BH <- power_graph_design_ind_5procedures(quantitative_tab_Diff_uninfo_BH, "A")
      power_logit_diff_uninfo_BH <- power_graph_design_ind_5procedures(binary_tab_Diff_uninfo_BH, "B")
      legende <- get_legend(power_lineR_diff_uninfo_BH)
      power_lineR_diff_uninfo_BH <- power_lineR_diff_uninfo_BH+theme(legend.position="none")
      power_logit_diff_uninfo_BH <- power_logit_diff_uninfo_BH+theme(legend.position="none")
      graph_power_ind_diff_uninfo_BH[[scenario]] <- plot_grid(power_lineR_diff_uninfo_BH, 
                                                              power_logit_diff_uninfo_BH, legende,
                                                              ncol=1, rel_heights=c(2.3,2.3,0.5) )
      
      
      
      
      # Creation of subpower graphics for independent case
      power_rare_lineR_maf <- subpower_graph_design_ind(quantitative_tab_rare_maf, "A")
      power_rare_logit_maf <- subpower_graph_design_ind(binary_tab_rare_maf, "B")
      legende <- get_legend(power_rare_lineR_maf)
      power_rare_lineR_maf <- power_rare_lineR_maf+theme(legend.position="none")
      power_rare_logit_maf <- power_rare_logit_maf+theme(legend.position="none")
      graph_power_rare_ind_maf[[scenario]] <- plot_grid(power_rare_lineR_maf, power_rare_logit_maf,
                                                legende, ncol=1, 
                                                rel_heights=c(2.3,2.3,0.5) )
      
      
      power_common_lineR_1bymaf <- subpower_graph_design_ind(quantitative_tab_common_1bymaf, "A")
      power_common_logit_1bymaf <- subpower_graph_design_ind(binary_tab_common_1bymaf, "B")
      legende <- get_legend(power_common_lineR_1bymaf)
      power_common_lineR_1bymaf <- power_common_lineR_1bymaf+theme(legend.position="none")
      power_common_logit_1bymaf <- power_common_logit_1bymaf+theme(legend.position="none")
      graph_power_common_ind_1bymaf[[scenario]] <- plot_grid(power_common_lineR_1bymaf, power_common_logit_1bymaf,
                                                legende, ncol=1, 
                                                rel_heights=c(2.3,2.3,0.5) )
      
      
    }
  }
  if(Case==c("large_m_and_m1")){
    graph_FDR_ind_large_m <- list(reference="", inverse="", constant="")
graph_power_ind_large_m <- list(reference="", inverse="", constant="")
graph_power_rare_ind_large_m <- list(reference="", inverse="", constant="")

for (scenario in c("reference","inverse","constant")) {
  
  if(scenario=="reference"){ #corresponding to scenario 1
    quantitative_beta=c("B43210.rda")
  }
  if(scenario=="inverse"){ #corresponding to scenario 2
    quantitative_beta=c("B12340.rda")
  }
  if(scenario=="constant"){ #corresponding to scenario 3
    quantitative_beta=c("B22220.rda")
  }
  quantitative_tab_large_m <- list()
  quantitative_tab_large_m_subgp <- list()
  r2=0.5
  for (size_m in c(1e+05,2e+05,5e+05)) { 
    #size_m is the total number of null hypotheses tested
    for(m1 in c(100,150,250)){
      #m1 is the number of causal variants
      linear_file_in=paste("Res_LineR","n",size_n,"m",size_m,"m1",m1,"rho",vrho,
                         "tb",blsize,"r2",r2,quantitative_beta,sep="_")
      res_FDR_power <- simulation_data[[linear_file_in]]$res_FDR_power
      res_Subpower <- simulation_data[[linear_file_in]]$res_Subpower
      name_procedures <- c("BH","wBH","wBHa","IHW","qvalue","swfdr",
                         "FDRreg","CAMT","wBH_un_cov", "wBHa_un_cov", 
                         "IHW_un_cov", "swfdr_un_cov", "fdrreg_un_cov", 
                         "CAMT_un_cov")
    
    # Quantitative dataframe containing power and FDR 
    # with MAF was used as the informative covariate
    quantitative_tab_large_m[[(length(quantitative_tab_large_m)+1)]] <- data.frame(
      name_procedures[1:8], unlist(c(res_FDR_power["Power",c(1:8)], use.names=F)),
      c(rep(size_m,8)), c(rep(m1,8)), 
      unlist(c(res_FDR_power["FDR",c(1:8)],use.names=F)), 
      unlist(c(res_FDR_power["se.FDR",c(1:8)],use.names=F)),
      unlist(c(res_FDR_power["se.Power",c(1:8)],use.names=F)) )
    colnames(quantitative_tab_large_m[[(length(quantitative_tab_large_m))]]) <- c("Procedure",
                                                                                  "Power","m",
                                                                                  "m1","FDR",
                                                                                  "se.FDR",
                                                                                  "se.Power")
    
    # Quantitative dataframe containing subpower
    # with MAF was used as the informative covariate
    colnames(res_Subpower) <- c("X1","X2","X3","X4",
                                "se.X1","se.X2","se.X3","se.X4")
    quantitative_tab_large_m_subgp[[(length(quantitative_tab_large_m_subgp)+1)]] <- data.frame(
      Procedure=name_procedures[c(1:8)],
      Power=c(res_Subpower$X1[c(1:8)], res_Subpower$X2[c(1:8)],
              res_Subpower$X3[c(1:8)], res_Subpower$X4[c(1:8)]),
      Subgroup=c(rep("Rare",length(res_Subpower[c(1:8),1])),
                 rep("Medium-Rare",length(res_Subpower[c(1:8),1])),
                 rep("Medium",length(res_Subpower[c(1:8),1])),
                 rep("Common",length(res_Subpower[c(1:8),1]))),
      m=rep(size_m,32), m1=rep(m1,32),
      se.Power=c(res_Subpower$se.X1[c(1:8)], res_Subpower$se.X2[c(1:8)],
                 res_Subpower$se.X3[c(1:8)], res_Subpower$se.X4[c(1:8)]) )
  }
}
# Final dataframes formatting
quantitative_tab_large_m <- data.frame(Reduce(rbind, quantitative_tab_large_m))
quantitative_tab_large_m$FDR2=quantitative_tab_large_m$FDR*100
quantitative_tab_large_m$Power2=quantitative_tab_large_m$Power*100
quantitative_tab_large_m$se.FDR2=quantitative_tab_large_m$se.FDR*100
quantitative_tab_large_m$se.Power2=quantitative_tab_large_m$se.Power*100

quantitative_tab_large_m_subgp <- data.frame(Reduce(rbind, quantitative_tab_large_m_subgp))
quantitative_tab_large_m_rare <- subset(quantitative_tab_large_m_subgp, 
                                        quantitative_tab_large_m_subgp$Subgroup=="Rare")
quantitative_tab_large_m_rare$Power2=quantitative_tab_large_m_rare$Power*100
quantitative_tab_large_m_rare$se.Power2=quantitative_tab_large_m_rare$se.Power*100

# Creation of FDR graphics for independent case
# with MAF was used as the informative covariate
graph_FDR_ind_large_m[[scenario]] <- FDR_graph_design_ind_large_m(quantitative_tab_large_m, "")

# Creation of power graphics for independent case
# with MAF was used as the informative covariate
graph_power_ind_large_m[[scenario]] <- power_graph_design_ind_large_m(quantitative_tab_large_m, "")

# Creation of subpower graphics for independent case
graph_power_rare_ind_large_m[[scenario]] <- subpower_graph_design_ind_large_m(quantitative_tab_large_m_rare, "")
}
  }
  if(Case==c("correlation")){ #case with correlations between SNPs
    size_m <- 8000 #total number of null hypotheses tested
    blsize <- 10 #bloc size
    r2 <- 0.2 #coefficient of determination
    
    graph_FDR_corr <- list(reference="", inverse="", constant="")
    graph_power_corr <- list(reference="", inverse="", constant="")
    graph_power_rare_corr <- list(reference="", inverse="", constant="")
    
    for (scenario in c("reference","inverse","constant")) {
      if(scenario=="reference"){ #corresponding to scenario 1
        binary_beta=c("B_2.2_1.8_1.5_1.3_1.rda")
        quantitative_beta=c("B43210.rda")
      }
      if(scenario=="inverse"){ #corresponding to scenario 2
        binary_beta=c("B_1.3_1.5_1.8_2.2_1.rda")
        quantitative_beta=c("B12340.rda")
      }
      if(scenario=="constant"){ #corresponding to scenario 3
        binary_beta=c("B_1.5_1.5_1.5_1.5_1.rda")
        quantitative_beta=c("B22220.rda")
      }
      
      quantitative_tab<-list()
      binary_tab<-list()
      quantitative_tab_subgp<-list()
      binary_tab_subgp<-list()
      
      for(m1 in c(5,10,15,20,25,50,100,150)){ 
        #m1 is the number of causal variants
        for (vrho in c(0.10,0.20,0.35,0.5,0.75)) {
          #vrho is the correlation value between variants
          linear_file_in <- paste("Res_LineR","n",size_n,"m",size_m,"m1",m1,
                                  "rho",vrho,"tb",blsize,"r2",r2,
                                  quantitative_beta,sep="_")
          res_FDR_power <- simulation_data[[linear_file_in]]$res_FDR_power
          res_Subpower <- simulation_data[[linear_file_in]]$res_Subpower
          name_procedures <- c("BH","wBH","wBHa","IHW","qvalue","swfdr",
                               "FDRreg","CAMT","wBH_un_cov", "wBHa_un_cov", 
                               "IHW_un_cov", "swfdr_un_cov", "fdrreg_un_cov", 
                               "CAMT_un_cov")
          
          # Quantitative dataframe containing power and FDR
          quantitative_tab[[(length(quantitative_tab)+1)]] <- data.frame(
            name_procedures[1:8], unlist(c(res_FDR_power["Power",c(1:8)], use.names=F)),
            c(rep(vrho,8)), c(rep(m1,8)), unlist(c(res_FDR_power["FDR",c(1:8)],
            use.names=F)), unlist(c(res_FDR_power["se.FDR",c(1:8)],use.names=F)),
            unlist(c(res_FDR_power["se.Power",c(1:8)],use.names=F)) )
          colnames(quantitative_tab[[(length(quantitative_tab))]]) <- c("Procedure",
                                                                        "Power","rho",
                                                                        "m1","FDR",
                                                                        "se.FDR",
                                                                        "se.Power")
          
          # Quantitative dataframe containing subpower
          colnames(res_Subpower) <- c("X1","X2","X3","X4",
                                      "se.X1","se.X2","se.X3","se.X4")
          
          quantitative_tab_subgp[[(length(quantitative_tab_subgp)+1)]] <- data.frame(
            Procedure=name_procedures[c(1:8)],
            Power=c(res_Subpower$X1[c(1:8)], res_Subpower$X2[c(1:8)],
                    res_Subpower$X3[c(1:8)], res_Subpower$X4[c(1:8)]),
            Subgroup=c(rep("Rare",length(res_Subpower[c(1:8),1])),
                       rep("Medium-Rare",length(res_Subpower[c(1:8),1])),
                       rep("Medium",length(res_Subpower[c(1:8),1])),
                       rep("Common",length(res_Subpower[c(1:8),1]))),
            rho=rep(vrho,32), m1=rep(m1,32),
            se.Power=c(res_Subpower$se.X1[c(1:8)], res_Subpower$se.X2[c(1:8)],
                       res_Subpower$se.X3[c(1:8)], res_Subpower$se.X4[c(1:8)]) )
          
          logit_file_in <- paste("Res_Logit","n",size_n,"m",size_m,"m1",m1,
                                 "rho",vrho,"tb",blsize,binary_beta,sep="_")
          
          res_FDR_power <- simulation_data[[logit_file_in]]$res_FDR_power
          res_Subpower <- simulation_data[[logit_file_in]]$res_Subpower
          name_procedures <- c("BH","wBH","wBHa","IHW","qvalue","swfdr",
                               "FDRreg","CAMT","wBH_un_cov", "wBHa_un_cov", 
                               "IHW_un_cov", "swfdr_un_cov", "fdrreg_un_cov", 
                               "CAMT_un_cov")
          
          # Binary dataframe containing power and FDR
          binary_tab[[(length(binary_tab)+1)]] <- data.frame(
            name_procedures[1:8], unlist(c(res_FDR_power["Power",c(1:8)], use.names=F)),
            c(rep(vrho,8)), c(rep(m1,8)), unlist(c(res_FDR_power["FDR",c(1:8)],
            use.names=F)), unlist(c(res_FDR_power["se.FDR",c(1:8)],use.names=F)),
            unlist(c(res_FDR_power["se.Power",c(1:8)],use.names=F)) )
          colnames(binary_tab[[(length(binary_tab))]]) <- c("Procedure",
                                                                        "Power","rho",
                                                                        "m1","FDR",
                                                                        "se.FDR",
                                                                        "se.Power")
          
          # Binary dataframe containing subpower
          colnames(res_Subpower) <- c("X1","X2","X3","X4",
                                      "se.X1","se.X2","se.X3","se.X4")
          
          binary_tab_subgp[[(length(binary_tab_subgp)+1)]] <- data.frame(
            Procedure=name_procedures[c(1:8)],
            Power=c(res_Subpower$X1[c(1:8)], res_Subpower$X2[c(1:8)],
                    res_Subpower$X3[c(1:8)], res_Subpower$X4[c(1:8)]),
            Subgroup=c(rep("Rare",length(res_Subpower[c(1:8),1])),
                       rep("Medium-Rare",length(res_Subpower[c(1:8),1])),
                       rep("Medium",length(res_Subpower[c(1:8),1])),
                       rep("Common",length(res_Subpower[c(1:8),1]))),
            rho=rep(vrho,32), m1=rep(m1,32),
            se.Power=c(res_Subpower$se.X1[c(1:8)], res_Subpower$se.X2[c(1:8)],
                       res_Subpower$se.X3[c(1:8)], res_Subpower$se.X4[c(1:8)]) )
        
          }
      }
      # Final dataframes formatting
      quantitative_tab <- data.frame(Reduce(rbind, quantitative_tab))
      quantitative_tab$FDR2=quantitative_tab$FDR*100
      quantitative_tab$Power2=quantitative_tab$Power*100
      quantitative_tab$se.FDR2=quantitative_tab$se.FDR*100
      quantitative_tab$se.Power2=quantitative_tab$se.Power*100
      
      binary_tab <- data.frame(Reduce(rbind, binary_tab))
      binary_tab$FDR2=binary_tab$FDR*100
      binary_tab$Power2=binary_tab$Power*100
      binary_tab$se.FDR2=binary_tab$se.FDR*100
      binary_tab$se.Power2=binary_tab$se.Power*100
      
      quantitative_tab_subgp <- data.frame(Reduce(rbind, quantitative_tab_subgp))
      quantitative_tab_rare <- subset(quantitative_tab_subgp,
                                      quantitative_tab_subgp$Subgroup=="Rare")
      quantitative_tab_rare$Power2=quantitative_tab_rare$Power*100
      quantitative_tab_rare$se.Power2=quantitative_tab_rare$se.Power*100
      
      binary_tab_subgp <- data.frame(Reduce(rbind, binary_tab_subgp))
      binary_tab_rare <- subset(binary_tab_subgp, 
                                binary_tab_subgp$Subgroup=="Rare")
      binary_tab_rare$Power2=binary_tab_rare$Power*100
      binary_tab_rare$se.Power2=binary_tab_rare$se.Power*100
      
      # Creation of FDR graphics for correlation case
      FDR_lineR <- FDR_graph_design_corr(quantitative_tab, "A")
      FDR_logit <- FDR_graph_design_corr(binary_tab, "B")
      legende <- get_legend(FDR_lineR)
      FDR_lineR <- FDR_lineR+theme(legend.position="none")
      FDR_logit <- FDR_logit+theme(legend.position="none")
      graph_FDR_corr[[scenario]] <- plot_grid(FDR_lineR, FDR_logit, legende, 
                                         ncol=1, rel_heights=c(2.3,2.3,0.5) )
      
      # Creation of power graphics for correlation case
      power_lineR <- power_graph_design_corr(quantitative_tab, "A")
      power_logit <- power_graph_design_corr(binary_tab, "B")
      legende <- get_legend(power_lineR)
      power_lineR <- power_lineR+theme(legend.position="none")
      power_logit <- power_logit+theme(legend.position="none")
      graph_power_corr[[scenario]] <- plot_grid(power_lineR,power_logit,legende,
                                                ncol=1,rel_heights=c(2.3,2.3,0.5))
      
      # Creation of subpower graphics for correlation case
      power_rare_lineR <- subpower_graph_design_corr(quantitative_tab_rare, "A")
      power_rare_logit <- subpower_graph_design_corr(binary_tab_rare, "B")
      legende <- get_legend(power_rare_lineR)
      power_rare_lineR <- power_rare_lineR+theme(legend.position="none")
      power_rare_logit <- power_rare_logit+theme(legend.position="none")
      graph_power_rare_corr[[scenario]] <- plot_grid(power_rare_lineR,
                                                     power_rare_logit,
                                                     legende, ncol=1,
                                                     rel_heights=c(2.3,2.3,0.5))
    }
  }
  if(Case==c("semisimulation")){ 
    #case where the correlation matrix between variants is based on a real dataset
    threshold=0.8 #correlation coefficient threshold 
    r2=0.8 #coefficient of determination
    
    graph_FDR_semisimu <- list(Ref="", Inv="", Con="")
    graph_power_semisimu <- list(Ref="", Inv="", Con="")
    graph_power_rare_semisimu <- list(Ref="", Inv="", Con="")
    
    for(scenario in c("Ref","Inv","Con")) {
      semisimu_tab<-list()
      semisimu_tab_sbgp <- list()
      for(m1 in c(20,25,50,100,150)){
        #m1 is the number of causal variants
        semisimu_file <- paste(scenario,"_SemiSimu_HIV_m1_",m1,"_threshold_",threshold,
                             "_R2_",r2,".rda",sep="")
        res_FDR_power <- simulation_data[[semisimu_file]]$res_FDR_power
        res_Subpower <- simulation_data[[semisimu_file]]$res_Subpower
        name_procedures<-c("BH","wBH","wBHa","IHW","qvalue","swfdr","FDRreg","CAMT")
        
        # Dataframe containing power and FDR
        semisimu_tab[[(length(semisimu_tab)+1)]]<-data.frame(
        name_procedures,unlist(c(res_FDR_power["Power",c(1:8)],use.names=F)),
                         c(rep(m1,8)),
                         unlist(c(res_FDR_power["FDR",c(1:8)],use.names=F)),
                         unlist(c(res_FDR_power["se.FDR",c(1:8)],use.names=F)),
                         unlist(c(res_FDR_power["se.Power",c(1:8)],use.names=F)))
        colnames(semisimu_tab[[(length(semisimu_tab))]])<-c("Procedure","Power",
                                                          "m1","FDR",
                                                          "se.FDR","se.Power")
        # Dataframe containing subpower
        colnames(res_Subpower) <- c("X1","X2","X3","X4","se.X1","se.X2","se.X3","se.X4")
        semisimu_tab_sbgp[[(length(semisimu_tab_sbgp)+1)]] <- data.frame(
        Procedure=name_procedures,
        Power=c(res_Subpower$X1,res_Subpower$X2,res_Subpower$X3,res_Subpower$X4),
        Subgroup=c(rep("Rare",length(res_Subpower[,1])),
                   rep("Medium-Rare",length(res_Subpower[,1])),
                   rep("Medium",length(res_Subpower[,1])),
                   rep("Common",length(res_Subpower[,1]))),
        m1=rep(m1,32),se.Power=c(res_Subpower$se.X1,res_Subpower$se.X2,
                                 res_Subpower$se.X3,res_Subpower$se.X4) )
      }
      # Final dataframes formatting
      semisimu_tab <- data.frame(Reduce(rbind, semisimu_tab))
      semisimu_tab$FDR2=semisimu_tab$FDR*100
      semisimu_tab$Power2=semisimu_tab$Power*100
      semisimu_tab$se.FDR2=semisimu_tab$se.FDR*100
      semisimu_tab$se.Power2=semisimu_tab$se.Power*100
      
      semisimu_tab_sbgp <- data.frame(Reduce(rbind, semisimu_tab_sbgp))
      semisimu_tab_rare <- subset(semisimu_tab_sbgp, 
                                semisimu_tab_sbgp$Subgroup=="Rare")
      semisimu_tab_rare$Power2=semisimu_tab_rare$Power*100
      semisimu_tab_rare$se.Power2=semisimu_tab_rare$se.Power*100
      
      # Creation of FDR graphics for simulations based on real dataset
      graph_FDR_semisimu[[scenario]] <- FDR_graph_semisimu(semisimu_tab)
      
      # Creation of power graphics for simulations based on real dataset
      graph_power_semisimu[[scenario]] <- power_graph_semisimu(semisimu_tab)
      
      # Creation of subpower graphics for simulations based on real dataset
      graph_power_rare_semisimu[[scenario]] <- power_graph_semisimu(semisimu_tab_rare) 
    }
  }
}


## ----Ind_maf_FDR_reference, fig.align='center', fig.width=8, fig.height=8---------------------------
graph_FDR_ind_maf$reference


## ----Ind_maf_FDR_inverse, fig.align='center', fig.width=8, fig.height=8-----------------------------
graph_FDR_ind_maf$inverse


## ----Ind_maf_FDR_constant, fig.align='center', fig.width=8, fig.height=8----------------------------
graph_FDR_ind_maf$constant


## ----Ind_maf_power_reference, fig.align='center', fig.width=8, fig.height=8-------------------------
graph_power_ind_maf$reference


## ----Ind_maf_power_inverse, fig.align='center', fig.width=8, fig.height=8---------------------------
graph_power_ind_maf$inverse


## ----Ind_maf_power_constant, fig.align='center', fig.width=8, fig.height=8--------------------------
graph_power_ind_maf$constant


## ----Ind_maf_power_rare_reference, fig.align='center', fig.width=8, fig.height=8--------------------
graph_power_rare_ind_maf$reference


## ----Ind_maf_power_rare_inverse, fig.align='center', fig.width=8, fig.height=8----------------------
graph_power_rare_ind_maf$inverse


## ----Ind_maf_power_rare_constant, fig.align='center', fig.width=8, fig.height=8---------------------
graph_power_rare_ind_maf$constant


## ----Ind_1bymaf_FDR_reference, fig.align='center', fig.width=8, fig.height=8------------------------
graph_FDR_ind_1bymaf$reference


## ----Ind_1bymaf_FDR_inverse, fig.align='center', fig.width=8, fig.height=8--------------------------
graph_FDR_ind_1bymaf$inverse


## ----Ind_1bymaf_FDR_constant, fig.align='center', fig.width=8, fig.height=8-------------------------
graph_FDR_ind_1bymaf$constant


## ----Ind_1bymaf_power_reference, fig.align='center', fig.width=8, fig.height=8----------------------
graph_power_ind_1bymaf$reference


## ----Ind_1bymaf_power_inverse, fig.align='center', fig.width=8, fig.height=8------------------------
graph_power_ind_1bymaf$inverse


## ----Ind_1bymaf_power_constant, fig.align='center', fig.width=8, fig.height=8-----------------------
graph_power_ind_1bymaf$constant


## ----Ind_1bymaf_power_common_reference, fig.align='center', fig.width=8, fig.height=8---------------
graph_power_common_ind_1bymaf$reference


## ----Ind_1bymaf_power_common_inverse, fig.align='center', fig.width=8, fig.height=8-----------------
graph_power_common_ind_1bymaf$inverse


## ----Ind_1bymaf_power_common_constant, fig.align='center', fig.width=8, fig.height=8----------------
graph_power_common_ind_1bymaf$constant


## ----Ind_uninfo_FDR_reference, fig.align='center', fig.width=8, fig.height=8------------------------
graph_FDR_ind_uninfo$reference


## ----Ind_uninfo_FDR_inverse, fig.align='center', fig.width=8, fig.height=8--------------------------
graph_FDR_ind_uninfo$inverse


## ----Ind_uninfo_FDR_constant, fig.align='center', fig.width=8, fig.height=8-------------------------
graph_FDR_ind_uninfo$constant


## ----Ind_uninfo_power_reference, fig.align='center', fig.width=8, fig.height=8----------------------
graph_power_ind_diff_uninfo_BH$reference


## ----Ind_uninfo_power_inverse, fig.align='center', fig.width=8, fig.height=8------------------------
graph_power_ind_diff_uninfo_BH$inverse


## ----Ind_uninfo_power_constant, fig.align='center', fig.width=8, fig.height=8-----------------------
graph_power_ind_diff_uninfo_BH$constant


## ----Ind_largem_fdr_reference, fig.align='center', fig.width=8, fig.height=4------------------------
graph_FDR_ind_large_m$reference


## ----Ind_largem_fdr_inverse, fig.align='center', fig.width=8, fig.height=4--------------------------
graph_FDR_ind_large_m$inverse


## ----Ind_largem_fdr_constant, fig.align='center', fig.width=8, fig.height=4-------------------------
graph_FDR_ind_large_m$constant


## ----Ind_largem_power_reference, fig.align='center', fig.width=8, fig.height=4----------------------
graph_power_ind_large_m$reference


## ----Ind_largem_power_inverse, fig.align='center', fig.width=8, fig.height=4------------------------
graph_power_ind_large_m$inverse


## ----Ind_largem_power_constant, fig.align='center', fig.width=8, fig.height=4-----------------------
graph_power_ind_large_m$constant


## ----Ind_largem_power_rare_reference, fig.align='center', fig.width=8, fig.height=4-----------------
graph_power_rare_ind_large_m$reference


## ----Ind_largem_power_rare_inverse, fig.align='center', fig.width=8, fig.height=4-------------------
graph_power_rare_ind_large_m$inverse


## ----Ind_largem_power_rare_constant, fig.align='center', fig.width=8, fig.height=4------------------
graph_power_rare_ind_large_m$constant


## ----Corr_reference_fdr, fig.align='center', fig.width=8, fig.height=8------------------------------
graph_FDR_corr$reference


## ----Corr_inverse_fdr, fig.align='center', fig.width=8, fig.height=8--------------------------------
graph_FDR_corr$inverse


## ----Corr_constant_fdr, fig.align='center', fig.width=8, fig.height=8-------------------------------
graph_FDR_corr$constant


## ----Corr_reference_power, fig.align='center', fig.width=8, fig.height=8----------------------------
graph_power_corr$reference


## ----Corr_inverse_power, fig.align='center', fig.width=8, fig.height=8------------------------------
graph_power_corr$inverse


## ----Corr_constant_power, fig.align='center', fig.width=8, fig.height=8-----------------------------
graph_power_corr$constant


## ----Corr_reference_power_rare, fig.align='center', fig.width=8, fig.height=8-----------------------
graph_power_rare_corr$reference


## ----Corr_inverse_power_rare, fig.align='center', fig.width=8, fig.height=8-------------------------
graph_power_rare_corr$inverse


## ----Corr_constant_power_rare, fig.align='center', fig.width=8, fig.height=8------------------------
graph_power_rare_corr$constant


## ----Semisimu_fdr_reference, fig.align='center', fig.width=8, fig.height=2--------------------------
graph_FDR_semisimu$Ref


## ----Semisimu_fdr_Inverse, fig.align='center', fig.width=8, fig.height=2----------------------------
graph_FDR_semisimu$Inv


## ----Semisimu_fdr_constant, fig.align='center', fig.width=8, fig.height=2---------------------------
graph_FDR_semisimu$Con


## ----Semisimu_power_reference, fig.align='center', fig.width=8, fig.height=2------------------------
graph_power_semisimu$Ref


## ----Semisimu_power_Inverse, fig.align='center', fig.width=8, fig.height=2--------------------------
graph_power_semisimu$Inv


## ----Semisimu_power_constant, fig.align='center', fig.width=8, fig.height=2-------------------------
graph_power_semisimu$Con


## ----Semisimu_power_rare_reference, fig.align='center', fig.width=8, fig.height=2-------------------
graph_power_rare_semisimu$Ref


## ----Semisimu_power_rare_Inverse, fig.align='center', fig.width=8, fig.height=2---------------------
graph_power_rare_semisimu$Inv


## ----Semisimu_power_rare_constant, fig.align='center', fig.width=8, fig.height=2--------------------
graph_power_rare_semisimu$Con

