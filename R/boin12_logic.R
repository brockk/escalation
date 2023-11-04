
# Note - the functions in this file were written bu Micahel Sweeting but they
# have been slightly edited by Kristian to get concordance with the published
# paper and software; and to achieve behaviour that works for escalation.

#' Michael Sweeting's implementation of the admissibility algo in BOIN12
#'
#' @return integer indices of admissible doses
#' @author Michael Sweeting
#' @importFrom stats pbeta
#' @noRd
boin12_admissible <- function(
    data,
    currentdose,
    ndoses,
    phi_t,
    phi_e,
    c_t,
    c_e
) {
  admissible <- unique(c(max(1, currentdose-1), currentdose, min(ndoses, currentdose+1)))
  for(d in admissible){
    n_t_d <- sum(data$Toxicity[data$Dose==d]) # number of toxicities seen at dose d
    n_e_d <- sum(data$Efficacy[data$Dose==d]) # number of efficacies seen at dose d
    n_d <- sum(data$Dose==d) # number of pts treated at dose d
    # Does the dose satisfy the tox. constraint?
    if(pbeta(phi_t, 1+n_t_d, 1+n_d - n_t_d, lower.tail=F) >= c_t){ # not admissible too tox
      admissible <- admissible[admissible != d] # remove dose d from admissible set
    }
    # Does the dose satisfy the eff. constraint?
    if(pbeta(phi_e, 1+n_e_d, 1+n_d - n_e_d, lower.tail=T) >= c_e){ # not admissible not eff
      admissible <- admissible[admissible != d] # remove dose d from admissible set
    }
  }
  admissible
}

#' Calculate probability that BOIN12 utility is greater than benchmark u_b
#'
#' Used to calculate desirability score.
#'
#' @author Michael Sweeting
#' @importFrom stats pbeta
#' @noRd
boin12_pbenchmark <- function(
    doses,
    data,
    u1,
    u2,
    u3,
    u4,
    u_b,
    alpha = 1,
    beta = 1
) {
  n <- sapply(doses, function(d){sum(data$Dose==d)})
  y1 <- sapply(doses, function(d){sum(data$Toxicity[data$Dose==d]==0 & data$Efficacy[data$Dose==d]==1)}) # no tox, eff
  y2 <- sapply(doses, function(d){sum(data$Toxicity[data$Dose==d]==0 & data$Efficacy[data$Dose==d]==0)}) # no tox, no eff
  y3 <- sapply(doses, function(d){sum(data$Toxicity[data$Dose==d]==1 & data$Efficacy[data$Dose==d]==1)}) # tox, eff
  y4 <- sapply(doses, function(d){sum(data$Toxicity[data$Dose==d]==1 & data$Efficacy[data$Dose==d]==0)}) # tox, no eff

  # pseudo-data
  x <- (u1*y1 + u2*y2 + u3*y3 + u4*y4)/100
  # utility benchmark is ub
  # prob u(d)>u_b
  pbenchmark <- 100*pbeta(u_b/100, alpha + x, beta + n-x, lower.tail=F)
  pbenchmark
}

#' Michael Sweeting's implementation of the dose-selection algo in BOIN12
#'
#' @return integer, index of next dose
#' @author Michael Sweeting
#' @importFrom BOIN get.boundary
#' @noRd
boin12_next_dose <- function(
    data,
    ndoses,
    maxN,
    maxN_dose = 12,
    start = 1,
    phi_t,
    phi_e,
    Nstar = 6,
    c_t = 0.95,
    c_e = 0.90,
    u1 = 100, u2, u3, u4 = 0,
    alpha = 1, beta = 1,
    verbose = FALSE
) {

  # get escalation and de-escalation boundaries of BOIN design based on phi.t
  # any ncohort > 1 ok for getting boundaries
  gb <- get.boundary(target=phi_t, ncohort = 2, cohortsize = 1 )
  lambda_e <- gb$lambda_e
  lambda_d <- gb$lambda_d

  # Highest utility deemed undesirable
  u_bar <- 100*phi_e*(1-phi_t) + u2*(1-phi_t)*(1-phi_e) + u3*phi_t*phi_e +
    u4*phi_t*(1-phi_e)
  # Benchmark utility
  u_b <- u_bar+(100-u_bar)/2

  currentdose <- data$Dose[nrow(data)]
  n_t <- sum(data$Toxicity[data$Dose==currentdose]) # number of toxicities seen at current dose
  n <- sum(data$Dose==currentdose) # number of pts treated at current dose
  p_t <- n_t/n

  if(nrow(data) == 0) {
    next_dose <- start
    admissible <- rep(FALSE, ndoses)
    admissible[start] <- TRUE
    utility <- rep(NA, ndoses)

    # Let "stopping for n" and "stopping for n at dose" be handled elsewhere
  # }
  # else if(nrow(data) == maxN){ # stop trial if maximum sample size
  #   # TODO
  #   # next_dose <- 998 # 998 indicates reached sample size stopping rule
  #   next_dose <- NA
  #   admissible <- rep(FALSE, ndoses)
  #   admissible[currentdose] <- TRUE
  # }
  # else if(n >= maxN_dose) { # stop trial if maximum number at a single dose is reached
  #   # TODO
  #   # next_dose <- 999 # 998 indicates reached sample size stopping rule
  #   next_dose <- NA
  #   admissible <- rep(FALSE, ndoses)
  #   admissible[currentdose] <- TRUE
  } else {
    # Admissible doses (for d-1, d and d+1)
    admissible <- boin12_admissible(data, currentdose, ndoses, phi_t, phi_e, c_t, c_e)

    utility <- boin12_pbenchmark(
      seq_len(ndoses),
      data=data,
      u1=u1, u2=u2, u3=u3, u4=u4, u_b=u_b,
      alpha=alpha, beta=beta
    )

    # Decision rule of BOIN12 (based on Table 2 in paper PLUS dose exploration rule described in Supp Material top of p8)
    # First look at dose exploration rule
    if(n > 8 & p_t < lambda_d & currentdose!=ndoses & sum(data$Dose==currentdose+1)==0){ # escalate
      next_dose <- currentdose + 1
    } else if(p_t >= lambda_d){ # de-escalate (if we can)
      # Mike's line below looks sensible but it does not agree with publication:
      # next_dose <- ifelse(currentdose-1 %in% admissible, currentdose-1, 0) # 0 indicates no admissible dose so must stop trial
      next_dose <- max(currentdose - 1, 1)
    } else if(p_t > lambda_e & p_t < lambda_d & n >= Nstar){ # choose d or d-1, whichever has the largest PDS
      if(currentdose-1 %in% admissible & currentdose %in% admissible){ # if both d and d-1 are admissible
        RDSmax.ind <- which.max(boin12_pbenchmark(
          c(currentdose-1, currentdose),
          data=data,
          u1=u1, u2=u2, u3=u3, u4=u4, u_b=u_b,
          alpha=alpha, beta=beta)
        ) # choose dose that maximises rank-based desirability score
        next_dose <- c(currentdose-1, currentdose)[RDSmax.ind]
      } else {
        # else choose the one that is admissible, or none (0) if neither is
        # next_dose <- ifelse(currentdose-1 %in% admissible, currentdose-1,
        #                     ifelse(currentdose %in% admissible, currentdose, 0))
        next_dose <- ifelse(currentdose-1 %in% admissible, currentdose-1,
                            ifelse(currentdose %in% admissible, currentdose, 1))
      }
    } else if(length(admissible)==0) {
      # if none admissible
      next_dose <- 0
    } else {
      # else we can choose either d-1, d, or d+1
      u <- boin12_pbenchmark(
        admissible,
        data=data,
        u1=u1, u2=u2, u3=u3, u4=u4, u_b=u_b,
        alpha=alpha, beta=beta
      )
      # choose dose that maximises rank-based desirability score
      # RDSmax.ind <- which.max(u) # which.max resolves ties by choosing low doses
      # BOIN12 Shiny app resolves ties by chossing high doses. Do that:
      RDSmax.ind <- tail(seq_along(u)[u == max(u)], 1)
      next_dose <- admissible[RDSmax.ind] # choose dose that maximises rank-based desirability score
    }
  }
  if(verbose){
    # table 1
    tab1 <- data.frame(matrix(nrow=7,ncol=ndoses))
    colnames(tab1) <- paste0("Dose ", 1:ndoses)
    for(i in 1:ndoses){
      tab1[,i] <- c(
        sum(data$Dose==i),
        sum(data$Dose==i & data$Toxicity==0 & data$Efficacy==1),
        sum(data$Dose==i & data$Toxicity==1 & data$Efficacy==1),
        sum(data$Dose==i & data$Toxicity==0 & data$Efficacy==0),
        sum(data$Dose==i & data$Toxicity==1 & data$Efficacy==0),
        sum(data$Dose==i & data$Toxicity==1),
        sum(data$Dose==i & data$Efficacy==1)
      )
    }
    rownames(tab1) <- c("No. Patients",
                        "No. (Toxicity=0, Efficacy=1)",
                        "No. (Toxicity=1, Efficacy=1)",
                        "No. (Toxicity=0, Efficacy=0)",
                        "No. (Toxicity=1, Efficacy=0)",
                        "No. Toxicity",
                        "No. Efficacy"
    )
    if(verbose) print(tab1)

    # table 2
    tab2 <- data.frame(matrix(nrow=3,ncol=ndoses))
    colnames(tab2) <- paste0("Dose ", 1:ndoses)
    for(i in 1:ndoses){
      tab2[,i] <- c(
        sum(data$Dose==i & data$Toxicity==1) / sum(data$Dose==i),
        sum(data$Dose==i & data$Efficacy==1) / sum(data$Dose==i),
        boin12_pbenchmark(i,
                          data=data,
                          u1=u1, u2=u2, u3=u3, u4=u4, u_b=u_b,
                          alpha=alpha, beta=beta)
      )
    }
    rownames(tab2) <- c("Pr(toxicity)",
                        "Pr(efficacy)",
                        "100 x p(utility > benchmark)"
    )
    if(verbose) print(tab2)
    if(next_dose==0){
      cat(paste0("\n The trial should be stopped because no doses are admissible \n"))
    } else if(next_dose==998){
      cat(paste0("\n The trial should be stopped because the maximum sample size has been reached \n"))
    } else if(next_dose==999){
      cat(paste0("\n The trial should be stopped because the sample size at a dose has reached ", maxN_dose, "\n"))
    } else {
      cat(paste0("\n Treat the next cohort of patients at dose level ", next_dose, "\n"))
    }
  }

  # return(next_dose)

  return(list(
    next_dose = next_dose,
    admissible = admissible,
    utility = utility
  ))
}
