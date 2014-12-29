######################################################################################################
# THIS R FUNCTION COMPUTES COMORBIDITY COMPOSITE SCORES FOR THE ELIXHAUSER COMORBIDITY SYSTEM
# FOR THE 30 (ORIGINAL) OR 29 (REDUCED, EXCLUDING CARDIAC ARRHYTHMIA) COMORBIDITIES.
#
#
# THE SCORES WERE DERIVED IN VAN WALRAVEN ET AL.'S PAPER (2009) AND THIS PAPER
#
# THE FUNCTION ASSUMES THAT THE ELIXHAUSER COMORBIDITIES HAVE BEEN CODED AS BINARY VARIABLES WHERE
# A VALUE OF 1 INDICATES THE COMORBIDITY IS PRESENT AND A VALUE OF 0 INDICATES THE COMORBIDITY IS
# ABSENT
#
# THE ELIXHAUSER COMORBIDITIES CAN BE CODED USING THE HCUP COMORBIDITY SOFTWARE -- SEE
# http://www.hcup-us.ahrq.gov/toolssoftware/comorbidity/comorbidity.jsp
#
# THE MOST RECENT VERSION OF THIS FUNCTION CAN BE FOUND AT:
# http://www.lerner.ccf.org/qhs/software/elixhauser_composite_scores
# or
# https://github.com/sandygriffith/elixhauser_composite
#
######################################################################################################
# THIS FUNCTION ASSUMES THAT THE COMORBIDITY NAMES IN THE DATA MATCH THOSE IN THE HEALTHCARE COST
# AND UTILIZATION PROJECT (HCUP) COMORBIDITY SOFTWARE.
#
# THE COMORBIDITY NAMES AS GIVEN IN THE HCUP COMORBIDITY SOFTWARE ALONG WITH THEIR DESCRIPTION ARE:
#
# AIDS = AIDS/HIV
# ALCOHOL = Alcohol Abuse
# ANEMDEF = Anemia Defficiency
# ARTH = Rheumatoid Arthritis
# BLDLOSS = Blood Loss Anemia
# CARDARRH = Cardiac Arrhythmia
# CHF = Congestive Heart Failure
# CHRNLUNG = Chronic Pulmonary Disease
# COAG = Coagulopathy
# DEPRESS = Depression
# DM = Diabetes without Chronic Complications
# DMCX = Diabetes with Chronic Complications
# DRUG = Drug Abuse
# HTN_C = Hypertension
# HYPOTHY = Hypothyroidism
# LIVER = Liver Disease
# LYMPH = Lymphoma
# LYTES = Fluid and Electrolyte Disorders
# METS = Metastatic Cancer
# NEURO = Other Neurological Disorders
# OBESE = Obesity
# PARA = Paralysis
# PERIVASC = Peripheral Vascular Disease
# PSYCH = Psychoses
# PULMCIRC = Pulmonary Circulation Disorder
# RENLFAIL = Renal Failure
# TUMOR = Solid Tumor without Metastasis
# ULCER = Peptic Ulcer Disease
# VALVE = Valvular Disease
# WGHTLOSS = Weight Loss

######################################################################################################

elix.composite <- function(
  
  ############################################################################################
  # REQUIRED PARAMETERS
  ############################################################################################
  
  ## name of data frame that includes the 0/1 coded comorbidities
  data,
  
  
  ############################################################################################
  # DEFAULT PARAMETERS
  ############################################################################################
  
  ## which composite score to compute; options are van.walraven (default), sid.30, and sid.29
  method = c("van.walraven", "sid.30", "sid.29"),
  
  ## logical value indicating whether cardiac arrhythmia is to be included in calculations
  card.arr = FALSE){
  
  
  #################################################################################
  ## BEGIN FUNCTION BODY ##
  #################################################################################
  
  method <- match.arg(method);
  
  ## The user must specify a data frame ##  
  if(missing(data))
    stop("A data frame must be specified")
  
  ## The comorbidity indicator names must match those provided by the HCUP Comorbidity Software
  if(!(all(c("AIDS", "ALCOHOL", "ANEMDEF", "ARTH", "BLDLOSS", "CHF", "CHRNLUNG", "COAG",
             "DEPRESS", "DM", "DMCX", "DRUG", "HTN_C", "HYPOTHY", "LIVER", "LYMPH",
             "LYTES", "METS", "NEURO", "OBESE", "PARA", "PERIVASC", "PSYCH", "PULMCIRC",
             "RENLFAIL", "TUMOR", "ULCER", "VALVE", "WGHTLOSS") %in% names(data))))
    stop("Comorbidity indicator names in data frame must have the following names:
         AIDS (AIDS/HIV)
         ALCOHOL (Alcohol Abuse)
         ANEMDEF (Anemia Defficiency)
         ARTH (Rheumatoid Arthritis)
         BLDLOSS (Blood Loss Anemia)
         CHF (Congestive Heart Failure)
         CHRNLUNG (Chronic Pulmonary Disease)
         COAG (Coagulopathy)
         DEPRESS (Depression)
         DM (Diabetes without Chronic Complications)
         DMCX (Diabetes with Chronic Complications)
         DRUG (Drug Abuse)
         HTN_C (Hypertension)
         HYPOTHY (Hypothyroidism)
         LIVER (Liver Disease)
         LYMPH (Lymphoma)
         LYTES (Fluid and Electrolyte Disorders)
         METS (Metastatic Cancer)
         NEURO (Other Neurological Disorders)
         OBESE (Obesity)
         PARA (Paralysis)
         PERIVASC (Peripheral Vascular Disease)
         PSYCH (Psychoses)
         PULMCIRC (Pulmonary Circulation Disorder)
         RENLFAIL (Renal Failure)
         TUMOR (Solid Tumor without Metastasis)
         ULCER (Peptic Ulcer Disease)
         VALVE (Valvular Disease)
         WGHTLOSS (Weight Loss)")
  
  ## All comorbidity indicators must be coded as 0/1 where 1 indicates the comorbidity
  ## is present and 0 indicates the comorbidity is absent  
  if(!all(as.matrix(subset(data, select = c(AIDS, ALCOHOL, ANEMDEF, ARTH, BLDLOSS,
                                            CHF, CHRNLUNG, COAG, DEPRESS, DM, DMCX, DRUG,
                                            HTN_C, HYPOTHY, LIVER, LYMPH, LYTES, METS,
                                            NEURO, OBESE, PARA, PERIVASC, PSYCH, PULMCIRC,
                                            RENLFAIL, TUMOR, ULCER, VALVE, WGHTLOSS))) %in% c(0, 1)))
    stop("All comorbidity indicators must be coded as 0/1 where 1 indicates the comorbidity is present and 0 indicates the comorbidity is absent")
  
  ## If the user wishes to include cardiac arrhythmia in the calculation, the variable must be named CARDARRH
  if(card.arr & !("CARDARRH" %in% names(data)))
    stop("Cardiac Arrhythmia indicator is either missing or needs to be renamed CARDARRH")
  
  ## If the user wishes to include cardiac arrhythmia in the calculation, the variable must be coded 0/1
  if(card.arr & !all(data$CARDARRH %in% c(0, 1)))
    stop("CARDARRH must be coded as 0/1 where 1 indicates cardiac arrhythmia is present and 0 indicates cardiac arrhythmia is absent")
  
  ## Compute van Walraven's summary score as derived by van Walraven et al. (2009)
  if(method == "van.walraven"){
    
    vw.score <- with(data, 0 * AIDS +
                       0 * ALCOHOL +
                       (-2) * ANEMDEF +
                       0 * ARTH +
                       (-2) * BLDLOSS +
                       7 * CHF +
                       3 * CHRNLUNG +
                       3 * COAG +
                       (-3) * DEPRESS +
                       0 * DM +
                       0 * DMCX +
                       (-7) * DRUG +
                       0 * HTN_C +
                       0 * HYPOTHY +
                       11 * LIVER +
                       9 * LYMPH +
                       5 * LYTES +
                       12 * METS +
                       6 * NEURO +
                       (-4) * OBESE +
                       7 * PARA +
                       2 * PERIVASC +
                       0 * PSYCH +
                       4 * PULMCIRC +
                       5 * RENLFAIL +
                       4 * TUMOR +
                       0 * ULCER +
                       (-1) * VALVE +
                       6 * WGHTLOSS)
    
    ## option to compute the score with cardiac arrhythmia
    if(card.arr){
      vw.score.card.arr <- vw.score + 5 * data$CARDARRH
      vw.score.card.arr
    }
    
    else
      
      vw.score
  }
  
  else
    
    ## compute the SID_30 score as derived in the present study
    if(method == "sid.30"){
      sid.30 <- with(data, 0 * AIDS +
                       0 * ALCOHOL +
                       0 * ANEMDEF +
                       0 * ARTH +
                       (-3) * BLDLOSS +
                       9 * CHF +
                       3 * CHRNLUNG +
                       12 * COAG +
                       (-5) * DEPRESS +
                       1 * DM +
                       0 * DMCX +
                       (-11) * DRUG +
                       (-2) * HTN_C +
                       0 * HYPOTHY +
                       7 * LIVER +
                       8 * LYMPH +
                       11 * LYTES +
                       17 * METS +
                       5 * NEURO +
                       (-5) * OBESE +
                       4 * PARA +
                       4 * PERIVASC +
                       (-6) * PSYCH +
                       5 * PULMCIRC +
                       7 * RENLFAIL +
                       10 * TUMOR +
                       0 * ULCER +
                       0 * VALVE +
                       10 * WGHTLOSS)
      
      ## option to compute the score with cardiac arrhythmia
      if(card.arr){
        sid.30.card.arr <- sid.30 + 8 * data$CARDARRH
        sid.30.card.arr
      }
      
      else
        
        sid.30
    }
  else
    
    ## compute the SID_29 score as derived in the present study
    if(method == "sid.29"){
      sid.29 <- with(data, 0 * AIDS +
                       (-2) * ALCOHOL +
                       0 * ANEMDEF +
                       0 * ARTH +
                       (-2) * BLDLOSS +
                       9 * CHF +
                       3 * CHRNLUNG +
                       9 * COAG +
                       (-4) * DEPRESS +
                       0 * DM +
                       (-1) * DMCX +
                       (-8) * DRUG +
                       (-1) * HTN_C +
                       0 * HYPOTHY +
                       5 * LIVER +
                       6 * LYMPH +
                       9 * LYTES +
                       13 * METS +
                       4 * NEURO +
                       (-4) * OBESE +
                       3 * PARA +
                       4 * PERIVASC +
                       (-4) * PSYCH +
                       5 * PULMCIRC +
                       6 * RENLFAIL +
                       8 * TUMOR +
                       0 * ULCER +
                       0 * VALVE +
                       8 * WGHTLOSS)
      sid.29
    }
}