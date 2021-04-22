#include <TMB.hpp>

namespace CppAD
{
void PrintFor(const char* before, const double& var) { }
}

template<class Type>
vector<Type> rmultinom(Type N, vector<Type> p)
{
  //multinomial
  int dim = p.size();
  vector<Type> x(dim);
  int Nint = CppAD::Integer(N);
  x.setZero();
  for(int i = 0; i < Nint; i++)
  {
    Type y = runif(0.0,1.0);
    for(int a = 0; a < dim; a++) if(y < p.head(a+1).sum())
    {
      x(a) += 1.0;
      break;
    }
  }
  return x;
}

template<class Type>
Type objective_function<Type>::operator() ()
{
  
  //TABLE OF CONTENTS
  //I. DATA INPUTS
  //II. PARAMETER DECLARATION
  //III. SETTING INTITAL VALUES FOR DATA AND BACK TRASFORMING PARAMETERS ("Pre-function" section)
  //IV. FUNCTIONS AND CALCULATIONS
  //    1. Growths and Eggs
  //    2. Variabilities
  //    3. Selectivity
  //    4. Mortality
  //    5. Numbers at age
  //    6. Biomass
  //    7. Spawning
  //    8. Eggs
  //    9. Catch at age
  //    10. Select
  //    11. Predicted biomass
  //    12. Residuals
  //    13. Projections
  //    14. TAC
  //    15. Report Calcs
  //    16. Annual Average Mortality
  //    17. Objective Functions
  //V. REPORT SECTION
  //VI. SIMULATION SECTION
  
  //I. DATA INPUTS
  //---------------------------------------------------------------------------------------------
  DATA_INTEGER(fyear); //First year
  DATA_INTEGER(ldyear); //Last year
  DATA_INTEGER(lyear); //Last year in retrospective analysis
  DATA_INTEGER(fage); //First age
  DATA_INTEGER(lage); //Last age
  DATA_INTEGER(targ_age); //Target age for target mortality estimation
  DATA_INTEGER(totlmort); //Total number of mortality sources
  DATA_INTEGER(fishmort); //Number of fishing mortality sources

  DATA_VECTOR(years); //Vector of years
  DATA_VECTOR(ryears); //Vector of years in retrospective analysis
  DATA_VECTOR(ages); //Vector of ages
  
  DATA_SCALAR(rhoSR); //Rho values that adjust the total variance
  DATA_SCALAR(rhoCG);
  DATA_SCALAR(rhoCT);
  DATA_SCALAR(rhoEG);
  DATA_SCALAR(rhoET);
  DATA_SCALAR(rhosel);
  
  DATA_SCALAR(sp_time); //Spawning time (fraction showing percent of the year passes before spawn)
  DATA_SCALAR(harv_time);
  
  DATA_MATRIX(in_watage); //Weight at age
  DATA_MATRIX(in_latage); //Length at age
  DATA_MATRIX(in_mat); //Maturity schedule at age and year
  
  DATA_SCALAR(H2O_T); //Values related to the environmental specific natural mortality
  DATA_SCALAR(Linf);
  DATA_SCALAR(vb_K);
  DATA_SCALAR(lnmedM);
  DATA_SCALAR(sdM); //Prior standard deviation of natural mortality
  DATA_INTEGER(surv_num);
  
  //Trap net data starts here
  DATA_MATRIX(in_obs_PAT); //Observed proportion at age
  DATA_VECTOR(in_NtildeT); //Number of fish sampled in trap net bio samps
  DATA_VECTOR(in_N_SampT); //Effective sample size
  DATA_VECTOR(in_harv_wgtT); //Trap net harvest biomass
  DATA_VECTOR(in_mnwgtT); //Mean weight of individual in catch
  DATA_VECTOR(in_effortT); //Observed effort by year
  DATA_SCALAR(maxNT);
  DATA_VECTOR(W_ageT);
  
  //Gill net data starts here
  DATA_MATRIX(in_obs_PAG); //Observed proportion at age
  DATA_VECTOR(in_NtildeG); //Number of fish sampled in trap net bio samps
  DATA_VECTOR(in_N_SampG); //Effective sample size
  DATA_VECTOR(in_harv_wgtG); //Gill net harvest biomass
  DATA_VECTOR(in_mnwgtG); //Mean weight of individual in catch
  DATA_VECTOR(in_effortG); //Observed effort by year
  DATA_VECTOR(in_effort_adjust); //Adjustment in effort for changes over time
  
  //Harvest adjustment vector for under reporting
  DATA_VECTOR(in_Tharv_adjust);
  DATA_VECTOR(in_Gharv_adjust);
  
  //Constants used to calculate egg production
  DATA_SCALAR(percent_female);
  DATA_SCALAR(eggs_per_kg);
  
  //Ref ages for selectivity
  DATA_SCALAR(reflenG);
  DATA_SCALAR(reflenT);
  
  //Target rates
  DATA_SCALAR(targ_ptn);
  DATA_SCALAR(targ_A);
  DATA_SCALAR(targ_SPR);
  
  //Optional section to output all the input values to check it worked:
  // std::cout<<"fyear: "<<fyear<<std::endl;
  // std::cout<<"ldyear: "<<ldyear<<std::endl;
  // std::cout<<"lyear: "<<lyear<<std::endl;
  // std::cout<<"fage: "<<fage<<std::endl;
  // std::cout<<"lage: "<<lage<<std::endl;
  // std::cout<<"targ_age: "<<targ_age<<std::endl;
  // std::cout<<"totlmort: "<<totlmort<<std::endl;
  // std::cout<<"fishmort: "<<fishmort<<std::endl;
  // std::cout<<"years: "<<years<<std::endl;
  // std::cout<<"ryears: "<<ryears<<std::endl;
  // std::cout<<"ages: "<<ages<<std::endl;
  // std::cout<<"rhoSR: "<<rhoSR<<std::endl;
  // std::cout<<"rhoCG: "<<rhoCG<<std::endl;
  // std::cout<<"rhoCT: "<<rhoCT<<std::endl;
  // std::cout<<"rhoEG: "<<rhoEG<<std::endl;
  // std::cout<<"rhoET: "<<rhoET<<std::endl;
  // std::cout<<"rhosel: "<<rhosel<<std::endl;
  // std::cout<<"sp_time: "<<sp_time<<std::endl;
  // std::cout<<"harv_time: "<<harv_time<<std::endl;
  // std::cout<<"in_watage: "<<in_watage<<std::endl;
  // std::cout<<"in_latage: "<<in_latage<<std::endl;
  // std::cout<<"in_mat: "<<in_mat<<std::endl;
  // std::cout<<"H2O_T: "<<H2O_T<<std::endl;
  // std::cout<<"Linf: "<<Linf<<std::endl;
  // std::cout<<"vb_K: "<<vb_K<<std::endl;
  // std::cout<<"lnmedM: "<<lnmedM<<std::endl;
  // std::cout<<"sdM: "<<sdM<<std::endl;
  // std::cout<<"surv_num: "<<surv_num<<std::endl;
  // std::cout<<"in_obs_PAT: "<<in_obs_PAT<<std::endl;
  // std::cout<<"in_NtildeT: "<<in_NtildeT<<std::endl;
  // std::cout<<"in_N_SampT: "<<in_N_SampT<<std::endl;
  // std::cout<<"in_harv_wgtT: "<<in_harv_wgtT<<std::endl;
  // std::cout<<"in_mnwgtT: "<<in_mnwgtT<<std::endl;
  // std::cout<<"in_effortT: "<<in_effortT<<std::endl;
  // std::cout<<"in_obs_PAG: "<<in_obs_PAG<<std::endl;
  // std::cout<<"in_NtildeG: "<<in_NtildeG<<std::endl;
  // std::cout<<"in_N_SampG: "<<in_N_SampG<<std::endl;
  // std::cout<<"in_harv_wgtG: "<<in_harv_wgtG<<std::endl;
  // std::cout<<"in_mnwgtG: "<<in_mnwgtG<<std::endl;
  // std::cout<<"in_effortG: "<<in_effortG<<std::endl;
  // std::cout<<"in_effort_adjust: "<<in_effort_adjust<<std::endl;
  // std::cout<<"in_Tharv_adjust: "<<in_Tharv_adjust<<std::endl;
  // std::cout<<"in_Gharv_adjust: "<<in_Gharv_adjust<<std::endl;
  // std::cout<<"percent_female: "<<percent_female<<std::endl;
  // std::cout<<"eggs_per_kg: "<<eggs_per_kg<<std::endl;
  // std::cout<<"reflenG: "<<reflenG<<std::endl;
  // std::cout<<"reflenT: "<<reflenT<<std::endl;
  // std::cout<<"targ_ptn: "<<targ_ptn<<std::endl;
  // std::cout<<"targ_A: "<<targ_A<<std::endl;
  // std::cout<<"targ_SPR: "<<targ_SPR<<std::endl;
  
  //II. PARAMETER DECLARATION
  //---------------------------------------------------------------------------------------------
  PARAMETER(log_sig); //Sigma value, used to convert rhos to SD just for obs error
  PARAMETER(lnM); //Log scale natural mortality
  
  PARAMETER(log_qT); //Catchability, trap net
  PARAMETER(log_qG); //Catchability, gill net
  
  PARAMETER(logselG_p1); //Double logistic selectivity curves
  PARAMETER(logselG_p2);
  PARAMETER(logselT_p1);
  PARAMETER(logselT_p2);
  
  PARAMETER_VECTOR(logdevT_p1); 
  PARAMETER(log_R0);
  PARAMETER_VECTOR(log_recdev);
  // PARAMETER_VECTOR(log_rec);
  
  PARAMETER(lnalpha);
  PARAMETER(lnbeta);
  
  PARAMETER_VECTOR(effort_devsT); 
  PARAMETER_VECTOR(effort_devsG); 
  
  //DERIVED QUANTITIES TO STORE TRUNCATED VERSIONS OF INPUT DATA
  //---------------------------------------------------------------------------------------------
  //Derived Biology Information
  matrix<Type> watage(ryears.size(),ages.size()+1); //Weight at age
  matrix<Type> latage(ryears.size(),ages.size()); //Length at age
  matrix<Type> mat(ryears.size(),ages.size()); //Maturity schedule
  //Derived Trap net values
  vector<Type> obs_CT; //Observed catch trap net
  matrix<Type> obs_PAT(ryears.size(),ages.size()); //Observed proportion at age, trap net
  vector<Type> NtildeT(ryears.size()); //Number of fish sampled in trap net bio samps
  vector<Type> N_SampT(ryears.size()); //Effective sample size trap net
  vector<Type> harv_wgtT(ryears.size()); //Trap net harvest biomass
  vector<Type> mnwgtT(ryears.size()); //Mean weight of individual in catch
  vector<Type> effortT(ryears.size()); //Observed effort by year
  //Gill net values
  vector<Type> obs_CG; //Observed catch gill net
  matrix<Type> obs_PAG(ryears.size(),ages.size()); //Observed proportion at age, gill net
  vector<Type> NtildeG(ryears.size()); //Number of fish sampled in gill net bio samps
  vector<Type> N_SampG(ryears.size()); //Effective sample size gill net
  vector<Type> harv_wgtG(ryears.size()); //Gill net harvest biomass
  vector<Type> mnwgtG(ryears.size()); //Gill net mean weight
  vector<Type> effortG(ryears.size()); //Observed effort by year
  vector<Type> effort_adjust(ryears.size()); //Adjustment in effort for changes over time
  //Harvest adjustment vector for under reporting
  vector<Type> Tharv_adjust(ryears.size());
  vector<Type> Gharv_adjust(ryears.size());

  //VARIABLES LISTED BY SECTION
  //---------------------------------------------------------------------------------------------
  //0. Quantities needed for all the model
  int i;
  int j;
  int k;

  //1. Growths and Eggs
  matrix<Type> G(ryears.size(),ages.size()); //Instantaneous Growth rate
  matrix<Type> pop_WA(ryears.size(),ages.size()); //Weight at age, beginning of year
  matrix<Type> spawn_WA(ryears.size(),ages.size()); //Weight at age, spawning time
  matrix<Type> harv_WA(ryears.size(),ages.size()); //Weight at age, harvest time
  vector<Type> eggs_per_female; //Calculated eggs per female
  matrix<Type> wght_fac;
  matrix<Type> egg_fac;

  // 2. Variabilities
  Type sig;
  Type sd_logCT;
  Type sd_logeffortT;
  Type sd_logCG;
  Type sd_logeffortG;
  Type sdSR;
  Type sd_sel;

  //3. Selectivity
  Type G_p1; //Selectivity Curve Parameter 1 Gill
  vector <Type> T_p1(ryears.size()); //Selectivity Curve Parameters 1 Trap
  Type G_p2; //Selectivity Curve Parameter 2 Gill
  Type T_p2; //Selectivity Curve Parameter 2 Trap
  matrix <Type> numG(ryears.size(),ages.size());
  matrix <Type> numT(ryears.size(),ages.size());
  vector <Type> denG(ryears.size());
  vector <Type> denT(ryears.size());
  matrix <Type> selG(ryears.size(),ages.size());
  matrix <Type> selT(ryears.size(),ages.size());

  //4. Mortality
  Type M;
  vector<Type> qT(ryears.size()); //Trap net fishing catchability
  vector<Type> qG(ryears.size()); //Gill net fishing catchability
  matrix<Type> FT(ryears.size(),ages.size()); //Trap net fishing mortality
  matrix<Type> FG(ryears.size(),ages.size()); //Gill net fishing mortality
  vector<Type> pred_effT(ryears.size()); //Predicted effort of trap net fishery
  vector<Type> pred_effG(ryears.size()); //Predicted effort of gill net fishery
  matrix<Type> F(ryears.size(),ages.size()); //Total Fishing mortality
  matrix<Type> Z(ryears.size(),ages.size());
  matrix<Type> S(ryears.size(),ages.size());
  matrix<Type> A(ryears.size(),ages.size());
  matrix<Type> S_spawn(ryears.size(),ages.size());
  matrix<Type> MD(ryears.size(),ages.size());
  vector<Type> AvgZFirstYears(ages.size());

  //5. Numbers at age
  matrix<Type> N(ryears.size(),ages.size());
  matrix<Type> N_spawn(ryears.size(),ages.size());

  //6. Biomass
  vector<Type> BIOMASS(ryears.size());
  vector<Type> BIOMASSATAGE;

  //7. Spawning
  vector<Type> SP_BIO(ryears.size());
  vector<Type> SP_BIOAGE;

  //8. Eggs
  vector<Type> eggs(ryears.size());
  vector<Type> eggsatage;
  vector<Type> PredR(ryears.size());

  //9. Catch at age
  matrix<Type> CAT;
  matrix<Type> CAG;
  matrix<Type> MDEAD;
  matrix<Type> TDEAD;
  vector<Type> CT(ryears.size());
  vector<Type> CG(ryears.size());
  matrix<Type> PAT(ryears.size(),ages.size());
  matrix<Type> PAG(ryears.size(),ages.size());

  //10. Selectivity last year
  vector<Type> selectT(ages.size());
  vector<Type> selectG(ages.size());

  //11. Predicted biomass
  vector<Type> BT;
  vector<Type> BG;

  //12. Residuals
  vector<Type> residCT;
  vector<Type> residCG;
  matrix<Type> prodPAT(ryears.size(),ages.size());
  matrix<Type> prodPAG(ryears.size(),ages.size());
  vector<Type> Ntrap;
  vector<Type> Ngill;
  matrix<Type> denomT(ryears.size(),ages.size());
  matrix<Type> denomG(ryears.size(),ages.size());
  matrix<Type> residPAT;
  matrix<Type> residPAG;
  vector<Type> resid_effT(ryears.size()-1);
  vector<Type> resid_effG(ryears.size()-1);

  //13. Projections
  vector<Type> Fproj;
  vector<Type> FTproj;
  vector<Type> FGproj;
  vector<Type> Zproj;
  vector<Type> Sproj;
  vector<Type> Mproj;
  vector<Type> Sproj_BASE;
  vector<Type> Sproj_T(ages.size());
  vector<Type> wfacproj;
  vector<Type> watageproj;
  Type YPR;
  vector<Type> Psurv(ages.size());
  vector<Type> Psurv_BASE(ages.size());
  vector<Type> Psurv_T(ages.size());
  Type SSBR;
  Type SSBR_T;
  Type SSBR_BASE;
  Type SPR;
  Type SPR_T;
  Type SSBR_RATIO_ET;
  Type RecSum;
  Type Rec_avg;
  vector<Type> RecVec(ryears.size());

  //14. TAC

  //15. Report Calcs
  Type AvgBiomasslb;
  Type AvgSPbiomasslb;
  Type AvgF_gill;
  Type AvgF_trap;
  Type AvgZ;
  Type AvgR;
  Type SSBRlb;
  Type SSBR_BASElb;
  Type SSBR_Tlb;
  Type YPRlb;
  Type avg_F;

  //16. Annual
  vector<Type> Aproj;
  Type avg_A;

  //17. Objective Functions
  Type NLL;
  Type NLP;
  Type f;

  Type SSQ_SR;
  Type Nrec;

  //III. SETTING INTITAL VALUES FOR DATA AND BACK TRASFORMING PARAMETERS ("Pre-function" section)
  //---------------------------------------------------------------------------------------------
  // Using for loop to delete years of data for retrospective analysis
  for(i=0;i<=ryears.size()-1;i++)
  {
    watage.row(i) = in_watage.row(i);
    mat.row(i) = in_mat.row(i);
    latage.row(i) = in_latage.row(i);
    obs_PAT.row(i) = in_obs_PAT.row(i);
    NtildeT(i) = in_NtildeT(i);
    N_SampT(i) = in_N_SampT(i);
    harv_wgtT(i) = in_harv_wgtT(i);
    mnwgtT(i) = in_mnwgtT(i);
    effortT(i) = in_effortT(i);

    obs_PAG.row(i) = in_obs_PAG.row(i);
    NtildeG(i) = in_NtildeG(i);
    N_SampG(i) = in_N_SampG(i);
    harv_wgtG(i) = in_harv_wgtG(i);
    mnwgtG(i) = in_mnwgtG(i);
    effortG(i) = in_effortG(i);
    effort_adjust(i) = in_effort_adjust(i);

    Tharv_adjust(i) = in_Tharv_adjust(i);
    Gharv_adjust(i) = in_Gharv_adjust(i);
  }

  //Add adjustments to the observed effort and catch
  effortG = effort_adjust.array()*effortG.array();
  obs_CT = (harv_wgtT/mnwgtT)/Tharv_adjust;
  obs_CG = (harv_wgtG/mnwgtG)/Gharv_adjust;
  
  lnM=lnmedM;

  //IV. FUNCTIONS AND CALCULATIONS
  //---------------------------------------------------------------------------------------------
  //1. Growths and Eggs
  //-----------------------------------------------
  //Calculate instantaneous growth from weight at age matrix:
  for (i=0; i<=ryears.size()-2; i++)
  {
    for (j=0; j<=ages.size()-2; j++)
    {
      //if (watage(i+1,j+1) <= watage(i,j)) G(i,j)=0;
      //else G(i,j)=log(watage(i+1,j+1))-log(watage(i,j));
      G(i,j)=log(watage(i+1,j+1))-log(watage(i,j));
      if (G(i,j)<0)
      {
        G(i,j)=0.0;
      }
      G(ryears.size()-1,j)=G(ryears.size()-2,j);
    }
    G(i,ages.size()-1)=G(i,ages.size()-2);
  }

  //Calculate weight at age matrices from different times of the year based on instantaneous growth
  for (i=0; i<=ryears.size()-1; i++)
  {
    for (j=0; j<=ages.size()-2; j++)
    {
      if(i>0)
      {
        pop_WA(i,j)=watage(i,j+1)*exp((-0.5*G(i-1,j)));
      }
      else
      {
        pop_WA(i,j)=watage(i,j+1)*exp((-0.5*G(i,j)));
      }
      spawn_WA(i,j)=watage(i,j+1)*exp((sp_time-0.5)*(G(i,j+1)));
      harv_WA(i,j)=watage(i,j+1);
    }
    pop_WA(i,ages.size()-1)=watage(i,ages.size());
    spawn_WA(i,ages.size()-1)=watage(i,ages.size());
    harv_WA(i,ages.size()-1)=watage(i,ages.size());
  }

  //Calculate weight and egg factors
  eggs_per_female=eggs_per_kg*spawn_WA.row(ryears.size()-1);
  wght_fac=mat.array()*spawn_WA.array();
  egg_fac=eggs_per_kg*wght_fac;

  // std::cout<<"G: "<<G<<std::endl;
  // std::cout<<"pop_WA: "<<pop_WA<<std::endl;
  // std::cout<<"spawn_WA: "<<spawn_WA<<std::endl;
  // std::cout<<"harv_WA: "<<harv_WA<<std::endl;
  // std::cout<<"eggs_per_female: "<<eggs_per_female<<std::endl;
  // std::cout<<"wght_fac: "<<wght_fac<<std::endl;
  // std::cout<<"egg_fac: "<<egg_fac<<std::endl;

  //2. Variabilities
  //-----------------------------------------------
  //Back calculate parameters from log or logit scales
  sig=exp(log_sig); //Single estimated sigma value
  sd_logCG=sig*rhoCG; //Gill net observation error variance
  sd_logCT=sig*rhoCT; //Trap net observation error variance
  sd_logeffortG=sig*rhoEG; //Gill net catchability process error variance
  sd_logeffortT=sig*rhoET; //Trap net catchability process error variance
  sdSR=sig*rhoSR; //Recruitment random walk process error variance
  sd_sel=sig*rhosel;
  
  // std::cout<<"sig: "<<sig<<std::endl;
  // std::cout<<"sd_logCG: "<<sd_logCG<<std::endl;
  // std::cout<<"sd_logCT: "<<sd_logCT<<std::endl;
  // std::cout<<"sd_logeffortG: "<<sd_logeffortG<<std::endl;
  // std::cout<<"sd_logeffortT: "<<sd_logeffortT<<std::endl;
  // std::cout<<"sdSR: "<<sdSR<<std::endl;
  // std::cout<<"sd_sel: "<<sd_sel<<std::endl;

  //3. Selectivity
  //-----------------------------------------------
  // Calculate the double logistic selectivity using curves calculated from parameters
  G_p1=exp(logselG_p1);
  G_p2=exp(logselG_p2);
  T_p2=exp(logselT_p2);
  T_p1(0)=exp(logselT_p1);
  
  for(i=1;i<=ryears.size()-1;i++)
  {
    T_p1(i)=exp(log(T_p1(i-1))+logdevT_p1(i-1));
  }

  for(i=0;i<=ryears.size()-1;i++)
  {
    for(j=0;j<=ages.size()-1;j++)
    {
      numG(i,j)=1/(pow((2*3.1415926),0.5)*G_p1*latage(i,j))*exp(-1.0*(pow((log(latage(i,j))-G_p2),2)/(2*pow(G_p1,2))));
      numT(i,j)=1/(1+exp(-T_p2*(latage(i,j)-T_p1(i))));
    }
    denG(i)=1/(pow((2*3.1415926),0.5)*G_p1*exp(G_p2));
    denT(i)=1/(1+exp(-T_p2*(reflenT-T_p1(i))));

    selG.row(i)=numG.row(i)/(denG(i)+0.000001);
    selT.row(i)=numT.row(i)/(denT(i)+0.000001);
  }

  // std::cout<<"G_p1: "<<G_p1<<std::endl;
  // std::cout<<"T_p1: "<<T_p1<<std::endl;
  // std::cout<<"G_p2: "<<G_p2<<std::endl;
  // std::cout<<"T_p2: "<<T_p2<<std::endl;
  // std::cout<<"numG: "<<numG<<std::endl;
  // std::cout<<"numT: "<<numT<<std::endl;
  // std::cout<<"denG: "<<denG<<std::endl;
  // std::cout<<"denT: "<<denT<<std::endl;
  // std::cout<<"selG: "<<selG<<std::endl;
  // std::cout<<"selT: "<<selT<<std::endl;
  
  //4. Mortality
  //-----------------------------------------------
  M=exp(lnM); //Natural Mortality
  qT(0)=exp(log_qT);
  qG(0)=exp(log_qG);

  for (i=1;i<=ryears.size()-1;i++)
  {
    qT(i)=exp(log(qT(i-1))+effort_devsT(i-1));
    qG(i)=exp(log(qG(i-1))+effort_devsG(i-1));
  }

  // Calculate fishing mortality as catchability times effort
  for (i=0;i<=ryears.size()-1;i++)
  {
    FT.row(i)=qT(i)*effortT(i)*selT.row(i);
    FG.row(i)=qG(i)*effortG(i)*selG.row(i);
  }

  F=FG+FT; //Total fishing mortality is the sum of gill and trap mortalities

  pred_effT=1;
  pred_effG=1;

  for(i=0;i<=ryears.size()-1;i++)
  {
    for(j=0;j<=ages.size()-1;j++)
    {
      Z(i,j)=F(i,j)+M;
      S(i,j)=exp(-1.0*Z(i,j));
      A(i,j)=1.0-S(i,j);
      S_spawn(i,j)=exp(-sp_time*Z(i,j));
    }
  }
  MD=Z-F;

  // Estimate an average Z in the first 3 years to be used to estimate recruitment in years before start of data set
  AvgZFirstYears=(Z.row(0).array()+Z.row(1).array()+Z.row(2).array())/3;

  // std::cout<<"qT: "<<qT<<std::endl;
  // std::cout<<"qG: "<<qG<<std::endl;
  // std::cout<<"effort_devsT: "<<effort_devsT<<std::endl;
  // std::cout<<"effort_devsG: "<<effort_devsG<<std::endl;
  // std::cout<<"FT: "<<FT<<std::endl;
  // std::cout<<"FG: "<<FG<<std::endl;
  // std::cout<<"F: "<<F<<std::endl;
  // std::cout<<"Z: "<<Z<<std::endl;
  // std::cout<<"S: "<<S<<std::endl;
  // std::cout<<"A: "<<A<<std::endl;
  // std::cout<<"S_spawn: "<<S_spawn<<std::endl;
  // std::cout<<"MD: "<<MD<<std::endl;
  // std::cout<<"AvgZFirstYears: "<<AvgZFirstYears<<std::endl;

  //5. Numbers at age
  //-----------------------------------------------

  N(0,0)=exp(log_R0);
  for(i=1;i<=ryears.size()-1;i++)
  {
    N(i,0)=exp(log_R0+log_recdev(i-1));
  }

  for(j=1;j<=ages.size()-1-6;j++)
  {
    N(0,j)=exp(log_R0+log_recdev(j-1+(ryears.size()-1)));
  }
  
  //Set the initial population size on the log scale
  // for(i=0;i<=ryears.size()-1;i++)
  // {
  //   N(i,0)=exp(log_rec(i));
  // }
  // 
  // for(j=1;j<=ages.size()-1-6;j++)
  // {
  //   N(0,j)=exp(log_rec(j+ryears.size()-1));
  // }
  
  //Specify 0 for older ages in year 1, or else the model messes up
  N(0,ages.size()-1)=0.0;
  N(0,ages.size()-1-1)=0.0;
  N(0,ages.size()-1-2)=0.0;
  N(0,ages.size()-1-3)=0.0;
  N(0,ages.size()-1-4)=0.0;
  N(0,ages.size()-1-5)=0.0;

  //Populate the abundance matrix using previous abundance and survival
  for (i=0;i<ryears.size()-1;i++)
  {
    for (j=0;j<ages.size()-1-1;j++)
    {
      N(i+1,j+1)=N(i,j)*S(i,j);
    }
    N(i+1,ages.size()-1)=(S(i,ages.size()-1)*N(i,ages.size()-1))+(S(i,ages.size()-1-1)*N(i,ages.size()-1-1));
  }

  for (i=0; i<=ryears.size()-1; i++)
  {
    for (j=0; j<=ages.size()-1; j++)
    {
      N_spawn(i,j)=percent_female*(N(i,j)*S_spawn(i,j));
    }
  }
  
  // std::cout<<"N: "<<N<<std::endl;
  // std::cout<<"N_spawn: "<<N_spawn<<std::endl;

  //6. Biomass
  //-----------------------------------------------
  for (i=0; i<=ryears.size()-1; i++)
  {
    BIOMASSATAGE = pop_WA.row(i).array()*N.row(i).array();
    BIOMASS(i)=BIOMASSATAGE.sum();
  }

  // std::cout<<"BIOMASS: "<<BIOMASS<<std::endl;

  //7. Spawning
  //-----------------------------------------------
  for (i=0; i<=ryears.size()-1; i++)
  {
    SP_BIOAGE=wght_fac.row(i).array()*N.row(i).array();
    SP_BIO(i)=SP_BIOAGE.sum();
  }

  // std::cout<<"SP_BIO: "<<SP_BIO<<std::endl;

  //8. Eggs
  //-----------------------------------------------
  for (i=0; i<=ryears.size()-1; i++)
  {
    eggsatage=egg_fac.row(i).array()*N_spawn.row(i).array();
    eggs(i)=eggsatage.sum();
    PredR(i)=exp(lnalpha)*eggs(i)*exp(-1.0*exp(lnbeta)*eggs(i));
  }

  // std::cout<<"eggs: "<<eggs<<std::endl;
  // std::cout<<"PredR: "<<PredR<<std::endl;

  //9. Catch at age
  //-----------------------------------------------
  CAT=(FT.array()/Z.array())*(A.array())*N.array();
  CAG=(FG.array()/Z.array())*(A.array())*N.array();

  MDEAD=(MD.array()/Z.array())*(A.array())*N.array();
  TDEAD=MDEAD+CAT+CAG;

  for (i=0; i<=ryears.size()-1; i++)
  {
    CT(i)=CAT.row(i).sum();
    PAT.row(i)=CAT.row(i)/(CT(i)+0.001);
    CG(i)=CAG.row(i).sum();
    PAG.row(i)=CAG.row(i)/(CG(i)+0.001);
  }

  // std::cout<<"CAT: "<<CAT<<std::endl;
  // std::cout<<"CAG: "<<CAG<<std::endl;
  // std::cout<<"MDEAD: "<<MDEAD<<std::endl;
  // std::cout<<"TDEAD: "<<TDEAD<<std::endl;
  // std::cout<<"CT: "<<CT<<std::endl;
  // std::cout<<"PAT: "<<PAT<<std::endl;
  // std::cout<<"CG: "<<CG<<std::endl;
  // std::cout<<"PAG: "<<PAG<<std::endl;

  //10. Select
  //-----------------------------------------------
  selectT=selT.row(ryears.size()-1);
  selectG=selG.row(ryears.size()-1);

  // std::cout<<"selectT: "<<selectT<<std::endl;
  // std::cout<<"selectG: "<<selectG<<std::endl;

  //11. Predicted biomass
  //-----------------------------------------------
  BT=mnwgtT.array()*CT.array();
  BG=mnwgtG.array()*CG.array();

  // std::cout<<"BT: "<<BT<<std::endl;
  // std::cout<<"BG: "<<BG<<std::endl;

  //11. Residuals
  //-----------------------------------------------
  // Total Catch
  residCT.array()=(log(obs_CT.array()+0.001)-log(CT.array()+0.001))/sd_logCT;
  residCG.array()=(log(obs_CG.array()+0.001)-log(CG.array()+0.001))/sd_logCG;

  // Catch Composition
  prodPAT.array()=PAT.array()*(1.0-PAT.array());
  prodPAG.array()=PAG.array()*(1.0-PAG.array());
  Ntrap=N_SampT+0.001;
  Ngill=N_SampG+0.001;

  for (i=0; i<=ryears.size()-1; i++)
  {
    for(j=0;j<=ages.size()-1;j++)
    {
      denomT(i,j)=(prodPAT(i,j)+0.000001)/(Ntrap(i)+0.000001)+0.000001;
      denomT(i,j)=pow(denomT(i,j),0.5);
      denomG(i,j)=(prodPAG(i,j)+0.000001)/(Ngill(i)+0.000001)+0.000001;
      denomG(i,j)=pow(denomG(i,j),0.5);
    }
  }
  residPAT=(obs_PAT.array()-PAT.array())/denomT.array();
  residPAG=(obs_PAG.array()-PAG.array())/denomG.array();

  for(i=0;i<=ryears.size()-2;i++)
  {
      resid_effG(i)=effort_devsG(i)/(sd_logeffortG+0.000001);
      resid_effT(i)=effort_devsT(i)/(sd_logeffortT+0.000001);
  }

  // std::cout<<"residCT: "<<residCT<<std::endl;
  // std::cout<<"residCG: "<<residCG<<std::endl;
  // std::cout<<"prodPAT: "<<prodPAT<<std::endl;
  // std::cout<<"prodPAG: "<<prodPAG<<std::endl;
  // std::cout<<"Ntrap: "<<Ntrap<<std::endl;
  // std::cout<<"Ngill: "<<Ngill<<std::endl;
  // 
  // std::cout<<"denomT: "<<denomT<<std::endl;
  // std::cout<<"denomG: "<<denomG<<std::endl;
  // std::cout<<"residPAT: "<<residPAT<<std::endl;
  // std::cout<<"residPAG: "<<residPAG<<std::endl;
  // std::cout<<"resid_effG: "<<resid_effG<<std::endl;
  // std::cout<<"resid_effT: "<<resid_effT<<std::endl;

  //13. Projections
  //-----------------------------------------------
  Fproj=(F.row(ryears.size()-1).array()+F.row(ryears.size()-1-1).array()+F.row(ryears.size()-1-2).array())/3;
  FTproj=(FT.row(ryears.size()-1).array()+FT.row(ryears.size()-1-1).array()+FT.row(ryears.size()-1-2).array())/3;
  FGproj=(FG.row(ryears.size()-1).array()+FG.row(ryears.size()-1-1).array()+FG.row(ryears.size()-1-2).array())/3;
  Zproj=(Z.row(ryears.size()-1).array()+Z.row(ryears.size()-1-1).array()+Z.row(ryears.size()-1-2).array())/3;
  Sproj=exp(-Zproj);
  Mproj=MD.row(ryears.size()-1);
  Sproj_BASE=exp(-Mproj);

  if (targ_age>fage)
  {
    Sproj_T.segment(0,targ_age-fage)=Sproj_BASE.segment(0,targ_age-fage);
  }
  Sproj_T.segment(targ_age-fage,lage-targ_age+1)=0.35;

  wfacproj=wght_fac.row(ryears.size()-1); //extracts maturity*weight at spawn from lyear
  watageproj=harv_WA.row(ryears.size()-1); //extracts watage vector from lyear

  YPR=0.0;
  Psurv(0)=1.0;
  Psurv_BASE(0)=1.0;
  Psurv_T=1.0;
  SSBR=0.0;
  SSBR_T=0.0;
  SSBR_BASE=0.0;

  for (i=0; i<=ages.size()-1; i++)
  {
    if (i<ages.size()-1)
    {
      Psurv(i+1)=Psurv(i)*Sproj(i);
      Psurv_BASE(i+1)=Psurv_BASE(i)*Sproj_BASE(i);
      Psurv_T(i+1)=Psurv_T(i)*Sproj_T(i);
    }
    SSBR+=percent_female*wfacproj(i)*Psurv(i)*exp(-(sp_time)*Zproj(i));
    SSBR_BASE+=percent_female*wfacproj(i)*Psurv_BASE(i)*exp(-(sp_time)*Mproj(i));
    if (i<(targ_age-fage))
    {
      SSBR_T+=percent_female*wfacproj(i)*Psurv_T(i)*exp(-(sp_time)*Mproj(i));
    }
    if (i>=(targ_age-fage))
    {
      SSBR_T+=percent_female*wfacproj(i)*Psurv_T(i)*exp(log(Sproj_T(ages.size()-1))*(sp_time));
    }
    YPR+=(Fproj(i)/Zproj(i))*(1.0-Sproj(i))*Psurv(i)*watageproj(i);
  }

  YPR+=(Fproj(ages.size()-1)/Zproj(ages.size()-1))*(1-Sproj(ages.size()-1))*Psurv(ages.size()-1)*watageproj(ages.size()-1)*(1/(1-Sproj(ages.size()-1)));
  SSBR+=percent_female*wfacproj(ages.size()-1)*Psurv(ages.size()-1)*exp(-(sp_time)*Zproj(ages.size()-1))*(1/(1-Sproj(ages.size()-1)) -1.0);
  SSBR_BASE+=percent_female*wfacproj(ages.size()-1)*Psurv_BASE(ages.size()-1)*exp(-(sp_time)*Mproj(ages.size()-1))*(1/(1-Sproj_BASE(ages.size()-1)) -1.0);
  SSBR_T+=percent_female*wfacproj(ages.size()-1)*Psurv_T(ages.size()-1)*exp(log(Sproj_T(ages.size()-1))*(sp_time))*(1/(1-Sproj_T(ages.size()-1)) -1.0);

  SPR=(SSBR+0.00000001)/(SSBR_BASE+0.00000001);
  SPR_T=(SSBR_T+0.00000001)/(SSBR_BASE+0.00000001);
  SSBR_RATIO_ET=(SSBR+0.0000001)/(SSBR_T+0.0000001);

  RecSum=0;
  for (i=ryears.size()-1-9;i<=ryears.size()-1;i++)
  {
    RecSum+=N(i,0);
  }
  Rec_avg=RecSum/10;

  // std::cout<<"Fproj: "<<Fproj<<std::endl;
  // std::cout<<"FTproj: "<<FTproj<<std::endl;
  // std::cout<<"FGproj: "<<FGproj<<std::endl;
  // std::cout<<"Zproj: "<<Zproj<<std::endl;
  // std::cout<<"Sproj: "<<Sproj<<std::endl;
  // std::cout<<"Mproj: "<<Mproj<<std::endl;
  // std::cout<<"Sproj_BASE: "<<Sproj_BASE<<std::endl;
  // 
  // std::cout<<"Sproj_T: "<<Sproj_T<<std::endl;
  // std::cout<<"wfacproj: "<<wfacproj<<std::endl;
  // std::cout<<"watageproj: "<<watageproj<<std::endl;
  // 
  // std::cout<<"YPR: "<<YPR<<std::endl;
  // std::cout<<"Psurv: "<<Psurv<<std::endl;
  // std::cout<<"Psurv_BASE: "<<Psurv_BASE<<std::endl;
  // std::cout<<"Psurv_T: "<<Psurv_T<<std::endl;
  // std::cout<<"SSBR: "<<SSBR<<std::endl;
  // std::cout<<"SSBR_T: "<<SSBR_T<<std::endl;
  // std::cout<<"SSBR_BASE: "<<SSBR_BASE<<std::endl;
  // std::cout<<"SPR: "<<SPR<<std::endl;
  // std::cout<<"SPR_T: "<<SPR_T<<std::endl;
  // std::cout<<"SSBR_RATIO_ET: "<<SSBR_RATIO_ET<<std::endl;
  // 
  // std::cout<<"RecSum: "<<RecSum<<std::endl;
  // std::cout<<"Rec_avg: "<<Rec_avg<<std::endl;


  //14. TAC
  //-----------------------------------------------

  //15. Report Calcs
  //-----------------------------------------------
  for(i=0;i<ryears.size();i++)
  {
    RecVec(i)=N(i,0);
  }

  AvgBiomasslb=0;
  AvgSPbiomasslb=0;
  for (i=ryears.size()-1-9;i<=ryears.size()-1;i++)
  {
    AvgBiomasslb+=BIOMASS(i);
    AvgSPbiomasslb+=SP_BIO(i);
  }

  AvgF_gill=FGproj.sum()/FGproj.size();
  AvgF_trap = FTproj.sum()/FTproj.size();
  AvgZ = Zproj.sum()/Zproj.size();
  AvgR = Rec_avg;
  AvgBiomasslb = AvgBiomasslb/10*2.2046;
  AvgSPbiomasslb = AvgSPbiomasslb/10*2.2046;
  SSBRlb = SSBR *2.2046;
  SSBR_Tlb = SSBR_T *2.2046;
  SSBR_BASElb = SSBR_BASE *2.2046;
  YPRlb = YPR*2.2046;
  avg_F=F.sum()/double(F.size());

  // std::cout<<"RecVec: "<<RecVec<<std::endl;
  // std::cout<<"AvgBiomasslb: "<<AvgBiomasslb<<std::endl;
  // std::cout<<"AvgSPbiomasslb: "<<AvgSPbiomasslb<<std::endl;
  // std::cout<<"AvgF_gill: "<<AvgF_gill<<std::endl;
  // std::cout<<"AvgF_trap: "<<AvgF_trap<<std::endl;
  // std::cout<<"AvgZ: "<<AvgZ<<std::endl;
  // std::cout<<"AvgR: "<<AvgR<<std::endl;
  // std::cout<<"SSBRlb: "<<SSBRlb<<std::endl;
  // std::cout<<"SSBR_Tlb: "<<SSBR_Tlb<<std::endl;
  // std::cout<<"SSBR_BASElb: "<<SSBR_BASElb<<std::endl;
  // std::cout<<"YPRlb: "<<YPRlb<<std::endl;
  // std::cout<<"avg_F: "<<avg_F<<std::endl;

  //16. Annual Average Mortality
  //-----------------------------------------------
  Aproj=(A.row(ryears.size()-1).array()+A.row(ryears.size()-1-1).array()+A.row(ryears.size()-1-2).array())/3.0;
  avg_A=(Aproj(ages.size()-1-1)+Aproj(ages.size()-1-2)+Aproj(ages.size()-1-3)+Aproj(ages.size()-1-4)+Aproj(ages.size()-1-5))/5;

  // std::cout<<"Aproj: "<<Aproj<<std::endl;
  // std::cout<<"avg_A: "<<avg_A<<std::endl;

  //17. Objective Functions
  //-----------------------------------------------
  NLP= 0.0; //Components related to priors and process error
  NLL= 0.0; //Components related to fit to data and observation error

  //Constraining the deviations around the estimated recruitment
  // NLP+=1000*log_relpop.sum();
  // std::cout<<"NLP: "<<NLP<<std::endl;

  //Penalty for average fishing mortality deviating from 0.2
  // NLP+=1000*pow(log((avg_F+0.000001)/0.2),2.0);
  // std::cout<<"NLP: "<<NLP<<std::endl;

  //Penalty for Natural Mortality M deviating from prior
  NLP+= 0.5/pow(sdM,2.0)*pow(lnmedM-lnM,2.0)+1.0*log(sdM);
  // std::cout<<"NLP M: "<<NLP<<std::endl;

  //Process errors around the trap net catchability random walk
  // NLP-=sum(dnorm(effort_devsT,0,sd_logeffortT,true));
  for(i=0;i<=ryears.size()-2;i++)
  {
    NLP+=0.5/pow(sd_logeffortT,2.0)*pow(effort_devsT(i),2.0)+log(sd_logeffortT);
  }
  // std::cout<<"NLP M and Trap catchability: "<<NLP<<std::endl;

  //Process errors around the gill net catchability random walk
  // NLP-=sum(dnorm(effort_devsG,0,sd_logeffortG,true));
  for(i=0;i<=ryears.size()-2;i++)
  {
    NLP+=0.5/pow(sd_logeffortG,2.0)*pow(effort_devsG(i),2.0)+log(sd_logeffortG);
  }
  // std::cout<<"NLP M and Trap and Gill catchability: "<<NLP<<std::endl;

  //Process errors around the trap net selectivity parameter
  // NLP-=sum(dnorm(logdevT_p1,0,sd_sel,true));
  for(i=0;i<=ryears.size()-2;i++)
  {
    NLP+=0.5/pow(sd_sel,2.0)*pow(logdevT_p1(i),2.0)+log(sd_sel);
  }
  // std::cout<<"NLP M and Trap and Gill catchability and selectivity: "<<NLP<<std::endl;
  

  SSQ_SR=0.0;
  Nrec=0;
  for(i=0;i<=ryears.size()-1-5;i++)
  {
    SSQ_SR+=pow(log((PredR(i)+0.0001)/(N(i+5,0)+0.0001)),2.0);
    Nrec=Nrec+1;
  }
  // std::cout<<"SSQ_SR: "<<SSQ_SR<<std::endl;
  // std::cout<<"Nrec: "<<Nrec<<std::endl;
  
  NLP+=(0.5/pow(sdSR,2.0))*SSQ_SR+Nrec*log(sdSR);
  // std::cout<<"NLP M and Trap and Gill catchability and selectivity and recruitment: "<<NLP<<std::endl;


  //NLL CALCULATION BASED ON SEPARABLE CATCH AND COMPOSITION
  //Observation error around total catch is normally distributed
  for(i=0;i<=ryears.size()-1;i++)
  {
    NLL+=0.5/pow(sd_logCT,2.0)*pow(log((.01+obs_CT(i))/(.01+CT(i))),2.0) + log(sd_logCT);
  }
  // std::cout<<"NLL Trap Catch Total: "<<NLL<<std::endl;

  for(i=0;i<=ryears.size()-1;i++)
  {
    NLL+=0.5/pow(sd_logCG,2.0)*pow(log((.01+obs_CG(i))/(.01+CG(i))),2.0) + log(sd_logCG);
  }
  // std::cout<<"NLL Trap and Gill Catch Total: "<<NLL<<std::endl;

  //Observation error around age composition in catch is multinomially distributed
  for(i=0;i<=ryears.size()-1;i++)
  {
    for(j=0;j<=ages.size()-1;j++)
    {
      NLL-= N_SampT(i)*obs_PAT(i,j)*log(.0001+PAT(i,j));
      NLL-= N_SampG(i)*obs_PAG(i,j)*log(.0001+PAG(i,j));

      //NLL-=NtildeT(i)*obs_PAT(i,j)*log(0.0001+PAT(i,j));
      //NLL-=NtildeG(i)*obs_PAG(i,j)*log(0.0001+PAG(i,j));
    }
  }
  // std::cout<<"NLL Trap and Gill Catch and Composition Total: "<<NLL<<std::endl;
  
  
  f=NLL+NLP;
  // f=0.0;
  // std::cout<<"f: "<<f<<std::endl;

  
  //Print out during optimization section
  //CppAD::PrintFor("Currently lnM=", lnM);
  //CppAD::PrintFor("Currently f=", f);

  // V. REPORT SECTION
  // ---------------------------------------------------------------------------------------------
  // Reports:
  REPORT(N);
  REPORT(N_spawn);
  REPORT(F);
  REPORT(FG);
  REPORT(FT);
  REPORT(M);
  REPORT(Z);
  
  REPORT(CT);
  REPORT(CG);
  
  REPORT(selG);
  REPORT(selT);
  
  // AD Reports:
  ADREPORT(N);
  ADREPORT(N_spawn);
  ADREPORT(F);
  ADREPORT(FG);
  ADREPORT(FT);
  ADREPORT(M);

  ADREPORT(CT);
  ADREPORT(CG);
  ADREPORT(PAT);
  ADREPORT(PAG);
  ADREPORT(NtildeT);
  ADREPORT(NtildeG);
  ADREPORT(N_SampT);
  ADREPORT(N_SampG);
  ADREPORT(BIOMASS);
  ADREPORT(SP_BIO);
  ADREPORT(qT);
  ADREPORT(qG);

  ADREPORT(SSBR);
  ADREPORT(SPR);
  ADREPORT(YPR);
  ADREPORT(AvgF_gill);
  ADREPORT(AvgF_trap);
  ADREPORT(Rec_avg);
  ADREPORT(AvgZ);

  ADREPORT(residCT);
  ADREPORT(residCG);
  ADREPORT(residPAT);
  ADREPORT(residPAG);
  ADREPORT(resid_effT);
  ADREPORT(resid_effG);

  //VI. SIMULATION SECTION
  //---------------------------------------------------------------------------------------------
  // SIMULATE{
  //   Type sd_logCT_sim = sd_logCT;
  //   Type sd_logCG_sim = sd_logCG;
  //   
  //   //TRAP NET TOTAL HARVEST BY WEIGHT
  //   vector<Type> CT_obs_sim = exp(rnorm(log(CT),sd_logCT_sim));
  //   // vector<Type> CT_obs_sim = exp(log(obs_CT)); //"Simulate" the real data, to check if I can recover the same estimates from identical data
  //   // vector<Type> CT_obs_sim = exp(log(CT)); //"Simulate" the predicted data without error
  //   vector<Type> harv_wgtT_sim = CT_obs_sim.array() * mnwgtT.array() * Tharv_adjust.array();
  //   
  //   //GILL NET TOTAL HARVEST BY WEIGHT
  //   vector<Type> CG_obs_sim = exp(rnorm(log(CG),sd_logCG_sim));
  //   // vector<Type> CG_obs_sim = exp(log(obs_CG)); //"Simulate" the real data, to check if I can recover the same estimates from identical data
  //   // vector<Type> CG_obs_sim = exp(log(CG)); //"Simulate" the predicted data without error
  //   vector<Type> harv_wgtG_sim = CG_obs_sim.array() * mnwgtG.array() * Gharv_adjust.array();
  // 
  //   
  //   matrix<Type> obs_PAT_sim(ryears.size(),ages.size());
  //   matrix<Type> obs_PAG_sim(ryears.size(),ages.size());
  //   
  //   //Simulate proportions at age using effective sample size (Ntrap/Ngill)
  //   
  //   for(i=0;i<=ryears.size()-1;i++)
  //   {
  //     obs_PAT_sim.row(i) = rmultinom(Ntrap(i),vector<Type>(PAT.row(i)))/Ntrap(i);
  //     obs_PAG_sim.row(i) = rmultinom(Ngill(i),vector<Type>(PAG.row(i)))/Ngill(i);
  //     
  //     // obs_PAT_sim.row(i) =vector<Type>(obs_PAT.row(i)); //"Simulate" the real data, to check if I can recover the same estimates from identical data
  //     // obs_PAG_sim.row(i) =vector<Type>(obs_PAG.row(i));
  //     // obs_PAT_sim.row(i) =vector<Type>(PAT.row(i)); //"Simulate" the predicted data without error
  //     // obs_PAG_sim.row(i) =vector<Type>(PAG.row(i));
  //   }
  //   REPORT(harv_wgtT_sim);
  //   REPORT(harv_wgtG_sim);
  //   REPORT(obs_PAT_sim);
  //   REPORT(obs_PAG_sim);
  // }
  
  return f;
}
