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
  //DATA INPUTS
  DATA_INTEGER(fyear); //First year
  DATA_INTEGER(lyear); //Last year
  DATA_INTEGER(lryear); //Last year in retrospective analysis
  DATA_INTEGER(fage); //First age
  DATA_INTEGER(lage); //Last age
  DATA_INTEGER(targ_age); //Target age for target mortality estimation
  DATA_INTEGER(asymptoteage); //Age at which the trap net asymptotes
  
  DATA_VECTOR(years); //Vector of years
  DATA_VECTOR(ryears); //Vector of years in retrospective analysis
  DATA_VECTOR(ages); //Vector of ages
  DATA_VECTOR(asymptoteages); //Vector of ages up to where the trap net selctivity asymptotes

  DATA_SCALAR(sp_time); //Spawning time (fraction showing percent of the year passes before spawn)
  DATA_MATRIX(in_watage); //Weight at age
  DATA_MATRIX(in_latage); //Length at age
  DATA_MATRIX(in_mat); //Maturity schedule at age and year

  DATA_SCALAR(lnmedM);
  DATA_SCALAR(sdM); //Prior standard deviation of natural mortality

  //Trap net data starts here
  DATA_MATRIX(in_obs_PAT); //Observed proportion at age
  DATA_VECTOR(in_NtildeT); //Number of fish sampled in trap net bio samps
  
  DATA_VECTOR(in_N_SampT); //Effective sample size

  DATA_VECTOR(in_harv_wgtT); //Trap net harvest biomass
  DATA_VECTOR(in_mnwgtT); //Mean weight of individual in catch
  DATA_VECTOR(in_effortT); //Observed effort by year

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
  
  //std::cout<<"in_mnwgtT: "<<in_mnwgtT<<std::endl;

  //DERIVED QUANTITIES FROM DATA SECTION
  
  vector<Type> obs_CT; //Observed catch trap net
  vector<Type> obs_CG; //Observed catch gill net

  matrix<Type> pop_WA(ryears.size(),ages.size()); //Weight at age, beginning of year
 
  matrix<Type> spawn_WA(ryears.size(),ages.size()); //Weight at age, spawning time
  matrix<Type> harv_WA(ryears.size(),ages.size()); //Weight at age, harvest time
  vector<Type> eggs_per_female; //Calculated eggs per female

  matrix<Type> G(ryears.size(),ages.size()); //Instantaneous Growth rate
  
  matrix<Type> watage(ryears.size(),ages.size()+1); //Weight at age
  matrix<Type> latage(ryears.size(),ages.size()); //Length at age
  matrix<Type> mat(ryears.size(),ages.size()); //Maturity schedule

  //Trap net values
  matrix<Type> obs_PAT(ryears.size(),ages.size()); //Observed proportion at age, trap net
  vector<Type> NtildeT(ryears.size()); //Number of fish sampled in trap net bio samps
  vector<Type> N_SampT(ryears.size()); //Effective sample size trap net
  vector<Type> harv_wgtT(ryears.size()); //Trap net harvest biomass
  vector<Type> mnwgtT(ryears.size()); //Mean weight of individual in catch
  vector<Type> effortT(ryears.size()); //Observed effort by year

  //Gill net values
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
  
  //Counter variables for for loops
  int i; 
  int j; 
  
  //Necessary for doing multivariate normal distribution stuff
  using namespace density;

  //PARAMETER DECLARATION
  PARAMETER(log_sig); //Sigma value, used to convert rhos to SD just for obs error
  PARAMETER(log_sdSR); //Log scale standard deviation around recruitment
  
  PARAMETER(log_sd_logeffortT); //Log scale standard deviation around age- year- specific trap catchability
  PARAMETER(log_sd_logeffortG); //Log scale standard deviation around age- year- specific gill catchability
  PARAMETER(logit_rhoalphaT); //Logit scale estimated inter-age correlation of catchability in trap net
  PARAMETER(logit_rhoalphaG); //Logit scale estimated inter-age correlation of catchability in gill net
  
  PARAMETER(lnM); //Log scale natural mortality
  PARAMETER_MATRIX(log_qT); //Log scale age- and year- specific catchability, trap net
  PARAMETER_MATRIX(log_qG); //Log scale age- and year- specific catchability, gill net

  PARAMETER_VECTOR(log_rec); //Log scale recruitment values
  PARAMETER(lnZavg);
  
  
  //Two extra parameters per gear type if you're doing a cLGCP likelihood model
  //A few extra derived values to make it work too
  // PARAMETER_VECTOR(log_std_log_gamT);
  // PARAMETER_VECTOR(log_std_log_gamG);
  // PARAMETER_MATRIX(log_gamT);
  // PARAMETER_MATRIX(log_gamG);
  // 
  // vector<Type> std_log_gamT(ages.size());
  // vector<Type> std_log_gamG(ages.size());
  // vector<Type> giT(ages.size()); //Vectors to hold estimated gamma values
  // vector<Type> giG(ages.size());
  // vector<Type> piT(ages.size()); //Vectors to hold adjusted estimated proportions at age data
  // vector<Type> piG(ages.size());
  
  
  
  // std::cout<<"log_sig: "<<log_sig<<std::endl;
  // std::cout<<"log_sdSR: "<<log_sdSR<<std::endl;
  // std::cout<<"log_sd_logeffortT: "<<log_sd_logeffortT<<std::endl;
  // std::cout<<"log_sd_logeffortG: "<<log_sd_logeffortG<<std::endl;
  // std::cout<<"logit_rhoalphaT: "<<logit_rhoalphaT<<std::endl;
  // std::cout<<"logit_rhoalphaG: "<<logit_rhoalphaG<<std::endl;
  // std::cout<<"lnM: "<<lnM<<std::endl;
  // std::cout<<"log_qT: "<<log_qT<<std::endl;
  // std::cout<<"log_qG: "<<log_qG<<std::endl;
  // std::cout<<"log_rec: "<<log_rec<<std::endl;
  
  //DERIVED QUANTITIES FROM PARAM SECTION
  vector<Type> log_initpop(ryears.size()+ages.size()-1-6);
  vector<Type> log_rec_devs(log_initpop.size()-1); //Recruitment deviations in random walk
  int counter; //counting variable that makes filling init pop easier
  matrix<Type> effort_devsT(ryears.size()-1,asymptoteages.size()); //Random walk error between trap net catchability
  matrix<Type> effort_devsG(ryears.size()-1,ages.size()); //Random walk error between gill net catchability
  matrix<Type> qT(ryears.size(),ages.size()); //Age- and year- specific catchability, trap net
  matrix<Type> qG(ryears.size(),ages.size()); //Age- and year- specific catchability, gill net
  
  Type sig;
  Type sd_logCT; //SD of Observation error on trap net catch
  Type sd_logCG; //SD of Observation error on gill net catch
  vector<Type> sd_logeffortT(asymptoteages.size()); //SD of Process error on trap net catchability
  vector<Type> sd_logeffortG(ages.size()); //SD of Process error on gill net catchability
  Type sdSR; //SD of Process error around random walk recruitment
  Type rhoalphaT; //Inter-age correlation of catchability in trap net
  Type rhoalphaG; //Inter-age correlation of catchability in gill net
  matrix<Type> cormatT(asymptoteages.size(),asymptoteages.size()); //Correlation matrix of age-specific catchability, trap net
  matrix<Type> cormatG(ages.size(),ages.size()); //Correlation matrix of age-specific catchability, gill net
  matrix<Type> sd_logeffortT_diag(asymptoteages.size(),asymptoteages.size()); //Matrix with SD process error values on diagonal
  matrix<Type> sd_logeffortG_diag(ages.size(),ages.size());
  matrix<Type> varcovT; //Variance-Covariance matrix made from sd and correlation of catchability random walk
  matrix<Type> varcovG; //Variance-Covariance matrix made from sd and correlation of catchability random walk
  
  matrix<Type> F;
  matrix<Type> FT(ryears.size(),ages.size());
  matrix<Type> FG(ryears.size(),ages.size());
  
  matrix<Type> Z(ryears.size(),ages.size());
  matrix<Type> A(ryears.size(),ages.size());
  matrix<Type> S(ryears.size(),ages.size());
  matrix<Type> S_spawn(ryears.size(),ages.size());
  
  matrix<Type> N(ryears.size(),ages.size());
  matrix<Type> N_spawn(ryears.size(),ages.size());
  
  matrix<Type> CAT;
  matrix<Type> CAG;
 
  matrix<Type> PAT(ryears.size(),ages.size());
  matrix<Type> PAG(ryears.size(),ages.size());

  matrix<Type> MD;
  matrix<Type> MDEAD;
  matrix<Type> TDEAD;
  
  vector<Type> AvgZFirstYears;

  Type M;
  
  vector<Type> CT(ryears.size());
  vector<Type> CG(ryears.size());

  Type NLL;
  Type NLP;
  Type f;

  Type avg_F;
  Type avg_A;
  vector<Type> SP_BIO(ryears.size());
  vector<Type> SP_BIOAGE;
  vector<Type> BIOMASS(ryears.size());
  vector<Type> BIOMASSATAGE;
  
  Type SSBR;
  Type SSBR_BASE;
  Type SPR;
  Type YPR;
  Type SSBR_T;
  Type SPR_T;
  Type SSBR_RATIO_ET;
  
  Type AvgF_gill;
  Type AvgF_trap;
  Type AvgZ;
  Type AvgR;
  Type AvgBiomasslb;
  Type AvgSPbiomasslb;
  Type SSBRlb;
  Type SSBR_BASElb;
  Type SSBR_Tlb;
  Type YPRlb;

  vector<Type> Aproj;
  vector<Type> Fproj;
  vector<Type> FTproj;
  vector<Type> FGproj;
  vector<Type> Zproj;
  vector<Type> Mproj;
  vector<Type> Sproj;
  vector<Type> Sproj_BASE;
  vector<Type> Sproj_T(ages.size());

  vector<Type> Psurv(ages.size());
  vector<Type> Psurv_BASE(ages.size());
  vector<Type> Psurv_T(ages.size());

  vector<Type> wfacproj;
  vector<Type> watageproj;
  
  vector<Type> eggs(ryears.size());
  vector<Type> eggsatage;
  matrix<Type> egg_fac;
  matrix<Type> wght_fac;
  
  vector<Type> BT;
  vector<Type> BG;
  
  vector<Type> natmort1;
  
  vector<Type> residCT;
  vector<Type> residCG;
  matrix<Type> residPAT;
  matrix<Type> residPAG;
  matrix<Type> denomT(ryears.size(),ages.size());
  matrix<Type> denomG(ryears.size(),ages.size());
  
  vector<Type> selectT;
  vector<Type> selectG;
  matrix<Type> resid_effT(ryears.size()-1,asymptoteages.size());
  matrix<Type> resid_effG(ryears.size()-1,ages.size());
  vector<Type> Ntrap;
  vector<Type> Ngill;
  
  matrix<Type> prodPAT(ryears.size(),ages.size());
  matrix<Type> prodPAG(ryears.size(),ages.size());

  vector<Type> RecVec(ryears.size());

  Type RecSum;
  Type Rec_avg;

  //SETTING INTITAL VALUES FOR DATA AND PARAMETERS

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
  // std::cout<<"watage: "<<watage<<std::endl;
  // std::cout<<"Tharv_adjust: "<<Tharv_adjust<<std::endl;
  
  effortG = effort_adjust.array()*effortG.array();
  //std::cout<<"EffortG: "<<effortG<<std::endl;
  
  obs_CT = (harv_wgtT/mnwgtT)/Tharv_adjust;
  obs_CG = (harv_wgtG/mnwgtG)/Gharv_adjust;

  
  for (i=0; i<=ryears.size()-2; i++)
  //for(i=0; i<=4; i++)
  {
    for (j=0; j<=ages.size()-2; j++)
	//for(j=0; j<=4; j++)
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

  //std::cout<<"G: "<<G<<std::endl;

  
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
 
  eggs_per_female=eggs_per_kg*spawn_WA.row(ryears.size()-1);
  
  wght_fac=mat.array()*spawn_WA.array();

  egg_fac=eggs_per_kg*wght_fac;
  //std::cout<<"egg_fac: "<<egg_fac<<std::endl;

  
  //FUNCTIONS AND CALCULATIONS
  //1. Get variances and correlations
  sig=exp(log_sig);
  sd_logCG=sig;
  sd_logCT=sig;
  sd_logeffortG.fill(exp(log_sd_logeffortG));
  sd_logeffortT.fill(exp(log_sd_logeffortT));
  // sd_logeffortG.fill(sig*rhoEG);
  // sd_logeffortT.fill(sig*rhoET);
  sdSR=exp(log_sdSR);
  
  rhoalphaT=exp(logit_rhoalphaT)/(1+exp(logit_rhoalphaT));
  rhoalphaG=exp(logit_rhoalphaG)/(1+exp(logit_rhoalphaG));
  
  // for (j=0; j<=ages.size()-1; j++){
  //   std_log_gamT(j)=exp(log_std_log_gamT(j));
  //   std_log_gamG(j)=exp(log_std_log_gamG(j));
  // }
  //3. Get mortality
  
  // std::cout<<"log_qT: "<<log_qT<<std::endl;
  // std::cout<<"log_qT.size()-1: "<<log_qT.size()-1<<std::endl;
  // std::cout<<"ages: "<<ages<<std::endl;
  // std::cout<<"ages.size(): "<<ages.size()<<std::endl;
  
  // Back-transform log scale parameters
  M=exp(lnM);
  for(i=0;i<=ryears.size()-1;i++)
  {
    for(j=0;j<=ages.size()-1;j++)
    {
      if(ages(j)<=asymptoteage)
      {
        qT(i,j)=exp(log_qT(i,j));
      }
      else
      {
        qT(i,j)=qT(i,(j-1));
      }
    }
  }
  // qT.array()=exp(log_qT.array());
  qG.array()=exp(log_qG.array());

  // std::cout<<"qT.array(): "<<qT.array()<<std::endl;
  // std::cout<<"qG.array(): "<<qG.array()<<std::endl;

  // Calculate the catchability deviations (effort_devs) by taking the difference between years
  for (i=1;i<=ryears.size()-1;i++)
  {
    effort_devsT.row(i-1)=log_qT.row(i)-log_qT.row(i-1);
    effort_devsG.row(i-1)=log_qG.row(i)-log_qG.row(i-1);
  }

  // std::cout<<"effort_devsT: "<<effort_devsT<<std::endl;
  // std::cout<<"effort_devsG: "<<effort_devsG<<std::endl;

  // Calculate fishing mortality as catchability times effort
  for (i=0;i<=ryears.size()-1;i++)
  {
    FT.row(i)=qT.row(i)*effortT(i);
    FG.row(i)=qG.row(i)*effortG(i);
  }

  //std::cout<<"FT: "<<FT<<std::endl;
  //std::cout<<"FG: "<<FG<<std::endl;

  F=FG+FT;

  // std::cout<<"F: "<<F<<std::endl;

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
  natmort1=MD.row(0);

  // Estimate an average Z in the first 3 years to be used to estimate recruitment in years before start of data set
  AvgZFirstYears=(Z.row(0).array()+Z.row(1).array()+Z.row(2).array())/3;
  // std::cout<<"AvgZFirstYears: "<<AvgZFirstYears<<std::endl;

  //4. Specify numbers at age from log_rec and calculate deviations

  counter=0;
  //Going backwards, calculate the init population at age in the first year
  //and the corresponding recruitment process error (while adding in mortality)
  for(j=ages.size()-1-6;j>=0;j--)
  {
   N(0,j)=exp(log_rec(counter));
   // if(j>0) log_rec_devs(counter)=log_rec(counter+1)-log_rec(counter)-AvgZFirstYears(j); //Add back in some specified mortality (in this case,0.5)
   if(j>0) log_rec_devs(counter)=log_rec(counter+1)-log_rec(counter)-lnZavg; //Add back in some specified mortality (in this case,0.5)
   counter++;
  }

  // std::cout<<"N first: "<<N<<std::endl;
  // std::cout<<"log_rec_devs first: "<<log_rec_devs<<std::endl;

  //Going forwards, calculate the recruitment in each year from the log_rec parameter
  //and the corresponding recruitment process error
  for (i=1;i<=ryears.size()-1;i++)
  {
    N(i,0)=exp(log_rec(counter));
    log_rec_devs(counter-1)=log_rec(counter)-log_rec(counter-1);
    counter++;
  }

  // std::cout<<"N second: "<<N<<std::endl;
  // std::cout<<"log_rec_devs second: "<<log_rec_devs<<std::endl;

  //Specify 0 for older ages in year 1, or else the model messes up
  N(0,ages.size()-1)=0.0;
  N(0,ages.size()-1-1)=0.0;
  N(0,ages.size()-1-2)=0.0;
  N(0,ages.size()-1-3)=0.0;
  N(0,ages.size()-1-4)=0.0;
  N(0,ages.size()-1-5)=0.0;

  //Populate the abundance matrix
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

  //std::cout<<"N_spawn: "<<N_spawn<<std::endl;

  //5. Get biomass
  //std::cout<<"pop_WA.row(0): "<<pop_WA.row(0).array()<<std::endl;
  //std::cout<<"N.row(0): "<<N.row(0).array()<<std::endl;
  //std::cout<<"Sum Product: "<<BIOMASSATAGE.sum()<<std::endl;

  for (i=0; i<=ryears.size()-1; i++)
  {
	BIOMASSATAGE = pop_WA.row(i).array()*N.row(i).array();
    BIOMASS(i)=BIOMASSATAGE.sum();
  }
  //std::cout<<"BIOMASS: "<<BIOMASS<<std::endl;

  //6. Get spawn_WA
  for (i=0; i<=ryears.size()-1; i++)
  {
	SP_BIOAGE=wght_fac.row(i).array()*N.row(i).array();
    SP_BIO(i)=SP_BIOAGE.sum();
  }

  //std::cout<<"SP_BIO: "<<SP_BIO<<std::endl;

  //7. Get number of eggs
  for (i=0; i<=ryears.size()-1; i++)
  {
	eggsatage=egg_fac.row(i).array()*N_spawn.row(i).array();
  eggs(i)=eggsatage.sum();
  }
  //std::cout<<"predR: "<<predR<<std::endl;

  //8. Get catch at age
  CAT=(FT.array()/Z.array())*(A.array())*N.array();
  CAG=(FG.array()/Z.array())*(A.array())*N.array();

  MDEAD=(MD.array()/Z.array())*(A.array())*N.array();
  TDEAD=MDEAD+CAT+CAG;

  //std::cout<<"CAT: "<<CAT<<std::endl;
  //std::cout<<"CT: "<<CT<<std::endl;
  //std::cout<<"CAT.row(0): "<<CAT.row(0)<<std::endl;
  //std::cout<<"CAT.row(0)/(CT(0)+0.001): "<<CAT.row(0)/(CT(0)+0.001)<<std::endl;


  for (i=0; i<=ryears.size()-1; i++)
  {
    CT(i)=CAT.row(i).sum();
    PAT.row(i)=CAT.row(i)/(CT(i)+0.001);
    CG(i)=CAG.row(i).sum();
    PAG.row(i)=CAG.row(i)/(CG(i)+0.001);
  }

  //std::cout<<"CT: "<<CT<<std::endl;
  //std::cout<<"PAT: "<<PAT<<std::endl;

  //9. Get selectivity
  // selectT=selT.row(years.size()-1);
  // selectG=selG.row(years.size()-1);

  selectT=1;
  selectG=1;

  //10. Get predicted biomass harvested by gear typedef
  BT=mnwgtT.array()*CT.array();
  BG=mnwgtG.array()*CG.array();

  //std::cout<<"selectT: "<<selectT<<std::endl;
  //std::cout<<"BT: "<<BT<<std::endl;


  //11. Calculate STANDARDIZED Residuals
  // Total Catch
  residCT=(log(obs_CT+0.001)-log(CT+0.001))/sd_logCT;
  residCG=(log(obs_CG+0.001)-log(CG+0.001))/sd_logCG;

  // Catch Composition
  prodPAT=PAT.array()*(1.0-PAT.array());
  prodPAG=PAG.array()*(1.0-PAG.array());
  Ntrap=N_SampT+0.001;
  Ngill=N_SampG+0.001;
  //std::cout<<"prodPAT: "<<prodPAT<<std::endl;
  //std::cout<<"Ntrap: "<<Ntrap<<std::endl;
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
  //std::cout<<"residPAT: "<<residPAT<<std::endl;

  for(i=0;i<=ryears.size()-2;i++)
  {
    for(j=0;j<=ages.size()-1;j++)
    {
	    resid_effG(i,j)=effort_devsG(i,j)/(sd_logeffortG(j)+0.000001);
    }
    for(j=0;j<=asymptoteages.size()-1;j++)
    {
      resid_effT(i,j)=effort_devsT(i,j)/(sd_logeffortT(j)+0.000001);
    }
  }
  // std::cout<<"resid_effT: "<<resid_effT<<std::endl;

  //12. Get projected values
  Fproj=(F.row(ryears.size()-1).array()+F.row(ryears.size()-1-1).array()+F.row(ryears.size()-1-2).array())/3;
  FTproj=(FT.row(ryears.size()-1).array()+FT.row(ryears.size()-1-1).array()+FT.row(ryears.size()-1-2).array())/3;
  FGproj=(FG.row(ryears.size()-1).array()+FG.row(ryears.size()-1-1).array()+FG.row(ryears.size()-1-2).array())/3;
  Zproj=(Z.row(ryears.size()-1).array()+Z.row(ryears.size()-1-1).array()+Z.row(ryears.size()-1-2).array())/3;
  Sproj=exp(-Zproj);

  Mproj=MD.row(ryears.size()-1);
  Sproj_BASE=exp(-Mproj);
  //std::cout<<"Sproj_BASE: "<<Sproj_BASE<<std::endl;

  if (targ_age>fage)
  {
    Sproj_T.segment(0,targ_age-fage)=Sproj_BASE.segment(0,targ_age-fage);
  }
  Sproj_T.segment(targ_age-fage,lage-targ_age+1)=0.35;
  //std::cout<<"Sproj_T: "<<Sproj_T<<std::endl;

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

  //std::cout<<"YPR: "<<YPR<<std::endl;
  //std::cout<<"SSBR: "<<SSBR<<std::endl;
  //std::cout<<"SPR: "<<SPR<<std::endl;

  RecSum=0;
  for (i=ryears.size()-1-9;i<=ryears.size()-1;i++)
  {
	RecSum+=N(i,0);
  }
  Rec_avg=RecSum/10;
  //std::cout<<"Rec_avg: "<<Rec_avg<<std::endl;

  //13. Get some additional important values to output later
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
  //std::cout<<"AvgBiomasslb: "<<AvgBiomasslb<<std::endl;

  SSBRlb = SSBR *2.2046;
  SSBR_Tlb = SSBR_T *2.2046;
  SSBR_BASElb = SSBR_BASE *2.2046;
  YPRlb = YPR*2.2046;

  //14. Get the average annual mortality rate
  Aproj=(A.row(ryears.size()-1).array()+A.row(ryears.size()-1-1).array()+A.row(ryears.size()-1-2).array())/3.0;
  avg_A=(Aproj(ages.size()-1-1)+Aproj(ages.size()-1-2)+Aproj(ages.size()-1-3)+Aproj(ages.size()-1-4)+Aproj(ages.size()-1-5))/5;
  //std::cout<<"Avg_A: "<<avg_A<<std::endl;

  //OBJECTIVE FUNCTION SECTION
  NLP= 0.0; //Components related to priors and process error
  NLL= 0.0; //Components related to fit to data and observation error

  //Strong penalty for Average F deviating from 0.2
  avg_F=F.sum()/double(F.size());
  //std::cout<<"Avg_F: "<<avg_F<<std::endl;
  //JRB commmented out next line in testing 6/18/19
  //NLP+= 1000.*pow(log((avg_F+0.000001)/.2),2.0);

  //Penalty for Natural Mortality M deviating from prior
  NLP+= 0.5/pow(sdM,2.0)*pow(lnmedM-lnM,2.0)+1.0*log(sdM);
  //std::cout<<"NLP M: "<<NLP<<std::endl;

  //Normally distributed deviations from random walk for trap and gill net catchability
  //Normally distributed deviations from random walk for trap selelectivity parameter p1
  //To run a multivariate normal distribution in TMB, first calculate correlation and variance covariance matrices:
  // Set up correlation matrix as a function of the age difference
  // Set up the matrices with catchability process error SD values on the diagonal
  sd_logeffortT_diag.fill(Type(0));
  sd_logeffortG_diag.fill(Type(0));
  
  // std::cout<<"sd_logeffortT: "<<sd_logeffortT<<std::endl;
  // std::cout<<"sd_logeffortG: "<<sd_logeffortG<<std::endl;
  
  for(i=0;i<=ages.size()-1;i++)
  {
    for(j=0;j<=ages.size()-1;j++)
    {
      // Correlation matrix as a function of age difference
      cormatG(i,j)=pow(rhoalphaG,abs(i-j));

      // Correlation matrix is a common but estimated constant
      // cormatG(i,j)=rhoalphaG;
    }
    // cormatG(i,i)=1;
    sd_logeffortG_diag(i,i)=sd_logeffortG(i);
  }

  for(i=0;i<=asymptoteages.size()-1;i++)
  {
    for(j=0;j<=asymptoteages.size()-1;j++)
    {
      // Correlation matrix as a function of age difference
      cormatT(i,j)=pow(rhoalphaT,abs(i-j));

      // Correlation matrix is a common but estimated constant
      // cormatT(i,j)=rhoalphaT;
    }
    // cormatT(i,i)=1;
    sd_logeffortT_diag(i,i)=sd_logeffortT(i);
  }

  // Calculate the variance covariance matrices based on correlation and SD diag matrices
  // Calculate the "density object" dmvnorm for easy density calculation in next step
  varcovT=sd_logeffortT_diag*cormatT*sd_logeffortT_diag;
  MVNORM_t<Type> my_dmvnormT(varcovT);
  varcovG=sd_logeffortG_diag*cormatG*sd_logeffortG_diag;
  MVNORM_t<Type> my_dmvnormG(varcovG);
  //Process error around age-correlated catchability is multivariate normal distributed
  for(i=0;i<=ryears.size()-2;i++)
  {
    NLP+= my_dmvnormT(effort_devsT.row(i));
    NLP+= my_dmvnormG(effort_devsG.row(i));
  }

  //Process error around random walk recruitment is normally distributed
  NLP-=sum(dnorm(log_rec_devs,0,sdSR,true));

  
  
  //Observation Error 1: NLL CALCULATION BASED ON SEPARABLE CATCH AND COMPOSITION W OPTIONS FOR EITHER
  //Total catch is log normally distributed
  for(i=0;i<=ryears.size()-1;i++)
  {
	  NLL+=0.5/pow(sd_logCT,2.0)*pow(log((.01+obs_CT(i))/(.01+CT(i))),2.0) + log(sd_logCT);
  }
  //std::cout<<"NLL Trap Catch Total: "<<NLL<<std::endl;
  for(i=0;i<=ryears.size()-1;i++)
  {
	  NLL+=0.5/pow(sd_logCG,2.0)*pow(log((.01+obs_CG(i))/(.01+CG(i))),2.0) + log(sd_logCG);
  }
  //std::cout<<"NLL Trap and Gill Catch: "<<NLL<<std::endl;

  //Catch composition is multinomially distributed with effective sample size
  for(i=0;i<=ryears.size()-1;i++)
  {
    // NLL-= dmultinom((N_SampT(i)*obs_PAT.row(i)),PAT.row(i),true)
	  for(j=0;j<=ages.size()-1;j++)
	  {
		  NLL-= N_SampT(i)*obs_PAT(i,j)*log(.0001+PAT(i,j));

		  NLL-= N_SampG(i)*obs_PAG(i,j)*log(.0001+PAG(i,j));
	  }
  }
  
  
  // std::cout<<"log_gamT: "<<log_gamT<<std::endl;
  // std::cout<<"std_log_gamT: "<<std_log_gamT<<std::endl;
  // //Catch composition is conditional Log Gaussian Cox Process:
  // for(i=0;i<=ryears.size()-1;i++)
  // {
  //   // NLL-= dmultinom((N_SampT(i)*obs_PAT.row(i)),PAT.row(i),true)
  //   for(j=0;j<=ages.size()-1;j++)
  //   {
  //   giT(j) = exp(log_gamT(i,j)*std_log_gamT(j));
  //   giG(j) = exp(log_gamG(i,j)*std_log_gamG(j));
  // 
  //   }
  // 
  //   std::cout<<"giT: "<<giT<<std::endl;
  //   std::cout<<"PAT.row(i).array(): "<<PAT.row(i).array()<<std::endl;
  // 
  //   // piT.array()= PAT.row(i).array()*giT.array();
  // 
  //   // piT = piT(j)/(piT.row(i).sum());
  //   //
  //   //
  //   //   piG(j) = PAG(i,j)*giG(j);
  //   //   piG(j) = piG(j)/(piG.row(i).sum());
  // 
  // 
  //     // NLL-= N_SampT(i)*obs_PAT(i,j)*log(.0001+piT(i,j));
  //     // NLL-= N_SampG(i)*obs_PAG(i,j)*log(.0001+piG(i,j));
  // }
  // //catch age comp over-dispersion effect nll;
  
  // for(i = 0;i < Y-1;++i){
  //   
  //   vector<Type> del = log_gam.row(i);
  //   
  //   //    nll += SCALE(AR1(ar_gam_age),std_log_gam)(del); 
  //   
  //   nll += AR1(ar_gam_age)(del);
  // 
  // std::cout<<"piT: "<<piT<<std::endl;
  // // std::cout<<"Total: "<<NLL+NLP<<std::endl;
  
  
  //Observation Error 2: NLL CALCULATION BASED COMBINED CATCH AND COMPOSITION
  

  // f=NLL+NLP;
  f=0.0;
  // std::cout<<"f: "<<f<<std::endl;

  //Print out during optimization section
  //CppAD::PrintFor("Currently lnM=", lnM);
  //CppAD::PrintFor("Currently f=", f);

  //REPORT SECTION
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
  ADREPORT(obs_PAT);
  ADREPORT(obs_PAG);
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

  
  SIMULATE{
    Type sd_logCT_sim = sd_logCT;
    Type sd_logCG_sim = sd_logCG;
    
    //TRAP NET TOTAL HARVEST BY WEIGHT
    vector<Type> CT_obs_sim = exp(rnorm(log(CT),sd_logCT_sim));
    // vector<Type> CT_obs_sim = exp(log(obs_CT)); //"Simulate" the real data, to check if I can recover the same estimates from identical data
    // vector<Type> CT_obs_sim = exp(log(CT)); //"Simulate" the predicted data without error
    vector<Type> harv_wgtT_sim = CT_obs_sim.array() * mnwgtT.array() * Tharv_adjust.array();
    
    //GILL NET TOTAL HARVEST BY WEIGHT
    vector<Type> CG_obs_sim = exp(rnorm(log(CG),sd_logCG_sim));
    // vector<Type> CG_obs_sim = exp(log(obs_CG)); //"Simulate" the real data, to check if I can recover the same estimates from identical data
    // vector<Type> CG_obs_sim = exp(log(CG)); //"Simulate" the predicted data without error
    vector<Type> harv_wgtG_sim = CG_obs_sim.array() * mnwgtG.array() * Gharv_adjust.array();

    
    matrix<Type> obs_PAT_sim(ryears.size(),ages.size());
    matrix<Type> obs_PAG_sim(ryears.size(),ages.size());
    
    //Simulate proportions at age using either effective sample size (Ntrap/Ngill)
    //or the true number of sampled indivituals (NtildeG/NtildeT)
    //or an adjusted amount to increase the ESS (reducing error)
    vector <Type> Ntrap_sim;
    vector <Type> Ngill_sim;
    
    for(i=0;i<=ryears.size()-1;i++)
    {
      obs_PAT_sim.row(i) = rmultinom(Ntrap(i),vector<Type>(PAT.row(i)))/Ntrap(i);
      obs_PAG_sim.row(i) = rmultinom(Ngill(i),vector<Type>(PAG.row(i)))/Ngill(i);
      
      // obs_PAT_sim.row(i) =vector<Type>(obs_PAT.row(i)); //"Simulate" the real data, to check if I can recover the same estimates from identical data
      // obs_PAG_sim.row(i) =vector<Type>(obs_PAG.row(i));
      // obs_PAT_sim.row(i) =vector<Type>(PAT.row(i)); //"Simulate" the predicted data without error
      // obs_PAG_sim.row(i) =vector<Type>(PAG.row(i));
    }

    REPORT(harv_wgtT_sim);
    REPORT(harv_wgtG_sim);
    REPORT(obs_PAT_sim);
    REPORT(obs_PAG_sim);
    
  }
  
  return f;

}
