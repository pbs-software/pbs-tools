// POP catch-age model. Revision history:
//
// May 6, 1997
// Implementation of catch-age model for POP described in
// Richards, Schnute and Olsen 1997 CJFAS June issue
// Includes variances as estimated parameters
//
// October 17, 2001
// Revised for 2001 POP assessment


DATA_SECTION
  init_int NYear                         // number T of years with data
  init_int NYear1                        // last year T' of surface reading
  init_int NAge                          // number A of age classes
  init_int NAge1                         // max. age A' for surface reading
  init_number PMin                       // min p_at for grouping classes
  init_int k                             // true age k of age class 1
  init_vector wpars(1,3)                 // weight parameters (w1,winf,lamw)
  init_vector mpars(1,2)                 // maturity parameters (lamm,b)
  init_matrix data_agewgt(1,NAge*NYear,1,4)
                                         // yr, age, prop, wgt
  init_matrix data_cpue(1,NYear,1,5)     // yr, catch, B1, B2, cf

  int NITerms                            // counter re grouping classes
  int NPTerms                            // counter re grouping classes

  vector cf(1,NYear)                     // proportion catch caught at survey
  vector D_t(1,NYear)                    // observed biomass in year t

  matrix w_at(1,NAge,1,NYear)            // observed weight at age a in year t
  matrix p_at(1,NAge,1,NYear)            // observed age a prop in year t
  vector wgta(1,NAge)                    // calculated weight at age a
  vector mata(1,NAge)                    // calculated maturity at age a


PARAMETER_SECTION
  init_bounded_number F(0.0,0.3,-1)      // historic fishing mortality
  init_bounded_number M(0.02,0.08)       // natural mortality
  init_bounded_number R(0.1,10.0)        // historic mean recruitment
  init_bounded_number gamma(-1.0,1.0)    // recruitment correlation
  init_bounded_number alpha(0.02,50.0)   // selectivity exponent
  init_bounded_number beta(0.0,1.0)      // selectivity at age class 1
  init_bounded_number q(0.1,10.0)        // catchability for research
  init_bounded_number q2(0.1,10.0)       // catchability for commerc.
  init_number rho(-1)                    // ratio of sig1^2 and kap^2
  init_bounded_number kap_2(.001,5.0)    // error (rec+index)
  init_bounded_number tau2_2(0.001,5.0)  // measurement error (ages)

  init_bounded_vector Rt(2-NAge1,NYear,1.0E-03,50.0,2)
                                         // historical recruitment vector

  number sig1_2                          // process error (recruits)
  number tau1_2                          // measurement error (index)

  vector B_t(1,NYear)                    // Biomass at time t
  vector beta_a(1,NAge)                  // Selectivity at age a
  vector C_t(1,NYear)                    // Catch in numbers at time t
  vector F_t(1,NYear)                    // Fishing mortality at time t
  vector P_t(1,NYear)                    // Expl. pop. at time t
  vector SSB(1,NYear)                    // Spawning stock biomass
  vector Pred_It(1,NYear)                // Abund. index at time t
  vector Pred_I2(1,NYear)                // Abund. index 2 at time t
  vector RecProd(1,NYear-k)              // Recruitment productivity

  matrix Nat(1,NAge,1,NYear)             // Numbers at age a, time t
  matrix u_at(1,NAge,1,NYear)            // Expl. proportion at age a, time t

  number S                               // exp(-M), survival
  vector betaNat(1,NAge)                 // elem_prod of beta_a*N_at

  vector xi_t(2-NAge1,NYear)             // xi_t
  number xiSS                            // sum of squares of xi_t
  vector eta_0t(1,NYear)                 // eta_0t
  number eta0tSS                         // sum of squares of eta_0t
  vector eta_t(1,NAge)                   // eta_at, for age a
  number etaatSS                         // sum of squares of eta_at
  number sumu                            // Collects terms
  number sumeta                          // Collects terms

  vector like_terms(1,5)                 //
  number penalty                         // a penalty value
  objective_function_value lf            // the objective function value

  sdreport_vector BPars(1,7)             // basic parameters:
                                         //  alpha,beta1,M,q1,q2,R,gamma
  sdreport_vector logBt(1,NYear)         // log(B_t)
  sdreport_vector logRt(1,NYear)         // log(Rt), starting at t=1
  sdreport_vector logSSB(1,NYear)        // log(SSB)
  sdreport_vector mcvec(1,16+NAge)       // MCMC results vector
  // MCMC vector:
  //   7 basic parameters
  //   NAge values Nat from last yr
  //   last 8 recruitment productivity values
  //   SSB from last yr


PRELIMINARY_CALCS_SECTION
  // extract proportion of catch taken prior to index.
  cf=column(data_cpue,5);
  D_t=column(data_cpue,2);               // Catch biomass at time t
  // Laura's code for observed weight at age a, time t
  // int cnt=1;
  // for (int t=1;t<=NYear;t++)
  //  for (int a=1;a<=NAge;a++,cnt++)
  //    w_at[a][t]=data_agewgt[cnt][4];
  // New vector of weights at age a
  wgta[1] = wpars[1];
  for (int a=2;a<=NAge;a++)
    wgta[a] = wpars[2] - (wpars[2] - wpars[1])*pow(wpars[3],a-1);
  // New matrix of weights at age a and time t
  for (int t=1;t<=NYear;t++) {
    int mage = NAge1+t-1;               // maximum age A_t
    if ( mage > NAge ) mage = NAge;
    for (int a=1;a<mage;a++)
      w_at[a][t] = wgta[a];
    w_at[mage][t] = (wgta[mage] + wpars[2])/2.0;
    } // for t=1,NYear
  // New maturity vector
  for (int a=1;a<=NAge;a++)
    mata[a] = pow( 1 - pow(mpars[1],a+k-1), 1/mpars[2] );
  // Observed age proportions at age a and year t
  int cnt=1; // counter for input records
  for (int t=1;t<=NYear;t++) {
    int kage=NAge1;                     // maximum age A'_t
    if ( t > NYear1 ) kage = NAge;
    double psum=0.0, pval=0.0;
    for (int a=1;a<=NAge;a++,cnt++) {
       p_at[a][t]=data_agewgt[cnt][3];
       pval=data_agewgt[cnt][3];
       if (a < kage) p_at[a][t] = pval;
       if (a == kage) p_at[a][t] = 1.0 - psum;
       if (a > kage) p_at[a][t]=0;
       psum += pval;
       } // for a=1,NAge
     }   // for t=1,NYear


PROCEDURE_SECTION
  Calc_Selectivity();
  Calc_Nat_and_Moments();
  Calc_Pred_It();
  Calc_Recruit_Resid();
  Calc_Index_Resid();
  Calc_Age_Resid();
  Calc_Objective_Function();

  if ( sd_phase() ) Calc_SDpars();

  if ( mceval_phase() ) {
    Calc_SDpars(); MCoutput(); };


FUNCTION Calc_Selectivity
  for (int a=1;a<NAge;a++)
    beta_a[a]=1.0-(1.0-beta)*pow(double(NAge-a)/double(NAge-1.0),alpha);
  beta_a[NAge]=1.0;


FUNCTION Calc_Nat_and_Moments
  int mage; // maximum age A_t
  S = exp( -M );

  for (int t=1;t<=NYear;t++)
  {
    if ( t==1 )
    {
      mage = NAge1;
      Nat[1][1] = Rt[1];
      for (int a=2;a<=NAge1;a++)
      {
        Nat[a][1] = Rt[2-a] * exp( -(F+M) * (a-1.0) );
      }
      Nat[NAge1][1] = Nat[NAge1][1] / (1.0 - exp( -(F+M) ));
    } // if t==1
    else
    {
      mage=NAge1+t-1;
      if ( mage > NAge )
        mage = NAge;  // add age class each year to NAge
      Nat[1][t]=Rt[t];
      for (int a=2;a<=mage;a++)
      {
        Nat[a][t]=S*( Nat[a-1][t-1]-u_at[a-1][t-1]*C_t[t-1] );
        if ( value(Nat[a][t]) <= 1.0E-08 )
        {
          penalty += 1.0E-08 - Nat[a][t];
          Nat[a][t] = 1.0E-08;
         }
      }
      Nat[mage][t]=S*( Nat[mage-1][t-1]+Nat[mage][t-1] -
                   (u_at[mage-1][t-1]+u_at[mage][t-1])*C_t[t-1]);
    }

    betaNat = elem_prod( beta_a, extract_column( Nat,t ) );
    P_t[t] = sum( betaNat );
    u_at.colfill(t, (betaNat / P_t[t]) );
    B_t[t] = sum( elem_prod( betaNat,extract_column(w_at,t) ) );
    C_t[t] = D_t[t] / sum( elem_prod(extract_column(u_at,t),
                                     extract_column(w_at,t)) );
    F_t[t] = log( P_t[t] / ( P_t[t] - C_t[t] ) );

  }     // for t=1,NYear


FUNCTION Calc_Pred_It
  for (int t=1;t<=NYear;t++)
  {
    Pred_It[t] = q  * ( B_t[t] - cf[t] * D_t[t] );
    Pred_I2[t] = q2 * ( B_t[t] - cf[t] * D_t[t] );
  }


FUNCTION Calc_Recruit_Resid
  int t;
  xi_t[2-NAge1] = log( Rt[2-NAge1] ) - log( R );
  xiSS = (1.0 - gamma*gamma) * xi_t[2-NAge1] * xi_t[2-NAge1];
  for (t=3-NAge1;t<=NYear;t++)
  {
    xi_t[t] = log(Rt[t]) - ((1.0-gamma)*log(R)) - (gamma*log(Rt[t-1]));
    xiSS += xi_t[t] * xi_t[t];
  }


FUNCTION Calc_Index_Resid
  int t;
  eta0tSS = 0.0;
  NITerms = 0;
  for (t=1;t<=NYear;t++)
  {
  // if the survey biomass estimate positive...
    if ( data_cpue[t][3] > 0.0 )
    {
      // fix for extra survey in 1995, stuff into G.B. Reed column.
      if ( t==33 )
        eta_0t[t] = log( data_cpue[t][3] ) - log( Pred_I2[t] );
      else
        eta_0t[t] = log( data_cpue[t][3] ) - log( Pred_It[t] );
      eta0tSS += eta_0t[t] * eta_0t[t];
      NITerms++;
    }

    if ( data_cpue[t][4] > 0.0 )
    {
      eta_0t[t] = log( data_cpue[t][4] ) - log( Pred_I2[t] );
      eta0tSS += eta_0t[t] * eta_0t[t];
      NITerms++;
    }
  }     // for t=1,NYear


FUNCTION Calc_Age_Resid
  int a, t;
  NPTerms = 0;
  etaatSS = 0;
  for (t=1;t<=NYear;t++)
  {
    if ( (t!=24) && (t!=26) )           // Missing data 1986 and 1988
    {
       int nages=1;
       double sumprop = 0.0, remprop=1.0;
       sumu = 0.0; sumeta = 0.0;

       for (a=1;a<NAge;a++)
       {
         remprop -= data_agewgt[ (t-1)*NAge+a ][3];
         sumprop += data_agewgt[ (t-1)*NAge+a ][3];
         sumu += u_at[a][t];
         if ((sumprop>=PMin) && (remprop>=PMin) && ((t>14) || (a<NAge1)) )
         {
           eta_t[a] = log( sumprop+1.0E-10 ) - log( sumu+1.0E-10 );
           sumeta += eta_t[a];
           sumu = 0.0;
           sumprop = 0.0;
           nages++;
           NPTerms++;
         }
         else
           eta_t[a] = 0.0;
       }    // for a=1,NAge

       // Ensure last age class.
       sumprop += data_agewgt[ (t-1)*NAge+NAge ][3];
       sumu += u_at[NAge][t];

       eta_t[NAge] = log( sumprop ) - log( sumu );
       sumeta += eta_t[NAge];

       for (a=1;a<=NAge;a++)
       {
         if ( value( eta_t[a] ) != 0.0 )
           eta_t[a] = eta_t[a] - ( sumeta / (double)nages );
           etaatSS += eta_t[a] * eta_t[a];
       }     // for a=1,NAge

    }     // if t<>24 and t<>26
  }     // for t=1,NYear


FUNCTION Calc_Objective_Function
  // NOTE: Adm requires neg log-lk so mulitply by 0.5
  sig1_2 = rho * kap_2;
  tau1_2 = (1.0-rho)*kap_2;
  like_terms[1] = (double)(NAge1+NYear-2) * log( sig1_2 );
  like_terms[2] = -log(1.0-gamma*gamma);
  like_terms[3] = (double)(NITerms) * log(tau1_2);
  like_terms[4] = (double)(NPTerms) * log(tau2_2);
  like_terms[5] = xiSS/sig1_2 + eta0tSS/tau1_2 + etaatSS/tau2_2;
  lf = 0.5*sum( like_terms ) + penalty;


FUNCTION Calc_SDpars
  // Basic parameters
  BPars[1]=alpha; BPars[2]=beta; BPars[3]=M;
  BPars[4]=q; BPars[5]=q2; BPars[6]=R; BPars[7]=gamma;
  // Calculate SSB
  for (int t=1;t<=NYear;t++) {
    SSB[t] = 0.0;
    for (int a=1;a<=NAge;a++)
      SSB[t] += mata[a]*w_at[a][t]*Nat[a][t];
  }
  // Calculate log states
  for (int t=1;t<=NYear;t++) {
    logBt[t]=log(B_t[t]); logRt[t]=log(Rt[t]);
    logSSB[t]=log(SSB[t]); }
  // Calculate recruitment productivities
  for (int t=1;t<=(NYear-k);t++)
    RecProd[t]=wgta[1]*Nat[1][t+k]/SSB[t];
  // Fill mcvec
  for (int i=1;i<=7;i++)
    mcvec[i]=BPars[i];
  for (int a=1;a<=NAge;a++)
    mcvec[7+a]=Nat[a][NYear];
  for (int t=1;t<=8;t++)
    mcvec[7+NAge+t]=RecProd[NYear-k-8+t];
  mcvec[16+NAge]=SSB[NYear];


FUNCTION MCoutput
  cout << mcvec << endl;


RUNTIME_SECTION
  convergence_criteria 1.0E-06


REPORT_SECTION
  Calc_SDpars();
  report << "# POP Catch-at-Age Results" << endl;
  report << "$nyr\n"     << "$$vector mode=\"numeric\" \n"    << NYear   << endl;
  report << "$nag1\n"    << "$$vector mode=\"numeric\" \n"    << NAge1   << endl;
  report << "$nage\n"    << "$$vector mode=\"numeric\" \n"    << NAge    << endl;
  report << "$k\n"       << "$$vector mode=\"numeric\" \n"    << k       << endl;
  report << "$pmin\n"    << "$$vector mode=\"numeric\" \n"    << PMin    << endl;
  report << "$objfn\n"   << "$$vector mode=\"numeric\" \n"    << lf      << endl;
  report << "$pnlty\n"   << "$$vector mode=\"numeric\" \n"    << penalty << endl;
  report << "$fmor\n"    << "$$vector mode=\"numeric\" \n"    << F       << endl;
  report << "$mmor\n"    << "$$vector mode=\"numeric\" \n"    << M       << endl;
  report << "$rmean\n"   << "$$vector mode=\"numeric\" \n"    << R       << endl;
  report << "$gma\n"     << "$$vector mode=\"numeric\" \n"    << gamma   << endl;
  report << "$al\n"      << "$$vector mode=\"numeric\" \n"    << alpha   << endl;
  report << "$bta\n"     << "$$vector mode=\"numeric\" \n"    << beta_a  << endl;
  report << "$qq\n"      << "$$vector mode=\"numeric\" \n"    << q       << endl;
  report << "$q2\n"      << "$$vector mode=\"numeric\" \n"    << q2      << endl;
  report << "$BPars\n"   << "$$vector mode=\"numeric\" \n"    << BPars   << endl;
  report << "$rho\n"     << "$$vector mode=\"numeric\" \n"    << rho     << endl;
  report << "$kap2\n"    << "$$vector mode=\"numeric\" \n"    << kap_2   << endl;
  report << "$sig12\n"   << "$$vector mode=\"numeric\" \n"    << sig1_2  << endl;
  report << "$tau12\n"   << "$$vector mode=\"numeric\" \n"    << tau1_2  << endl;
  report << "$tau22\n"   << "$$vector mode=\"numeric\" \n"    << tau2_2  << endl;
  report << "$Bt\n"      << "$$vector mode=\"numeric\" \n"    << B_t     << endl;
  report << "$SSB\n"     << "$$vector mode=\"numeric\" \n"    << SSB     << endl;
  report << "$RecProd\n" << "$$vector mode=\"numeric\" \n"    << RecProd << endl;
  report << "$mcvec0\n"  << "$$vector mode=\"numeric\" \n"    << mcvec   << endl;
  report << "$cdata\n"   << "$$data ncol=5 modes=\"numeric numeric numeric numeric numeric\" colnames=\"yr catch B1 B2 cf\" byrow=TRUE\n" << data_cpue  << endl;
  report << "$uat\n"     << "$$matrix mode=\"numeric\" ncol=" << NYear << "\n" << u_at    << endl;
  report << "$Nat\n"     << "$$matrix mode=\"numeric\" ncol=" << NYear << "\n" << Nat     << endl;
  report << "$wgtat\n"   << "$$matrix mode=\"numeric\" ncol=" << NYear << "\n" << w_at    << endl;
  report << "$pat\n"     << "$$matrix mode=\"numeric\" ncol=" << NYear << "\n" << p_at    << endl;
  report << "$wgta\n"    << "$$vector mode=\"numeric\" \n"    << wgta    << endl;
  report << "$mata\n"    << "$$vector mode=\"numeric\" \n"    << mata    << endl;
