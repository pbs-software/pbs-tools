DATA_SECTION
  init_int nobs
  init_matrix data(1,nobs,1,2) // age, size
  vector age(1,nobs)
  vector size(1,nobs)

PARAMETER_SECTION
  init_number Linf
  init_number K
  init_number t0
  init_number sigma
  vector pred_s(1,nobs)
  objective_function_value f
  // The MCMC iteration saves only quantities with a requested
  // sdreport or likeprof.
  sdreport_vector pars(1,4)   // for all parameters
  sdreport_number LK          // Linf * K
  likeprof_number VonBLinf    // Linf, renamed for profiling

PRELIMINARY_CALCS_SECTION
  age = column(data,1);
  size = column(data,2);

PROCEDURE_SECTION
  pred_s = Linf*(1-exp(-K*(age-t0)));
  LK = Linf*K; VonBLinf = Linf;
  pars[1] = Linf; pars[2] = K; pars[3] = t0; pars[4] = sigma;
  f = norm2(pred_s - size);
  f = nobs*log(sigma) + f / (2.0 * sigma * sigma);

REPORT_SECTION
  report << "Final parameters:" << endl  << endl;
  report << "Linfinity" << "\t" << Linf  << endl;
  report << "K        " << "\t" << K     << endl;
  report << "t0       " << "\t" << t0    << endl;
  report << "sigma    " << "\t" << sigma << endl << endl;
  report << "Function:" << "\t" << f << endl;
