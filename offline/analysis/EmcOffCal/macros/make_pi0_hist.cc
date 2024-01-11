string WARNMAP_FILE_NAME;
string COEF_FILE_NAME_IN;
string COEF_FILE_NAME_SUPERMOD_IN;


string PI0_HIST_ROOT_FILE_NAME;
string PI0_HIST_TABLE_FILE_NAME_TYPE;
string PI0_HIST_TABLE_FILE_NAME_SECTOR;
string PI0_HIST_TABLE_FILE_NAME_TOWER;

string UNCALIB_LIST_FILE_NAME_IN;

void make_pi0_hist_iter(int n_iteration, char* run_data_list)
{
   
   gSystem->Load("/direct/phenix+u/workarea/manion/build/EMC_Calib/.libs/libEmcOffCal.so");

   WARNMAP_FILE_NAME = gSystem->Getenv("WARNMAP_FILE_NAME");
   COEF_FILE_NAME_IN = gSystem->Getenv("COEF_FILE_NAME_IN");
   COEF_FILE_NAME_SUPERMOD_IN = gSystem->Getenv("COEF_FILE_NAME_SUPERMOD_IN");


   PI0_HIST_ROOT_FILE_NAME         = gSystem->Getenv("PI0_HIST_ROOT_FILE_NAME");
   PI0_HIST_TABLE_FILE_NAME_TYPE   = gSystem->Getenv("PI0_HIST_TABLE_FILE_NAME_TYPE");
   PI0_HIST_TABLE_FILE_NAME_SECTOR = gSystem->Getenv("PI0_HIST_TABLE_FILE_NAME_SECTOR");
   PI0_HIST_TABLE_FILE_NAME_TOWER  = gSystem->Getenv("PI0_HIST_TABLE_FILE_NAME_TOWER");
   
   UNCALIB_LIST_FILE_NAME_IN  = gSystem->Getenv("UNCALIB_LIST_FILE_NAME_IN");
   
   Pi0Analysis* pi0ana = new Pi0Analysis(true, WARNMAP_FILE_NAME.c_str(), COEF_FILE_NAME_IN.c_str(), COEF_FILE_NAME_SUPERMOD_IN.c_str(), UNCALIB_LIST_FILE_NAME_IN.c_str());
   pi0ana->InitMassHist(PI0_HIST_ROOT_FILE_NAME.c_str(), true, true, true);

   string fname_cluster_data;
   ifstream if_run_data_list(run_data_list);
   while (if_run_data_list >> fname_cluster_data) {
      cout << fname_cluster_data << endl;
      
      
     //  const double mom_min_target = 0.8;
//       const double mom_min_pair   = 0.1;
//       const double pT_min_pair    = 1.0

      const double mom_min_target_pbsc = 0.8;//0.8 old value
      const double mom_min_target_pbgl = 0.8;
      const double mom_min_pair_pbsc = 0.3;//0.1 old value
      const double mom_min_pair_pbgl = 0.3;
      const double pT_min_pair_pbsc    = 1.0;//1.0 old value
      const double pT_min_pair_pbgl    = 1.0;
      //const double max_asymm      = 1.0;
      const double tof_min = 0;
      const double tof_max = 0;
      const double frac_energy_in_target = 0.0;

      pi0ana->MakeMassHist(fname_cluster_data.c_str(),
                           mom_min_target_pbsc, mom_min_target_pbgl, mom_min_pair_pbsc, mom_min_pair_pbgl, pT_min_pair_pbsc, pT_min_pair_pbgl, frac_energy_in_target, tof_min, tof_max);
   }
   cout<<"done makin mass histograms"<<endl;
   pi0ana->AnalysisRunByRun();
   pi0ana->Analysis(
		    PI0_HIST_TABLE_FILE_NAME_TYPE.c_str(),
		    PI0_HIST_TABLE_FILE_NAME_SECTOR.c_str(), 
		    PI0_HIST_TABLE_FILE_NAME_TOWER.c_str()
		    );
}
