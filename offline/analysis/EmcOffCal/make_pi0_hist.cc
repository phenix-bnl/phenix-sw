#include <string>
#include <sstream>
#include <TSystem.h>
#include "Pi0Analysis.h"
#include <iostream>
#include "make_pi0_hist.h"


using namespace std;
string WARNMAP_FILE_NAME;
string COEF_FILE_NAME_IN;
string COEF_FILE_NAME_SUPERMOD_IN;


string PI0_HIST_ROOT_FILE_NAME;
string PI0_HIST_TABLE_FILE_NAME_TYPE;
string PI0_HIST_TABLE_FILE_NAME_SECTOR;
string PI0_HIST_TABLE_FILE_NAME_TOWER;

string UNCALIB_LIST_FILE_NAME_IN;

//void make_pi0_hist_iter(int n_iteration, char* run_data_list)
void make_pi0_hist_iter(int n_iteration)
{
   
   gSystem->Load("libEmcOffCal.so");

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

   //string fname_cluster_data;
   char fname_cluster_data[500];
   ostringstream name;
   int Nsub = 10;
   for(int i=0; i<Nsub;i++){
     name.str("");
     name << "clusterListAA" << i <<".txt";//seperate data list to avoid ROOT memory problem.//modified by HG
     //name << "clusterListAAbadtest.txt";
     ifstream if_run_data_list(name.str().c_str());
     //while (if_run_data_list >> fname_cluster_data) {
     while (if_run_data_list.getline(fname_cluster_data,500)){
       cout << fname_cluster_data << endl;
       
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
       //const double bbc_max_charge_ns =300.;//~52% most peripheral for run11AuAu
       const double bbc_max_charge_ns =450.;
       //if(i==0) const double bbc_max_charge_ns= 700.;// ~60% most peripheral for Run12CuAu //HG
       //else const double bbc_max_charge_ns= 500.;// ~60% most peripheral for Run12UU  //HG
//        if(i==0) const double bbc_max_charge_ns= 650.;// ~53% most peripheral for Run12CuAu //HG
//        else const double bbc_max_charge_ns= 350.;// ~53% most peripheral for Run12UU  //HG
       //if(i==0) const double bbc_max_charge_ns= 450.;// ~31% most peripheral for Run12CuAu //HG
       //else const double bbc_max_charge_ns= 80.;// ~31% most peripheral for Run12UU  //HG
      
       //       if(i==0) const double bbc_max_charge_ns= 500.;// ~38% most peripheral for Run12CuAu //HG
       //       else const double bbc_max_charge_ns= 300.;// ~50% most peripheral for Run12UU  //HG
       pi0ana->MakeMassHist(fname_cluster_data/*.c_str()*/,mom_min_target_pbsc, mom_min_target_pbgl, mom_min_pair_pbsc, mom_min_pair_pbgl, pT_min_pair_pbsc, pT_min_pair_pbgl, frac_energy_in_target, tof_min, tof_max, bbc_max_charge_ns);


     }
   }
   cout<<"done makin mass histograms"<<endl;
   pi0ana->AnalysisRunByRun();
   pi0ana->Analysis(
		    PI0_HIST_TABLE_FILE_NAME_TYPE.c_str(),
		    PI0_HIST_TABLE_FILE_NAME_SECTOR.c_str(), 
		    PI0_HIST_TABLE_FILE_NAME_TOWER.c_str()
		    );
   delete pi0ana;
}
