string WARNMAP_DIR;
string WARNMAP_FILE_NAME;

void make_warnmap(Char_t* run_data_list)
{
   gROOT->SetStyle("Plain");
//     gStyle->SetHistLineWidth(2);
   gStyle->SetLabelFont(22, "XYZ");
   gStyle->SetTitleFont(22, "XYZ");
   gStyle->SetStatFont(22);
   gStyle->SetTextFont(22);
//     gStyle->SetOptFit();
   gStyle->SetPalette(1);
   
   gSystem->Load("libEmcOffCal.so");
   gSystem->ListLibraries();
   
   WARNMAP_DIR = gSystem->Getenv("WARNMAP_DIR");
   WARNMAP_FILE_NAME = gSystem->Getenv("WARNMAP_FILE_NAME");


   //////////////////////////////////////////////////////////////////////////
   // RUN GROUPS
   // For the first calibration run, only need one run range.
   // Multiple run ranges are implemented for later warn maps more refined
   // to do analysis.
   //////////////////////////////////////////////////////////////////////////
   /*
   // for run 9
   const Int_t n_run_range = 3;
   const Int_t run_range[n_run_range + 1] = {
   276385, 276740, 277390, 277790
   };
   */
   
   //   for run8
   /*
   const Int_t n_run_range = 4;
   const Int_t run_range[n_run_range + 1] = 
     {
       247624, 249472, 251826, 252937, 253635
     };
   */
   // for run 12
   const Int_t n_run_range = 1;
   const Int_t run_range[n_run_range + 1] = 
     {
       358000, 400000
     };
   


   //////////////////////////////////////////////////////////////////
   // ECORE RANGES
   // One needs to find out hot towers in various ecore ranges
   // because some towers that are hot in one region might not be 
   // in other ecore ranges. The final hot tower map is a twr by twr 
   // or of the hot towers in all ranges
   //////////////////////////////////////////////////////////////////
   const Int_t n_ecore_range = 5;
   const Double_t ecore_range[6] = {
     //0.1, 0.2, 0.5, 1, 5, 30
     0.2, 0.3, 0.5, 1, 5, 30
   };
   
   
   MakeWarnmap* mkwarnmap
     = new MakeWarnmap( n_ecore_range, ecore_range,
			n_run_range, run_range );
   cout<<"1"<<endl;
   mkwarnmap->AddNhitInRun(run_data_list);
   cout<<"2"<<endl;
   
   // Loosen these cuts if the background in the inv. mass distrib
   // is too high for the pi0 to be visible. ()
   const double sigma_cut_pbsc =  8.0;//standard cut at 8 sigma.
   const double sigma_cut_pbgl = 15.0;
   cout<<"3"<<endl;
   mkwarnmap->CalcDeadTower();
   mkwarnmap->CalcHotTower(sigma_cut_pbsc, sigma_cut_pbgl);
   mkwarnmap->SetAroundAndEdge();
   mkwarnmap->DumpResult(WARNMAP_FILE_NAME.c_str());
}
