#include<string>
void run_ana_dst(int nevnt = -1)
{

  // uncomment the below lines to run w/o the database (DEBUG ONLY)
  //

//    TMutDatabaseCntrl::set_database_access("arms_from_database",false);
//    TMutDatabaseCntrl::set_database_access("internal_alignment_corrections",false);
//    TMutDatabaseCntrl::set_database_access("global_alignment_corrections",false);
//    TMutDatabaseCntrl::set_database_access("disable_HV",false);
//    TMutDatabaseCntrl::set_database_access("dead_channels",false);
//    TMutDatabaseCntrl::set_database_access("dead_FEMs",false);
//    TMutDatabaseCntrl::set_database_access("mmsfull_cmfull_mmnfull.root",false);
//    TMutDatabaseCntrl::print();

  
  setMapFileScale(1.0);
  char* infile = "mb_jpsi_5may03.root";
  //char* infile = "poor_res.root";
  char* outfile = "new_mutoo_dst.proot";
  char* outfile_ndst = "new_ndst.root";
  dfileopen(infile);
  //  doutfileopen("ndst.root");
  dstout_fopen(outfile,outfile_ndst);
  drun(-1);
  end_all();
}




