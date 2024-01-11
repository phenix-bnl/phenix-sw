void run_one()
{
  TMutDatabaseCntrl::set_database_access("arms_from_database",false);
  TMutDatabaseCntrl::set_database_access("internal_alignment_corrections",false);
  TMutDatabaseCntrl::set_database_access("global_alignment_corrections",false);
  TMutDatabaseCntrl::set_database_access("disable_HV",false);
  TMutDatabaseCntrl::set_database_access("dead_channels",false);
  TMutDatabaseCntrl::set_database_access("dead_FEMs",false);
  TMutDatabaseCntrl::set_database_access("mmsfull_cmfull_mmnfull.root",false);
  TMutDatabaseCntrl::print();

  setMapFileScale(1.0);  
  const char* datafile = "../../../response_r/mutoo_slowsim.root";
  dfileopen(datafile);
  dstout_fopen("mutoo_dst.root");
  drun(1);
}



