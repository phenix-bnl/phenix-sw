void run_one(int nevnt = 1)
{
  TMutDatabaseCntrl::set_database_access("arms_from_database",false);
  TMutDatabaseCntrl::set_database_access("internal_alignment_corrections",false);
  TMutDatabaseCntrl::set_database_access("global_alignment_corrections",false);
  TMutDatabaseCntrl::set_database_access("disable_HV",false);
  TMutDatabaseCntrl::set_database_access("dead_channels",false);
  TMutDatabaseCntrl::set_database_access("dead_FEMs",false);
  TMutDatabaseCntrl::set_database_access("mmsfull_cmfull_mmnfull.root",false);
  TMutDatabaseCntrl::print();
  const char* datafile = "/phenix/data23/kelly/slow_sim_jpsi/4k_jpsi_0u_mutoo_slowsim.root";
  dfileopen(datafile);
  dstout_fopen("dst_out.root");
  drun(nevnt);
  end_all();
}



