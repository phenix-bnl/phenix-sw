void run_ana_dst(int nevnt = -1)
{
  const char* datafile = "/phenix/data07/kelly/slow_sim_jpsi/4k_jpsi_100u_mutoo_slowsim.root";
  //const char* datafile = "/direct/phenix+data09/hpereira/Simulation/mutoo_dst/jpsi_sim3000.root";
  //const char* datafile = "foo.root";
  dfileopen(datafile);
  dstout_fopen("blah.root");
  drun(nevnt);
  end_all();
}



