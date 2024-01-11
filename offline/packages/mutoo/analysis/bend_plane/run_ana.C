void run_ana(int nevnt = 10)
{
  const char* datafile = "/phenix/data23/kelly/slow_sim_jpsi/1k_oscar_mutr_selected_jpsi_slowsim.root";
  dfileopen(datafile);
  dstout_fopen("dst_out.root");
  drun(nevnt);
  end_all();
}



