void run_ana_dst(int nevnt = -1)
{
  const char* datafile = "mutoo_demo_dst.root";
  dfileopen(datafile);
  drun(nevnt);
  end_all();
}



