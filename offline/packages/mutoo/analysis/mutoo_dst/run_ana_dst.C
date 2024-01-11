void run_ana_dst(int nevnt = 1)
{
  const char* datafile = "/phenix/data25/kelly/mutoo_flt_dst/full_dst40321-0003.root";
  dfileopen(datafile);
  drun(nevnt);
  //  end_all();
}



