void run_ana(const char* datafile = "mutoo_dst.root",
	     const char* outputfile = "mutoo_ntuple.root")
{
  dfileopen(datafile);
  set_ntuple_name(outputfile);
  drun(-1);
  end_all();
}



