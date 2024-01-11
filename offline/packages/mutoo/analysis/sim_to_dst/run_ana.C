void run_ana(const char* slowdst = "mutoo_slowsim.root",
	     char* outdst  = "mutoo_dst.root",
	     char* outnt   = "mutoo_ntuple.root",
	     char* ndst    = "sim_ndst.root")
{

  //  setMapFileFlag(3);  // new flag to have field on North
  //  setMapFileScale(1.0);  
  //  const char* datafile = "mutoo_slowsim.root";
  dfileopen(slowdst);
  dstout_fopen(outdst,ndst,outnt);
  drun(-1);
  end_all();
}



