#include<string>
void run_one(int nevnt = 1)
{

  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("BFIELDFLAG", 3);
  setMapFileScale(1.0);

  char* infile = "missed.root";
  char* infile = "mb_jpsi_5may03.root";
  char* outfile = "new_mutoo_dst.proot";  
  char* outfile_ndst = "mutoo_ndst.proot";  
  dfileopen(infile);
  dstout_fopen(outfile, outfile_ndst);
  drun(1);

}

