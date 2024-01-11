#include<string>
void run_ana_mut_dst(int nevnt = 1)
{
  char* infile = "/phenix/data25/phnxreco/run2_pp_zeroField2/dsts/DST_run2_pp_zeroField2-0000040321-0000.proot";
  char* outfile = "test1.root";
  dfileopen(infile);
  dstout_fopen(outfile);
  drun(nevnt);
  //  end_all();
}

