
void clust_calib_cmd(char* listfile,int start=-1,int stop=-1){
  gSystem->Load("/phenix/u/htorii/lib/libmicrodst.so");
  gSystem->Load("/phenix/u/htorii/lib/libclusttr.so");
  //  gSystem->Load("/phenix/u/htorii/local/photon/clusttr/.libs/libclusttr.so");
  gROOT->LoadMacro("chain_udst.cc");
  chain_udst(listfile,start,stop);
  char outfname[128];
  sprintf(outfname,"%s_%d_%d",listfile,start,stop);
  cout<<" Output file format file is "<<outfname<<endl;
  clust_calib(0,outfname);
};
//

