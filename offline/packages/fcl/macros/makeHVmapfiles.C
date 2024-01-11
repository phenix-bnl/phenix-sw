void makeHVmapfiles()
{
  // makeHVmapfiles.C macro uses the FclIndexer to create 
  // configuration files for the FCAL lv script in
  // /home/phenix/haggerty/dac/lv
  // The output files of this macro should be placed in
  // /home/phenix/haggerty/dac/src and the lv script must be
  // modified to point to these.

  //#nominal
  //set inputFile(0,2,0) "s1_1000v.dat"
  //set inputFile(0,2,1) "s2_1000v.dat"
  //set inputFile(0,2,2) "s3_1000v.dat"
  //
  //#nominal
  //set inputFile(0,2,0) "south_run5_1.dat"
  //set inputFile(0,2,1) "south_run5_2.dat"
  //set inputFile(0,2,2) "south_run5_3.dat"
 
  // Version 1 sets the HV of columns 0-2 to 850V
  // and columns 3-8 1200V

  gSystem->Load("libfun4all.so");
  gSystem->Load("libfcl.so");
 
  FclIndexer* fcli = FclIndexer::Instance();
  
  double LVNorth[96];
  double LVSouth[96];

  for(int i = 0; i < 96; i++)
  {
    LVNorth[i] = 0;
    LVSouth[i] = 0;
  }
  
  int tlv;
  int lvidx;
  int highval = 1200;
  int lowval = 850;

  for(int ir = 0; ir < 10; ir++)
    for(int ic = 0; ic < 9; ic++)
    {
      tlv = ic<3 ? lowval : highval;

      lvidx = fcli->getLVNorth(ir,ic);
      if(-1 < lvidx && lvidx<96)
	LVNorth[lvidx] = tlv;
      lvidx = fcli->getLVSouth(ir,ic);
      if(-1 < lvidx && lvidx<96)
	LVSouth[lvidx] = tlv;
      

    }
  

  //Corrections
  LVSouth[55] = 850;

  //South Background Counters
  LVSouth[90] = float(0xbb5)/4095.0*2000.0;
  LVSouth[91] = float(0xa3f)/4095.0*2000.0;
  LVSouth[92] = float(0xc00)/4095.0*2000.0;
  LVSouth[93] = float(0xa66)/4095.0*2000.0;
  LVSouth[94] = float(0xaa6)/4095.0*2000.0;
  LVSouth[95] = float(0x866)/4095.0*2000.0;

  //North Background Counters
  LVNorth[90] = float(0xb85)/4095.0*2000.0;
  LVNorth[91] = float(0xc10)/4095.0*2000.0;
  LVNorth[92] = float(0xc00)/4095.0*2000.0;
  LVNorth[93] = float(0x932)/4095.0*2000.0;
  LVNorth[94] = float(0xb25)/4095.0*2000.0;
  LVNorth[95] = float(0x9ff)/4095.0*2000.0;

  //printarray
  std::cout<<"Chan\tSouth\tNorth\n";

  for(int ichan = 0; ichan < 96; ichan++)
  {
    std::cout<<ichan<<"\t"<<LVSouth[ichan]<<"\t"<<LVNorth[ichan]<<std::endl;
  }


  int ifile = 0;
  char fname[1024];
  char hvhex[1024];

  for(int ifile = 0; ifile < 3; ifile ++)
  {
    sprintf(fname,"north_run5_%d.dat",ifile+1);
    ofstream nfile(fname);
    nfile<<"c8\n";

    sprintf(fname,"south_run5_%d.dat",ifile+1);
    ofstream sfile(fname);
    sfile<<"c8\n";

    for(int ichan = ifile*32; ichan < (ifile+1)*32; ichan++)
    {
      sprintf(hvhex,"%x\n",LVNorth[ichan]/2000.0*4095.0 );
      nfile<<hvhex;

      sprintf(hvhex,"%x\n",LVSouth[ichan]/2000.0*4095.0 );
      sfile<<hvhex;
    }
  }
  return;
}
