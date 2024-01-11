
//
// Small macro used to get gain average values for PbSc sectors
// for a given run.
// see emcCalibrationDataHelper class for meaning of details param.
//
// Here's the result from this macro, executed on 25-March-2004
// using runnumber 92446 (last good Run3 pp run)
// trim(92446,"xmax:-3:-9:-15") gives :
//
//
// Trimmed mean results for run 92446 details=xmax:-3:-9:-15
//
//   Trim percent    0.00          10.00          20.00          30.00          40.00          45.00          47.50        
// W0              147.85 +- 0.61 146.84 +- 0.49 146.30 +- 0.38 146.01 +- 0.26 145.78 +- 0.13 145.67 +- 0.08 145.55 +- 0.04
// W1              155.23 +- 0.80 151.95 +- 0.51 151.54 +- 0.39 151.24 +- 0.27 151.13 +- 0.13 151.10 +- 0.08 151.02 +- 0.04
// W2              142.10 +- 0.78 139.72 +- 0.58 138.82 +- 0.45 138.41 +- 0.32 137.77 +- 0.17 137.53 +- 0.10 137.42 +- 0.05
// W3              127.58 +- 1.22 123.37 +- 0.71 122.91 +- 0.49 122.46 +- 0.33 122.47 +- 0.17 122.45 +- 0.10 122.49 +- 0.05
// E2              131.15 +- 0.65 129.25 +- 0.52 128.37 +- 0.39 127.77 +- 0.26 127.43 +- 0.14 127.45 +- 0.09 127.46 +- 0.05
// E3              129.74 +- 0.77 126.85 +- 0.58 125.10 +- 0.43 124.26 +- 0.28 124.03 +- 0.15 124.06 +- 0.09 124.09 +- 0.04

struct result
{
  float value;
  float error;
};

void trim(int runnumber, const char* details="xmin:-3:-9:-15")
{
  gSystem->Load("libPgCalInstance.so");
  gSystem->Load("libemcOMpg.so");
  emcCalibrationDataHelper cdh(runnumber,false);

  cdh.getGainBaseLine(0);
  
  const int N = 7;

  float percent[N] = { 0,0.1,0.2,0.3,0.4,0.45,0.475 };
  result R[6][N];

  char* mdetails = new char[strlen(details)+10];

  for ( int isector = 0; isector < 6; ++isector )
    {
      for ( int i = 0; i < 7; ++i ) 
	{
	  sprintf(mdetails,"%2d:%s",percent[i]*100,details);

	  R[isector][i].value = cdh.getGainBaseLine(isector,"value",mdetails),
	  R[isector][i].error = cdh.getGainBaseLine(isector,"error",mdetails);
	}
      printf("\n");
    }


  printf("Trimmed mean results for run %5d details=%s\n\n",
	 runnumber,details);

  printf("  Trim percent ");
  for ( int i = 0; i < 7; ++i ) 
    {
      printf("%7.2f %7c",percent[i]*100,' ');
    }
  printf("\n");

  for ( int isector = 0; isector < 6; ++isector )
    {
      printf("%s             ",EmcIndexer::EmcSectorId(isector));
    
      for ( int i = 0; i < 7; ++i ) 
	{
	  printf("%7.2f +-%5.2f ",
		 R[isector][i].value,
		 R[isector][i].error);
	}
      printf("\n");
    }

  

}
