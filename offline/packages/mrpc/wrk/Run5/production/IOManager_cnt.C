// This macro handle setting all of the output nodes for the various types 
// of output files.

#include <stdio.h> 
#include <time.h> 

void DST_CNT_MinBias_IOManager(const char *dst_CNT_MinBias_file = "DST_CNT_MinBias.root")
{
  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllDstOutputManager *dst_CNT_MinBias_Manager  = new Fun4AllDstOutputManager("DST_CNT_MinBias_OUT",  dst_CNT_MinBias_file);

  // CNT
  dst_CNT_MinBias_Manager->AddEventSelector("MB");
  dst_CNT_MinBias_Manager->AddNode("PHCentralTrack");
  dst_CNT_MinBias_Manager->AddNode("PHGlobal");
  dst_CNT_MinBias_Manager->AddNode("TrigLvl1");
  dst_CNT_MinBias_Manager->AddNode("EventHeader");
  // CGL & DCH track
  dst_CNT_MinBias_Manager->AddNode("CglTrack");
  dst_CNT_MinBias_Manager->AddNode("DchTrack");
  dst_CNT_MinBias_Manager->AddNode("PHDchTrackOut");
  dst_CNT_MinBias_Manager->AddNode("PHTrackOut");
  // MRPC
  dst_CNT_MinBias_Manager->AddNode("MrpcRaw");
  dst_CNT_MinBias_Manager->AddNode("MrpcHit");
  // SYNC
  dst_CNT_MinBias_Manager->AddNode("Sync");

  se->registerOutputManager(dst_CNT_MinBias_Manager);
}


int setProcObject()
{
  const char *inFile                    = gSystem->Getenv("PRDFNAME");
  const char *DST_CNT_MinBias_File     = gSystem->Getenv("DST_CNT_MinBias_NAME");

  // need add after DST Slicing
    
  FILE *f;
  char catfilename[100]; 
  sprintf(catfilename,"%s.txt",inFile);
  cout << "Saving process object: "<< catfilename << endl;
  cout << "input file  : " << inFile   << endl;
  cout << "DST_CNT_MinBias file : " << DST_CNT_MinBias_File  << endl;

  time_t rawtime; 
  struct tm * timeinfo; 
  time ( &rawtime ); 
  timeinfo = localtime( &rawtime ); 
  
  f = fopen(catfilename,"a");
  fprintf(f,"%s",asctime(timeinfo) ); 
  fprintf(f,"%s\n",inFile);
  fprintf(f,"%s\n",DST_CNT_MinBias_File);
  
  fclose(f);
  return 0;
  
  
  else {
    cout << "One of the ENV is missing : PLEASE Check all ENV values in the farmer scripts or setup.csh" << endl;
  }
  return -1;
}
