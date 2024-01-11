// This macro handle setting all of the output nodes for the various types 
// of output files.

#include <stdio.h> 
#include <time.h> 

void DST_MRPC_MinBias_IOManager(const char *dst_MRPC_MinBias_file = "DST_MRPC_MinBias.root")
{
  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllDstOutputManager *dst_MRPC_MinBias_Manager  = new Fun4AllDstOutputManager("DST_MRPC_MinBias_OUT",  dst_MRPC_MinBias_file);

  dst_MRPC_MinBias_Manager->AddEventSelector("MB");
  dst_MRPC_MinBias_Manager->AddNode("EventHeader");
  dst_MRPC_MinBias_Manager->AddNode("TrigLvl1");
  dst_MRPC_MinBias_Manager->AddNode("T0Out");
  dst_MRPC_MinBias_Manager->AddNode("VtxOut");
  dst_MRPC_MinBias_Manager->AddNode("BbcOut");
  dst_MRPC_MinBias_Manager->AddNode("BbcRaw");
  dst_MRPC_MinBias_Manager->AddNode("MrpcRaw");
  dst_MRPC_MinBias_Manager->AddNode("MrpcHit");
  dst_MRPC_MinBias_Manager->AddNode("Sync");

  se->registerOutputManager(dst_MRPC_MinBias_Manager);
}


int setProcObject()
{
  const char *inFile                    = gSystem->Getenv("PRDFNAME");
  const char *DST_MRPC_MinBias_File     = gSystem->Getenv("DST_MRPC_MinBias_NAME");

  // need add after DST Slicing
    
  FILE *f;
  char catfilename[100]; 
  sprintf(catfilename,"%s.txt",inFile);
  cout << "Saving process object: "<< catfilename << endl;
  cout << "input file  : " << inFile   << endl;
  cout << "DST_MRPC_MinBias file : " << DST_MRPC_MinBias_File  << endl;

  time_t rawtime; 
  struct tm * timeinfo; 
  time ( &rawtime ); 
  timeinfo = localtime( &rawtime ); 
  
  f = fopen(catfilename,"a");
  fprintf(f,"%s",asctime(timeinfo) ); 
  fprintf(f,"%s\n",inFile);
  fprintf(f,"%s\n",DST_MRPC_MinBias_File);
  
  fclose(f);
  return 0;
  
  
  else {
    cout << "One of the ENV is missing : PLEASE Check all ENV values in the farmer scripts or setup.csh" << endl;
  }
  return -1;
}
