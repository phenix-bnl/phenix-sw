void runplothilo
{
  gSystem->AddIncludePath("-I${OFFLINE_MAIN}/include"); 
  gSystem->Load("libmpc.so");
  gROOT->ProcessLine(".L plothilo.C++");
  
  plothilolimit("mpchilo.root");
  plothilo("mpchilo.root");

  return;
}

