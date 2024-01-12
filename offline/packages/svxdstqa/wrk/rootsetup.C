{
   gROOT->ProcessLine(TString::Format(".include %s/include", gSystem->Getenv("OFFLINE_MAIN")));
   gROOT->ProcessLine(TString::Format(".include ../"));

   gSystem->Load("libfun4all.so");
   gSystem->Load("libfun4allfuncs.so");
   gSystem->Load("libTHmul.so");
   gSystem->Load("libSvxDstQA.so");

}
