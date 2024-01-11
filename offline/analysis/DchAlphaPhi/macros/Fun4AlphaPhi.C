void Fun4AlphaPhi(char* infile = "dst.root",char *outfile = "histos.root")
{

  //Introduce ourself to the human
  std::cout << "Fun4AlphaPhi " << outfile;
  std::cout << " - inputfile: " << infile << std::endl;
   
  //-----------------Libraries
  gSystem->Load("libfun4allfuncs.so");  
  gSystem->Load("DchAlphaPhi.so");
 
  //-----------------Fun4All server

  Fun4AllServer *se = Fun4AllServer::instance();    

  Fun4AllHistoManager *hm = new Fun4AllHistoManager("hm");
  hm->setOutfileName(outfile);
  DchAlphaPhi *tr = new DchAlphaPhi(hm);
  se->registerSubsystem(tr); 

  //-----------------Fun4All server

  Fun4AllDstInputManager *in1 = new Fun4AllDstInputManager ("DSTin1", "DST");
  se->registerInputManager (in1);
  in1->AddFile(infile);
 
  se->run();
  se->End(); 
}


