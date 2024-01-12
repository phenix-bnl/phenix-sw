void run_multifiles(const char *fname = "highpt_udst.root",
		    int maxevents =0,
		    const char *outname = "highPtOut.root",
		    const char *libname = "libanadst.so")	       
{

  bool snglflag;
  gSystem->Load("libemc.so");
  //  gSystem->Load("libsupport.so");
  gSystem->Load("libfun4all.so");
  gSystem->Load("libpi0qa.so");
// load this if you want to run any recalreco modules
  gSystem->Load("libSubsysReco.so");

// gSystem->Load("libemct0recalreco.so");

  // if ndstfile ends with "root"
  // then it is a single file.  otherwise,
  // it is a file with a list of the ndst files

  ifstream infilelist;
  char ndstname[1000];

  int len = strlen( fname );
  if ( (len<4) || (strncmp(fname+len-4,"root",4)==0) )
    {
      snglflag = true;
      strncpy( ndstname, fname, strlen(fname)+1 );
    }
  else
    {
      snglflag = false;
      infilelist.open( fname );
      infilelist >> ndstname;
    }

  Fun4AllServer *se = Fun4AllServer::instance();

  qa_pi0 *Api0 = new qa_pi0(outname); 
  se->registerSubsystem(Api0);
  Fun4AllInputManager *in = new Fun4AllDstInputManager("PWGin","DST");
  se->registerInputManager(in);

  while ( true )
    {
      cout << "processing file " << ndstname << endl;
      se->fileopen("PWGin", ndstname );
      se->run(maxevents);
      se->fileclose("PWGin");

      if ( !snglflag ) infilelist >> ndstname;
      else             break;
      if ( infilelist.eof() == true ) break;
    }

  //  anaexit();// do all your cleanup (save files, etc) here

// obsolete, use se->End();
//  se->EndRun();
  se->End();
  gSystem->Exit(0);
}





