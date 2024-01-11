void run_multifiles(const char *fname = "highpt_udst.root",
		    int maxevents =0)
{

  bool snglflag;

  gSystem->Load("./install/lib/libtrigger.so");
  gSystem->Load("./libanadst_trighelp.so");


  maxevents = 4000;
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

  cout << "___thtest___ running " << maxevents << " events on all files" << endl;  
  while ( true )
    {
      cout << "___thtest___ processing file " << ndstname << endl;
      anainit();
      dfileopen( ndstname );
      drun(maxevents);
      dclose();
      anaexit();

      if ( !snglflag ) infilelist >> ndstname;
      else             break;
      if ( infilelist.eof() == true ) break;
    }

  gSystem->Exit(0);
}





