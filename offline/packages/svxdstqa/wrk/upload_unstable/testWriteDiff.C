void testWriteDiff(char* fname, int run)
{
  gSystem->Load("libpdbcalBase.so");
  gSystem->Load("libPgCal.so");
  gSystem->Load("libPgCalInstance.so");
  gSystem->Load("libsvx.so");
  
  int verbosity = 0;
  SvxPixelHotDeadMap pmap(verbosity);
  
  string pixelfilename(fname);
  
  int runpixel0 = run;
  int runpixel1 = run;
  
  char blank = 127;
  pmap.setBlankStatus(blank);
  bool readpixelstat = pmap.readPixelMapFromFile(runpixel0, runpixel1 ,pixelfilename);
  cout << "Read pixel map from file: " << (readpixelstat ? "success" : "failure") << endl;
  
  const unsigned int NMODULE = 60;
  const unsigned int NROC = 8;
  for(unsigned int mod=0;mod<NMODULE;++mod)
  {
    for(unsigned int roc=0;roc<NROC;++roc)
    {
      bool writepixelstat = pmap.writePixelMapToDatabase(runpixel0,runpixel1,mod,roc);
      cout << "Write pixel map to DB: " << (writepixelstat ? "success" : "failure") << endl;
    }
  }
  
//   bool writepixelstat = pmap.writePixelMapToDatabase(runpixel0,runpixel1,0,0);
//   cout << "Write pixel map to DB: " << (writepixelstat ? "success" : "failure") << endl;
}
