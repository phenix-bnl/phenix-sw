{
  gSystem->Load("libpdbcalBase.so");
  gSystem->Load("libPgCal.so");
  gSystem->Load("libPgCalInstance.so");
  gSystem->Load("libsvx.so");
  
  int verbosity = 0;
  SvxPixelHotDeadMap pmap(verbosity);
  
  string pixelfilename("/direct/phenix+hhj2/dcm07e/vtx/deadmaps_Run14HeAu200/pixel_reference_deadmap_run14he3au.dat");
  
  int runpixel0 = 415751;
  int runpixel1 = 416893;
  
  bool readpixelstat = pmap.readPixelMapFromFile(runpixel0, runpixel1 ,pixelfilename);
  cout << "Read pixel map from file: " << (readpixelstat ? "success" : "failure") << endl;
  
  pmap.printBadPixels(0, 0);
  const unsigned int NMODULE = 60;
  const unsigned int NROC = 8;
  for(unsigned int mod=0;mod<NMODULE;++mod)
  {
    for(unsigned int roc=0;roc<NROC;++roc)
    {
      bool writepixelstat = pmap.writeReferencePixelMapToDatabase(runpixel0,runpixel1,mod,roc);
      cout << "Write pixel map to DB: " << (writepixelstat ? "success" : "failure") << endl;
    }
  }
  
//   bool writepixelstat = pmap.writeReferencePixelMapToDatabase(runpixel0,runpixel1,0,3);
//   cout << "Write pixel map to DB: " << (writepixelstat ? "success" : "failure") << endl;
}
