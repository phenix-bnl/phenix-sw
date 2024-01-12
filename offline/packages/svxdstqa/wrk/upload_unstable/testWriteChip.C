void testWriteChip(char* fname, int run)
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
  
  bool readpixelstat = pmap.readChipMapFromFile(runpixel0, runpixel1 ,pixelfilename);
  cout << "Read chip map from file: " << (readpixelstat ? "success" : "failure") << endl;
  
  bool writepixelstat = pmap.writeChipMapToDatabase(runpixel0);
}
