void zdc2tree(const char *fname, const int pass = 0, const int nevent = 0, const char *ztreefname = "zdctree.root")
{
  gSystem->Load("libPISARoot.so");
  gSystem->Load("libcalibzdc.so");
  gSystem->Load("libzdc.so");
  gSystem->Load("libbbc.so");
  gSystem->Load("libtriggerUtility.so");

  cout << " MY START PROCESSING OF EVENTS" << endl;
  zdcprdf2tree(fname, pass, nevent, ztreefname);
}
