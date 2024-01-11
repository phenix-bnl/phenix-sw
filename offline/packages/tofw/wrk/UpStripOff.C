void UpStripOff()
{
  gSystem->Load("libPgCalInstance.so");
  gSystem->Load("libtofw.so");

  TofwCalib *tofwcalib=new TofwCalib();
  tofwcalib->set_debug(1);

  bool getdb;
  getdb=tofwcalib->fetchstripoff(230079);
}
