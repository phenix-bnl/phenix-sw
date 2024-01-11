// merge.C
//
void sample(const char *ofname="sample.root"){
  gROOT->Macro("tofCalibByTreeInit.C");
  TreeToHist *ana = new TreeToHist(TofAddress, TofGeometry, TofCalib);
  ana->setTofGlobalT("tofGlobalT_rundep.txt");

  ana->Loop_a_file("tree/run0000010758tree0000.root");
  ana->Loop_a_file("tree/run0000010761tree0000.root");
  ana->Loop_a_file("tree/run0000010761tree0001.root");
  ana->Loop_a_file("tree/run0000010761tree0002.root");
  ana->Loop_a_file("tree/run0000010761tree0003.root");
  ana->Loop_a_file("tree/run0000010761tree0004.root");
  ana->Loop_a_file("tree/run0000010763tree0000.root");
  ana->Loop_a_file("tree/run0000010763tree0001.root");
  ana->Loop_a_file("tree/run0000010763tree0002.root");
  ana->Loop_a_file("tree/run0000010763tree0003.root");
  ana->Loop_a_file("tree/run0000010765tree0000.root");
  ana->Loop_a_file("tree/run0000010767tree0000.root");
  ana->Loop_a_file("tree/run0000010767tree0001.root");
  ana->Loop_a_file("tree/run0000010767tree0003.root");
  ana->Loop_a_file("tree/run0000010767tree0004.root");
  ana->Loop_a_file("tree/run0000010767tree0005.root");
  ana->Loop_a_file("tree/run0000010767tree0006.root");
  ana->Loop_a_file("tree/run0000010767tree0007.root");
  ana->Loop_a_file("tree/run0000010767tree0008.root");
  ana->Loop_a_file("tree/run0000010902tree0000.root");
  ana->Loop_a_file("tree/run0000010902tree0001.root");
  ana->Loop_a_file("tree/run0000010902tree0002.root");
  ana->Loop_a_file("tree/run0000010902tree0003.root");
  ana->Loop_a_file("tree/run0000010902tree0004.root");
  ana->Loop_a_file("tree/run0000010907tree0000.root");
  ana->Loop_a_file("tree/run0000010909tree0000.root");
  ana->Loop_a_file("tree/run0000010927tree0000.root");
  ana->Loop_a_file("tree/run0000010927tree0001.root");
  ana->Loop_a_file("tree/run0000010927tree0002.root");
  ana->Loop_a_file("tree/run0000010927tree0003.root");
  ana->Loop_a_file("tree/run0000010938tree0000.root");
  ana->Loop_a_file("tree/run0000010938tree0001.root");
  ana->Loop_a_file("tree/run0000010948tree0000.root");
  ana->Loop_a_file("tree/run0000010948tree0001.root");
  ana->Loop_a_file("tree/run0000010948tree0002.root");
  ana->Loop_a_file("tree/run0000010948tree0004.root");
  ana->Loop_a_file("tree/run0000010948tree0005.root");
  ana->Loop_a_file("tree/run0000010948tree0006.root");
  ana->Loop_a_file("tree/run0000010948tree0007.root");
  ana->Loop_a_file("tree/run0000010948tree0008.root");

  ana->Write(ofname);
}
