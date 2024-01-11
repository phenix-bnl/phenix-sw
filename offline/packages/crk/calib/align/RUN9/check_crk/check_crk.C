#include "check_crk.h"

using namespace std;
using namespace findNode;

check_crk::check_crk(const char* outfile) : SubsysReco("check_crk")
{
  strcpy(OutFileName, outfile);
  cout <<" Tree will be saved to : " << OutFileName << endl;
}

int check_crk::Init(PHCompositeNode *topNode)
{
  fout = new TFile(OutFileName,"recreate");

  tree = new TTree("tree", "tree DST");
  tree->Branch("npmt", &npmt, "npmt/I");
  tree->Branch("pmt", pmt, "pmt[npmt]/S");
  tree->Branch("npe", npe, "npe[npmt]/F");

  hist_WS = new TH1F("npe_WS","npe_WS",115,0,115);
  hist_WN = new TH1F("npe_WN","npe_WN",115,0,115);
  hist_ES = new TH1F("npe_ES","npe_ES",115,0,115);
  hist_EN = new TH1F("npe_EN","npe_EN",115,0,115);

  cout << "Complete Initialization" << endl;

  return 0;
}

int check_crk::process_event(PHCompositeNode *topNode)
{
  // informational message...
  static int ncalls = 0;
  if (ncalls%1000 == 0) cout << "RICHAlignment_track::process_event Ncalls = "
                             << ncalls << endl;

  //  GetNodes(topNode);

  CrkHit *d_crk = getClass<CrkHit>(topNode, "CrkHit");

  npmt = 0;
  int totnhit = (int)d_crk->get_CrkNHit();
  
  for(int ihit=0; ihit<totnhit; ihit++)
    {
      pmt[npmt] = d_crk->get_pmt(ihit);
      npe[npmt] = d_crk->get_npe(ihit);
      PMT = pmt[npmt];
      NPE = npe[npmt];

      if(PMT<1280) hist_WS->Fill(NPE);
      if(1280*1<=PMT && PMT<1280*2) hist_WN->Fill(NPE);
      if(1280*2<=PMT && PMT<1280*3) hist_ES->Fill(NPE);
      if(1280*3<=PMT && PMT<1280*4) hist_EN->Fill(NPE);	
      
      npmt++;
    }
  tree->Fill();
  ncalls++;
  return 0;
}

int check_crk::End(PHCompositeNode *topNode)
{
  fout->cd();
  tree->Write();
  hist_WS->Write();
  hist_WN->Write();
  hist_ES->Write();
  hist_EN->Write();
  fout->Close();

  return 0;
}
/*
void check_crk::GetNodes(PHCompositeNode *topNode)
{
  //CrkHit
  PHTypedNodeIterator<CrkHit> PHCiter(topNode);
  PHCNode_t *PHCNode = PHCiter.find("CrkHit");
  if (PHCNode) d_crk = PHCNode->getData();
  if (!d_crk) cout << "CrkHit not found" << endl;
}
*/
