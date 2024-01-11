
#include <TFile.h>
#include <PHGlobal.h>
#include <TH1.h>
#include "Tools.h"

//______________________________________________________________________
void Tools::dAuCentrality(
  TFile *fclZdcFile, TFile *bbcFile, PHGlobal* &evt, 
  const Float_t TRIGGER_FRACTION, 
  Float_t &bbc, Float_t &fcl, Float_t &zdc)
{
  float z = evt->getZVertex();
  int zb  = int((z+40.)/2.0);
  if (z<-5000)          zb=0; 
  if (z>-5000 && z<-40) zb=1; 
  if (z>-5000 && z>38)  zb=39; 

  char bbcname[80]; sprintf(bbcname,"bbc_%dvertex",zb);
  
  float zdcES=evt->getZdcEnergyS();
  float bbcQS=evt->getBbcChargeS();
  //  float fclSG=evt->get_FclTotlS() ; // to be used with pro49

  TH1F *bbcHisto = static_cast<TH1F*>(bbcFile   ->Get(bbcname));
  //  TH1F *fclHisto= static_cast<TH1F*>(fclZdcFile->Get("fclCal"));
  TH1F *zdcHisto = static_cast<TH1F*>(fclZdcFile->Get("zdcCal"));

  if (bbcHisto) bbc = 100*TRIGGER_FRACTION*(1-flattened(bbcHisto,bbcQS));
  //  if (fclHisto) fcl = 100*TRIGGER_FRACTION*(1-flattened(fclHisto,fclSG));
  if (zdcHisto) zdc = 100*TRIGGER_FRACTION*(1-Tools::flattened(zdcHisto,zdcES));
}




