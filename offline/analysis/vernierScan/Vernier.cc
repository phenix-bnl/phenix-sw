#include "Vernier.h"

//General PHENIX tools
#include <phool.h>
#include <getClass.h>
#include <PHCompositeNode.h>
#include <Fun4AllServer.h>
#include <Fun4AllHistoManager.h>
//Data classes I am using in analysis
#include <PHGlobal.h>
#include <PreviousEvent.h>
#include <SpinDataEventOutv2.h>
#include <SpinEvent.h>
#include <BbcOutv1.h>
#include <BbcRaw.h>
#include <ZdcOutv2.h>
#include <ZdcRaw.h>
#include <TrigLvl1.h>

//Root header files
#include <TFile.h>
#include <TTree.h>
#include <TH1.h>

using namespace std;
using namespace findNode;

const double Vernier::historange = 25000.;

// ================================================================
Vernier::Vernier(const string &outfile) : 
  SubsysReco ("Vernier"),
  OutFileName(outfile),
  fout(NULL),
  finpol(NULL),
  Tout(NULL),
  cross_id(-1),
  trigraw(-1),
  trigscaled(-1),
  evtnumber(0),
  bbc_z(NAN),
  zdcll1_z(NAN)
{
  return ;
}

//================================================================================
//                         Init
//================================================================================
int Vernier::Init(PHCompositeNode *topNode)
{  
 
  //=============== output file =====================================
  fout = new TFile(OutFileName.c_str(),"RECREATE", "Trig Scalars");

  //================== TTree =========================================
  
  Tout = new TTree("Tout","TTree with vertex information");
  
  Tout->Branch("cross_id",     &cross_id,       "cross_id/I");
  Tout->Branch("evtnumber",    &evtnumber,      "evtnumber/I");
  Tout->Branch("trigraw",      &trigraw,        "trigraw/I");
  Tout->Branch("triglive",     &triglive,       "triglive/I");
  Tout->Branch("trigscaled",   &trigscaled,     "trigscaled/I");
  Tout->Branch("bbc_z",        &bbc_z,          "bbc_z/D");
  Tout->Branch("zdcll1_z",     &zdcll1_z,       "zdcll1_z/D");
  
  //====================== histograms ==================================
  sprintf (namestring, "BBC_novtxcut_Z");
  BBC_novtxcut_Z    = new TH1F(namestring, namestring, 1200, -300.0, 300.0);
  sprintf (namestring, "BBC_Z");
  BBC_Z    = new TH1F(namestring, namestring, 1200, -300.0, 300.0);  
  sprintf (namestring, "BBC_narrow_Z");
  BBC_narrow_Z      = new TH1F(namestring, namestring, 1200, -300.0, 300.0);  
  sprintf (namestring, "ZDC_wide_Z");
  ZDC_wide_Z        = new TH1F(namestring, namestring, 1200, -300.0, 300.0);
  sprintf (namestring, "ZDC_narrow_Z");
  ZDC_narrow_Z      = new TH1F(namestring, namestring, 1200, -300.0, 300.0);

  BBC_RATE         = new TH1F("BBC_RATE",       "Crossing combined BBC rate",    asize, 0.0, historange);
  BBCNARROW_RATE   = new TH1F("BBCNARROW_RATE","Crossing combined BBCNARROW rate",    asize, 0.0, historange);
  ZDCNARROW_RATE   = new TH1F("ZDCNARROW_RATE", "Crossing combined ZDCNARROW rate", asize, 0.0, historange);
  ZDCWIDE_RATE     = new TH1F("ZDCWIDE_RATE",   "Crossing combined ZDCWIDE rate",  asize, 0.0, historange);

  for(int k=0;k<120;k++)
    {
      sprintf (namestring, "BBC_RATE%d",k);
      BBCRATE[k]          = new TH1F(namestring, namestring, histosize, 0.0, historange);
   
      sprintf (namestring, "BBC_GL1P%d",k);
      BBC_gl1p[k]             = new TH1F(namestring, namestring, histosize, 0.0, historange); 

      sprintf (namestring, "BBCNARROW_RATE%d",k);
      BBCNARROWRATE[k]          = new TH1F(namestring, namestring, histosize, 0.0, historange); 

      sprintf (namestring, "BBCNARROW_GL1P%d",k);
      BBCNARROW_gl1p[k]             = new TH1F(namestring, namestring, histosize, 0.0, historange);
      
      sprintf (namestring, "ZDCNARROW_RATE%d",k);
      ZDCNARROWRATE[k]       = new TH1F(namestring, namestring, histosize, 0.0, historange);
      
      sprintf (namestring, "ZDCNARROW_GL1P%d",k);
      ZDCNARROW_gl1p[k]          = new TH1F(namestring, namestring, histosize, 0.0, historange); 

      sprintf (namestring, "ZDCWIDE_RATE%d",k);
      ZDCWIDERATE[k]        = new TH1F(namestring, namestring, histosize, 0.0, historange);
    
      sprintf (namestring, "ZDCWIDE_GL1P%d",k);
      ZDCWIDE_gl1p[k]           = new TH1F(namestring, namestring, histosize, 0.0, historange); 

      sprintf (namestring, "Clock_GL1P%d",k);
      Clock_gl1p[k]           = new TH1F(namestring, namestring, histosize, 0.0, historange);    
    }

  BBCgl1p             = new TH1F("BBCgl1p",      "BBCgl1p",       histosize, 0.0, historange);
  BBCnarrowgl1p       = new TH1F("BBCnarrowgl1p", "BBCnarrowgl1p", histosize, 0.0, historange);
  Zdcwidegl1p         = new TH1F("Zdcwidegl1p",  "Zdcwidegl1p",   histosize, 0.0, historange);
  ZDCnarrowgl1p       = new TH1F("ZDCnarrowgl1p","ZDCnarrowgl1p", histosize, 0.0, historange);
  Clockgl1p           = new TH1F("Clockgl1p",    "Clockgl1p",     histosize, 0.0, historange);

  //================== intializing variable =============================
  ncalls           = 0;
  bbc_tot          = 0;
  bbcnarrow_tot    = 0;
  zdcnarrow_tot    = 0;
  zdcwide_tot      = 0;

  for(int l = 0; l < 120; l++)
    {
      for(int j = 0; j < asize; j++)
	{
	  bbc_a[l][j]=0;
	  bbcnarrow_a[l][j]=0;
	  clock_a[l][j]=0;
	  zdcnarrow_a[l][j]=0;
	  zdcwide_a[l][j]=0;  
	}
    }

  cout << " " << endl;
  cout << "Initializing variables." << endl;//debug
  cout << " " << endl;
  return 0;
}

//======================================================================
//                 Process Event
//======================================================================
int Vernier::process_event(PHCompositeNode *topNode)
{   
  //============== counting no. of events ===============
  ncalls++;
  if((ncalls % 100000) == 0)
  {
    cout << "No of events processed:  = " << ncalls << endl;
  }

  // ============= reading required data nodes =========
  SpinDataEventOut *d_sde = getClass<SpinDataEventOutv2> (topNode,"SpinDataEventOut");
  if (!d_sde)
    {
      cout << PHWHERE << "Dude, what are you doing?  Your nodes are not in the tree." << endl;
      return 0;
    }
  TrigLvl1 *d_trig = getClass<TrigLvl1>(topNode,"TrigLvl1");
  if(!d_trig)
    {
      cout << PHWHERE << "TrigLevel1 Node not in the tree" << endl;
      return 0;
    }
  BbcOut *bbcout = getClass<BbcOut> (topNode, "BbcOut");
  if (!bbcout) 
    {
      cout<<"BbcOut does not exist"<<endl;
      return 0;
    }
  ZdcOut *zdcout = getClass<ZdcOut> (topNode, "ZdcOut");
  if (!zdcout) 
    {
      cout<<"ZdcOut does not exist"<<endl;
      return 0;
    }

  // ======= getting event by event info =======================
  /*
    For RUN5 & RUN6:
    GL1P(board number, scaler type)
    all on board 0
    type 0 = BBCLL1
    type 1 = Clock
    type 2 = ZDCLL1
    type 3 = ZDCNS

    For RUN8:
    all on board 0
    type 0 = BBCLL1
    type 1 = Clock
    type 2 = ZDCNS
    type 3 = ZDCLL1narrow

    For Run13, Run15 
    board 0
    0 = BBCll1
    1 = BBCLL1 narrow
    2 = ZDCLL1 narrow
    3 = ZDCLL1 wide

    board 1
    1 = Clock **verify

  */

  bbc_zvtx       = bbcout->get_VertexPoint();//which trigger?
  zdc_zvtx       = zdcout->get_Zvertex();//which trigger?

  eventsequence  = d_sde->GetEventSequence();
  gl1crossingID  = d_sde->GetGL1PCrossingID(0);

  gl1clock       = d_sde->GetGL1PScalerCount(1,3);//check
  bbcll1         = d_sde->GetGL1PScalerCount(0,0);
  bbcnarrow      = d_sde->GetGL1PScalerCount(0,1);
  zdcnarrow      = d_sde->GetGL1PScalerCount(0,2);
  zdcwide        = d_sde->GetGL1PScalerCount(0,3);

  cross_id      = gl1crossingID;
  evtnumber     = eventsequence;
  bbc_z         = bbc_zvtx;
  zdcll1_z      = zdc_zvtx;

  trigscaled    = d_trig->get_lvl1_trigscaled();
  triglive      = d_trig->get_lvl1_triglive();
  trigraw       = d_trig->get_lvl1_trigraw();

  //=======================================================
  if(gl1crossingID >= 120)
    {
      cout << "crossing ID out of bound: " << gl1crossingID << endl;
      return 0;
    }
  ievent = static_cast<int>(eventsequence/1000.0);

  clock_a[gl1crossingID][ievent]     += gl1clock;
  bbc_a[gl1crossingID][ievent]       += bbcll1;
  bbcnarrow_a[gl1crossingID][ievent] += bbcnarrow;
  zdcwide_a[gl1crossingID][ievent]   += zdcwide;
  zdcnarrow_a[gl1crossingID][ievent] += zdcnarrow;

  bbc_tot       += bbcll1;
  bbcnarrow_tot += bbcnarrow;
  zdcnarrow_tot += zdcnarrow;
  zdcwide_tot   += zdcwide;
  clock_tot     += gl1clock;
  
  //================== filling TTree ======================= 
  Tout->Fill(); 

  //======== filling histo's for vtx distribution for diff. triggers ==========
  if(fabs(bbc_zvtx) <= 300.0)
    {
      if(trig_bbcnovtxcut(trigscaled)== true)
	BBC_novtxcut_Z->Fill(bbc_zvtx);
      if(trig_bbcnarrow(trigscaled) == true)
	BBC_narrow_Z->Fill(bbc_zvtx);
    }
  if(fabs(zdc_zvtx) <= 300.0)
    {
      if(trig_zdcwide(trigscaled) == true)
	ZDC_wide_Z->Fill(zdc_zvtx);
      if(trig_zdcnarrow(trigscaled) == true)
	ZDC_narrow_Z->Fill(zdc_zvtx);
    }

  return 0;
}

//========================================================================================================
//                                    End
//========================================================================================================
int Vernier::End(PHCompositeNode *topNode)
{
  // ============ Filling Histos =====================================
 for(int i = 0; i < asize; i++)
    { 
      for(int k=0;k<120;k++)
	{
	  //=== filling histogram with scalers for each bunch crossing ID ===
	  Clock_gl1p[k]->Fill(i, static_cast<double>(clock_a[k][i]));
	  BBC_gl1p[k]->Fill(i, static_cast<double>(bbc_a[k][i]));
	  BBCNARROW_gl1p[k]->Fill(i, static_cast<double>(bbcnarrow_a[k][i]));
	  ZDCNARROW_gl1p[k]->Fill(i, static_cast<double>(zdcnarrow_a[k][i]));
	  ZDCWIDE_gl1p[k]->Fill(i, static_cast<double>(zdcwide_a[k][i]));

	  Clockgl1p->Fill(i, static_cast<double>(clock_a[k][i]));
	  BBCgl1p->Fill(i, static_cast<double>(bbc_a[k][i]));
	  BBCnarrowgl1p->Fill(i, static_cast<double>(bbcnarrow_a[k][i]));
	  ZDCnarrowgl1p->Fill(i, static_cast<double>(zdcnarrow_a[k][i]));
	  Zdcwidegl1p->Fill(i, static_cast<double>(zdcwide_a[k][i]));

	  if(clock_a[k][i] > 0)
	    {
	      //=== filling histogram with rates for each bunch crossing ID ===
	      BBCRATE[k]->Fill(i, static_cast<double>(bbc_a[k][i])/static_cast<double>(clock_a[k][i]));
	      BBCNARROWRATE[k]->Fill(i, static_cast<double>(bbcnarrow_a[k][i])/static_cast<double>(clock_a[k][i]));
	      ZDCNARROWRATE[k]->Fill(i, static_cast<double>(zdcnarrow_a[k][i])/static_cast<double>(clock_a[k][i]));
	      ZDCWIDERATE[k]->Fill(i, static_cast<double>(zdcwide_a[k][i])/static_cast<double>(clock_a[k][i]));

	      //collecting the total rate summing over all bunch crossings 
	      BBC_RATE->Fill(i, static_cast<double>(bbc_a[k][i])/static_cast<double>(clock_a[k][i]));
	      BBCNARROW_RATE->Fill(i, static_cast<double>(bbcnarrow_a[k][i])/static_cast<double>(clock_a[k][i]));
	      ZDCNARROW_RATE->Fill(i, static_cast<double>(zdcnarrow_a[k][i])/static_cast<double>(clock_a[k][i]));
	      ZDCWIDE_RATE->Fill(i, static_cast<double>(zdcwide_a[k][i])/static_cast<double>(clock_a[k][i]));
	    }
	}
    } 


  fout->Write();  
  fout->Close();

  cout << " " << endl;
  cout << "Writing Output File." << endl;//debug
  cout << " " << endl;  
  return 0;
}

//============= User Defined Functions ===============

bool Vernier::trig_bbcnovtxcut(const int trig_pattern)
{
  if((trig_pattern&0x00000002) > 0)
    return true;
    else
      return false;
}

bool Vernier::trig_bbc(const int trig_pattern)
{
  if((trig_pattern&0x00000001) > 0)
    return true;
    else
      return false;
}

bool Vernier::trig_bbcnarrow(const int trig_pattern)
{
  if((trig_pattern&0x00000010) > 0)
    return true;
  else 
    return false;
}

bool Vernier::trig_zdcwide(const int trig_pattern)
{
  if((trig_pattern&0x08000004) > 0)
    return true;
  else
    return false;
}

bool Vernier::trig_zdcnarrow(const int trig_pattern)
{
  if((trig_pattern&0x00000020) > 0)
    return true;
  else
    return false;
}

