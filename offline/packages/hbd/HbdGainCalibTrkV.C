#include <HbdGainCalibTrkV.h>
#include <SubsysReco.h>

#include <Event.h>
#include <EventTypes.h>
#include <RunHeader.h>
#include <PHCompositeNode.h>
#include <getClass.h>
#include <PHAngle.h>
#include <PHTimeStamp.h>
#include "HbdBlobList.h"
#include "PHCentralTrack.h"
#include "PHSnglCentralTrack.h"
#include <hbdAdcCalib.hh>
#include <recoConsts.h>
#include <Fun4AllServer.h>

#include <TH1.h>
#include <TF1.h>

#include <cmath>
#include <cstdlib>
#include <iostream>

using namespace std;
using namespace findNode;

HbdGainCalibTrkV::HbdGainCalibTrkV(const string &Name): SubsysReco(Name)
{

  //
  //  Reasonable defaults...
  //
  committopdbcal     = 0;     //dont commit is the default
  commitsuccess      = 0;     //1 if constants successfully commited to database
  verificationstatus = 0; //set to failure as default
  evtcounter         = 0;
  alldone            = 0;
  BlobList           = 0;
  fexpo = 0;

//  pdbcaltables.push_back("calibhbdadc"); // pdbcal table name
}

HbdGainCalibTrkV::~HbdGainCalibTrkV()
{
  delete fexpo;
  return;
}

int HbdGainCalibTrkV::Init(PHCompositeNode *topNode)
{
  char name[500];

  Fun4AllServer *se = Fun4AllServer::instance();

  //
  // Histograms declaration
  //
  const char *ARM[]  ={"E","W"};
  const char *SIDE[] ={"S","N"};
  const char *SECT[] ={"0","1","2","3","4","5"};
  int index=0;
  
  for (int i=0; i<Narms; i++)
    {
      for (int j=0; j<Nsides; j++)
        {
          for (int k=0; k<Nsect; k++)
            {
	      Gain_mod[index]      = 1.;
	      Gain_mod_err[index]  = 1.;
	      
              sprintf(name,"h_mod_%s%s%s",ARM[i],SIDE[j],SECT[k]);
              h_mod_gain[i][j][k] = new TH1F(name,name,2048,0.5,2048.5);
	      se->registerHisto(h_mod_gain[i][j][k]);
	      index++;
	    }
        }
    }
  
  fexpo       = new TF1("fexpo","expo",10,18);

  return 0;
  
}

int HbdGainCalibTrkV::InitRun(PHCompositeNode *topNode)
{
  RunHeader *runh = getClass<RunHeader>(topNode,"RunHeader");
  if(!runh)
    {
      cout << PHWHERE << "Hbd Module Gain Calibrator:: No RunHeader!" << endl;
      return 0;
    }
  BeginTime = new PHTimeStamp(runh->get_TimeStart());
  EndTime = new PHTimeStamp(runh->get_TimeStop());

  return 0;
}


void HbdGainCalibTrkV::identify(ostream& out) const
{
  cout << Name() << endl;
  return ;
}

int HbdGainCalibTrkV::process_event(PHCompositeNode *topNode)
{

  //
  //  This routine is used to determine gains for the
  //  databases, interpret the data, and determine a
  //  gain correction constant.
  //
  // If this is not a data event, skip it
  //

  if (alldone)
    {
      return 0;
    }

  BlobList = findNode::getClass<HbdBlobList>(topNode,"HbdBlobList");
  if(!BlobList)
    {
      cout << PHWHERE << "Hbd Module Gain Calibrator:: No HbdBlobList!" << endl;
      return 0;
    }

  PHCentralTrack *trk = getClass<PHCentralTrack>(topNode,"PHCentralTrack");
  if(!trk)
    {
      cout << PHWHERE << "Hbd Module Gain Calibrator:: No PHCentralTrack!" << endl;
      return 0;
    }

  arm = -1;
  side = -1;
  sector = -1;
  charge =-9999.;
  blobz = -9999.;
  hbd_dphi = -9999.;
  hbd_dz = -9999.;
  size = -1;


  //
  // Use peripheral events with < 35 Central Arms tracks which corresponds to > 60% centrality
  //
  Number_Of_Tracks = trk->get_npart();
  if (Number_Of_Tracks>=35) return 0;

  //
  // Loop over all HBD clusters
  //
  for(Int_t iThisBlob = 0 ; iThisBlob < (int)BlobList->get_nBlobs(); iThisBlob++)
    { // cluster loop

      //
      // Since we need the scintillation hits we consider only single pad clusters
      //
      size = BlobList->get_blob(iThisBlob)->get_size();
      if (size!=1) continue;

      //
      // Some HBD blob variables filled in HbdWisClusterizer module
      //
      blobx = BlobList->get_blob(iThisBlob)->get_blobx();
      bloby = BlobList->get_blob(iThisBlob)->get_bloby();
      blobz = BlobList->get_blob(iThisBlob)->get_blobz();
      charge = BlobList->get_blob(iThisBlob)->get_charge();
      sector = BlobList->get_blob(iThisBlob)->get_sector();
      PHAngle blob_phi(atan2(bloby,blobx));

      //
      // Veto flag
      //
      AssociationFlag = 0;

      //
      // Loop over all Central Tracks
      //
      for(Int_t iThisTrack=0 ; iThisTrack < Number_Of_Tracks; iThisTrack++)
	{ // track loop

	  //
	  // Track projections (x, y, z) to HBD
	  //
	  hbd_px = trk->get_track(iThisTrack)->get_phbdx();
	  hbd_py = trk->get_track(iThisTrack)->get_phbdy();
	  hbd_pz = trk->get_track(iThisTrack)->get_phbdz();

	  //
	  // Track projection PHI to HBD
	  //
	  PHAngle hbd_pphi(atan2(hbd_py,hbd_px));

	  //
	  // HBD matching variables
	  //
	  hbd_dphi = blob_phi - hbd_pphi;
	  hbd_dz = blobz - hbd_pz;

	  //
	  // Veto on the track
	  //
	  if (fabs(hbd_dphi)<0.05 && fabs(hbd_dz)<5.)
	    {
	      AssociationFlag = 1;
	    }
	}

      //
      // Use only clusters that do not match to any Central track
      //
      if(AssociationFlag==0)
	{
	  arm = 0;
	  side = 0;
	  
	  if(sector>=6){arm = 1;}
	  
	  if(sector<6 && blobz<0) // East south
	    {
	      side=0;
	    }
	  if(sector>5 && blobz>0) // West north
	    {
	      side=1;
	      sector = sector-6;
	    }
	  if(sector<6 && blobz>0) // East north
	    {
	      side=1;
	    }
	  if(sector>5 && blobz<0) // West south
	    {
	      side=0;
	      sector= sector-6;
	    }
	  
	  if(sector<6) h_mod_gain[arm][side][sector] ->Fill(charge);
	  
	}
    }
  evtcounter++;
/*
  if (!alldone && evtcounter > 40000)
    {
      alldone = 1;
    }
*/
    return 0;    
}


int HbdGainCalibTrkV::End(PHCompositeNode *topNode)
{
  cout << "HbdGainCalibTrackVETO::End of Analysis: Just exit" << endl;

  return 0;

  cout << "HbdGainCalibTrackVETO::End of Analysis: going to check whether to calibrate or not" << endl;

  if (alldone)
    {
      Calibrate();
      Verify(); //verify will set the verification status
    }
  else
    {
      verificationstatus = 0; //failed
      cout << "Not enough statistics. Did not calibrate" << endl;
    }


   if (committopdbcal && verificationstatus)
     {
       CommitConstantsToDatabase();
     }

   return 0;
 }



int HbdGainCalibTrkV::Calibrate()
{
   fexpo->SetParameters(1,1);
   fexpo->SetLineColor(2);
   int bin1 =1;
   int bin2 =1;
   int Module=0;
  
   for (int arm=0; arm<Narms; arm++)
     {
       for (int side=0; side<Nsides; side++)
	 {
	   for (int sect=0; sect<Nsect; sect++)
	     {
	       h_mod_gain[arm][side][sect]->Scale(1.0/evtcounter);
	       h_mod_gain[arm][side][sect]->SetAxisRange(0,500,"X");
	       bin1 = h_mod_gain[arm][side][sect]->GetMaximumBin();
	       bin2 = h_mod_gain[arm][side][sect]->FindBin(h_mod_gain[arm][side][sect]->GetMean());

               h_mod_gain[arm][side][sect]->Fit("fexpo","Q","R",bin1+2,bin2+10);

	       Gain_mod[Module]     = -1.0*(1/fexpo->GetParameter(1));
	       Gain_mod_err[Module] = -1.0*(1/fexpo->GetParError(1));
	       Module++;
	     }
	 }
     }

   cout<<PHWHERE <<" Gain values have been extracted " <<endl;
   return 1;
 }



int HbdGainCalibTrkV::Verify()
{
  verificationstatus = 1; // remember 1 means SUCCESS
  if (verbosity>0 && verificationstatus==1) cout << "HBD Module Gain Calibrations Verified." << endl;
   return 0;
}

int HbdGainCalibTrkV::CommitConstantsToDatabase()
{

  cout << "HBDMODULEGAINTRKV:: SUCCESSFULLY CALIBRATED AND COMMITING TO THE DATABASE." << endl;
  cout << "             From: " << *BeginTime << " To: " << *EndTime << endl;
  hbdAdcCalib hbdadc;
  
  for (int i_mod=0; i_mod<24; i_mod++)
    {
      hbdadc.setModuleGain(i_mod, Gain_mod[i_mod], Gain_mod_err[i_mod]);
    }

  hbdadc.updateModuleGain(*BeginTime, *EndTime);
  
  commitsuccess = 1;
  
  return 0;
}
