//INCLUDECHECKER: Removed this line: #include "emcCalibratorFactory.h"
#include "mEmcMIPCorr3Module.h"
#include "EmcStaticData.h"
#include "EmcSector.h"
//INCLUDECHECKER: Removed this line: #include "PHTimeStamp.h"
#include "emcDataManager.h"
#include "EmcIndexer.h"
#include "emcGains.h"
#include "emcDBMS.h"
#include "EmcSimuRawDataReCal.h"

#include "dEmcRawDataWrapper.h"
#include "EmcEnergyAfterBurnerv1.h"

//INCLUDECHECKER: Removed this line: #include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include <recoConsts.h>
//INCLUDECHECKER: Removed this line: #include "PHNodeIterator.h"
//INCLUDECHECKER: Removed this line: #include "Event.h"

#include <iostream>
//INCLUDECHECKER: Removed this line: #include <memory>

using namespace std;

typedef PHIODataNode<dEmcRawDataWrapper> dEmcRawDataNode_t;

//..................
EmcSimuRawDataReCal::EmcSimuRawDataReCal()
{
  final_gain = new float[NTOWER];
  db_gain = new float[NTOWER];
  mip_gain = new float[NTOWER];
  burner_gain = new float[NTOWER];
  // Pedestals are presently set to zero (Tony Frawley, 6/27/2002)
  pedestal = new float[NTOWER];

  emcEnergyaftb = new EmcEnergyAfterBurnerv1();
}
//..................
EmcSimuRawDataReCal::~EmcSimuRawDataReCal()
{
  delete [] final_gain;
  delete [] db_gain;
  delete [] mip_gain;
  delete [] burner_gain;
  delete [] pedestal;

  delete emcEnergyaftb;
}
//......................
void EmcSimuRawDataReCal::Reset()
{
  for(int i = 0; i<NTOWER; i++) {
    final_gain[i] = 1;
    db_gain[i] = 1;
    mip_gain[i] = 1;
    burner_gain[i] = 1;
    pedestal[i] = 0;
  }
} 
//......................
void EmcSimuRawDataReCal::SetCalibConst(int runNumber)
{

  cout << "EmcSimuRawDataRecal: Run number is: " << runNumber << endl;

  Reset();

  if(runNumber>0) {
    SetGainFromDB();
    SetGainFromMIP(runNumber);
    SetGainFromAfterBurner(runNumber);

    for(int i = 0; i<NTOWER; i++) 
    {
      final_gain[i] = db_gain[i]*mip_gain[i]*burner_gain[i];
    }
  } 
}
//.......................
void EmcSimuRawDataReCal::SetGainFromDB()
{
  for(int itower = 0; itower <NTOWER; itower++) {
    db_gain[itower] = GetGainFactorFromDB(itower);
  }
}
//.......................
float EmcSimuRawDataReCal::GetGainFactorFromDB(int twrId)
{
  static emcGains gains[172];
  static bool eCalLoaded = false;
  static bool     gOK[172];

  EmcStaticData * sd = EmcStaticData::buildEmcStaticData() ;
  EmcSector * sector ;
  int iS, iST, iCh;
  float encal, norm0, nothing, normt ;
  float c0,g0,cf ;
  float kappa = 1.0/5.9950 ;
  int absPosition;

  if(!eCalLoaded) {
    PHTimeStamp when;
    when.setToSystemTime();
    for (iS=0;iS<8;iS++){
      sector = sd->getSector(iS) ;
      if( !sector ) {
        // Sector will be built from file if timestamp=0, 
        // from DB otherwise (assuming there's calibration
        // constants valid at timestamp in the DB, of course!)
        sd ->buildEmcSector( EmcIndexer::EmcSectorId(iS), & when) ;
        /*sector =*/ sd->getSector(iS) ;
      }
    }
    emcDataManager * dm = emcDataManager::GetInstance();
    for (absPosition=0;absPosition<172;absPosition++){
      int code = emcCalFEM::FEMCode(absPosition,0,0,0);
      gains[absPosition].SetSource(emcDBMS::get());
      //      cout<<"Collecting through DataManager: absPosition "<<absPosition;
      gOK[absPosition] = dm->Read(gains[absPosition], when, code);
      //      cout<<((gOK[absPosition])? " OK " : " Failed ")<<endl;
    }
    eCalLoaded = true;
  }
  EmcIndexer::PXPXSM144CH(twrId, absPosition, iCh) ;
  if(!gOK[absPosition]) return 1;
  normt = gains[absPosition].getValue(iCh,0);
  if(normt==0.) return 1;
  EmcIndexer::iPXiSiST(twrId, iS, iST) ;
  sector = sd->getSector(iS) ;
  if ( iS < 6 ) {
    sector->GetEnergyCalibration(iST,encal,norm0,nothing) ;
  } else {
    sector->GetEnergyCalibration(iST,c0,g0,cf) ;
    encal = c0*g0*cf ;
    norm0 = kappa ;
  }

  return  encal*norm0/normt;

}
//.......................
void EmcSimuRawDataReCal::SetGainFromMIP(int run)
{
  mEmcMIPCorr3Module* mEmcMIPCorr3 = mEmcMIPCorr3Module::instance();

  int arm, sect, iy, iz;
  for(int itower = 0; itower <NTOWER; itower++) {
     EmcIndexer::TowerLocation(itower, arm, sect, iy, iz);
     mip_gain[itower] = mEmcMIPCorr3->get_corr_twr_mip(arm, sect, iz, iy)*
			mEmcMIPCorr3->get_corr_run(arm, sect, run);
  }
}
//.......................
void EmcSimuRawDataReCal::SetGainFromAfterBurner(int runNumber)
{
  emcEnergyaftb->initialize(runNumber);
  int arm, sect, iy, iz;
  for(int itower = 0; itower <NTOWER; itower++) {
    EmcIndexer::TowerLocation(itower, arm, sect, iy, iz);
    burner_gain[itower] = emcEnergyaftb->get_tower_scalefactor(arm, sect, iy, iz);
  }
}
//..........................
PHBoolean EmcSimuRawDataReCal::event(PHCompositeNode* root)
{
  int dbg=0;

    if(dbg)
      cout << "EmcSimuRawDataReCal:" << endl;

  PHNodeIterator iter(root);
  dEmcRawDataWrapper* dEmcRawDataReCal = 0;
  dEmcRawDataNode_t *uncalnode = static_cast<dEmcRawDataNode_t *>(iter.findFirst("PHIODataNode","dEmcRawDataReCal"));
  if(uncalnode) {
    dEmcRawDataReCal = uncalnode->getData();
  } else {
    cout<<" EmcSimuRawDataReCal: could not find table dEmcRawDataReCal"<<endl;
    return 0;
  }

  dEmcRawDataWrapper* dEmcRawData = 0;
  dEmcRawDataNode_t *rawnode = static_cast<dEmcRawDataNode_t *>(iter.findFirst("PHIODataNode","dEmcRawData"));
  if(rawnode) {
    dEmcRawData = rawnode->getData();
  } else {
    cout<<" EmcSimuRawDataReCal: could not find table dEmcRawData"<<endl;
    return 0;
  }

  recoConsts *rc = recoConsts::instance();
  float lowgain_convfac = rc->get_FloatFlag("EMCTOWERLOWGAIN", 0.001); 
  float highgain_convfac = rc->get_FloatFlag("EMCTOWERHIGHGAIN", 0.008); 

  //... now do de-calibration ...
  for( size_t iraw = 0; iraw<dEmcRawData->RowCount(); iraw++) {
    dEmcRawDataReCal->set_id(iraw, dEmcRawData->get_id(iraw));
    dEmcRawDataReCal->set_evno(iraw, dEmcRawData->get_evno(iraw));
    dEmcRawDataReCal->set_hwkey(iraw, dEmcRawData->get_hwkey(iraw));
    dEmcRawDataReCal->set_swkey(iraw, dEmcRawData->get_swkey(iraw));
    dEmcRawDataReCal->set_type(iraw, dEmcRawData->get_type(iraw));
    dEmcRawDataReCal->set_tdc(iraw, dEmcRawData->get_tdc(iraw));
            
    int towerkey = dEmcRawData->get_swkey(iraw);

    int towerID = getTowerID(towerkey);

    // Leave the pre values as they are

    float tmp_lpre = (float)dEmcRawData->get_adclopre(iraw);
    float tmp_hpre = (float)dEmcRawData->get_adchipre(iraw);

    // Adjust the post values to get the signal right for the real gains
    // Remember lower gain translates to more ADC counts for the same energy
    // Higher gain has fewer ADC counts for the same energy

    // Get the energy from hipost and hipre (actually minus the energy here)

    float hi_energy = (float) (dEmcRawData->get_adchipost(iraw)-
			       dEmcRawData->get_adchipre(iraw)) * highgain_convfac;

    // This has to be divided by 16, since the net ADC counts will be 
    // multiplied by 16 in L2EmcDataAccessor before being multiplied by 
    // the gain 

    hi_energy = hi_energy/16;

    // Now convert the energy back into counts using the real data gain

    float tmp_hpost = (hi_energy/final_gain[towerID] + 
		       dEmcRawData->get_adchipre(iraw)) + pedestal[towerID];

    // Get the energy from lopost and lopre

    float lo_energy = (float) (dEmcRawData->get_adclopost(iraw)-
			       dEmcRawData->get_adclopre(iraw)) * lowgain_convfac;

    // Now convert the energy back into counts using the real data gain

    float tmp_lpost = (lo_energy/final_gain[towerID] + 
		       dEmcRawData->get_adclopre(iraw)) + pedestal[towerID];



    // set the values in the new table

    dEmcRawDataReCal->set_adclopre(iraw, (short) tmp_lpre);
    if(tmp_lpost<0)
      {
	if(dbg)
	  cout << "   -------- adclopost changed from " << tmp_lpost;

	tmp_lpost = 100;

	if(dbg)
	  cout << " to " << tmp_lpost <<  " to force Lvl2 to use high" <<  endl;
      }
    dEmcRawDataReCal->set_adclopost(iraw, (short) tmp_lpost);
    dEmcRawDataReCal->set_adchipre(iraw, (short) tmp_hpre);
    dEmcRawDataReCal->set_adchipost(iraw, (short) tmp_hpost);

    if(dbg)
      {
	int net_counts = - (dEmcRawDataReCal->get_adclopost(iraw) - 
			    dEmcRawDataReCal->get_adclopre(iraw));
	if(net_counts>3071)
	  net_counts =  -16 * (dEmcRawDataReCal->get_adchipost(iraw) - 
			       dEmcRawDataReCal->get_adchipre(iraw));

	cout << " **** towerID " << towerID  
	     << " final_gain " << final_gain[towerID] 
	     << " input energy = " << -1 * lo_energy 
	     << " corrected net counts " << net_counts 
	     << endl;
      }	

  }

  dEmcRawDataReCal->SetRowCount(dEmcRawData->RowCount());
  
  return True;
}
//
//... get TowerID from softkey ...
int EmcSimuRawDataReCal::getTowerID(int towerkey)
{
    int iS; 

    int iz=towerkey%100;
    towerkey/=100;
    int iy=towerkey%100;
    towerkey/=100;
    int sector=towerkey%10;
    towerkey/=10;
    int arm=towerkey;

    if(arm==1) {
      iS = sector + 6;	
      if(iS>=8) iS -=4; 	
    } else {
      iS = sector;
    }
    int towerID = EmcIndexer::getTowerId(iS, iz, iy);
    return towerID;
}
