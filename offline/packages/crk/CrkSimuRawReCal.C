#include <CrkSimuRawReCal.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHNodeIterator.h>
#include <RunToTime.hh>

#include <PdbRichADC.hh>
#include <PdbRichPar.hh>
#include <CrkCal.h>
#include <dCrkRawWrapper.h>
#include <dCrkUcalWrapper.h>


#include <cstdlib>
#include <iostream>
#include <memory>

using namespace std;

typedef PHIODataNode<dCrkRawWrapper> dCrkRawNode_t;
typedef PHIODataNode<dCrkUcalWrapper> dCrkUcalNode_t;

//......................
void CrkSimuRawReCal::Reset()
{
  memset(pedestal,0,sizeof(pedestal));
  memset(gain,0,sizeof(gain));
} 
//......................
void CrkSimuRawReCal::SetCalibConst(PHTimeStamp time)
{
  Reset();

  CrkCal<PdbRichADC> adcphe_cal("adcphe");
  CrkCal<PdbRichADC> adcped_cal("adcped");
  CrkCal<PdbRichPar> t0_cal("T0");
  CrkCal<PdbRichPar> tac_cal("TAC_GAIN");
  CrkCal<PdbRichPar> slew_cal("SLEW");

  // Read calibration data from file
  adcphe_cal.fetch_DB(time);
  adcped_cal.fetch_DB(time);
  t0_cal.fetch_DB(time);
  tac_cal.fetch_DB(time);
  slew_cal.fetch_DB(time);

  PdbRichPar t0, tac, slew;
  PdbRichADC adcphe,adcped;

  int PMT2, PMT3, PMT4;
  int PMT1phe,PMT1ped;
  float pedphe, pedwphe, PEphe, PEwphe, Npedphe, N1phe, N2phe, N3phe, chisqrphe;
  float pedped, pedwped, PEped, PEwped, Npedped, N1ped, N2ped, N3ped, chisqrped;
  float t0v, tacv, slewv;
  for(int i=0;i<NPMT;i++) {
    adcphe_cal.getval(i,adcphe);
    adcped_cal.getval(i,adcped);
    t0_cal.getval(i,t0);
    tac_cal.getval(i,tac);
    slew_cal.getval(i,slew);
    
    adcphe.getValues(PMT1phe,pedphe,pedwphe,PEphe,PEwphe,Npedphe,N1phe,N2phe,N3phe,chisqrphe);
    adcped.getValues(PMT1ped,pedped,pedwped,PEped,PEwped,Npedped,N1ped,N2ped,N3ped,chisqrped);
    t0.getValues(PMT2,t0v);
    tac.getValues(PMT3,tacv);
    slew.getValues(PMT4,slewv);

    if(PEphe == 0 || PEphe < pedped) { // the channel is disabled
      gain[i] = 0.0;
    } else {
      gain[i]  = PEphe - pedped;
    }
    pedestal[i]   = pedped;
  }
}
//......................

void
CrkSimuRawReCal::SetCalibConst(int runNumber)
{
  PHTimeStamp TimeStamp;
  //.. this part is based on PRECO real data reconstruction ..
  //  auto_ptr<RunToTime> runTime(new RunToTimePg);
  RunToTime *runTime = RunToTime::instance();
  auto_ptr<PHTimeStamp> ts(runTime->getBeginTime(runNumber)) ;

  if (ts.get() != 0)
    {
      TimeStamp = *ts;
    }
  else
    {
      cout << "ERROR: Can NOT get Time Stamp !!!" << endl;
      exit(1);
    }

  SetCalibConst(TimeStamp);
}

//..........................
PHBoolean CrkSimuRawReCal::event(PHCompositeNode* topNode)
{

  int dbg=0;

  if(dbg)
    cout << "CrkSimuRawReCal:" << endl;

  //... now get table data ...
  PHNodeIterator iter(topNode);

  dCrkRawWrapper* dCrkRawReCal = 0;
  dCrkRawNode_t *uncalnode = static_cast<dCrkRawNode_t *>(iter.findFirst("PHIODataNode","dCrkRawReCal"));
  if(uncalnode) {
    dCrkRawReCal = uncalnode->getData();
  } else {
    cout<<" CrkRawReCal: could not find table dCrkRawReCal"<<endl;
    return false;
  }

  dCrkRawWrapper* dCrkRaw = 0;
  dCrkRawNode_t *rawnode = static_cast<dCrkRawNode_t *>(iter.findFirst("PHIODataNode","dCrkRaw"));
  if(rawnode) {
    dCrkRaw = rawnode->getData();
  } else {
    cout<<" CrkRawReCal: could not find table dCrkRaw"<<endl;
    return false;
  }

  dCrkUcalWrapper* dCrkUcal = 0;
  dCrkUcalNode_t *dcrkucalnode = static_cast<dCrkUcalNode_t *>(iter.findFirst("PHIODataNode","dCrkUcal"));
  if(dcrkucalnode) {
    dCrkUcal = dcrkucalnode->getData();
  } else {
    cout<<" CrkRawReCal: could not find table dCrkUcal"<<endl;
    return false;
  }

  if(dbg)
    cout <<" There are " << dCrkRaw->RowCount() <<" entries in dCrkRaw" << endl;

  //... now do de-calibration ...
  for(unsigned int iraw = 0; iraw<dCrkRaw->RowCount(); iraw++) {

    int real_pmtnum = RenumberPmt(dCrkRaw->get_pmt(iraw));

    dCrkRawReCal->set_pmt(iraw, real_pmtnum);
    dCrkRawReCal->set_tdc(iraw, dCrkRaw->get_tdc(iraw));

    //.. response is smeared from dCrkUcal data assuming gaussian for adc ..
    float tmp_npe = (dCrkRaw->get_adc(iraw) - dCrkUcal->get_ped(iraw))/dCrkUcal->get_gain(iraw);
    
    short int final_npe = (short) (tmp_npe*gain[iraw] + pedestal[iraw]); 

    dCrkRawReCal->set_adc(iraw, final_npe);

    if(dbg)
      {
	cout << " pmt in " << dCrkRaw->get_pmt(iraw)
	     << " out " << dCrkRawReCal->get_pmt(iraw)
	     <<  " tmp_npe " << tmp_npe 
	     << " adc in " << dCrkRaw->get_adc(iraw) 
	     << " ped in " <<  dCrkUcal->get_ped(iraw)
	     << " gain in " << dCrkUcal->get_gain(iraw)
	     << " adc out " << dCrkRawReCal->get_adc(iraw)
	     << " ped out " << pedestal[iraw]
	     << " gain out " << gain[iraw]
	     << endl;
      }
  }
  dCrkRawReCal->SetRowCount(dCrkRaw->RowCount());
  return true;
}

int CrkSimuRawReCal::RenumberPmt(int pmtin)
{

  int dbg=0;

  // This takes as input the pmt number from simulations and calculates
  // which pmt number in real data would correspond to the same femid
  // and channel - which is what run 2 Lvl2 uses to get pmt number.

  // If you are confused, look at:
  // Lvl2_distribution/dbprimitives/L2RichDBPrimitives.C, in method 
  // get_pmtnum_table (you will still be confused, but it will help- Tony)

  // The pmt number is just
  // (femid*16 + cable*16 + chan)
  // where cable is different for real and simulated data:
  // real cable = {5,0,6,1,7,2,8,3,9,4} where 
  // sim cable =  {0,5,1,6,2,7,3,8,4,9}

  // get the femid

  // femid is from 0 to 32
  int femid = pmtin/160;

  // get the cable within the femid (0 to 9)

  int tmp = pmtin - femid*160;

  int sim_cable = tmp/16;

  int chan = tmp - sim_cable *16;

  // we now want to get the PMT number for which real_cable  
  // would equal sim_cable

  int real_cable;
  if(sim_cable < 5)
    real_cable = sim_cable + 5;
  else
    real_cable = sim_cable - 5;

  int pmtout = femid*160 + real_cable*16 + chan;

  if(dbg)
    {
      cout << "pmtin " << pmtin 
	   << " femid " << femid 
	   << " sim_cable " << sim_cable 
	   << " real_cable " << real_cable 
	   << " chan " << chan
	   << " pmtout " << pmtout
	   << endl;
    }
  
  return pmtout;
}
