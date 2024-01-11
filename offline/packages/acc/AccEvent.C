#include "RunHeader.h"
#include "AccRaw.h"
#include "AccHit.h"
#include "AccGeometry.h"
#include "AccCalib.h"

#include "AccEvent.h"

#include "AccSim.h"
#include "AerPISAHit.h"
#include "AerGeaHits.h"

#include "getClass.h"
#include "Event.h"
#include "PHIODataNode.h"

#include <iostream>

using namespace std;

//________________________________________________________________
AccEvent::AccEvent() : debug(0)
{
  //  Keep your pointers from dangling!!!
  d_raw=0;
  d_hit=0;

  d_sim = new AccSim();
  d_sim->SetCalibPara();
}

//________________________________________________________________
AccEvent::~AccEvent()
{
  if(d_sim) delete d_sim;
}

//________________________________________________________________
int AccEvent::Reset(PHCompositeNode *top)
{
  //  This routine should perform resets.
  
  if (debug) cout << "AccEvent::Reset() : Resetting the ACC " <<endl;
  if (d_raw) d_raw->Reset();
  if (d_hit) d_hit->Reset();
  
  return 0;
}

//________________________________________________________________
int AccEvent::process_event(PHCompositeNode *top)
{
  //  This routine should perform analysis of the event.

  // Find Nodes
  d_raw = findNode::getClass<AccRaw>(top,"AccRaw");
  d_hit = findNode::getClass<AccHit>(top,"AccHit");

  // Fills the Raw objects
  if (debug) cout << "AccEvent::process_event() , fill Raw data ... " << endl;
  AccEvent::DcmToRaw(top);

  return 0;
}

//________________________________________________________________
PHBoolean AccEvent::DcmToRaw(PHCompositeNode* top)
{
  // Fill the "RAW" data from PRDF.

  PHNodeIterator iter(top);

  // Extract the Event object from the node.
  Event* event = findNode::getClass<Event>(top,"PRDF");
  if(!event){
    return False;
  }

  // get packet for ACC
  int iraw = 0;
  
  for(int npacket=0; npacket<2; npacket++){
    
    int acc_packet_id = ACC::ACC_PACKET_ID + 4*npacket;
    Packet* pACCN = event->getPacket(acc_packet_id);
    if(pACCN){
      // box id loop
      // this id (ibox) is corresponding to PreAmp id.
      for(int ibox=0;ibox<ACC::ACC_NBOX_HALF;ibox++){
	// Add a raw data and set raw count
	d_raw->AddRaw(iraw);
	d_raw->set_nraw(iraw+1);
	
	// replace boxid
	int boxid_old = ACC::convertBoxID(ibox);
	
	d_raw->set_boxid(iraw, ibox+ACC::ACC_NBOX_HALF*npacket);
	
	for(int ipmt=0;ipmt<2;ipmt++){
	  int ich = ACC::getPmtID(ipmt, boxid_old);
	  int tdc      = pACCN->iValue(ich,0); // tdc
	  int adc_post = pACCN->iValue(ich,1); // adc post sample
	  int adc_pre  = pACCN->iValue(ich,2); // adc pre sample
	  int adc      = adc_post - adc_pre;
	  
	  // Fill values
	  d_raw->set_adc(iraw, ipmt, adc);
	  d_raw->set_tdc(iraw, ipmt, tdc);
	  d_raw->set_adcpost(iraw, ipmt, adc_post);
	  d_raw->set_adcpre(iraw, ipmt, adc_pre);
	  
	  if(debug && adc>0) {
	    cout << "box " << ibox+80*npacket<<"  "<<boxid_old << " pmt " << ipmt << " adc " << adc << " tdc " << tdc << endl;
	  }
	}
	iraw++; // increment pointer to next raw data
      }
    } 
    delete pACCN;
  }

  return True;
}

//________________________________________________________________
PHBoolean AccEvent::RawToHit(PHCompositeNode* top, AccGeometry* geo, AccCalib* calib)
{

  // Apply calibration to Raw data
  int iraw = 0;	
  int ihit = 0;
  for(int ibox=0;ibox<ACC::ACC_NBOX;ibox++){
    int adc[2];
    int tdc[2];
    float npe[2];
    int status[2];

    for(int ipmt=0;ipmt<2;ipmt++){
      int ich    = ACC::getPmtID(ipmt, ibox);
      adc[ipmt]  = d_raw->get_adc(iraw, ipmt);
      tdc[ipmt]  = d_raw->get_tdc(iraw, ipmt);
      npe[ipmt]  = calib->get_Npe(ich, adc[ipmt]);
      float timing = calib->get_Timing(ich, adc[ipmt], tdc[ipmt]);

      if(npe[ipmt]>0 && npe[ipmt]<100 
         && timing>-100 && timing<100){
        status[ipmt] = 1;
      }
      else{
        status[ipmt] = 0;
      }

      iraw++;
    }

    // Fill hit 
    if(status[0]==1 && status[1]==1){
      float Npe  = npe[0] + npe[1];
      float Tof  = calib->get_Tof(ibox, adc, tdc);
      float Tdiff = calib->get_Tdiff(ibox, adc, tdc);
      float Zpos = calib->get_Zpos(ibox, adc, tdc);

      d_hit->set_boxid(ihit, ibox);
      d_hit->set_npe(ihit, Npe);
      d_hit->set_tof(ihit, Tof);
      d_hit->set_tdiff(ihit, Tdiff);

      PHPoint hitLocal, hitGlobal;
      // set hit point in local coordinate
      hitLocal.setX(0.);
      hitLocal.setY(0.);
      hitLocal.setZ(Zpos);

      // loacl to global
      hitGlobal = geo->fromLocalToGlobal(ibox, hitLocal);

      d_hit->set_xyz(ihit, 0, hitGlobal.getX());
      d_hit->set_xyz(ihit, 1, hitGlobal.getY());
      d_hit->set_xyz(ihit, 2, hitGlobal.getZ());
 
      ihit++;
    }
  }

  return True;
}

//________________________________________________________________
PHBoolean AccEvent::GeaToRaw(PHCompositeNode* top)
{
  // --- Set Parameters ---
  int ADC[ACC::ACC_NBOX][2], TDC[ACC::ACC_NBOX][2];
  int ADC_post[ACC::ACC_NBOX][2],ADC_pre[ACC::ACC_NBOX][2];

  // --- Initialize Parameters ---
  d_sim -> InitParameters();
  for(int ncnt=0; ncnt<ACC::ACC_NBOX; ncnt++)
  {
    for(int ipmt=0; ipmt<2; ipmt++)
    {
      ADC[ncnt][ipmt] = 0;
      TDC[ncnt][ipmt] = d_sim -> GetSimTDCPed(ncnt,ipmt);
      ADC_post[ncnt][ipmt] = 0;
      ADC_pre[ncnt][ipmt]  = 0;
    }
  }

  // --- get AerGeaHits object if it exists. ---
  aergea = findNode::getClass<AerGeaHits>(top,"AerGeaHits");
  if (!aergea)
  { cout << "AerEvent: AerGea Node missing" << endl; }

  // --- Extract the aerRaw object. ---
  d_raw = findNode::getClass<AccRaw>(top,"AccRaw");
  if (!d_raw)
    {
    cout << PHWHERE << " raw data not in Node Tree" << endl;
  }

  unsigned int nhits = aergea->get_nhits();

  // --- Start PISA Hit Loop ---
  for(unsigned int ihit=0; ihit<nhits; ihit++ ){
    AerPISAHit *aerghit = aergea -> get_AerPISAHits(ihit);
    int   icounter;
    float gen[15];
    icounter = aerghit -> GetLayer();
    gen[0]   = aerghit -> GetX();          gen[1]   = aerghit -> GetY();
    gen[2]   = aerghit -> GetZ();          gen[3]   = aerghit -> GetPx();
    gen[4]   = aerghit -> GetPy();         gen[5]   = aerghit -> GetPz();
    gen[6]   = aerghit -> GetPathLength(); gen[7]   = aerghit -> GetTOF();
    gen[8]   = aerghit -> GetDele();       gen[9]   = aerghit -> GetNtrack();
    gen[10]  = aerghit -> GetStepLength(); gen[11]  = aerghit -> GetEtot();
    gen[12]  = aerghit -> GetCharge();     gen[13]  = aerghit -> GetPtot();
    gen[14]  = aerghit -> GetIdPart();

//     // printout
//     cout << "AccEvent::GeaToRaw - input[" << ihit << "]: ";
//     for( unsigned int i=0; i<15; i++ )
//     { cout << gen[i] << " "; }
//     cout << endl;
    
    // --- calculate the photon propagation in Aerogel ---
    d_sim -> CalcCherenkov(icounter,gen);
  } // end of PISA Hit Loop

  // --- Get Simulation ADC & TDC for each PMT ---
  //ofstream outfile1; outfile1.open("SignalPos.dat");
  //ofstream outfile2; outfile2.open("SignalNeg.dat");

  for(int ncnt=0; ncnt<ACC::ACC_NBOX; ncnt++){
    /*
    //Convert Box ID & PMT ID
    int convid  = d_sim -> ConvertBoxId(ncnt);
    int convpmt = d_sim -> ConvertPmtId(ncnt);
    */

    // ADC (0->North Side, 1->South Side)
    for(int ipmt=0; ipmt<2; ipmt++){
      ADC[ncnt][ipmt]      = d_sim -> GetSimADC(ncnt,ipmt);
      ADC_post[ncnt][ipmt] = d_sim -> GetSimADCPost(ncnt,ipmt);
      ADC_pre[ncnt][ipmt]  = d_sim -> GetSimADCPre(ncnt,ipmt);
    }

    // TDC (0->North Side, 1->South Side)
    bool flag_tdc_n = true;
    bool flag_tdc_s = true;
    for(int i=0; i<1000; i++){
      float sig_n = d_sim -> GetSimSignal(ncnt,i,0);
      float sig_s = d_sim -> GetSimSignal(ncnt,i,1);
      if(sig_s >= 0.010 && flag_tdc_s){
        float TDC_Gain = d_sim -> GetSimTDCGain(ncnt,1);
        TDC[ncnt][1] += (int)((i/10.0)*TDC_Gain);
        flag_tdc_s = false;
      }
      if(sig_n >= 0.010 && flag_tdc_n){
        float TDC_Gain = d_sim -> GetSimTDCGain(ncnt,0);
        TDC[ncnt][0] += (int)((i/10.0)*TDC_Gain);
        flag_tdc_n = false;
      }
      //if(sig_p > 0) outfile1 << i <<" "<< sig_p << endl;
      //if(sig_n > 0) outfile2 << i <<" "<< sig_n << endl;
    } // End of TDC Loop
    //    cout<<"Box Number = "<<ncnt<<endl;
    //    cout<<"ADC = "<<ADC[ncnt][0]<<" : "<<ADC[ncnt][1]<<endl;
    //    cout<<"TDC = "<<TDC[ncnt][0]<<" : "<<TDC[ncnt][1]<<endl<<endl;

  } // End of Counter Loop
  // Fill Data to DST
  
  int iraw = 0;
  for(int ibox=0; ibox<ACC::ACC_NBOX; ibox++)
  {
    d_raw -> AddRaw(iraw);
    d_raw -> set_nraw(iraw+1);
    d_raw -> set_boxid(iraw, ibox);

    for(int ipmt=0; ipmt<2; ipmt++){
      d_raw -> set_adc(iraw, ipmt, ADC[ibox][ipmt]);
      d_raw -> set_tdc(iraw, ipmt, TDC[ibox][ipmt]);
      d_raw -> set_adcpost(iraw, ipmt, ADC_post[ibox][ipmt]);
      d_raw -> set_adcpre(iraw, ipmt, ADC_pre[ibox][ipmt]);
      
//       cout << "AccEvent::GeaToRaw - output[" << iraw << "," << ibox << "," << ipmt << "]: " 
//         << d_raw -> get_adc(iraw, ipmt) << " "
//         << d_raw -> get_tdc(iraw, ipmt) << " " 
//         << d_raw -> get_adcpost(iraw, ipmt) << " " 
//         << d_raw -> get_adcpre(iraw, ipmt) << " "
//         << endl;
    }

    iraw++;
  }

  return True;
}
//________________________________________________________________
PHBoolean AccEvent::RawToRec(PHCompositeNode* top, AccGeometry* geo)
{
  // --- Set Parameters ---
  int   iraw = 0;
  int   ihit = 0;
  int   ADC[ACC::ACC_NBOX][2], TDC[ACC::ACC_NBOX][2];
  int   TdcPed[2];
  float AdcGain[2], TdcGain[2];
  float npe[2], time[2];
  int   status[2];

  // --- Extract the aerRaw object. ---
  d_raw = findNode::getClass<AccRaw>(top,"AccRaw");

  // --- Extract the aerHit object. ---
  d_hit = findNode::getClass<AccHit>(top,"AccHit");

  // --- Restore from AccRaw ---
  for(int icnt=0; icnt<ACC::ACC_NBOX; icnt++){
    //int boxid  = d_sim -> ConvertBoxId(icnt);

    status[0] = 0;  status[1] = 0;

    for(int ipmt=0; ipmt<2; ipmt++){
      // --- Get Raw Data from AccRaw Node ---
      ADC[icnt][ipmt] = d_raw -> get_adc(iraw,ipmt);
      TDC[icnt][ipmt] = d_raw -> get_tdc(iraw,ipmt);
      //      cout<<"BOX ID = "<<icnt<<" : "<<"PMT = "<<ipmt<<endl;
      //      cout<<ADC[icnt][ipmt]<<":"<<TDC[icnt][ipmt]<<endl;

      // --- Get Calibration Constants for Simulation ---
      AdcGain[ipmt] = d_sim -> GetSimADCGain(icnt,ipmt);
      TdcPed[ipmt]  = d_sim -> GetSimTDCPed(icnt,ipmt);
      TdcGain[ipmt] = d_sim -> GetSimTDCGain(icnt,ipmt);

      // --- Apply Calibration for Aerogel ---
      npe[ipmt]  = (float)ADC[icnt][ipmt]/AdcGain[ipmt];
      time[ipmt] = ((float)TDC[icnt][ipmt]-TdcPed[ipmt])/TdcGain[ipmt];

      // --- set flag ---
      if(npe[ipmt] > 0){
        status[ipmt] = 1;
      }

    }

    iraw++;

    if(status[0] == 1 || status[1] ==1){
      float Npe   = npe[0] + npe[1];
      float Tof   = (time[0] + time[1])/2.0;
      float Tdiff = time[0] - time[1];
      float Zpos  = -9999;

      d_hit->AddHit(ihit);
      d_hit->set_nhit(ihit+1);
      d_hit->set_boxid(ihit, icnt);
      d_hit->set_npe(ihit, Npe);
      d_hit->set_tof(ihit, Tof);
      d_hit->set_tdiff(ihit, Tdiff);

      PHPoint hitLocal, hitGlobal;
      // set hit point in local coordinate
      hitLocal.setX(0.);
      hitLocal.setY(0.);
      hitLocal.setZ(Zpos);

      // loacl to global
      hitGlobal = geo->fromLocalToGlobal(icnt, hitLocal);

      d_hit->set_xyz(ihit, 0, hitGlobal.getX());
      d_hit->set_xyz(ihit, 1, hitGlobal.getY());
      d_hit->set_xyz(ihit, 2, hitGlobal.getZ());

      ihit++;
    }
  }

  return True;
}
