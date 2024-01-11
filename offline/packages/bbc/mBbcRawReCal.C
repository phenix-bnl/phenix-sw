#include "mBbcRawReCal.h" 

#include "BbcCalib.hh"
#include "dBbcRawWrapper.h"
#include "dBbcUcalWrapper.h"

#include "PISAEvent.h"

#include "getClass.h"
#include "PHCompositeNode.h"

#include <iostream>

using namespace std;

PHBoolean
mBbcRawReCal::event(PHCompositeNode *topNode)
{
  //cout<<"mBbcRawReCal::Event: making table dBbcRawReCal from dBbcRaw "<<endl;

  // Get a pointer to the BBC real calibrations
  bbccalib = findNode::getClass<BbcCalib>(topNode,"BbcCalibPar");
  if (!bbccalib){
    cout << "Could not find BbcCalibPar object, quit!" << endl;
    bbccalib=0;
    exit(1);
  }

  // Find the node that we will write the modified raw data to
  dBbcRawWrapper* BbcRawReCalWrapper = findNode::getClass<dBbcRawWrapper>(topNode,"dBbcRawReCal");
  if (!BbcRawReCalWrapper)
    {
    cout<<" could not find table dBbcRawReCal, quit!"<<endl;
    exit(1);
  }

  // Find the node that we will read the raw data from
  dBbcRawWrapper* BbcRawWrapper = findNode::getClass<dBbcRawWrapper>(topNode,"dBbcRaw");
  if(!BbcRawWrapper) 
{
    cout<<" Could not find table dBbcRaw, quit!"<<endl;
    exit(1);
  }
  
  
  // Add code here to loop over channels and call the decalibration methods
  BbcRawReCalWrapper->SetRowCount(128);
  for(int i=0;i<128;i++) {

    BbcRawReCalWrapper->set_Pmt(i,BbcRawWrapper->get_Pmt(i));
    BbcRawReCalWrapper->set_Arm(i,BbcRawWrapper->get_Arm(i));
    BbcRawReCalWrapper->set_Half(i,BbcRawWrapper->get_Half(i));
    BbcRawReCalWrapper->set_Ring(i,BbcRawWrapper->get_Ring(i));
    BbcRawReCalWrapper->set_Tube(i,BbcRawWrapper->get_Tube(i));
    BbcRawReCalWrapper->set_Adc(i,UnCalibAdc(i,BbcRawWrapper->get_Adc(i)));
    float UCtdc0 = UnCalibTdc0(i,BbcRawWrapper->get_Tdc0(i));
    if(UCtdc0 > 2600) UCtdc0 = UCtdc0 + 300.0;    // needed because sim overflows are too small
    BbcRawReCalWrapper->set_Tdc0(i,UCtdc0);
    float UCtdc1 = UnCalibTdc1(i,BbcRawWrapper->get_Tdc1(i));
    if(UCtdc1 > 2600) UCtdc1 = UCtdc1 + 300.0;    // needed because sim overflows are too small
    BbcRawReCalWrapper->set_Tdc1(i,UCtdc1);

    /*
    cout << "Raw: " << endl;
    cout << " PMT " << BbcRawWrapper->get_Pmt(i)
     << " Arm " << BbcRawWrapper->get_Arm(i)
     << " Half " << BbcRawWrapper->get_Half(i)  
     << " Ring " << BbcRawWrapper->get_Ring(i)  
     << " Tube " << BbcRawWrapper->get_Tube(i)  
     << " ADC " << BbcRawWrapper->get_Adc(i)
     << " TDC0 " << BbcRawWrapper->get_Tdc0(i)
     << " TDC1 " << BbcRawWrapper->get_Tdc1(i)
     << endl;
    */
  }
  
  // There needs to be a check for simVertexFlag == 2 here
    {
      // This should be run for simVertexFlag == 2, ie. single particle sims
      
      // If we are processing a PISA file from a single particle simulation,
      // there will be no hits in the BBC so the level 2 triggers will not be
      // able to get a vertex position
      // Here we check the simVertex flag and if it is 2, we get the Z vertex
      // position from the PISA file and make up some BBC data that will return
      // that vertex position in level 2
      
      
      PISAEventHeader *pisaEventHeader = PISAEventHeader::GetEventHeader();
      int pisaRunNumber = pisaEventHeader->GetIDRun();
      float simZ0Vertex = pisaEventHeader->GetZvertex();
      
      cout << endl << "Getting Z vertex from PISA file run header: " 
	   << " Run number = " << pisaRunNumber 
	   << " Z vertex " << simZ0Vertex <<  endl;
      
      // Now put a hit in one PMT in each BBC arm (choose PMT 10 and 75) that 
      // will yield this vertex
      // Specify the TDC value in arm 0 and calculate it in arm 1 
      
      int isouth=10;
      int inorth=75;
      float ADC = 1500.0;
      float tdcs = 1500;
      
      BbcRawReCalWrapper->set_Adc(isouth,ADC);
      BbcRawReCalWrapper->set_Tdc0(isouth,tdcs);
      BbcRawReCalWrapper->set_Tdc1(isouth,tdcs);
      
      // Now calculate which value of tdc2 will yield simZ0Vertex
      // The time difference is 
      
      const float C=29.9792458;   // the light velocity [cm/ns] 
      float tdiff = 2.0 * simZ0Vertex/C;
      
      float ts = tdcs * (bbccalib->getTdcGain1()->getCalibPar(isouth))->getPar1();
      
      float tn = ts + tdiff;
      
      float tdcn = tn / (bbccalib->getTdcGain1()->getCalibPar(inorth))->getPar1();

      //cout << "ts = " << ts << " tdiff = " << tdiff << " tn = " 
      //     << tn << " tdcn = " << tdcn << endl;

      BbcRawReCalWrapper->set_Adc(inorth,ADC);
      BbcRawReCalWrapper->set_Tdc0(inorth,tdcn);
      BbcRawReCalWrapper->set_Tdc1(inorth,tdcn);
      
      cout << "Added data to PMT " << isouth << " and PMT " 
	   << inorth << " to simulate vertex of " << simZ0Vertex << endl << endl;

      /*
	for(int i=0;i<128;i++) 
	{
	cout << "RawRecal: " << endl;
	cout << " PMT " << BbcRawReCalWrapper->get_Pmt(i)
	<< " ADC " << BbcRawReCalWrapper->get_Adc(i)
	<< " TDC0 " << BbcRawReCalWrapper->get_Tdc0(i)
	<< " TDC1 " << BbcRawReCalWrapper->get_Tdc1(i)
	<< endl;
	}
      */
    }

  return True;
}

int
mBbcRawReCal::restore()
{
  PHTimeStamp ts;
  //  where to get the timestamp from ?
  
  bbccalib->restore(ts);
  
  return 0;
}

// Uncalibrate TDC0, TDC1, ADC here.....

float
mBbcRawReCal::UnCalibAdc(int ipmt, int Adc)
{
  float Pedestal = (bbccalib->getPedestal()->getCalibPar(ipmt))->getPeakChannel();
  float PmtGain  = (bbccalib->getPmtGain()->getCalibPar(ipmt))->getPeakChannel();
  float AdcGain  = (bbccalib->getAdcGain()->getCalibPar(ipmt))->getPar1();
  if (PmtGain < 0.0001)
    {
      return 0.0;
    }

  // Undo the simulations decalibration, then apply the real data decalibration
  // Assumes: signal=(ADC_real-PED_real)*GAIN_real = (ADC_sim-PED_sim)*GAIN_sim

  float Adc_UnCalib= (Adc - 890.)*(1/0.070) / (AdcGain * PmtGain) + Pedestal;

  /*
  cout << ipmt << " Adc " << Adc << " AdcGain " << AdcGain << " PmtGain " 
       << PmtGain << " Pedestal " << Pedestal << " Adc_UnCalib " 
       << Adc_UnCalib << endl;
  */

  return Adc_UnCalib;
}

float
mBbcRawReCal::UnCalibTdc0(int ipmt, int Tdc0) 
{
  float TdcGain  = (bbccalib->getTdcGain0()->getCalibPar(ipmt))->getPar1();
  float Tdc0_UnCalib=(Tdc0*0.007)/TdcGain; 

  /*
  cout << ipmt << " Tdc0 " << Tdc0 <<" TdcGain " << TdcGain <<" Tdc0_UnCalib " 
       << Tdc0_UnCalib << endl;
  */

  return Tdc0_UnCalib;
}

float
mBbcRawReCal::UnCalibTdc1(int ipmt, int Tdc1)
{
  float TdcGain  = (bbccalib->getTdcGain1()->getCalibPar(ipmt))->getPar1();
  float Tdc1_UnCalib=(Tdc1*0.007)/TdcGain;

  /*
  cout << ipmt <<" Tdc1 " << Tdc1 <<" TdcGain " << TdcGain << " Tdc1_UnCalib " 
       << Tdc1_UnCalib << endl;
  */

  return Tdc1_UnCalib;
}
