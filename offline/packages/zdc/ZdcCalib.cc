#include "ZdcCalib.hh"

ClassImp(ZdcCalib);

using namespace std;

ZdcCalib::ZdcCalib()
{
  restore();
}

float ZdcCalib::getCharge(int PmtIndx, int ADC)
{
  float Pedestal = (cpedestal.getCalibPar(PmtIndx))->getPeakChannel();
  float PmtGain  = (cpmtgain.getCalibPar(PmtIndx))->getPeakChannel();
  float AdcGain  = (cadcgain.getCalibPar(PmtIndx))->getPar1();
  float EnergyGain  = (cadcgain.getCalibPar(PmtIndx))->getPar0();
  int   Status  =  (cadcgain.getCalibPar(PmtIndx))->getStatus();

  if ( Status==1 ) return 0.;		// bad HW channel (FEM OK)
  if (PmtGain<0.0001) return(0.0);
  if (EnergyGain<0.0001) EnergyGain = 1.0;

  if ( Status==2 )
    {
      //
      //-*** apply non-linearity corrections
      //
      float Corrected_ADC = zdc_correction( ADC-Pedestal );
      return Corrected_ADC*AdcGain*EnergyGain/PmtGain;
    }
  else if ( (float)ADC > Pedestal )
    {
      return ( (ADC-Pedestal)*AdcGain*EnergyGain/PmtGain );
    }
  // else
  //   {
      return 0.;		// return 0 if there is no hit
      //    }
}

float ZdcCalib::zdc_correction(const float a) const
{
//////////////////////////////////////////////////////////////////////////////////////
//THis function returns the corrected values of ZDC module amplitude.
// "a" is "measured" amplitude for ZDC module. 
// Units are in ADC channels.
// Corrections are applied for d x Au run 2003 data only and for South ZDC(gain 1.25pC/MIP)
// No corrections should be applied for North ZDC
// A.Denisov, May 1, 2003
//////////////////////////////////////////////////////////////////////////////////////

  float ac = a;
  if ( ac>1200. )
    {
      if ( ac>3200. ) ac=3200.;
      float x = ac-1200.;
      ac = ac/(1.-1.e-6*(-9.54354*x+3.29744e-1*x*x-1.57850e-4*x*x*x+2.77478e-8*x*x*x*x));
      x = ac-1200.;
      ac = ac/(1.-1.e-5*(2.45339*x+4.04407e-3*x*x-1.77304e-6*x*x*x+2.61160e-10*x*x*x*x));
    }

  if(a> 3200.) ac = a * ac/3200.;
  return ac;
}

/// return the hit time for tvc 0 in ns.
float ZdcCalib::getHitTime0(int PmtIndx, int TDC, int ADC)
{
  float tdc0lutcorrection;
  if (ADC < 0 || ADC > 4095)
    {
      return( INVALID_FLOAT );
    }

  tdc0lutcorrection = (ctdc0lut.getCalibParLUT(PmtIndx))->getlut(ADC);
  if ( tdc0lutcorrection == INVALID_FLOAT )
    {
      return( INVALID_FLOAT );
    }

  float t0offset = (cslewing0.getCalibPar(PmtIndx))->getPar0();	// t0 offset

  float OverFlow  = (coverflow0.getCalibPar(PmtIndx))->getPeakChannel();
  float dOverFlow = (coverflow0.getCalibPar(PmtIndx))->getDeviation();

  float tdc2ns;		// conversion from tdc to ns.
  tdc2ns  = (ctdcgain0.getCalibPar(PmtIndx))->getPar1();

  float timing;

  // cut on time 6 sigma from the overflow
  if ( (TDC>4*dOverFlow) && (TDC<(OverFlow - 6*dOverFlow)) ) {
    timing = TDC*tdc2ns - tdc0lutcorrection - t0offset;
/*
cout << "ch " << PmtIndx
     << " TDC " << TDC
     << " tdc0lutcorrection " << tdc0lutcorrection
     << " t0offset " << t0offset << endl;
*/
    return( timing );
  } else {
    return( INVALID_FLOAT );
  }
}

/// return the hit time for tvc 1 in ns.
float ZdcCalib::getHitTime1(int PmtIndx, int TDC, int ADC)
{
  float tdc1lutcorrection;
  if (ADC < 0 || ADC > 4095)
    {
      return( INVALID_FLOAT );
    }
  tdc1lutcorrection = (ctdc1lut.getCalibParLUT(PmtIndx))->getlut(ADC);
  if ( tdc1lutcorrection == INVALID_FLOAT )
    {
      return( INVALID_FLOAT );
    }

  float t0offset = (cslewing1.getCalibPar(PmtIndx))->getPar0();	// t0 offset

  float OverFlow = (coverflow1.getCalibPar(PmtIndx))->getPeakChannel();
  float dOverFlow= (coverflow1.getCalibPar(PmtIndx))->getDeviation();

  float tdc2ns;		// conversion from tdc to ns.
  tdc2ns = (ctdcgain1.getCalibPar(PmtIndx))->getPar1();

  float timing;

  // cut on time 6 sigma from the overflow
  if ( (TDC>4*dOverFlow) && (TDC < (OverFlow - 6*dOverFlow)) ) {
    timing = TDC*tdc2ns - tdc1lutcorrection - t0offset;
/*
cout << "ch " << PmtIndx
     << " TDC " << TDC
     << " tdc1lutcorrection " << tdc1lutcorrection
     << " t0offset " << t0offset << endl;
*/
    return( timing );
  } else {
    return( INVALID_FLOAT );
  }
}

///
float ZdcCalib::getAdc(int PmtIndx, int ADC)
{
  float Pedestal = (cpedestal.getCalibPar(PmtIndx))->getPeakChannel();
  return (ADC-Pedestal);
}

///
void ZdcCalib::showParameters()
{
  printf(" ### Loaded calibration parameters ###\n");

  printf("Ped:        ");
  for (int PmtIndx=0; PmtIndx<8; PmtIndx++ ) {
    printf("%9.3g", (cpedestal.getCalibPar(PmtIndx))->getPeakChannel());
  }

  printf("\nEnergyGain: ");
  for (int PmtIndx=0; PmtIndx<8; PmtIndx++ ) {
    printf("%9.5g", (cadcgain.getCalibPar(PmtIndx))->getPar0() );
  }

  printf("\nAdcGain:    ");
  for (int PmtIndx=0; PmtIndx<8; PmtIndx++ ) {
    printf("%9.5g", (cadcgain.getCalibPar(PmtIndx))->getPar1() );
  }

  printf("\nOflow0:     ");
  for (int PmtIndx=0; PmtIndx<8; PmtIndx++ ) {
    printf("%9.5g", (coverflow0.getCalibPar(PmtIndx))->getPeakChannel() );
  }

  printf("\nOflow1:     ");
  for (int PmtIndx=0; PmtIndx<8; PmtIndx++ ) {
    printf("%9.5g", (coverflow1.getCalibPar(PmtIndx))->getPeakChannel() );
  }

  printf("\nOffset0:    ");
  for (int PmtIndx=0; PmtIndx<8; PmtIndx++ ) {
    printf("%9.5g", (cslewing0.getCalibPar(PmtIndx))->getPar0());
  }

  printf("\nOffset1:    ");
  for (int PmtIndx=0; PmtIndx<8; PmtIndx++ ) {
    printf("%9.5g", (cslewing1.getCalibPar(PmtIndx))->getPar0());
  }

  printf("\nSlew0:      ");
  for (int PmtIndx=0; PmtIndx<8; PmtIndx++ ) {
    printf("%9.5g", (cslewing0.getCalibPar(PmtIndx))->getPar1());
  }

  printf("\nSlew1:      ");
  for (int PmtIndx=0; PmtIndx<8; PmtIndx++ ) {
    printf("%9.5g", (cslewing1.getCalibPar(PmtIndx))->getPar1());
  }

  printf("\nTGain0:     ");
  for (int PmtIndx=0; PmtIndx<8; PmtIndx++ ) {
    printf("%9.6g", (ctdcgain0.getCalibPar(PmtIndx))->getPar1());
  }

  printf("\nTGain1:     ");
  for (int PmtIndx=0; PmtIndx<8; PmtIndx++ ) {
    printf("%9.6g", (ctdcgain1.getCalibPar(PmtIndx))->getPar1());
  }

  printf("\n");
}

int ZdcCalib::restore(const PHTimeStamp& time)
{
  int status = 0; 

  status += coverflow0.restore(time, "overflow0");  
  status += coverflow1.restore(time, "overflow1");  
  status += cpedestal.restore(time, "pedestal"); 
  status += cpmtgain.restore(time,  "pmtgain");  
  status += cadcgain.restore(time,  "adc");  
  status += ctdcgain0.restore(time, "tdc0"); 
  status += ctdcgain1.restore(time, "tdc1"); 
  status += cslewing0.restore(time, "slewpar0"); 
  status += cslewing1.restore(time, "slewpar1"); 
  status += ctdc0lut.restorelut(time, "tdc0lut"); 
  status += ctdc1lut.restorelut(time, "tdc1lut"); 
  status += ctzero.restore(time, "tzero"); 
  status += czvtx.restore(time, "zvtx"); 
  status += csmdoffset.restore(time, "smdoffset"); 

  return(status); 
}

int ZdcCalib::restore(const char* filename, const char *type)
{
  int status = 0; 

  if ( strcmp(type,"overflow0")==0 )      status += coverflow0.restore(filename,type);
  else if ( strcmp(type,"overflow1")==0 ) status += coverflow1.restore(filename,type);
  else if ( strcmp(type,"pedestal")==0 )  status += cpedestal.restore(filename,type);
  else if ( strcmp(type,"pmtgain")==0 )   status += cpmtgain.restore(filename,type);
  else if ( strcmp(type,"adc")==0 )       status += cadcgain.restore(filename,type);
  else if ( strcmp(type,"tdc0")==0 )      status += ctdcgain0.restore(filename,type);
  else if ( strcmp(type,"tdc1")==0 )      status += ctdcgain1.restore(filename,type);
  else if ( strcmp(type,"slewpar0")==0 )  status += cslewing0.restore(filename,type);
  else if ( strcmp(type,"slewpar1")==0 )  status += cslewing1.restore(filename,type);
  else if ( strcmp(type,"tdc0lut")==0 )   status += ctdc0lut.restore(filename,type);
  else if ( strcmp(type,"tdc1lut")==0 )   status += ctdc1lut.restore(filename,type);
  else if ( strcmp(type,"tzero")==0 )     status += ctzero.restore(filename,type);
  else if ( strcmp(type,"zvtx")==0 )      status += czvtx.restore(filename,type);
  else if ( strcmp(type,"smdoffset")==0 ) status += csmdoffset.restore(filename,type);
  else
    {
      cerr << PHWHERE << "ZdcCalib::restore() unknown type " << type << " specified" << endl;
    }

  return status; 
}

int ZdcCalib::restore(const char* filename)
{
  PHString InFile = filename;
  PHString f00 = ".overflow0";
  PHString f01 = ".overflow1";
  PHString f02 = ".pedestal";
  PHString f03 = ".pmtgain";
  PHString f04 = ".adc";
  PHString f05 = ".tdc0";
  PHString f06 = ".tdc1";
  PHString f07 = ".slewpar0";
  PHString f08 = ".slewpar1";
  PHString f09 = ".tdc0lut";
  PHString f10 = ".tdc1lut";
  PHString f11 = ".tzero";
  PHString f12 = ".zvtx";
  PHString f13 = ".smdoffset";

  PHString name0 = InFile + f00;
  PHString name1 = InFile + f01;
  PHString name2 = InFile + f02;
  PHString name3 = InFile + f03;
  PHString name4 = InFile + f04;
  PHString name5 = InFile + f05;
  PHString name6 = InFile + f06;
  PHString name7 = InFile + f07;
  PHString name8 = InFile + f08;
  PHString name9 = InFile + f09;
  PHString name10 = InFile + f10;
  PHString name11 = InFile + f11;
  PHString name12 = InFile + f12;
  PHString name13 = InFile + f13;

  int status = 0; 

  status += coverflow0.restore( name0.getString(),"overflow0");  
  status += coverflow1.restore( name1.getString(),"overflow1");  
  status += cpedestal.restore( name2.getString(),"pedestal"); 
  status += cpmtgain.restore(  name3.getString(),"pmtgain");  
  status += cadcgain.restore(  name4.getString(),"adc");  
  status += ctdcgain0.restore( name5.getString(),"tdc0"); 
  status += ctdcgain1.restore( name6.getString(),"tdc1"); 
  status += cslewing0.restore( name7.getString(),"slewpar0"); 
  status += cslewing1.restore( name8.getString(),"slewpar1"); 
  status += ctdc0lut.restorelut( name9.getString(),"tdc0lut"); 
  status += ctdc1lut.restorelut( name10.getString(),"tdc1lut" ); 
  status += ctzero.restore( name11.getString(),"tzero" ); 
  status += czvtx.restore( name12.getString(),"zvtx" ); 
  status += csmdoffset.restore( name13.getString(),"smdoffset" ); 

  return(status); 
}

int ZdcCalib::restore()
{
  coverflow0.restore("overflow0");  
  coverflow1.restore("overflow1");  
  cpedestal.restore("pedestal"); 
  cpmtgain.restore("pmtgain");  
  cadcgain.restore("adc");  
  ctdcgain0.restore("tdc0"); 
  ctdcgain1.restore("tdc1"); 
  cslewing0.restore("slewpar0"); 
  cslewing1.restore("slewpar1"); 
  ctdc0lut.restore("tdc0lut");
  ctdc1lut.restore("tdc1lut");
  ctzero.restore("tzero");
  czvtx.restore("zvtx");
  csmdoffset.restore("smdoffset");

  return(0); 
}
