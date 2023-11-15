// $Id: PdbMutCalibStrip_v2.cc,v 1.1 2011/07/13 21:20:11 slash Exp $

//  Declaration of class PdbMutCalibStrip
//  Purpose: Store MuTr calibration information; one object, one strip
//  Author: silvermy (silvermy@lanl.gov)

#include <PdbMutCalibStrip_v2.hh>

#include <cstdlib>
#include <iomanip>
#include <iostream>
#include <map>
#include <sstream>

using namespace std;
//_______________________________________
PdbMutCalibStrip_v2::PdbMutCalibStrip_v2( )
{
  set_indices(0, 0, 0, 0, 0, 0, 0); // init to dummy values
  zero();
    //std::cout << "PdbMutCalibStrip_v2::PdbMutCalibStrip_v2" << std::endl;
}

//_______________________________________
PdbMutCalibStrip_v2::PdbMutCalibStrip_v2( const PdbMutCalibStrip& ref )
{
  // store indices
  set_indices(
    ref.getArm(),
    ref.getStation(),
    ref.getOctant(),
    ref.getHalfOctant(),
    ref.getGap(),
    ref.getPlane(),
    ref.getStrip() );

  // copy second strip parameters into first
  set_pedestal( ref.get_pedestal() );
  set_gain( ref.get_gain() );
  set_rms( ref.get_rms() );
  setStatus( ref.getStatus() );
  setSaturation( ref.getSaturation() );
  setStep( ref.getStep() );

  calibpar.clear();
  for( int i = 0; i < ref.get_ncalibpar(); i++ )
  {
    calibpar.push_back(ref.get_calibpar(i)); 
  }

  calibpar_rms.clear();

    //std::cout << "PdbMutCalibStrip_v2::PdbMutCalibStrip_v2 (copy constructor)" << std::endl;
}


//______________________________________________________
PdbMutCalibStrip_v2::PdbMutCalibStrip_v2(
  const int iarm, const int istation, const int ioctant,
  const int ihalfoctant, const int igap, const int iplane,
  const int istrip)
{
  set_indices(iarm, istation, ioctant, ihalfoctant, igap, iplane, istrip);
  zero();
}

//______________________________________________________
void PdbMutCalibStrip_v2::print() const
{ write(cout); }


//______________________________________________________
void PdbMutCalibStrip_v2::write(std::ostream& os) const
{
  os << setiosflags(ios::fixed)
     << setw(1); // one digit only for the first numbers
  os << ArmNum << " "
     << StationNum << " "
     << OctantNum << " "
     << HalfOctantNum << " "
     << GapNum << " "
     << PlaneNum<< " ";
  os << setw(3) << StripNum << " ";
  os << setw(8) << setprecision(3)
     << pedestal << " "
     << setw(6) << gain << " "
     << setw(5) << rms << " " << setw(7);

  for (unsigned int i=0; i<calibpar.size(); i++) { os << calibpar[i] << " " << setw(9) << setprecision(6); }

  os << Status << " " 
     << Step << " " 
     << Saturation;

  os << endl;

}

//______________________________________________________
void PdbMutCalibStrip_v2::read(istream& is)
{
  // opposite of write method
  is >> ArmNum
     >> StationNum
     >> OctantNum
     >> HalfOctantNum
     >> GapNum
     >> PlaneNum;
  is >> StripNum
     >> pedestal
     >> gain
     >> rms;
  for (int i=0; i<NMUTCALIBPAR_PIECEWISE; i++)
  { is >> calibpar[i]; }
  
  is >> Status
     >> Step
     >> Saturation;
}

//______________________________________________________
void PdbMutCalibStrip_v2::zero()
{
  pedestal = 0;
  gain = 0;
  rms = 0;

  calibpar.clear();
  for (int i=0; i<NMUTCALIBPAR_PIECEWISE; i++) calibpar.push_back(0.0);

  calibpar_rms.clear();
  for (int i=0; i<NMUTCALIBPAR_PIECEWISE; i++) calibpar_rms.push_back(0.0);

  Status = 0;
  Step = 0;
  Saturation = 0;
}


//_______________________________________
int PdbMutCalibStrip_v2::getAdc(const int dac) const
{
    //std::cout << "PdbMutCalibStrip_v2::getAdc" << std::endl;
    float fdac = (float) dac;

    return getAdc(fdac);
}

//_______________________________________
int PdbMutCalibStrip_v2::getAdc(const float charge) const
{
    //std::cout << "PdbMutCalibStrip_v2::getAdc" << std::endl;

  int dac = (int) (charge + 0.5);
  // check input somewhat
  if ( dac<0 || dac>MAXMUTRDAC )
    {
      std::cerr << "PdbMutCalibStrip::getAdc - DAC value " << dac
           << " is out of bounds " << std::endl;
      return -999;
    }
  if (gain == 0)
    {
      return (int) (pedestal+0.5); // no gain => no charge
    }

  float adcval;
  float lowadc, highadc;
  float lowdac, highdac;
  bool good_adc = false;
  int idac;

  for (idac=0; idac < NMUTCALIBPAR_PIECEWISE-1; idac++){
    if(charge <= DAC_VALUES[idac+1]){
      good_adc = true;
      break;
    }
  }

  if (good_adc){
    lowdac = (float)DAC_VALUES[idac];
    lowadc = calibpar[idac];
    highdac = (float)DAC_VALUES[idac+1];
    highadc = calibpar[idac+1];

    adcval = lowadc + (charge - lowdac)*(highadc - lowadc)/(highdac - lowdac);
  }
  else{
    std::cerr << "PdbMutCalibStrip::getAdc - DAC value " << dac
         << " is out of bounds " << std::endl;
    return -999;
    
  }

  // we are done. Now reverse it back to the real scale
  adcval = MAXMUTRADC - adcval;
  int adc = (int) (adcval+0.5); // return nearest integer

  return adc;

}

//_______________________________________
int PdbMutCalibStrip_v2::getDac(const int adc) const
{
    //std::cout << "PdbMutCalibStrip_v2::getDac" << std::endl;

    float dacval = getCharge(adc);
    int dac = (int) (dacval+0.5);
    if (dac > MAXMUTRDAC)
      {
        return MAXMUTRDAC; // this is the maximum DAC value
      }

    return dac;
}

//_______________________________________
float PdbMutCalibStrip_v2::getCharge(const int adc) const
{
  if (gain == 0)
    {
      return 0; // no gain => no charge
    }

  // just do the things done in getAdc backwards..
  float adc_calib = (float)(MAXMUTRADC - adc);

  float dacval;
  float lowadc = 0.0;
  float highadc= 0.0;
  float lowdac= 0.0;
  float highdac= 0.0;
  bool good_calib = false;
  int idac;

  for (idac=0; idac < NMUTCALIBPAR_PIECEWISE-1; idac++){
    if(adc_calib <= calibpar[idac+1]){
      good_calib = true;
      break;
    }
  }

  if (good_calib){
    lowdac = (float)DAC_VALUES[idac];
    lowadc = calibpar[idac];
    highdac = (float)DAC_VALUES[idac+1];
    highadc = calibpar[idac+1];
  
    dacval = lowdac + (adc_calib - lowadc)*(highdac - lowdac)/(highadc - lowadc);
  }
  else{
    dacval = 0.0;
  }


  return dacval;

}

//_______________________________________
void
PdbMutCalibStrip_v2::set_calibparAll (const float temp[NMUTCALIBPAR_PIECEWISE])
{
  for (int i=0; i<NMUTCALIBPAR_PIECEWISE; i++)
    {
      calibpar[i] = temp[i];
    }
}


//_______________________________________
void
PdbMutCalibStrip_v2::get_calibparAll (float temp[NMUTCALIBPAR_PIECEWISE]) const
{
  for (int i=0; i<NMUTCALIBPAR_PIECEWISE; i++)
    {
      temp[i] = calibpar[i];
    }
}

//_______________________________________
PdbMutCalibStrip_v2& PdbMutCalibStrip_v2::operator = (const PdbMutCalibStrip& ref )
{
 
  // store indices
  set_indices(
    ref.getArm(),
    ref.getStation(),
    ref.getOctant(),
    ref.getHalfOctant(),
    ref.getGap(),
    ref.getPlane(),
    ref.getStrip() );
   
  // copy second strip parameters into first
  set_pedestal( ref.get_pedestal() );
  set_gain( ref.get_gain() );
  set_rms( ref.get_rms() );
  setStatus( ref.getStatus() );
  setSaturation( ref.getSaturation() );
  setStep( ref.getStep() );

  calibpar.clear();
  for( int i = 0; i < ref.get_ncalibpar(); i++ )
    calibpar.push_back(ref.get_calibpar(i)); 

  calibpar_rms.clear();

  return *this;

}

