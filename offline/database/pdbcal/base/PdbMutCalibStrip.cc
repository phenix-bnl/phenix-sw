// $Id: PdbMutCalibStrip.cc,v 1.16 2019/06/07 16:21:16 slash Exp $

//  Declaration of class PdbMutCalibStrip
//  Purpose: Store MuTr calibration information; one object, one strip
//  Author: silvermy (silvermy@lanl.gov)

#include <PdbMutCalibStrip.hh>

#include <phool.h> // for PHWHERE

#include <cstdlib>
#include <iomanip>
#include <iostream>
#include <map>
#include <sstream>

using namespace std;
static const int MAXITERATIONS = 10;
static const int MAXSTEPS = 2;
//Warn us a bit..
static const int maxWarnStrip=6;
static std::map<int,int> warnedStrips;

//______________________________________________________
PdbMutCalibStrip::PdbMutCalibStrip()
{
  set_indices(0, 0, 0, 0, 0, 0, 0); // init to dummy values
  zero();
}

//______________________________________________________
PdbMutCalibStrip::PdbMutCalibStrip(
  const int iarm, const int istation, const int ioctant,
  const int ihalfoctant, const int igap, const int iplane,
  const int istrip)
{
  set_indices(iarm, istation, ioctant, ihalfoctant, igap, iplane, istrip); 
  zero();
}

//______________________________________________________
PdbMutCalibStrip::PdbMutCalibStrip( const PdbMutCalibStrip& ref )
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
  
  for( int i = 0; i < ref.get_ncalibpar(); i++ )
  { set_calibpar( i, ref.get_calibpar(i) ); }

}

//_________________________________________________________________
PdbMutCalibStrip& PdbMutCalibStrip::operator = (const PdbMutCalibStrip& ref )
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
  
  for( int i = 0; i < ref.get_ncalibpar(); i++ )
  { set_calibpar( i, ref.get_calibpar(i) ); }
  
  return *this;
  
}

//______________________________________________________
void PdbMutCalibStrip::zero() 
{
  pedestal = 0;
  gain = 0; 
  rms = 0;

  for (int i=0; i<NMUTCALIBPAR; i++) calibpar[i] = 0;

  Status = 0;
  Step = 0;
  Saturation = 0;
}

//______________________________________________________
void PdbMutCalibStrip::print() const 
{ write(cout); }

//______________________________________________________
void PdbMutCalibStrip::write(ostream& os) const 
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
  
  for (int i=0; i<NMUTCALIBPAR; i++) { os << calibpar[i] << " " << setw(9) << setprecision(6); }
  
  os << setw(4) << Status << " " 
     << setw(1) << Step << " " 
     << setw(3) << Saturation;
  os << endl;

}

//______________________________________________________
void PdbMutCalibStrip::read(istream& is) 
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
  for (int i=0; i<NMUTCALIBPAR; i++)
  { is >> calibpar[i]; }
  
  is >> Status
     >> Step
     >> Saturation;

}

void 
PdbMutCalibStrip::set_indices (const int iarm, const int istation, const int ioctant,
	                       const int ihalfoctant, const int igap,const  int iplane,
	                       const int istrip)
{
  ArmNum =        iarm;
  StationNum =    istation;
  OctantNum =     ioctant;
  HalfOctantNum = ihalfoctant;
  GapNum =        igap;
  PlaneNum =      iplane;
  StripNum =      istrip;
}

void 
PdbMutCalibStrip::set_values (const float fped, const float fgain, const float frms)
{  
  pedestal = fped;
  gain =     fgain; 
  rms =      frms;
}

void 
PdbMutCalibStrip::set_calibparAll (const float temp[NMUTCALIBPAR])
{
  for (int i=0; i<NMUTCALIBPAR; i++)
    {
      calibpar[i] = temp[i];
    }
}

void 
PdbMutCalibStrip::get_indices (int & iarm, int & istation, int & ioctant,
	     int & ihalfoctant, int & igap, int & iplane,
	     int & istrip) const
{
  iarm =        ArmNum;         
  istation = 	StationNum;     
  ioctant = 	OctantNum;      
  ihalfoctant = HalfOctantNum;  
  igap = 	GapNum;         
  iplane = 	PlaneNum;       
  istrip = 	StripNum;       
}

void 
PdbMutCalibStrip::get_values (float & fped, float & fgain, float & frms) const
{
  fped =  pedestal;
  fgain = gain;     
  frms =  rms;      
}

void 
PdbMutCalibStrip::get_calibparAll (float temp[NMUTCALIBPAR]) const
{
  for (int i=0; i<NMUTCALIBPAR; i++)
    {
      temp[i] = calibpar[i];
    }
} 

int
PdbMutCalibStrip::getUniqueId () const
{
  int id = StripNum;
  // arm: 0,1; bit 17
  // station: 0,1,2; bits 15,16
  // octant: 0-7; bits 12-14 
  // hoct: 0,1; bit 11
  // gap: 0,1,2; bit 9,10
  // plane: 0,1; bit 8
  // strip: 0..160 (max on North); bits 0-7
 
  id += PlaneNum << 8;
  id += GapNum << 9;
  id += HalfOctantNum << 11;
  id += OctantNum << 12; 
  id += StationNum << 15; 
  id += ArmNum << 17; 

  return id;
} 

// bool
// PdbMutCalibStrip::getName(char *name = 
// 			  "muTr_ArmX_StaY_OctZ_HOctW_GapI_PlaneJ_StripK") const
// {
//   ostringstream namestring;
//   namestring << "muTr_Arm" << ArmNum << "_Sta" << StationNum << "_Oct" << OctantNum
// 	     << "_HOct" << HalfOctantNum << "_Gap" << GapNum << "_Plane" << PlaneNum
// 	     << "_Strip" << StripNum;
//   strcpy(name,namestring.str().c_str());
//   return true;
// } 

// bool
// PdbMutCalibStrip::getName(string &name) const
// {
//   ostringstream namestring;
//   namestring << "muTr_Arm" << ArmNum << "_Sta" << StationNum << "_Oct" << OctantNum
// 	     << "_HOct" << HalfOctantNum << "_Gap" << GapNum << "_Plane" << PlaneNum
// 	     << "_Strip" << StripNum;
//   name = namestring.str();
//   return true;
// } 


//_______________________________________________________
string PdbMutCalibStrip::getName( void ) const
{
  ostringstream namestring;
  namestring << "muTr_Arm" << ArmNum << "_Sta" << StationNum << "_Oct" << OctantNum
       << "_HOct" << HalfOctantNum << "_Gap" << GapNum << "_Plane" << PlaneNum
       << "_Strip" << StripNum;
  return namestring.str();
} 


int 
PdbMutCalibStrip::getAdc(const int dac) const
{
  float fdac = (float) dac;

  return getAdc(fdac); 
}

int 
PdbMutCalibStrip::getAdc(const float charge) const
{
  int dac = (int) (charge + 0.5);
  // check input somewhat
  if ( dac<0 || dac>MAXMUTRDAC )
    {
      //      cerr << "PdbMutCalibStrip::getAdc - DAC value " << dac 
      //	   << " is out of bounds " << endl;
      return -999;
    } 
  if (gain == 0)
    {
      return (int) (pedestal+0.5); // no gain => no charge
    } 
  float convped = MAXMUTRADC - pedestal; // pedestal is a value around MAXMUTRADC
  // therefore conv(enient)ped is a value around 0. 
  // use values that increase with the signal throughout routine
  // and return reversed value
  float adcval = convped + gain*charge; // default is just a straight line

  // Make an 'integration' of how much the non-linear deviation has cost you
  // up to this point
  int nsteps = dac/Step;
  // interpolation of non-linear terms for better accuracy to part of next step
  float stepfactor = (charge/Step - nsteps)/Step;
  float olddiff = 0;
  float sumdev = calcNonLinContrib(nsteps, stepfactor, &olddiff);
  // float sumdev = sumNonLinContrib(dacval); // faster alternative?

  adcval -= sumdev;
  // we are done. Now reverse it back to the real scale
  adcval = MAXMUTRADC - adcval;
  int adc = (int) (adcval+0.5); // return nearest integer

  return adc;
}

int 
PdbMutCalibStrip::getDac(const int adc) const
{
  float dacval = getCharge(adc); 
  int dac = (int) (dacval+0.5);
  if (dac > MAXMUTRDAC)
    {
      return MAXMUTRDAC; // this is the maximum DAC value
    } 

  return dac;
}

float 
PdbMutCalibStrip::getCharge(const int adc) const
{
  if (gain == 0)
    {
      return 0; // no gain => no charge
    } 

  // just do the things done in getAdc backwards..
  float dacval = (pedestal - (float)adc)/gain; // initial guess
  int dac = (int) (dacval+0.5);
  if (dac > MAXMUTRDAC)
    {
      return (float) MAXMUTRDAC; // this is the maximum DAC value
    } 

  // Make an 'integration' of how much the non-linear deviation has cost you
  // up to this point
  int nsteps = dac/Step;
  // interpolation of non-linear terms for better accuracy to part of next step
  float stepfactor = (dacval/Step - nsteps)/Step;
  float olddiff = 0;
  float sumdev = calcNonLinContrib(nsteps, stepfactor, &olddiff);
  // float sumdev = sumNonLinContrib(dacval); // faster alternative?
  // Some minor adjustments can be needed: maybe off-by-one or a few steps
  dacval += sumdev/gain;

  int extrastep, diffstep, sign, iteration = 0;
   
  while (iteration < MAXITERATIONS) 
    { 
      extrastep = (int) (dacval/Step - nsteps); 
      if (extrastep == 0) break; 

      // we didn't go far enough or too far, avoid an infinite loop also 
      iteration++;
      stepfactor = (dacval/Step - nsteps - extrastep)/Step; 
      // part of a step that we are undershooting in iteration  

      if (extrastep>0)
	{
	  sign = 1; // go forward
	}
      else
	{
	  extrastep = - extrastep;
	  stepfactor = - stepfactor;
	  sign = -1; // back up
	}
      sumdev = calcNonLinContrib(extrastep, stepfactor, &olddiff, 
				 nsteps, sign);

      dacval += sumdev/gain;
      diffstep = sign*extrastep;
      nsteps += diffstep; 
      extrastep = (int) (dacval/Step - nsteps);  
    
      if ( extrastep!=0 ) 
	{ // continued iteration: maybe not necessary?
	  if ( (extrastep+diffstep) == 0) // 
	    { // You just want to go back where you just came from. 
	      iteration = MAXITERATIONS + 1; // abort, keep value
	    }
	  else if ( extrastep > 0 && diffstep > 0 && 
		    dacval > Saturation) // 
	    { // You want to move even further into Saturation land  
	      // This is a sign that we have gone too far into uncertain 
	      // territory. Let's back up a step and stop here.
	      dacval -= sumdev/gain;
	      iteration = MAXITERATIONS + 1; // abort, keep value
	    }
	  else if (abs(extrastep) > MAXSTEPS || abs(extrastep) > abs(diffstep) )
	    { // something wrong - trying too many steps
	      // possibly a bad channel or bad fit
	      iteration = MAXITERATIONS; // abort, back to initial/linear guess 
	      //Do we have an entry yet?
	      std::map<int,int>::iterator stripiter = warnedStrips.find(getUniqueId());
 	      if ( stripiter != warnedStrips.end() )
 		{
 		  //not the first time add it on
 		  (*stripiter).second++;
 		}
 	      else
 		{
 		  //If this is the first one add it to the vector
 		  warnedStrips[getUniqueId()]=1;
 		}
	      
	      //Only print the number of times we should
	      if ( warnedStrips[getUniqueId()] == maxWarnStrip )
		{
		  cerr << PHWHERE << " suspicious channel (5 times or more) "
		       << getName() << endl;
		}
	    }
	}
    }

  if (iteration == MAXITERATIONS)
    { // did not converge! we'll have to settle for the initial guess
      dacval = (pedestal - (float) adc)/gain; 
      return -dacval; // negative return value flags 'failure' 
    }
  return dacval;
}

// linear approximations
int 
PdbMutCalibStrip::getLinAdc(const int dac) const
{
  float fdac = (float) dac;

  return getLinAdc(fdac); 
}

int 
PdbMutCalibStrip::getLinAdc(const float charge) const
{
  int dac = (int) (charge + 0.5);
  // check input somewhat
  if ( dac<0 || dac>MAXMUTRDAC )
    {
      cerr << "PdbMutCalibStrip::getLinAdc - DAC value " << dac 
	   << " is out of bounds " << endl;
      return -999;
    } 
  float adcval = pedestal - gain*charge;
  int adc = (int) (adcval+0.5); // return nearest integer

  return adc;
}

int 
PdbMutCalibStrip::getLinDac(const int adc) const
{
  float dacval = getLinCharge(adc); 
  int dac = (int) (dacval+0.5);
  if (dac > MAXMUTRDAC)
    {
      return MAXMUTRDAC; // this is the maximum DAC value
    } 

  return dac;
}

float 
PdbMutCalibStrip::getLinCharge(const int adc) const
{
  if (gain == 0)
    {
      return 0; // no gain => no charge
    } 
  // just do the things done in getAdc backwards..
  float dacval = (pedestal - (float)adc)/gain; // initial guess
  int dac = (int) (dacval+0.5);
  if (dac > MAXMUTRDAC)
    {
      return (float) MAXMUTRDAC; // this is the maximum DAC value
    } 
  else 
    {
      return dacval;
    }
}

// help method to calculate non linear contributions
float 
PdbMutCalibStrip::calcNonLinContrib(const int nsteps, const float stepfactor, 
				    float *olddiff, const int firststep, 
				    const int sign) const
{
  float center = calibpar[0];
  float dacstepval, distCenter, distSat, stepadd;
  float sumdev = 0;
  for (int i = 0; i<= nsteps; i++) 
    {
      dacstepval = (firststep + i*sign)*Step;
      distCenter = dacstepval - center;
      if (distCenter<0) // low side
	{
	  stepadd = calibpar[1]*distCenter*distCenter;
	}
      else
	{
	  if ( (dacstepval - Saturation) < 0) 
	    { // not yet saturated: normal
	      *olddiff = calibpar[2]*distCenter*distCenter;
	      stepadd = *olddiff;
	    }
	  else 
	    {// we have passed saturation point, new rules apply here
	      // olddiff is kept from previous step
	      distSat = dacstepval - Saturation; 
	      stepadd = *olddiff + calibpar[3]*distSat;
	    }
	}
      if (i != nsteps) 
	{
	  sumdev += stepadd;
	}
      else 
	{
	  // how large part of this correction should be kept, and added 
	  // to sumdev? It should be proportional to how large a part of 
	  // this last step that should be included, e.g. stepfactor
	  sumdev += stepadd*stepfactor;

	}
    }

  return sumdev*sign;
}

// a hopefully faster sum version.., not for the iterative correction though
float 
PdbMutCalibStrip::sumNonLinContrib(const float dacval) const
{
  // use the well-known formula:
  // Sum^{i=N}_{i=0} i^2 = N(N+1)((2N+1)/6
  // 1) Figure out how many steps we should go to either side of the center
  // 2) Then sum up the contributions for the high and low end.
  // 3) If we are out in the saturation region, we should make a linear sum 
  // also - we need to calculate the last non-saturated diff for that  
  // Sum^{i=N}_{i=0} i = N(N+1)/2

  float sumdev = 0;
  float sum2, nextterm, corr;
  // Part 1:
  int nhigh, nlow, nlowmiss;
  float center = calibpar[0];
  int centerstep = (int) (center/Step);
  float centerdiff = center/Step - centerstep;
  float stepfactorhigh, stepfactorlow, stepfactorlowmiss;
  int dacstep = (int) (dacval/Step);

  if (dacval > Saturation)
    {
      // Part 3
      // what was the latest diff value?
      int laststep = (int) (Saturation/Step);
      if (laststep*Step == Saturation) laststep--;
      float distCenter = laststep*Step - center;
      float olddiff = calibpar[2]*distCenter*distCenter;
      int nsat = dacstep - laststep - 1;
      float stepfactorsat = (dacval/Step - laststep - nsat - 1)/Step;
      float satdiff = Saturation/Step - laststep;

      float sum = nsat*(nsat+1)/2.0;
      corr = - nsat*satdiff;
      sumdev += (nsat+stepfactorsat) * olddiff;
      sumdev += calibpar[3]*Step*(sum+corr+stepfactorsat*nsat);
      
      nhigh = laststep - centerstep;
      stepfactorhigh = 0;
      nlow = centerstep;
      nlowmiss = 0;
      stepfactorlow = 0;
      stepfactorlowmiss = 0;
    }
  else if (dacval > center)
    {
      nhigh = dacstep - centerstep;
      stepfactorhigh = ((dacval - center)/Step - nhigh)/Step;
      nlow = centerstep;
      nlowmiss = 0;
      stepfactorlow = 0;
      stepfactorlowmiss = 0;
    }
  else
    {
      nhigh = 0;
      stepfactorhigh = 0;
      nlow = dacstep;
      nlowmiss = centerstep - nlow;
      stepfactorlow = (dacval/Step - nlow)/Step;
      stepfactorlowmiss = (center/Step - nlowmiss)/Step;
    } 
  
  // The complication is that the steps do no generally start exactly at the
  // value, so we need a correction also
  // Sum (i-center)^2 = Sum (i-(centerstep + centerdiff))^2 = 
  // Sum (i-centerstep)^2 - 2*centerdiff*Sum (i-centerstep) + centerdiff^2*N
  // This is valid for the high side; for the low side, we switch - to +
  // for the centerdiff term  
  // Part 2:

  // High sum
  if (nhigh > 0) 
    {
      sum2 = nhigh*(nhigh+1)*(2*nhigh+1)/6;
      corr = centerdiff*(centerdiff*nhigh - nhigh*(nhigh+1)); 
      nextterm = (nhigh+1-centerdiff)*(nhigh+1-centerdiff);
      sumdev += calibpar[2]*Step*Step*(sum2 + corr + stepfactorhigh*nextterm);
    }
  // Low sum
  if (nlow > 0) 
    {
      int ntot = nlow + nlowmiss;
      sum2 = ntot*(ntot+1)*(2*ntot+1)/6;
      corr = centerdiff*(centerdiff*(ntot+1) + ntot*(ntot+1)); 
      nextterm = (ntot+1+centerdiff)*(ntot+1+centerdiff);
      float sum2miss = nlowmiss*(nlowmiss+1)*(2*nlowmiss+1)/6.0;
      float nexttermmiss = (nlowmiss+1+centerdiff)*(nlowmiss+1+centerdiff);
      corr -= centerdiff*(centerdiff*nlowmiss + nlowmiss*(nlowmiss+1)); 
      sumdev += calibpar[1]*Step*Step*(sum2 - sum2miss + corr +
				       stepfactorlow*nextterm -
				       stepfactorlowmiss*nexttermmiss );      
    }

  return sumdev;
}
