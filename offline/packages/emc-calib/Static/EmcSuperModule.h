#ifndef __EMCSUPERMODULE_H__
#define __EMCSUPERMODULE_H__

/** ABC of static characteristics of an EMCAL SuperModule.

  Can be used to store a lot of hardware specific information
  The only other important function - it provides for mapping of 
  the supermodule production numbers to the Sector sequential 
  SuperModule numbers
  It should also be the place to store specific information
  about FEE crate currently allocated to this supermodule
  NOTE: anything what can be treated like tower-specific calibration
  (even if it is due to pecularities of the FEE) should be stored in the 
  Sector object just in the order of towers in that sector
*/  

#include <Rtypes.h>

class EmcSuperModule
{
 public:
  EmcSuperModule();

  virtual ~EmcSuperModule();
  ///   Returns production Id (if known)
  virtual int   getProductionId() = 0;
  ///
  virtual float getScrLightYield(int & ) = 0;
	///
  virtual float getMuPeak(int & ) = 0;
	///
  virtual float getLaserRaw(int & ) = 0;
	///
  virtual float getIntSPD() = 0;
	///
  virtual float getIntSPDTP( ) = 0;
	///
   virtual int   getLgcNumber(int & Twr) = 0;
      /// gets U0 information from WA98. Nedded for debugging purposes on online side only.
   virtual float getU0(int & ) = 0;
      /// gets UT information from WA98. Nedded for debugging purposes on online side only.
   virtual float getUT(int & ) = 0;
      /// gets AY information from WA98. Nedded for debugging purposes on online side only.
   virtual float getAY(int & Twr) = 0;
      /// gets VY information from WA98. Nedded for debugging purposes on online side only.
   virtual float getVY(int & Twr) = 0;
      /// gets BL information from WA98. Nedded for debugging purposes on online side only.
   virtual float getBL(int & Twr) = 0;
      /// gets RS information from WA98. Nedded for debugging purposes on online side only.
   virtual float getRS(int & Twr) = 0;
      /// gets PMT signal of AY diode from WA98. Nedded for debugging purposes on online side only. Information is used in G0.
   virtual float getAYPeak(int & Twr) = 0;
      /// gets PIN signal of AY diode from WA98. Nedded for debugging purposes on online side only. Information is used in G0.
   virtual float getAYRef(int & Twr) = 0;
      /// gets PMT signal of AY diode from tests in 902. Nedded for debugging purposes on online side only.
   virtual float getTestPeak(int & Twr) = 0;
      /// gets PIN signal of AY diode from tests in 902. Nedded for debugging purposes on online side only. 
   virtual float getTestRef(int & Twr) = 0;
      /// gets additional calibration factor from WA98. Nedded for debugging purposes on online side only. Information is used in C0.
   virtual float getGC(int & Twr) = 0;
      /// gets calibration factor for PHENIX extracted from WA98 data.
      /// C0 = 10/487.5 * U0 * UT * GC
   virtual float getC0(int & Twr) = 0;
      /// gets calibration factor for PHENIX extracted from WA98 data.
      /// G0 = PM(AY)_T_WA98 / PIN(AY)_T_WA98
   virtual float getG0(int & Twr) = 0;
      ///
   virtual float getCF(int & Twr) = 0;
      ///

   virtual bool  LoadSMData(){return false;}
};

#endif
