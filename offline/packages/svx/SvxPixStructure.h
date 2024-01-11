// =======================
// FILE: SvxPixStructure.h
// =======================

#ifndef __SVXPIXSTRUCTURE_HH_
#define __SVXPIXSTRUCTURE_HH_

#include <PHObject.h>
#include <phool.h>

#include <iostream>

class SvxRawhit;
class SvxRawhitList;

/**
 * @brief  The abstract class for a pixel structure.
 *
 * Created by V. L. Rykov on 15-Feb-2004
 */
class SvxPixStructure : public PHObject
{
 public:
  // Constructor(s) & Destructor
  // """""""""""""""""""""""""""
  SvxPixStructure() {
    /*std::cout << "SvxPixStructure object created" << std::endl;*/
  }

  virtual ~SvxPixStructure() {
    /*std::cout << "SvxPixStructure object destroyed" << std::endl;*/
  }

  // Standard functions of all inheritors of PHObject classes...
  // """""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  virtual void Reset   ()        { PHOOL_VIRTUAL_WARN("Reset()"  )           ;}
  virtual int isValid  ()  const { PHOOL_VIRTUAL_WARN("isValid()"); return 0 ;}
  virtual void identify(std::ostream &os=std::cout) const {
    os << "Identify yourself: virtual SvxPixStructure object" << std::endl;
  }

  // Handling parameters
  // """""""""""""""""""
  // Get parameters
  virtual int   get_nXpitch    () const
    { PHOOL_VIRTUAL_WARN("int get_nXpitch() const"     ); return -9999 ;}
  virtual double get_xPitch     () const
    { PHOOL_VIRTUAL_WARN("double get_xPitch() const"    ); return -9999.;}
  virtual double get_xhalfWidth () const
    { PHOOL_VIRTUAL_WARN("double get_xhalfWidth() const"); return -9999.;}
  virtual int   get_nZpitch    () const
    { PHOOL_VIRTUAL_WARN("int get_nZpitch() const"     ); return -9999 ;}
  virtual double get_zPitch     () const
    { PHOOL_VIRTUAL_WARN("double get_zPitch() const"    ); return -9999.;}
  virtual double get_zhalfWidth () const
    { PHOOL_VIRTUAL_WARN("double get_zhalfWidth() const"); return -9999.;}

  // Print parameters
  virtual void printPar()  const { PHOOL_VIRTUAL_WARN("void printPar() const"); }

  // Manipulations of rawPtr list
  // """"""""""""""""""""""""""""
  virtual unsigned int get_nRawptr() const
    { PHOOL_VIRTUAL_WARN("unsigned int get_nRawptr() const"        ); return 0    ;}
  virtual unsigned int getRawListLength() const
    { PHOOL_VIRTUAL_WARN("unsigned int getRawListLength() const"   ); return 0    ;}
  virtual SvxRawhit*   get_rawPtr  (const unsigned int ind) const
    { PHOOL_VIRTUAL_WARN("SvxRawhit* getRawPtr(const unsigned int)"); return NULL ;}

  virtual SvxRawhit* addRawhit(SvxRawhit* rawhit)
    { PHOOL_VIRTUAL_WARN("SvxRawhit* addRawhit(SvxRawhit*)"        ); return NULL ;}
  virtual int removeRawhit(SvxRawhit* rawhit)
    { PHOOL_VIRTUAL_WARN("int removeRawhit(SvxRawhit*)"            ); return 0 ;}
  virtual void rawlistReset()
    { PHOOL_VIRTUAL_WARN("void rawlistReset()"                     );             ;}

  // Handling pixelList
  // """"""""""""""""""
  // Find fired pixel(s)
  virtual int firePixels(double x, double z, double chrg)
    {PHOOL_VIRTUAL_WARN("int firePixels(double,double,double)"); return -9999             ;}

  virtual int firePixels(double xin,double zin,double xout,double zout,double chrg)
    {PHOOL_VIRTUAL_WARN("int firePixels(double,double,double,double,double)"); return -9999 ;}

  virtual int firePixelsAndShareCharge(double xin,double zin,double xout,double zout,double chrg)
    {PHOOL_VIRTUAL_WARN("int firePixelsAndShareCharge(double,double,double,double,double)"); return -9999 ;}
  virtual int firePixelsAndShareChargeXZ(double xin,double zin,double xout,double zout,double chrg)
    {PHOOL_VIRTUAL_WARN("int firePixelsAndShareChargeXZ(double,double,double,double,double)"); return -9999 ;}

  // Get data
  virtual int            get_xrow       (const unsigned int i) const
    { PHOOL_VIRTUAL_WARN("int get_xrow(unsigned int) const"       ); return -9999 ;}
  virtual int            get_zcolumn    (const unsigned int j) const
    { PHOOL_VIRTUAL_WARN("int get_zcolumn(unsigned int) const"    ); return -9999 ;}
  virtual int            get_charge     (const unsigned int k) const
    { PHOOL_VIRTUAL_WARN("int get_charge(unsigned int) const"     ); return -9999 ;}
  
  // Pixel position
  virtual double getXpixel(const int ix)                        const
    { PHOOL_VIRTUAL_WARN("double getXpixel(const int) const"       ); return -9999.;}
  virtual double getZpixel(const int iz)                        const
    { PHOOL_VIRTUAL_WARN("double getZpixel(const int) const"       ); return -9999.;}

  // Print data
  virtual void printData()  const { PHOOL_VIRTUAL_WARN("printData()") ;}

  ClassDef(SvxPixStructure, 1);
};

#endif
