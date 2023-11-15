// Declaration of class PdbSvxPixelHotDeadChipMap
// Purpose: SVX Pixel hot and dead maps
//          Now the map is implemented as array of module, ROC, column, and row.
// Author: Hiroyuki Sako

#ifndef __PDBSVXPIXELHOTDEADCHIPMAP_HH__
#define __PDBSVXPIXELHOTDEADCHIPMAP_HH__

#include <string>

#include "PdbCalChan.hh"

class PdbSvxPixelHotDeadChipMap : public PdbCalChan
{
 public:
 
  enum SvxPixelChipStatus {
    SVX_CHIP_UNKNOWN=-3 , /** Channel is in unknown status */
    SVX_CHIP_OUTOFRANGE=-2,    /**<  out of range */
    SVX_CHIP_DEAD=-1,     /**< Channel never has output. */
    SVX_CHIP_NORMAL=0,    /**< Channel is functioning normally. */
    SVX_CHIP_HOT=1        /**< Channel has output whether there is an event or not. */
  };

 public:
   PdbSvxPixelHotDeadChipMap();

   PdbSvxPixelHotDeadChipMap (const PdbSvxPixelHotDeadChipMap &);

   virtual ~PdbSvxPixelHotDeadChipMap();
   
  int getStatus() const;

  int getStatusNoRangeCheck() const;

  char getRawStatus() const;

  int getModule() const;

  int getROC() const;

  void setStatus (const int module, const int ROC, const int status);

  void setRawStatus (const int module, const int ROC, const signed char rawStatus);

  virtual void print() const;
	
 private:
  bool rangeCheck(const int module, const int ROC) const;
  bool rangeCheck() const;

  static const int NMODULE=60;
  static const int NROC=8;
  static const int NCOLUMN=32;
  static const int NROW=256;
  unsigned char cModule; //0-59
  unsigned char cROC;    //0-8

  signed char cStatus;

  ClassDef (PdbSvxPixelHotDeadChipMap, 1);
};

#endif /* __PDBSVXPIXELHOTDEADCHIPMAP_HH__ */

