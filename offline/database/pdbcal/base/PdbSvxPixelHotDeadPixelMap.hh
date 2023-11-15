// Declaration of class PdbSvxPixelHotDeadPixelMap
// Purpose: SVX Pixel hot and dead maps
//          Now the class is for one pixel with module, ROC, column, and row.
//          Only with hot or dead pixel
// Author: Hiroyuki Sako

#ifndef __PDBSVXPIXELHOTDEADPIXELMAP_HH__
#define __PDBSVXPIXELHOTDEADPIXELMAP_HH__

#include <string>

#include "PdbCalChan.hh"

class PdbSvxPixelHotDeadPixelMap : public PdbCalChan
{
 public:
 
//   enum SvxPixelChannelStatus {
//     SVX_PIXEL_NORMAL=0b00000000,
//     SVX_PIXEL_COLD=0b00000001,
//     SVX_PIXEL_HOT=0b00000010,
//     SVX_PIXEL_UNSTABLE=0b00000011,
//     SVX_PIXEL_VERYCOLD=0b00000100,
//     SVX_PIXEL_VERYHOT=0b00000101,
//     SVX_PIXEL_DEAD=0b00000110,
//     SVX_PIXEL_UNKNOWN=0b00000111,
//     
//     SVX_CLUSTER_COLD=0b00001000,
//     SVX_CLUSTER_HOT=0b00010000,
//     SVX_CLUSTER_UNSTABLE=0b00011000,
//     SVX_CLUSTER_VERYCOLD=0b00100000,
//     SVX_CLUSTER_VERYHOT=0b00101000,
//     SVX_CLUSTER_DEAD=0b00110000,
//     SVX_CLUSTER_UNKNOWN=0b00111000,
//     
//     SVX_PIXEL_OUTOFRANGE=0b10000000  /** Channel is out-of-range */ 
//   };
  
  enum SvxPixelChannelStatus {
    SVX_PIXEL_NORMAL=0x00,
    SVX_PIXEL_COLD=0x01,
    SVX_PIXEL_HOT=0x02,
    SVX_PIXEL_UNSTABLE=0x03,
    SVX_PIXEL_VERYCOLD=0x04,
    SVX_PIXEL_VERYHOT=0x05,
    SVX_PIXEL_DEAD=0x06,
    SVX_PIXEL_UNKNOWN=0x07,

    SVX_CLUSTER_COLD=0x08,
    SVX_CLUSTER_HOT=0x10,
    SVX_CLUSTER_UNSTABLE=0x18,
    SVX_CLUSTER_VERYCOLD=0x20,
    SVX_CLUSTER_VERYHOT=0x28,
    SVX_CLUSTER_DEAD=0x30,
    SVX_CLUSTER_UNKNOWN=0x38,

    SVX_PIXEL_OUTOFRANGE=0x80  /** Channel is out-of-range */ 
    };

 public:
   PdbSvxPixelHotDeadPixelMap();

   PdbSvxPixelHotDeadPixelMap (const PdbSvxPixelHotDeadPixelMap &);

   virtual ~PdbSvxPixelHotDeadPixelMap();
   
  int getStatus() const;

  int getStatusNoRangeCheck() const;

  char getRawStatus() const;

  void setStatus (const int module, const int ROC, const int column, const int row, const int status);

  void setRawStatus (const int module, const int ROC, const int column, const int row, const signed char rawStatus);
  
  int getModule() const;
  int getROC() const;
  int getColumn() const;
  int getRow() const;

  virtual void print() const;
	
 private:
  static const int NMODULE=60;
  static const int NROC=8;
  static const int NCOLUMN=32;
  static const int NROW=256;

  bool rangeCheck(const int module, const int ROC, const int column, const int row) const;
  bool rangeCheck() const;

  unsigned char cModule; //0-59
  unsigned char cROC;    //0-8
  unsigned char cColumn; //0-31
  unsigned char cRow;    //0-255
  signed char   cStatus;

  ClassDef (PdbSvxPixelHotDeadPixelMap, 1);
};

#endif /* __PDBSVXPIXELHOTDEADPIXELMAP_HH__ */

