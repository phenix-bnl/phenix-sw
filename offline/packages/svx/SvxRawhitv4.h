// ===================
// FILE: SvxRawhitv4.h
// ===================

#ifndef __SVXRAWHITV4_HH_
#define __SVXRAWHITV4_HH_

#include "SvxRawhit.h"
#include "SvxRawhitList.h"

/**
 * Implementation class (ver. 4) of SvxRawhit.
 * Added isOkForClustering flag.
 *
 * @date  Created by D. McGlinchey <darren.mcglinchey@colorado.edu> on 10/10/2013
 */
class SvxRawhitv4 : public SvxRawhit
{

 public:

  // Constructor(s) & destructor
  // """""""""""""""""""""""""""
  SvxRawhitv4(SvxRawhitList* lst = NULL, SvxRawhit* rawhit = NULL);
  SvxRawhitv4(SvxRawhit*     rawhit                              );
  virtual ~SvxRawhitv4()
    {/*std::cout << "SvxRawhitv4 object destroyed" << std::endl;*/}

  // The "standard PHObject response" functions...
  // """""""""""""""""""""""""""""""""""""""""""""
  void Reset();
  void identify(std::ostream &os=std::cout) const;

  // Set the data members
  // """"""""""""""""""""
  void set_hitID      (const int val)
    { hitID      =         val; if ( rawhitList ) rawhitList->set_hitIDsorted(false)    ;}
  void set_svxSection (const int val)
    { svxSection = (short) val; if ( rawhitList ) rawhitList->set_sensorIDsorted(false) ;}
  void set_layer      (const int val)
    { layer      = (short) val; if ( rawhitList ) rawhitList->set_sensorIDsorted(false) ;}
  void set_ladder     (const int val)
    { ladder     = (short) val; if ( rawhitList ) rawhitList->set_sensorIDsorted(false) ;}
  void set_sensor     (const int val)
    { sensor     = (short) val; if ( rawhitList ) rawhitList->set_sensorIDsorted(false) ;}

  void set_sensorSection (const int val)
    { sensorSection = (short) val ; if ( rawhitList ) rawhitList->set_sensorIDsorted(false) ;}
  void set_sensorReadout (const int val)
    { sensorReadout = (short) val ; if ( rawhitList ) rawhitList->set_sensorIDsorted(false) ;}
  void set_channel       (const int val)
    { channel       =         val ; if ( rawhitList ) rawhitList->set_sensorIDsorted(false) ;}

  void set_adc           (const int val) { adc           =         val ;}
  void set_sensorType    (const int val) { sensorType    = (short) val ;}
  void set_pixelModule   (const int val) { pixelModule   = (short) val ;}
  void set_pixelROC      (const int val) { pixelROC      = (short) val ;}
  void set_HotDeadFlag   (const int val) { HotDeadFlag   =         val ;}
  void set_isOkForClustering (const bool val) {isOkForClustering = val ;}


  // Get the data members
  // """"""""""""""""""""
  int get_sensorSection() const { return (int) sensorSection ;}
  int get_sensorReadout() const { return (int) sensorReadout ;}
  int get_sensorType   () const { return (int) sensorType    ;}
  int get_channel      () const { return       channel       ;}
  int get_adc          () const { return       adc           ;}
  int get_pixelModule  () const { return       pixelModule   ;}
  int get_pixelROC     () const { return       pixelROC      ;}
  int get_HotDeadFlag  () const { return       HotDeadFlag   ;}
  bool get_isOkForClustering () const {return isOkForClustering;}

  // Sortability
  void set_ListPointer (SvxRawhitList* lst) { rawhitList  = lst ;}
  void set_compareChan (unsigned int   val) { compareChan = val ;}
  Int_t Compare        (const TObject* rawhit) const;
  
  // Methods
  // """""""
  virtual SvxHit* Clone()           { return new SvxRawhitv4(this); }
  virtual void    Copy(SvxHit* hit);
  void print() const;

 protected:

  // Data member definition
  short sensorSection ; ///< sensor section number
  short sensorReadout ; ///< sensor readout number: 0 pixel/x-strip; 1 u-strip
  short sensorType    ; ///< Sensor type (1-9 pixel, 11-19 & 21-29 stipixel)
  int   HotDeadFlag   ; ///< Flag on Rawhit if it fails Hot/Dead Map check 0=Good, -1=Dead, 1=Hot, 2=ADC:RMS Fail(strip)
  int   channel       ; ///< channel number
  int   adc           ; ///< adc value (must be "int", NOT "short")
  short pixelModule   ; //   pixel module (half-ladder) number (range: 0-59)
  short pixelROC      ; //   pixel readout chip number (range: 0-7)
  bool isOkForClustering;///< Flag if this Rawhit should be used in clustering, true=yes, false=no

  /// Pointer to the container
  SvxRawhitList*       rawhitList;    //! 

  /// Comparison/sorting of sensorSection, sensorReadout & channel (varies from 0 to 3)
  static unsigned int compareChan;

  ClassDef(SvxRawhitv4,1)
};

#endif
