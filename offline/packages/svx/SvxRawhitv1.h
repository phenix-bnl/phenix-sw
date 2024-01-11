// ===================
// FILE: SvxRawhitv1.h
// ===================

#ifndef __SVXRAWHITV1_HH_
#define __SVXRAWHITV1_HH_

#include "SvxRawhit.h"
#include "SvxRawhitList.h"

/**
 * Implementation class (ver. 1) of SvxRawhit.
 *
 * @date  Created  by V. L. Rykov on 08-Feb-2004
 * @date  Modified by V. L. Rykov on 11-May-2004: Sorting related stuff is added.
 */
class SvxRawhitv1 : public SvxRawhit
{

 public:

  // Constructor(s) & destructor
  // """""""""""""""""""""""""""
  SvxRawhitv1(SvxRawhitList* lst = NULL, SvxRawhit* rawhit = NULL);
  SvxRawhitv1(SvxRawhit*     rawhit                              );
  virtual ~SvxRawhitv1()
    {/*std::cout << "SvxRawhitv1 object destroyed" << std::endl;*/}

  // The "standard PHObject response" functions...
  // """""""""""""""""""""""""""""""""""""""""""""
  void Reset();
  void identify(std::ostream &os=std::cout) const;

  // Set the data members
  // """"""""""""""""""""
  void set_hitID      (const int val)
    { hitID      =         val; if ( rawhitList ) rawhitList->unSort() ;}
  void set_svxSection (const int val)
    { svxSection = (short) val; if ( rawhitList ) rawhitList->unSort() ;}
  void set_layer      (const int val)
    { layer      = (short) val; if ( rawhitList ) rawhitList->unSort() ;}
  void set_ladder     (const int val)
    { ladder     = (short) val; if ( rawhitList ) rawhitList->unSort() ;}
  void set_sensor     (const int val)
    { sensor     = (short) val; if ( rawhitList ) rawhitList->unSort() ;}

  void set_sensorSection (const int val)
    { sensorSection = (short) val ; if ( rawhitList ) rawhitList->unSort() ;}
  void set_sensorReadout (const int val)
    { sensorReadout = (short) val ; if ( rawhitList ) rawhitList->unSort() ;}
  void set_channel       (const int val)
    { channel       =         val ; if ( rawhitList ) rawhitList->unSort() ;}

  void set_adc           (const int val) { adc           =         val ;}
  void set_sensorType    (const int val) { sensorType    = (short) val ;}

  // Get the data members
  // """"""""""""""""""""
  int get_sensorSection() const { return (int) sensorSection ;}
  int get_sensorReadout() const { return (int) sensorReadout ;}
  int get_sensorType   () const { return (int) sensorType    ;}
  int get_channel      () const { return       channel       ;}
  int get_adc          () const { return       adc           ;}

  // Sortability
  void set_ListPointer (SvxRawhitList* lst) { rawhitList  = lst ;}
  void set_compareChan (unsigned int   val) { compareChan = val ;}
  Int_t Compare        (const TObject* rawhit) const;
  
  // Methods
  // """""""
  virtual SvxHit* Clone()           { return new SvxRawhitv1(this); }
  virtual void    Copy(SvxHit* hit);
  void print() const;

 protected:

  // Data member definition
  short sensorSection ; ///< sensor section number
  short sensorReadout ; ///< sensor readout number: 0 pixel/x-strip; 1 u-strip
  short sensorType    ; ///< Sensor type (1-9 pixel, 11-19 & 21-29 stipixel)
  int   channel       ; ///< channel number
  int   adc           ; ///< adc value (must be "int", NOT "short")

  /// Pointer to the container
  SvxRawhitList*       rawhitList;    //! 

  /// Comparison/sorting of sensorSection, sensorReadout & channel (varies from 0 to 3)
  static unsigned int compareChan;

  ClassDef(SvxRawhitv1,1)
};

#endif
