#ifndef __HBDMINICELLV1_H_
#define __HBDMINICELLV1_H_

#include "PHObject.h"
#include "HbdMiniCell.h"

class HbdMiniCellv1 : public HbdMiniCell
{

 public:

  HbdMiniCellv1();
  HbdMiniCellv1(HbdMiniCellv1 *cell);  
  virtual ~HbdMiniCellv1() {}

  // The "standard PHObject response" functions...
  void Reset();
  int  isValid() const;
  void identify(std::ostream &os=std::cout) const;

  // Set the data members
  void set_adcch (const short val) {adcch = val; return;}
  void set_charge (const short val) {charge = val; return;}

  // Get the data members
  short get_adcch () const { return adcch;}
  short get_charge () const { return charge;}

 protected:

  // Data member definition

  short adcch;   	 // ADC channel number
  short charge;          // Charge deposit
                         // It is (adc0+1+2)-(adc8+9+10).
                         // Note, it is not divided by "3" !!!
                         // This is to avoid using "float" (4 bytes)

  ClassDef(HbdMiniCellv1,1)

};

#endif /* __HBDMINICELLV1_H_ */
