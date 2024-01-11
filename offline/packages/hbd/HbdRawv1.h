#ifndef __HBDRAWV1_H_
#define __HBDRAWV1_H_

#include "PHObject.h"
#include "HbdRaw.h"

#define HBDSAMPLE 32


class HbdRawv1 : public HbdRaw
{

 public:

  HbdRawv1();
  HbdRawv1(HbdRawv1 *raw);  
  virtual ~HbdRawv1() {}

  // The "standard PHObject response" functions...
  void Reset();
  int  isValid() const;
  void identify(std::ostream &os=std::cout) const;

  // Set the data members
  void set_padid (const int val) {padid = val; return;}
  void set_clock (const int sample, const short val) {clock[sample] = val; return;}
  void set_rawadc (const int sample, const short val) {rawadc[sample] = val; return;}
  void set_charge (const int sample, const float val) {charge[sample] = val; return;}

  // Get the data members
  int get_padid () const { return padid;}
  short get_clock (const int sample) const { return clock[sample];}
  short get_rawadc (const int sample) const { return rawadc[sample];}
  float get_charge (const int sample) const { return charge[sample];}

 protected:

  // Data member definition

  int padid;   	            // pad id
  short clock[HBDSAMPLE];   // time-of-flight
  short rawadc[HBDSAMPLE];  // Raw ADC
  float charge[HBDSAMPLE];  // Charge converted

  ClassDef(HbdRawv1,1)

};

#endif /* __HBDRAWV1_H_ */
