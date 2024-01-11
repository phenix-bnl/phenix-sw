
#ifndef __ACCRAWV1_H_
#define __ACCRAWV1_H_

#include "TClonesArray.h"

#include "AccRaw.h"
#include "AccSnglRawv1.h"

class AccRawv1 : public AccRaw
{
 public:
  AccRawv1();
  AccRawv1(const AccRawv1&);
  AccRawv1& operator=(const AccRawv1&);
  virtual ~AccRawv1();

  AccRawv1* clone() const;

  // the standard PHObject response functions
  void Reset();
  int isValid() const;
  void identify(std::ostream& os=std::cout) const;

  // actual implementations of the set/get methods
  void set_nraw (const unsigned int NRAW) {nRaw = NRAW; return;}
  int  get_nraw () const {return nRaw;}

  // routines to manipulate the particle array
  int set_TClonesArraySize (const unsigned int nch);
  void AddRaw              (const unsigned int ich);
  void RemoveRaw           (const unsigned int ich);
  AccSnglRawv1* AddRaw     (const unsigned int ich, const AccSnglRaw& sngl);
  AccSnglRawv1* get_raw    (const unsigned int ich) const;

  void set_boxid(const int iraw, const int val);
  void set_adc(const int iraw, const int ipmt, const int val);
  void set_tdc(const int iraw, const int ipmt, const int val);
  void set_adcpost(const int iraw, const int ipmt, const int val);
  void set_adcpre(const int iraw, const int ipmt, const int val);

  int get_boxid(const int iraw) const;
  int get_adc(const int iraw, const int ipmt) const;
  int get_tdc(const int iraw, const int ipmt) const;
  int get_adcpost(const int iraw, const int ipmt) const;
  int get_adcpre(const int iraw, const int ipmt) const;

 protected:
  TClonesArray* GetRaw() const {return AccRawHits;}
  unsigned int  nRaw;
  TClonesArray* AccRawHits;

 private:
  // copy this to dest
  void copyto(AccRawv1& dest) const;

  ClassDef(AccRawv1,1)
};

#endif