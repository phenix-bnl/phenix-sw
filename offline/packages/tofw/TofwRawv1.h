#ifndef __TOFWRAWV1_H_
#define __TOFWRAWV1_H_

#include <TofwRaw.h>
#include <TofwSnglRawv1.h>

class TClonesArray;

class TofwRawv1 : public TofwRaw 
{
 public:
  TofwRawv1();
  TofwRawv1(const TofwRawv1&);
  TofwRawv1& operator=(const TofwRawv1&);
  virtual ~TofwRawv1();

  TofwRawv1* clone() const;

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
  TofwSnglRawv1* AddRaw      (const unsigned int ich, const TofwSnglRaw& sngl);
  TofwSnglRawv1* get_raw     (const unsigned int ich) const;

  
  void set_boxid(const int iraw, const int val);
  void set_chamberid(const int iraw, const int val);
  void set_stripid(const int iraw, const int val);
  void set_t3(const int iraw, const int ich, const int val);
  void set_t4(const int iraw, const int ich, const int val);
  void set_q1(const int iraw, const int ich, const int val);
  void set_q3(const int iraw, const int ich, const int val);
  void set_tvc(const int iraw, const int ich, const float val);
  void set_qvc(const int iraw, const int ich, const float val);
  
  int get_boxid(const int iraw) const;
  int get_chamberid(const int iraw) const;
  int get_stripid(const int iraw) const;
  int get_t3(const int iraw, const int ich) const;
  int get_t4(const int iraw, const int ich) const;
  int get_q1(const int iraw, const int ich) const;
  int get_q3(const int iraw, const int ich) const;
  float get_tvc(const int iraw, const int ich) const;
  float get_qvc(const int iraw, const int ich) const;

 protected:
  TClonesArray* GetRaw() const {return TofwRawHits;}
  unsigned int  nRaw;
  TClonesArray* TofwRawHits;

 private:
  // copy this to dest
  void copyto(TofwRawv1& dest) const;

  ClassDef(TofwRawv1,1)
};

#endif
