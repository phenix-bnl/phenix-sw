#ifndef __MRPCRAWV1_H_
#define __MRPCRAWV1_H_

#include <TClonesArray.h>

#include <MrpcRaw.h>
#include <MrpcSnglRawv1.h>

class MrpcRawv1 : public MrpcRaw
{
 public:
  MrpcRawv1();
  MrpcRawv1(const MrpcRawv1&);
  MrpcRawv1& operator=(const MrpcRawv1&);
  virtual ~MrpcRawv1();

  MrpcRawv1* clone() const;

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
  MrpcSnglRawv1* AddRaw     (const unsigned int ich, const MrpcSnglRaw& sngl);
  MrpcSnglRawv1* get_raw    (const unsigned int ich) const;

  void set_slatid(const int iraw, const int val);
  void set_t3(const int iraw, const int ich, const int val);
  void set_t4(const int iraw, const int ich, const int val);
  void set_q1(const int iraw, const int ich, const int val);
  void set_q3(const int iraw, const int ich, const int val);
  void set_qvc(const int iraw, const int ich, const int val);
  void set_t3_dig(const int iraw, const int ich, const int val);
  void set_t4_dig(const int iraw, const int ich, const int val);

  int get_slatid(const int iraw) const;
  int get_t3(const int iraw, const int ich) const;
  int get_t4(const int iraw, const int ich) const;
  int get_q1(const int iraw, const int ich) const;
  int get_q3(const int iraw, const int ich) const;
  int get_qvc(const int iraw, const int ich) const;
  int get_t3_dig(const int iraw, const int ich) const;
  int get_t4_dig(const int iraw, const int ich) const;

 protected:
  TClonesArray* GetRaw() const {return MrpcRawHits;}
  unsigned int  nRaw;
  TClonesArray* MrpcRawHits;

 private:
  // copy this to dest
  void copyto(MrpcRawv1& dest) const;

  ClassDef(MrpcRawv1,1)
};

#endif
