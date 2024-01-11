#ifndef __MRPCHITV1_H_
#define __MRPCHITV1_H_

#include <TClonesArray.h>

#include <MrpcHit.h>
#include <MrpcSnglHitv1.h>

class MrpcHitv1 : public MrpcHit
{
 public:
  MrpcHitv1();
  MrpcHitv1(const MrpcHitv1&);
  MrpcHitv1& operator=(const MrpcHitv1&);
  virtual ~MrpcHitv1();

  MrpcHitv1* clone() const;

  // the standard PHObject response functions
  void Reset();
  int isValid() const;
  void identify(std::ostream& os=std::cout) const;

  // actual implementations of the set/get methods
  void set_nhit (const unsigned int NHIT) {nHit = NHIT; return;}
  int  get_nhit () const {return nHit;}

  // routines to manipulate the particle array
  int set_TClonesArraySize (const unsigned int nhit);
  void AddHit              (const unsigned int ihit);
  void RemoveHit           (const unsigned int ihit);
  MrpcSnglHitv1* AddHit     (const unsigned int ihit, const MrpcSnglHit& sngl);
  MrpcSnglHitv1* get_hit    (const unsigned int ihit) const;

  void set_slatid(const int ihit, const int val);
  void set_time(const int ihit, const float val);
  void set_time_dig(const int ihit, const float val);
  void set_charge(const int ihit, const float val);
  void set_xyz(const int ihit, int ixyz, const float val);

  int   get_slatid(const int ihit) const;
  float get_time(const int ihit) const;
  float get_time_dig(const int ihit) const;
  float get_charge(const int ihit) const;
  float get_xyz(const int ihit, const int ixyz) const;

 protected:
  TClonesArray* GetHit() const {return MrpcHits;}
  unsigned int  nHit;
  TClonesArray* MrpcHits;

 private:
  // copy this to dest
  void copyto(MrpcHitv1& dest) const;

  ClassDef(MrpcHitv1,1)
};

#endif
