#ifndef __MUTRGHITARRAY__
#define __MUTRGHITARRAY__

#include "PHObject.h"
#include <map>

class MutrgHit;
class TClonesArray;

class MutrgHitArray : public PHObject{
public:
  typedef std::map<unsigned int,MutrgHit*> private_map;
  typedef std::map<unsigned int,MutrgHit*>::iterator private_itr;
  typedef std::map<unsigned int,MutrgHit*>::const_iterator const_private_itr;
  typedef std::pair<private_itr,private_itr> private_itr_pair;
  typedef std::pair<const_private_itr,
		    const_private_itr> const_private_itr_pair;

  MutrgHitArray(const char *mutrg_hit_class="");
  virtual ~MutrgHitArray(void);
  virtual MutrgHitArray* Create(const char *mutrg_hit_class="");
  virtual void Reset(void);
  virtual void Clear(Option_t* ="") { Reset(); }

  virtual int Set(const MutrgHitArray *mutrg_hits_org);

  virtual int Readback(void);

  virtual MutrgHit* Insert(unsigned int key);
  virtual MutrgHit* Insert(int arm,int station,int octant,
			   int halfoctant,int strip);
  virtual MutrgHit* Insert(int arm,int station,int octant,
			   int halfoctant,int gap,int cathode,int strip);

  virtual MutrgHit* InsertCheck(unsigned int key);
  virtual MutrgHit* InsertCheck(int arm,int station,int octant,
				int halfoctant,int strip);
  virtual MutrgHit* InsertCheck(int arm,int station,int octant,
				int halfoctant,int gap,int cathode,int strip);

  virtual MutrgHit* Find(unsigned int key) const;
  virtual const_private_itr Begin(void) const;
  virtual const_private_itr End(void) const;
  virtual const_private_itr_pair Range(void) const;
  virtual const_private_itr_pair Range(int arm) const;
  virtual const_private_itr_pair Range(int arm,int station) const;
  virtual const_private_itr_pair Range(int arm,int station,
				       int octant,int halfoctant) const;

  virtual void Remove(unsigned int key);
  virtual void Remove(const MutrgHit *hit);

  virtual void ExtendHitClock(int nclk);
  virtual void ShiftHitClock(int nclk);

  const private_map* Get(void) const{return &mutrg_hitmap;}

  virtual const char* GetMutrgHitName(void);

  virtual void print(std::ostream &os=std::cout) const;

protected:
  std::map<unsigned int,MutrgHit*> mutrg_hitmap; //!
  TClonesArray *mutrg_hits;

  ClassDef(MutrgHitArray,1)
};

#endif /* __MUTRGHITARRAY__ */
