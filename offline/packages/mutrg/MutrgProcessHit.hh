#ifndef __MUTRGPROCESSHIT__
#define __MUTRGPROCESSHIT__

#include <string>

class Event;
class PHCompositeNode;
class TMutHitMap;
class MutrgHeaderArray;
class MutrgHit;
class MutrgHitArray;

class MutrgProcessHit{
public:
  MutrgProcessHit(bool init_flag=true);
  virtual ~MutrgProcessHit(void);
  virtual void CreateObject(void);

  virtual int Init(PHCompositeNode *node,bool flag_reg=false);
  virtual int InitRun(PHCompositeNode *node,bool flag_reg=false);
  virtual int ProcessEvent(PHCompositeNode *node);

  virtual int FillHit(PHCompositeNode *node);
  virtual int FillHit(Event *evt);
  virtual int Associate(TMutHitMap *mut_hitmap);

  virtual int SetMutrgHitArray(MutrgHitArray *hits,bool flag_delete=false);
  virtual int SetMutrgHeaderArray(MutrgHeaderArray *headers,
				  bool flag_delete=false);
  virtual MutrgHitArray* GetMutrgHitArray(void){return mutrg_hits;}
  virtual MutrgHeaderArray *GetMutrgHeaderArray(void){return mutrg_headers;}
  virtual MutrgHitArray* RegMutrgHitArray(PHCompositeNode *node,
					  const char *name,
					  const char *rename="");
  virtual MutrgHeaderArray* RegMutrgHeaderArray(PHCompositeNode *node,
						const char *name,
						const char *rename="");

  const char *ClassName(void) const{return class_name.c_str();}

protected:
  std::string class_name;
  MutrgHitArray *mutrg_hits;
  MutrgHeaderArray *mutrg_headers;
};

#endif /* __MUTRGPROCESSHIT__ */
