#ifndef __EMCREJECTAFTERBURNER_H__
#define __EMCREJECTAFTERBURNER_H__

#include <TString.h>
#include <PHAfterBurner.h>

class PHCompositeNode;
class PHTimeStamp;

class EmcRejectAfterBurner : public PHAfterBurner
{
public:
  EmcRejectAfterBurner();
  virtual ~EmcRejectAfterBurner(){}

  void initialize(PHCompositeNode *topNode );
  void apply     (PHCompositeNode *udstNode);
 
  //Sets...
  void set_rejectdir(const char *dir);
  int  set_run(const int run);
  int  set_rejectfile(const char *f);
 
  //Gets...
  void dump() const;
 
  // These are necessary for all PHObjects:
  int  isValid() const;
  void identify (std::ostream &os=std::cout) const;
  void Reset() {}

  void Clear(Option_t *option = "");

  // Database access methods
  PHBoolean FetchFromDatabase(PHTimeStamp* Tsearch);
  PHBoolean FetchFromDatabase(int RunNUmber);
  PHBoolean UpdateDatabase(PHTimeStamp* Tbeg, PHTimeStamp* Tend);
  
protected:
  TString rejectdir;			//! directory with reject files
  Int_t   EmcNumReject;			// number of rejects found
  UChar_t RejectList[8][96][48];	// [sector][zpos][ypos]
 
  ClassDef(EmcRejectAfterBurner,2)
};

#endif	// __EMCREJECTAFTERBURNER_H__

