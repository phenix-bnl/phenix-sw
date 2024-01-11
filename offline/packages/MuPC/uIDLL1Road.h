#ifndef __UIDLL1ROAD_HH__
#define __UIDLL1ROAD_HH__

#include "PHObject.h"
#include <iostream>

class uIDLL1SnglRoad;

class uIDLL1Road : public PHObject
{


 public:
  virtual ~uIDLL1Road() {}


  //  Virtual methods should be over-ridden...
  virtual void set_nRoad_deep(const unsigned int NTRACK); 
  virtual int  get_nRoad_deep() const ;

  //  "Set" functions add(remove) uIDLL1SnglRoad objects to(from) the collection...
  virtual int  set_TClonesArraySize(const unsigned int ntrk) {return 0;}
  virtual void AddDeepRoad       (const unsigned int itrk) {return;}
  virtual void RemoveDeepRoad    (const unsigned int itrk) {return;}
  virtual uIDLL1SnglRoad* AddDeepRoad(const unsigned int itrk, const uIDLL1SnglRoad &road) {return NULL;}

  //  "Get" function retreives a pointer to any of the objects in the collection...
  virtual uIDLL1SnglRoad* get_deep_road(const unsigned int itrk) const;

  //  "Clone" method allows to make additional containers based upon this one...
  virtual uIDLL1Road* clone() const;


  // Standard functions of all inheritors of PHObject classes...
  virtual void Reset();
  virtual int isValid() const;
  virtual void identify(std::ostream &os = std::cout) const;


  ClassDef(uIDLL1Road,1)
};
#endif /* __UIDLL1ROAD_HH__ */
