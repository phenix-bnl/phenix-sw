#ifndef __emcObjectFillerRegistry_h__
#define __emcObjectFillerRegistry_h__

#include <list>
#include <iostream>

class emcObjectFiller;
class PHObject;
class PHCompositeNode;

/** (OLD?) (Singleton) Registry for object fillers. 
@ingroup deprecated
*/

class emcObjectFillerRegistry
{

public:
  static void add(emcObjectFiller*);
  static void remove(emcObjectFiller*);

  static emcObjectFiller* findFiller(PHCompositeNode*, PHObject&,
				    int verbose=0);

  static emcObjectFiller* findFiller(PHObject&,
				    int verbose=0);

  static void print(std::ostream& out = std::cout);

private:
  static std::list<emcObjectFiller*>* fillers(void);
};

#endif
