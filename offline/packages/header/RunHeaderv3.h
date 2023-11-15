#ifndef RUNHEADERV3_H
#define RUNHEADERV3_H

#include "RunHeaderv2.h"

#include <iostream>
#include <ctime>

///
class RunHeaderv3: public RunHeaderv2
{
 public:
  /// ctor
  RunHeaderv3();

  /// dtor
  virtual ~RunHeaderv3() {}
  
  RunHeaderv3* clone() const { return new RunHeaderv3(*this); }

  /// Clear Event 
  void Reset();

  /** identify Function from PHObject
      @param os Output Stream 
   */
  void identify(std::ostream& os = std::cout) const;

  /** identify v3 data
      @param os Output Stream 
   */
  void identifyv3(std::ostream& os = std::cout) const;

   /// get Current in Inner Coild
   int get_currentInner() const {return currentInner;}
   /// set Current in North Magnet
   void set_currentInner(const int icur) {currentInner = icur; return;}

 protected:

   int currentInner;

 private: // prevent doc++ from showing ClassDef
   ClassDef(RunHeaderv3,1)

};

#endif /* __RUNHEADERV3_H */

