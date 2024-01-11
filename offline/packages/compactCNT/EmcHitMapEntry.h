#ifndef __EMCHITMAPENTRY_H_
#define __EMCHITMAPENTRY_H_

#include <iostream>

class EmcHitMapEntry
{
 public:
  EmcHitMapEntry();
  virtual ~EmcHitMapEntry() {}

  virtual void identify(std::ostream &os=std::cout) const;

  // Here are the very explicit set routines...
  void set_emcrawtdc(const int val) {emcrawtdc = val;}
  void set_emcrawadc(const int val) {emcrawadc = val;}
  void set_emcrawadclg(const int val) {emcrawadclg = val;}
  void set_id(const short int i) {id = i;};
  // Here are the very explicit "get" routines...
  int get_emcrawtdc() const {return emcrawtdc;}
  int get_emcrawadc() const {return emcrawadc;}
  int get_emcrawadclg() const {return emcrawadclg;}
  short int get_id() {return id;}

 protected:
  short int id;
  int emcrawtdc; 
  int emcrawadc; 
  int emcrawadclg;
};

#endif /* EMCHITMAPENTRY */
