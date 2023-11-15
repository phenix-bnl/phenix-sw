//  Declaration of class PdbTRDLike
//  Author: Cesar Luiz da Silva

#ifndef __PDBTRDLIKE_HH__
#define __PDBTRDLIKE_HH__

#include "PdbCalChan.hh"
#ifndef __CINT__
#include <cstddef>
#endif

struct likerange
{
  float min;
  float max;
  float prob_e;
  float prob_p;
};

class PdbTRDlike : public PdbCalChan
{
public:
  PdbTRDlike();
  PdbTRDlike(const likerange binrange); //variable range with Ppion and Pelectron
  virtual ~PdbTRDlike( );

  likerange get_likebin();
   
  void set_likebin(likerange);

  virtual void print() const;
  
private:
   void zero();
   
private:
  float min;
  float max;
  float prob_e;
  float prob_p;

  ClassDef(PdbTRDlike,1);
};

#endif /* __PDBTRDLIKE_HH__ */
