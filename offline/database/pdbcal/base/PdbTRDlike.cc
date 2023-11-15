//  Implementation of class PdbTRDLike
//  Author: Cesar Luiz da Silva

#include <iostream>
#include "PdbTRDlike.hh"

using namespace std;

PdbTRDlike::PdbTRDlike()
{
   zero();
}

PdbTRDlike::PdbTRDlike(likerange bin)
{
  set_likebin(bin);
}

void 
PdbTRDlike::zero()
{
  min = 0.;
  max = 0.;
  prob_e = 0.;
  prob_p = 0.;
}

PdbTRDlike::~PdbTRDlike()
{
}

likerange 
PdbTRDlike::get_likebin()
{
  likerange likebintmp;
  likebintmp.min = min;
  likebintmp.max = max;
  likebintmp.prob_e = prob_e;
  likebintmp.prob_p = prob_p;
  return likebintmp;
}

void 
PdbTRDlike::set_likebin(likerange bin)
{
  min = bin.min;
  max = bin.max;
  prob_e = bin.prob_e;
  prob_p = bin.prob_p;
}

void 
PdbTRDlike::print() const
{
  cout << "value range : [" << min << ", " << max << "]" << endl
       << "probability to be an electron : " << prob_e << endl
       << "probability to be other particle : " << prob_p << endl; 
}







