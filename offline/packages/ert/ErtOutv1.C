#include <iostream>
#include "ErtOutv1.h"

#define ERThit_max 944

ClassImp(ErtOutv1);

using namespace std;

ErtOutv1::ErtOutv1()
{
 ERTtrigmode= new int[ERThit_max];
 ERTarm= new int[ERThit_max];
 ERTsector= new int[ERThit_max];
 ERTsm= new int[ERThit_max];
 for (int i=0; i<ERThit_max; i++)
   {
     ERTtrigmode[i] = -9999;
     ERTarm[i] = -9999;
     ERTsector[i] = -9999;
     ERTsm[i] = -9999;
   }
  ERThit_N = 0;
}

ErtOutv1::ErtOutv1(const ErtOutv1& o)
{
  ERTtrigmode= new int[ERThit_max];
  ERTarm= new int[ERThit_max];
  ERTsector= new int[ERThit_max];
  ERTsm= new int[ERThit_max];
  o.copyTo(*this);
}

ErtOutv1&
ErtOutv1::operator=(const ErtOutv1& o)
{
  if ( this != &o ) 
    {
      o.copyTo(*this);
    }
  return *this;
}

void
ErtOutv1::copyTo(ErtOutv1& o) const
{ 
  o.ERThit_N = ERThit_N;
  for ( int i = 0; i < ERThit_N; ++i )
    {
      o.ERTtrigmode[i] = ERTtrigmode[i];
      o.ERTarm[i] = ERTarm[i];
      o.ERTsm[i] = ERTsm[i];
      o.ERTsector[i] = ERTsector[i];
    }
}

ErtOutv1::~ErtOutv1()
{  
 delete [] ERTtrigmode;
 delete [] ERTarm;
 delete [] ERTsector;
 delete [] ERTsm;
}

int 
ErtOutv1::isValid() const
{
  return((ERThit_N>0) ? 1 : 0);
}

void 
ErtOutv1::Reset()
{
  for (int i = 0; i<ERThit_N; i++)
    {
      ERTtrigmode[i] = -9999;
      ERTarm[i] = -9999;
      ERTsector[i] = -9999;
      ERTsm[i] = -9999;
    }
  ERThit_N=0; 
  return;
}

void 
ErtOutv1::identify(ostream& out) const
{
  out << "identify yourself: I am a ErtOutv1 object" << endl;
  for ( int i = 0; i < ERThit_N; ++i )
    {
      out << "mode=" << ERTtrigmode[i] << " arm=" << ERTarm[i]
	  << " sector=" << ERTsector[i] << " sm=" << ERTsm[i]
	  << endl;
    }
}


int 
ErtOutv1::set_ERTbit(int trigmode, int arm, int sector, int sm)
{
  // ERThit_N is updated at the end so
  // we fill the correct index here
  ERTtrigmode[ERThit_N]=trigmode;
  ERTarm[ERThit_N]=arm;
  ERTsector[ERThit_N]=sector;
  ERTsm[ERThit_N]=sm;
  ERThit_N++;

  if(ERThit_N >= ERThit_max){
    ERThit_N--;
    return(-1);
  } 

 return 0;
} 


int
ErtOutv1::get_ERTbit(int trigmode, int arm, int sector, int sm) const
{
  int ii;
  for (ii = 0;ii < ERThit_N;ii++)
    {
      if (ERTtrigmode[ii] == trigmode &&
	  ERTarm[ii] == arm &&
	  ERTsector[ii] == sector &&
	  ERTsm[ii] == sm)
	{
	  return 1;
	}
    }
  return 0;
}

