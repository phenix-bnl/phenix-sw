#include <iostream>
#include "PbScIndexer.h"

PbScIndexer * PbScIndexer::single=0;
int PbScIndexer::access_count    =0;

PbScIndexer::PbScIndexer()
{
}

PbScIndexer::~PbScIndexer()
{
}
// ********************************************************************** 

PbScIndexer * PbScIndexer::buildPbScIndexer()
{
  if(!single) single = new PbScIndexer();
  access_count++;
  return single;
};
// ********************************************************************** 

int PbScIndexer::deletePbScIndexer()
{
 if(single)
   {
     access_count--;
     delete single;
		 single =  0;
   }
 return access_count;
};
 
     
//_____________________________________________________________________________
int 
PbScIndexer::xySMiSM(int x, int y)
{
  int rv = -1;

  if ( x >= 0 && x < 6 && y >= 0 && y < 3 ) 
    {
      rv = y*6 + x; 
    }

  return rv;
}  

// ********************************************************************** 
 
inline void  PbScIndexer::SMxySM(int i, int & x, int & y) 
{ 
  y = i/6; 
  x = i-y*6; 
}; 
       
// ********************************************************************** 
  
  int PbScIndexer::SMxySMTiST(int iSM ,int xSMTower, int ySMTower) 
{ 
  int x, y;
  SMxySM(iSM, x, y);
  return (y*12+ySMTower)*72+x*12+xSMTower; 
};    
 
// ********************************************************************** 
  
  int PbScIndexer::SMiSMTiST(int iSM ,int iSMTower)
{ 
  int x, y, xx, yy;
  SMxySM(iSM, x, y);
  iSM144TxySM144T(iSMTower, xx, yy);
  return (y*12+yy)*72+x*12+xx; 
};    
 
 
// ********************************************************************** 
 
   void PbScIndexer::iSTxyST(int const  iTower, int  & xTower, int & yTower) 
{ 
  yTower = iTower/72; 
  xTower = iTower%72; 
};  
 
// ********************************************************************** 
 
  void PbScIndexer::iST_SMInd(int const iTower, int & ism, int & ismt, int & xsmt, int & ysmt) 
{ 
  int x, y, xsm, ysm;
  iSTxyST(iTower, x, y);
  xsm  = x/12;
  ysm  = y/12;
  ism      = ysm*6 + xsm;
  xsmt     = x%12;
  ysmt     = y%12;
  ismt     = ysmt*12+xsmt;
}; 

//////////////////////////////////////////////////////////////////////////////////                    
 
