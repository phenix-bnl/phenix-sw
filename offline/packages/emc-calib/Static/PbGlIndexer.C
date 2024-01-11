#include <iostream>
#include "PbGlIndexer.h"

PbGlIndexer * PbGlIndexer::single=0;
int PbGlIndexer::access_count=0;

//____________________________________________________________________
PbGlIndexer::PbGlIndexer()
{
}

PbGlIndexer::~PbGlIndexer()
{
}

//____________________________________________________________________
PbGlIndexer * PbGlIndexer::buildPbGlIndexer()
{
  if(!single) single = new PbGlIndexer();
  access_count++;
  return single;
};
//--------------------------------------------------------------------
int PbGlIndexer::deletePbGlIndexer()
{
	if(single)
		{
			access_count--;
			delete single;
		}
	return access_count;
};

//--------------------------------------------------------------------
  void PbGlIndexer::iCHi24Packi24PackT(int & Channel, int & SMInFee, int & TowerInSM) 
{ 
//
  int asics   = Channel/24;          //  2 tower wide columns
  int preamp = (Channel%24)/4;      //  2 tower wide rows counted 
                                        //  from above (0 at the top)
  int SMRow =  preamp/2;
  int SMCol =  asics/3;  

  SMInFee = 2 + SMRow + SMCol * 3;

  int input  = Channel%4;
//  convert input into x/y for cells within 'module' scope
  int xc, yc;
  yc = input/2;
  xc = 1-input%2;

  int RowInSM = ((preamp)-SMRow*2)*2+yc;
  int ColInSM = (asics-SMCol*3)*2+xc;
  TowerInSM =RowInSM*6+ColInSM;   
};

//--------------------------------------------------------------------
  int PbGlIndexer::iCHi144PackT(int & Channel) 
{ 
//
int PackList[144] = {
1, 0, 7, 6, 13, 12, 19, 18, 25, 24, 31, 30, 37, 36, 
43, 42, 49, 48, 55, 54, 61, 60, 67, 66, 3, 2, 9, 8, 
15, 14, 21, 20, 27, 26, 33, 32, 39, 38, 45, 44, 51, 
50, 57, 56, 63, 62, 69, 68, 5, 4, 11, 10, 17, 16, 23, 
22, 29, 28, 35, 34, 41, 40, 47, 46, 53, 52, 59, 58, 
65, 64, 71, 70, 73, 72, 79, 78, 85, 84, 91, 90, 97, 
96, 103, 102, 109, 108, 115, 114, 121, 120, 127, 
126, 133, 132, 139, 138, 75, 74, 81, 80, 87, 86, 
93, 92, 99, 98, 105, 104, 111, 110, 117, 116, 123, 
122, 129, 128, 135, 134, 141, 140, 77, 76, 83, 82, 
89, 88, 95, 94, 101, 100, 107, 106, 113, 112, 119, 
118, 125, 124, 131, 130, 137, 136, 143, 142};

return PackList[Channel];
};

//--------------------------------------------------------------------
  int PbGlIndexer::i24Packi24PackTiCH(int & iSM,int & iSMTower)   
{ 
  int x, y;
  iSM24TxySM24T(iSMTower, x, y);
  int xSM = (iSM-2)/3;
  int ySM = (iSM-2)%3; 
  int asics = (x/2)+xSM*3;
  int ym = y/2+ySM*2;
  int preamp = ym;
  int xc = x%2;
  int yc = y%2;
  int cell = 2*yc+(1-xc);
  return asics*24+preamp*4+cell;
};
//--------------------------------------------------------------------
int PbGlIndexer::i144PackTiCH(int & iPackT)   
{ 
int PackList [144] = {
1, 0, 25, 24, 49, 48, 3, 2, 27, 26, 51, 50,
5, 4, 29, 28, 53, 52, 7, 6, 31, 30, 55, 54,
9, 8, 33, 32, 57, 56, 11, 10, 35, 34, 59, 58, 
13, 12, 37, 36, 61, 60, 15, 14, 39, 38, 63, 62,
17, 16, 41, 40, 65, 64, 19, 18, 43, 42, 67, 66, 
21, 20, 45, 44, 69, 68, 23, 22, 47, 46, 71, 70, 
73, 72, 97, 96, 121, 120, 75, 74, 99, 98, 123, 122, 
77, 76, 101, 100, 125, 124, 79, 78, 103, 102, 127, 126, 
81, 80, 105, 104, 129, 128, 83, 82, 107, 106, 131, 130, 
85, 84, 109, 108, 133, 132, 87, 86, 111, 110, 135, 134, 
89, 88, 113, 112, 137, 136, 91, 90, 115, 114, 139, 138, 
93, 92, 117, 116, 141, 140, 95, 94, 119, 118, 143, 142};

return PackList[iPackT];
};
//--------------------------------------------------------------------
  inline void  PbGlIndexer::SMxySM(int i, int & x, int & y) 
{ 
  y = i/8; 
  x = i%8; 
};  


//--------------------------------------------------------------------
int 
PbGlIndexer::xySMiSM(int x, int y)
{
  int rv = -1;

  if ( x >= 0 && x < 8 && y >= 0 && y < 4 ) 
    {
      rv = y*8 + x; 
    }

  return rv;
}

//--------------------------------------------------------------------
  int PbGlIndexer::SMxySMTiST(int iSM ,int xSMTower, int ySMTower) 
{ 
  int x, y;
  SMxySM(iSM, x, y);
  return (y*12+ySMTower)*96+x*12+xSMTower; 
};    

//--------------------------------------------------------------------
  int PbGlIndexer::SMiSMTiST(int iSM ,int iSMTower)
{ 
  int x, y, xx, yy;
  SMxySM(iSM, x, y);
  iSM144TxySM144T(iSMTower, xx, yy);
  return (y*12+yy)*96+x*12+xx; 
};    

//--------------------------------------------------------------------
  void PbGlIndexer::iSM24TxySM24T(int ismt, int & xsmt, int & ysmt)   
{ 
  ysmt = ismt/6; 
  xsmt = ismt - ysmt*6; 
};  

 
//--------------------------------------------------------------------
   void PbGlIndexer::iSTxyST(int const iTower, int  & xTower, int & yTower) 
{ 
  yTower = iTower/96; 
  xTower = iTower%96; 
};

//--------------------------------------------------------------------
  void PbGlIndexer::iST_SMInd(int const iTower, int & ism, int & ismt, int & xsmt, int & ysmt) 
{ 
  int x, y, xsm, ysm;
  iSTxyST(iTower, x, y);
  xsm  = x/12;
  ysm  = y/12;
  ism      = ysm*8 + xsm;
  xsmt     = x%12;
  ysmt     = y%12;
  ismt     = ysmt*12+xsmt;
};                    

                   
//*************************************************************************

int   PbGlIndexer::SM24iSM24TiST(int iSM24, int iSM24T){
  int xt24, yt24, xSM24, ySM24;
  ySM24 = iSM24/16;
  xSM24 = iSM24%16;
  yt24  = iSM24T/6;
  xt24  = iSM24T%6;
  return (yt24+ySM24*4)*96+xSM24*6+xt24;
 ;
}
