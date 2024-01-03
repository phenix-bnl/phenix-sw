// $Id: PHGlobal_Muon.C,v 1.4 2006/04/05 18:49:07 pinkenbu Exp $
#include "PHGlobal_Muon.h"

/*!
  \file    PHGlobal_Muon.C
  \brief   Muon specific global variables
  \author  Hugo Pereira
  \version $Revision: 1.4 $
  \date    $Date: 2006/04/05 18:49:07 $
*/

using namespace std;

ClassImp(PHGlobal_Muon)

static int shutup = 0;
  
//______________________________________________
void PHGlobal_Muon::ShutUp(const int i)
{
  shutup = i;
  return ;
}

//______________________________________________
void PHGlobal_Muon::Reset()
{
  cout << PHWHERE << "ERROR: Reset() not implemented by daughter function" << endl;
  return ;
}

//______________________________________________
void PHGlobal_Muon::identify(ostream& os) const
{
  os << "identify yourself: virtual PHGlobal_Muon Object" << std::endl;
  return ;
}

//______________________________________________
int PHGlobal_Muon::isValid() const
{
  cout << PHWHERE << "isValid() not implemented by daughter function" << endl;
  return 0;
}

//______________________________________________
void PHGlobal_Muon::warning(const char* field) const
{
  if (!shutup)
    {
      cout << "PHGlobal_Muon::using virtual function, doing nothing" << endl;
      cout << "Offending field == " << field << endl;
    }
  return ;
}

