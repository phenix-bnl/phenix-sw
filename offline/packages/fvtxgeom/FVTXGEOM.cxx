// $Id: FVTXGEOM.cxx,v 1.9 2011/02/08 23:10:53 youzy Exp $

/*!
	\file FVTXGEOM.cxx
	\brief widely used utility functions and enumerations
	\author H. Pereira Da Costa
	\version $Revision: 1.9 $
	\date $Date: 2011/02/08 23:10:53 $
*/
#include <FVTXGEOM.h>
#include <cmath>

ClassImp( FVTXGEOM )

const Float_t FVTXGEOM::RAD_TO_DEG = 180.0/M_PI;
const Float_t FVTXGEOM::DEG_TO_RAD = M_PI/180.0;
//const int FVTXGEOM:_config = 3;

//_______________________________________________
void FVTXGEOM::PRINT(std::ostream& os, const std::string& message)
{
	const int max_col=80;
	if(!message.size()) {
		os << std::string(max_col,'-') << std::endl;
		return;
	}
	int fill = max_col - message.size() - 2;
	int pre = static_cast<int>(std::floor(fill/2.0));
	int post = fill - pre;
	os << std::string(pre,'-') << " ";
	os << message << " ";
	os << std::string(post,'-') << std::endl;	
}

int
FVTXGEOM::get_halfwedge_id(const int arm,  const int cage,  const int station, const int sector, const int column)
{
  // This is a calculation to get the halfWedgeId.  The generalized
  // formula is:
  // halfWedge = 4*sector + column + (radius - 1 + plane)*(-1)^(radius+1)

  int halfWedgeNumber;
  halfWedgeNumber = 4*sector + column;
  
  return halfWedgeNumber;
}
