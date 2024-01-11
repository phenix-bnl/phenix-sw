
// $Id: FVTXOO.cxx,v 1.3 2014/12/31 19:00:15 jinhuang Exp $

/*!
	\file FVTXOO.cxx
	\brief widely used utility functions and enumerations
	\author H. Pereira Da Costa
  \version $Revision: 1.3 $
  \date    $Date: 2014/12/31 19:00:15 $
*/

#include "FVTXOO.h"
#include <cmath>
#include <algorithm>

//_______________________________________________________
void FVTXOO::PRINT(std::ostream& os, const std::string& message)
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

//_______________________________________________________
//! return true if the event number is special, i.e. 1,2...5,10,20...50,100,200,...
bool
FVTXOO::special_event_num(const int event_num)
{
  const double significand = event_num/pow(10,(int)(log10(event_num)));

  if (fmod (significand,1) == 0 ) return true;
//  if (fmod (significand,1) == 0 && significand<=5) return true;
  else return false;
}
