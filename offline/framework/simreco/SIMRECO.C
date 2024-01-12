// $Id: SIMRECO.C,v 1.1 2008/10/18 13:22:16 hpereira Exp $

/*!
	\file SIMRECO.cxx
	\brief widely used utility functions and enumerations
	\author H. Pereira Da Costa
  \version $Revision: 1.1 $
  \date    $Date: 2008/10/18 13:22:16 $
*/

#include "SIMRECO.h"
#include <cmath>
#include <algorithm>

//_______________________________________________________
void SIMRECO::PRINT(std::ostream& os, const std::string& message)
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
