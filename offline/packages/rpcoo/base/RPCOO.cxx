// $Id: RPCOO.cxx,v 1.2 2008/08/28 00:49:43 kempel Exp $

#include "RPCOO.h"
#include <cmath>
//INCLUDECHECKER: Removed this line: #include <algorithm>

/*!
	\file RPCOO.cxx
	\brief widely used utility functions and enumerations
	\author H. Pereira Da Costa
  \version $Revision: 1.2 $
  \date    $Date: 2008/08/28 00:49:43 $
*/

//_______________________________________________________
void RPCOO::PRINT(std::ostream& os, const std::string& message)
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
