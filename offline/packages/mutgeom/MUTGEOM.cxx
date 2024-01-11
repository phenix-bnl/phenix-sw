// $Id: MUTGEOM.cxx,v 1.4 2006/12/20 11:57:20 hpereira Exp $

/*!
  \file MUTGEOM.cxx
  \brief widely used utility functions and enumerations
  \author H. Pereira Da Costa
  \version $Revision: 1.4 $
  \date    $Date: 2006/12/20 11:57:20 $
*/

#include <MUTGEOM.h>
#include <cmath>

//_______________________________________________
void MUTGEOM::PRINT(std::ostream& os, const std::string& message){
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
