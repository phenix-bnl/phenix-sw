// $Id: MUIGEOM.cxx,v 1.2 2006/12/20 17:04:41 hpereira Exp $

/*!
\file MUIGEOM.cxx
\brief widely used utility functions and enumerations
\author H. Pereira Da Costa
\version $Revision: 1.2 $
\date    $Date: 2006/12/20 17:04:41 $
*/

#include <MUIGEOM.h>
#include <cmath>

//_______________________________________________
void MUIGEOM::PRINT(std::ostream& os, const std::string& message){
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
