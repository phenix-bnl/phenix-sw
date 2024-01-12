#ifndef __PHHijingOscarOutputManager_H__
#define __PHHijingOscarOutputManager_H__

// $Id: PHHijingOscarOutputManager.h,v 1.1 2009/08/11 21:46:29 hpereira Exp $

/*!
   \file PHHijingOscarOutputManager.h
   \brief dedicated output manager to write phpythia contents into oscar formated text file
   \author Hugo Pereira
   \version $Revision: 1.1 $
   \date $Date: 2009/08/11 21:46:29 $
*/

#include <PHPyOscarOutputManager.h>
#include <Fun4AllOutputManager.h>

#include <string>
#include <vector>
#include <iostream>
#include <fstream>

class PHNodeIOManager;
class PHCompositeNode;

class PHHijingOscarOutputManager: public PHPyOscarOutputManager
{
  
  public:

  //! constructor
  PHHijingOscarOutputManager(
    const std::string &myname = "OSCAROUT" , 
    const std::string &filename = "oscar.txt"): 
    PHPyOscarOutputManager( myname, filename )
  {}
  
  //! destructor
  virtual ~PHHijingOscarOutputManager()
  {}
  
  //! write event to output
  int Write(PHCompositeNode *startNode);
  
};

#endif /* __PHHijingOscarOutputManager_H__ */
