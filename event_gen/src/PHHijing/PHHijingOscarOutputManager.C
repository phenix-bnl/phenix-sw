// $Id: PHHijingOscarOutputManager.C,v 1.1 2009/08/11 21:46:29 hpereira Exp $

/*!
   \file PHHijingOscarOutputManager.h
   \brief dedicated output manager to write phpythia contents into oscar formated text file
   \author Hugo Pereira
   \version $Revision: 1.1 $
   \date $Date: 2009/08/11 21:46:29 $
*/

#include <getClass.h>
#include <TMCParticle.h>

#include <iostream>
#include <iomanip>
#include <string>
#include <vector>

#include "PHHijingOscarOutputManager.h"
#include "PHPythiaContainer.h"
#include "PHHijingHeader.h"

using namespace std;

//___________________________________________________________________________________
int PHHijingOscarOutputManager::Write(PHCompositeNode *startNode)
{
  
  // Get PYTHIA Header
  PHHijingHeader* header = findNode::getClass<PHHijingHeader>(startNode, "PHHijingHeader" );
  if (!header)
  {
    cout << "PHHijingOscarOutputManager::Write - unable to get PHPythiaHeader, is Node missing?" << endl;
    return -1;
  }
  
  // Get PYTHIA Particles
  PHPythiaContainer* phhijing = findNode::getClass<PHPythiaContainer>(startNode, "PHHijing");
  if (!phhijing)
  {
    cout << "PHHijingOscarOutputManager::Write - unable to get PHPythia, is Node missing?" << endl;
    return -1;
  }

  if( !out ) 
  { 
    cout << "PHHijingOscarOutputManager::Write - output stream is invalid." << endl;
    return -1;
  }
  
  return _Write( phhijing, 
    header->GetPrimaryVertexX(),
    header->GetPrimaryVertexY(),
    header->GetPrimaryVertexZ() );

}

