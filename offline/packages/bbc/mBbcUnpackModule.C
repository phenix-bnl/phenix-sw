#include "mBbcUnpackModule.h"
#include "BbcEvent.hh"
//INCLUDECHECKER: Removed this line: #include "BbcCalib.hh"

#include "getClass.h"
#include <iostream>

using namespace std;

PHBoolean
mBbcUnpackModule::event(PHCompositeNode *root)
{

  BbcCalib* calibPar = findNode::getClass<BbcCalib>(root, "BbcCalibPar");
  if (!calibPar)
    {
      cout << PHWHERE << "Can't find BbcCalibPar" << endl;
      return False;
    }

  BbcEvent bbc;
  bbc.setCalibDataAll(calibPar);
  int iret = bbc.setRawData(root);
  return iret;

}
