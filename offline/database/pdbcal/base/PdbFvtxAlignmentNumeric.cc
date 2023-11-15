// $Id: PdbFvtxAlignmentNumeric.cc,v 1.1 2013/11/05 00:02:32 jinhuang Exp $

/*!
 * \file PdbFvtxAlignmentNumeric.cc
 * \brief 
 * \author Jin Huang <jhuang@bnl.gov>
 * \version $Revision: 1.1 $
 * \date $Date: 2013/11/05 00:02:32 $
 */

#include "TGeoManager.h"
#include <iostream>

#include "PdbFvtxAlignmentNumeric.hh"

using namespace std;

PdbFvtxAlignmentNumeric::PdbFvtxAlignmentNumeric()
{
}

PdbFvtxAlignmentNumeric::~PdbFvtxAlignmentNumeric()
{

}

void
PdbFvtxAlignmentNumeric::print(unsigned int depth) const
{
  cout << "PdbFvtxAlignmentNumeric::print - contains " << _data.size()
      << " geometry data" << endl;

  if (depth >= 1)
    for (GeoData_t::const_iterator it1 = _data.begin(); it1 != _data.end();
        it1++)
      {
        cout << "|--- ID " << (*it1).first << " containts "
            << (*it1).second.size() << " records" << endl;

        if (depth >= 2)
          for (GeoDataRec_t::const_iterator it2 = (*it1).second.begin();
              it2 != (*it1).second.end(); it2++)
            {
              cout << "|--- |--- " << (*it2).first << " \t= " << (*it2).second
                  << "" << endl;
            }
      }
}
