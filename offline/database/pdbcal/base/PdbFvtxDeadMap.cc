// $Id: PdbFvtxDeadMap.cc,v 1.1 2013/01/22 06:52:11 jinhuang Exp $                                                                                             

/*!
 * \file PdbFvtxDeadMap.cc
 * \brief 
 * \author Jin Huang <jhuang@bnl.gov>
 * \version $Revision: 1.1 $
 * \date $Date: 2013/01/22 06:52:11 $
 */

#include <iostream>

#include "PdbFvtxDeadMap.hh"

using namespace std;

PdbFvtxDeadMap::PdbFvtxDeadMap()
{

}

PdbFvtxDeadMap::PdbFvtxDeadMap(const PdbFvtxDeadMap & map) : //
    comment(map.comment), //
    key_word(map.key_word), //
    records(map.records)
{
}

PdbFvtxDeadMap::~PdbFvtxDeadMap()
{

}

void
PdbFvtxDeadMap::print() const
{
  TObject::Print();

  if (comment.length())
    cout << "\t comment = " << comment << endl;
  cout << "\t This map contains " << get_n_record() << " records: ";

  for (dead_map_record_vec::const_iterator it = records.begin();
      it != records.end(); ++it)
    {
      cout << (*it) << "\t";
    }

  cout << endl;
}
