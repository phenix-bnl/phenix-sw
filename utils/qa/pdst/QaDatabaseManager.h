#ifndef __QADATABASEMANAGER_H__
#define __QADATABASEMANAGER_H__

#include <string>
#include <vector>

#include "QaEntry.h"

//------------------------------------------------------
// Since ODBC++ and PHENIX declare types differently
// and we don't wish to alter either, this manager
// handles access to the QA database. It will know
// only ODBC++ types and the other modules will know
// only PHENIX types, this should work around the
// conflict. For now, it is only implemented in
// PhysicsqaReco.
// To write this object, I borrow heavily from an
// example of database submission provided by Irina
//
//               Michael P. McCumber
//               mccumber@grad.physics.sunysb.edu
//               8/25/2004
//------------------------------------------------------

class QaDatabaseManager
{

 public:
  QaDatabaseManager();
  virtual ~QaDatabaseManager() {}

  void WriteToDatabase(std::string DatabaseName, std::string UserName, std::string TableName, int RunNumber, int SegmentNumber, std::string Tag, std::vector<QaEntry> Cesar);

};

#endif /*__QADATABASEMANAGER_H__ */
