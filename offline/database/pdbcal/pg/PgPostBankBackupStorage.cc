// $Id: PgPostBankBackupStorage.cc,v 1.2 2014/01/31 16:50:40 jinhuang Exp $                                                                                             

/*!
 * \file PgPostBankBackupStorage.cc
 * \brief 
 * \author Jin Huang <jhuang@bnl.gov>
 * \version $Revision: 1.2 $
 * \date $Date: 2014/01/31 16:50:40 $
 */

#include "PgPostBankBackupStorage.hh"

#include "PgPostBankManager.hh"
#include "PgPostHelper.hh"
#include "PgPostCalBankIterator.hh"
#include "PgPostBankWrapper.hh"
#include "PgPostBankWrapper2.hh"
#include "PgPostApplication.hh"
#include "PgPostCalBank.hh"
#include "PdbCalBank.hh"

#include <stdlib.h>
#include <sstream>
#include <iostream>
#include <cassert>
#include <string>

using namespace std;

PgPostBankBackupStorage::PgPostBankBackupStorage(PdbCalBank * b) :
    TNamed("NotAssigned", "NotAssigned"), obj_classname("NotAssigned")
{
  if (!b)
    {
      cout
          << "PgPostBankBackupStorage::PgPostBankBackupStorage - Fatal Error - "
          << "invalid bank" << endl;
      exit(1);
    }

  if (string(b->ClassName()) == string("PgPostBankWrapper")
      or string(b->ClassName()) == string("PgPostBankWrapper2"))
    {
      cout
          << "PgPostBankBackupStorage::PgPostBankBackupStorage - Fatal Error - "
          << "PgPostBankBackupStorage do not accept bank wrappers by design"
          << endl;
      exit(1);
    }

  bank = b;
}

PgPostBankBackupStorage::PgPostBankBackupStorage() :
    TNamed("NotAssigned", "NotAssigned"), obj_classname("NotAssigned"), bank(
        NULL)
{

}

PgPostBankBackupStorage::~PgPostBankBackupStorage()
{
  if (bank)
    delete bank;
}

//! use this storage object to recover the PdbCalBankWrapper/PdbCalBankWrapper2
PgPostCalBank *
PgPostBankBackupStorage::createBank()
{
  if (!isValid())
    {
      cout << "PgPostBankBackupStorage::createBank - ERROR - "
          << "invalid content" << endl;

      return NULL;
    }

  PgPostCalBank * bw = NULL;

  PdbCalBank * b_clone = static_cast<PdbCalBank *>(bank->Clone());
  assert(b_clone);

  PdbClassMap<PdbCalBank> *classMap = PdbClassMap<PdbCalBank>::instance();
  assert(classMap);

  if (obj_classname == "PgPostBankWrapper")
    {
      bw = new PgPostBankWrapper(b_clone);
      assert(bw);
      bw->setBankID(obj_header.getBankID());
    }
  else if (obj_classname == "PgPostBankWrapper2")
    {
      bw = new PgPostBankWrapper2(b_clone);
      assert(bw);
      bw->setBankID2(obj_header.getBankID());
    }
  else if (obj_classname == "PgPostCalBank")
    {
      cout << "PgPostBankBackupStorage::createBank - WARNING - "
          << "empty PgPostCalBank object is created for table "
          << database_header.getTableName() << " where rid = "
          << database_header.getRId() << endl;
      bw = dynamic_cast<PgPostCalBank *>(b_clone);
      assert(bw);
    }
  else if (classMap->find( get_obj_classname().c_str() ) != classMap->end())
    {
      cout << "PgPostBankBackupStorage::createBank - WARNING - "
          << "Direct stream of " << get_obj_classname()
          << " object without wrapper in database, table "
          << database_header.getTableName() << " where rid = "
          << database_header.getRId() << endl;
      bw = dynamic_cast<PgPostCalBank *>(b_clone);
      assert(bw);
    }
  else
    {
      cout << "PgPostBankBackupStorage::createBank - ERROR - "
          << "invalid obj_classname of " << obj_classname << " with bankID = "
          << obj_header.getBankID() << endl;

      return NULL;
    }

  bw->setInsertTime(obj_header.getInsertTime());
  bw->setStartValTime(obj_header.getStartValTime());
  bw->setEndValTime(obj_header.getEndValTime());
  bw->setDescription(obj_header.getDescription().c_str());
  bw->setUserName(obj_header.getUserName().c_str());
  bw->setTableName(obj_header.getTableName().c_str());

  return bw;
}

string
PgPostBankBackupStorage::BankHeader::get_id_string() const
{
  stringstream o;
  o << tableName << "_";
  o.fill('0');
  o.width(5);
  o << rid;
  o << "_";
  o.width(1);
  o << bankID;
  o << "_" << insertTime.getTics();
  o << "_" << startValTime.getTics();
  o << "_" << endValTime.getTics();

  return o.str();
}

void
PgPostBankBackupStorage::BankHeader::Print(Option_t *option) const
{
//  TObject::Print(option);
  cout << "ID = " << get_id_string() << " from user " << userName
      << " and described as " << description << endl;
}

void
PgPostBankBackupStorage::set_obj_info(const PgPostCalBank * bw)
{
  assert(bw);

  PdbClassMap<PdbCalBank> *classMap = PdbClassMap<PdbCalBank>::instance();
  if (string(bw->ClassName()) == string("PgPostBankWrapper"))
    {
      obj_header.setBankID(bw->getBankID().getInternalValue());
    }
  else if (string(bw->ClassName()) == string("PgPostBankWrapper2"))
    {
      obj_header.setBankID(bw->getBankID2().getInternalValue());
    }
  else if (string(bw->ClassName()) == string("PgPostCalBank"))
    {
      cout
          << "PgPostBankBackupStorage::set_obj_info - WARNING - empty PgPostCalBank presented"
          << bw->ClassName() << endl;
    }
  else if (classMap->find(bw->ClassName()) != classMap->end())
    {
      cout
          << "PgPostBankBackupStorage::set_obj_info - WARNING - directly stream "
          << bw->ClassName() << "without wrapper" << endl;
    }
  else
    {
      cout
          << "PgPostBankBackupStorage::set_obj_info - ERROR - unknown object in database record: "
          << bw->ClassName() << endl;
      exit(1);
    }

  obj_classname = bw->ClassName();
  obj_header.setInsertTime(bw->getInsertTime());
  obj_header.setStartValTime(bw->getStartValTime());
  obj_header.setEndValTime(bw->getEndValTime());
  if (bw->getDescription().getString())
    obj_header.setDescription(bw->getDescription().getString());
  if (bw->getUserName().getString())
    obj_header.setUserName(bw->getUserName().getString());
  if (bw->getTableName().getString())
    obj_header.setTableName(bw->getTableName().getString());
}

void
PgPostBankBackupStorage::Print(Option_t *option) const
{
  TNamed::Print(option);

  cout << "Database header : ";
  database_header.Print(option);
  cout << "Bank wrapper header : ";
  obj_header.Print(option);

  assert(bank);
  bank->print();
}

void
PgPostBankBackupStorage::format_name_title()
{
  SetName(database_header.get_id_string().c_str());
  SetTitle(database_header.getDescription().c_str());
}

size_t
PgPostBankBackupStorage::getLength() const
{
  return bank->getLength();
}

PdbCalChan&
PgPostBankBackupStorage::getEntry(size_t pos)
{
  return bank->getEntry(pos);
}

bool
PgPostBankBackupStorage::isValid() const
{
  if (!bank)
    {
      cout << "PgPostBankBackupStorage::isValid - Failed - invalid bank pointer"
          << endl;
      return false;
    }
  if (string(bank->ClassName()) == string("PgPostBankWrapper")
      or string(bank->ClassName()) == string("PgPostBankWrapper2"))
    {
      cout
          << "PgPostBankBackupStorage::isValid - Failed - incorrect bank object"
          << endl;
      return false;
    }

  if (database_header.getBankID() == BankHeader::INVALID_BANKID)
    {
      cout << "PgPostBankBackupStorage::isValid - Failed - invalid bank id"
          << endl;
      return false;
    }
  if (database_header.getRId() == BankHeader::INVALID_BANKID)
    {
      cout << "PgPostBankBackupStorage::isValid - Failed - invalid rid" << endl;
      return false;
    }
  if (database_header.getTableName() == "not assigned")
    {
      cout << "PgPostBankBackupStorage::isValid - Failed - invalid table name"
          << endl;
      return false;
    }
  if (obj_classname == "" or obj_classname == "NotAssigned")
    {
      cout << "PgPostBankBackupStorage::isValid - ERROR - "
          << "invalid obj_classname = " << obj_classname << endl;

      return false;
    }

  return true;
}
