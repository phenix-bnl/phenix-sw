// $Id: PdbFvtxDeadMap.hh,v 1.2 2013/02/05 20:31:55 jinhuang Exp $                                                                                             

/*!
 * \file PdbFvtxDeadMap.hh
 * \brief 
 * \author Jin Huang <jhuang@bnl.gov>
 * \version $Revision: 1.2 $
 * \date $Date: 2013/02/05 20:31:55 $
 */

#ifndef PDBFVTXDEADMAP_H_
#define PDBFVTXDEADMAP_H_

#include <string>
#include <vector>
#include <stdint.h>

#include "PdbCalChan.hh"

/*!
 * \brief PdbFvtxDeadMap
 */
class PdbFvtxDeadMap : public PdbCalChan
{
public:
  PdbFvtxDeadMap();

  PdbFvtxDeadMap(const PdbFvtxDeadMap &);

  virtual
  ~PdbFvtxDeadMap();

  virtual void
  print() const;

  //! data type of dead channel record, formatted in TFvtxDeadMap::dead_map_coder
  typedef uint32_t dead_map_record;

  //! data type of a list of dead channel record
  typedef std::vector<dead_map_record> dead_map_record_vec;

  virtual const char *
  GetTitle() const
  {
    return get_key_word();
  }

  //! add a dead channel, formatted in TFvtxDeadMap::dead_map_coder
  virtual void
  add_record(dead_map_record rec)
  {
    records.push_back(rec);
  }

  //! get ith dead channel, formatted in TFvtxDeadMap::dead_map_coder
  virtual dead_map_record
  get_record(int i) const
  {
    return records[i];
  }

  //! get list of dead channels, formatted in TFvtxDeadMap::dead_map_coder
  virtual const dead_map_record_vec &
  get_records() const
  {
    return records;
  }

  //! get # of dead channels
  virtual size_t
  get_n_record() const
  {
    return records.size();
  }

  //! comment string
  virtual void
  set_comment(const char * desc)
  {
    comment = desc;
  }

  //! comment string
  virtual const char *
  get_comment() const
  {
    return comment.c_str();
  }

  //! key_word string
  virtual void
  set_key_word(const char * desc)
  {
    key_word = desc;
  }

  //! comment string
  virtual const char *
  get_key_word() const
  {
    return key_word.c_str();
  }

protected:

  //! comment string
  std::string comment;

  //! key_word string
  std::string key_word;

  //! list of dead channels, formatted in TFvtxDeadMap::dead_map_coder
  dead_map_record_vec records;

  ClassDef(PdbFvtxDeadMap,1);
};

#endif /* PDBFVTXDEADMAP_H_ */
