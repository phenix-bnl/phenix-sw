// ================================
// FILE: SvxCentralTrackRecalList.h
// ================================
#ifndef __SVXCENTRALTRACKRECALLIST_H_
#define __SVXCENTRALTRACKRECALLIST_H_

#include <iostream>
#include <phool.h>
#include <PHObject.h>
#include <TClonesArray.h>
#include <SvxCentralTrackRecal.h>

class SvxCentralTrackRecalList : public PHObject
{

 private:
  enum { SVXNCNTRECAL = 4000 };
  //! Default initial size of SvxCentralTrackRecal list
  
 protected:
  TClonesArray* m_list; // Hit container
  int m_id_unused;


 public:
  // Constructor & Destructor
  // """""""""""""""""""""""""""
  SvxCentralTrackRecalList(const unsigned int length=SVXNCNTRECAL);
  virtual ~SvxCentralTrackRecalList();
  
  // The "standard PHObject response" functions...
  // """""""""""""""""""""""""""""""""""""""""""""
  void Reset    ();
  int  isValid  () const;
  void identify (std::ostream &os=std::cout) const;

  // Add/remove/set/get methods...
  // """""""""""""""""""""""""""""
  SvxCentralTrackRecal* AddEntry(const int ihit=-1);
  void RemoveEntry(const unsigned int ihit);

  SvxCentralTrackRecal* GetSvxCNTRecal(const unsigned int ihit) const
    { return dynamic_cast<SvxCentralTrackRecal*>(m_list->UncheckedAt(ihit)); }
  int GetNentries() const
    { return m_list->GetLast()+1; }
 
  // Routines to manipulate the cluster array...
  // """""""""""""""""""""""""""""""""""""""""""
  int Compress();
  int set_TClonesArraySize(const unsigned int nhit);
  
  // Methods
  // """""""
  void print() const;

  //---
  ClassDef(SvxCentralTrackRecalList,1)

};

#endif /* __SVXCENTRALTRACKRECALLIST_H_ */
