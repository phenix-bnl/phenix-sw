#ifndef __PDBMPCPINMAP_HH__
#define __PDBMPCPINMAP_HH__

#include "PdbCalChan.hh"

class PdbMpcPinMap : public PdbCalChan {
 public:
  PdbMpcPinMap();
  virtual ~PdbMpcPinMap(){}
  
  void set_fee576(const short ich) { fee576 = ich; }
  void set_pinfee576(const short ipinch) { pinfee576 = ipinch; }
  void set(const short ich, const short ipinch)
  {
    fee576 = ich;
    pinfee576=ipinch;
  }
  
  short get_fee576() const { return fee576; }
  short get_pinfee576() const { return pinfee576; }
  virtual void Reset();
  virtual void print() const;
  
private:
  short fee576;	// FEE Channel
  short pinfee576; // Pin FEE Channel

  ClassDef(PdbMpcPinMap,1);

};

#endif /* __PDBMPCPINMAP_HH__ */
