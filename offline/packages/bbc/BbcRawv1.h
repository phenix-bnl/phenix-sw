#ifndef __BBCRAWV1_H
#define __BBCRAWV1_H

#include "BbcRaw.h"

class TClonesArray;

///
class BbcRawv1 : public BbcRaw
{
 public:
  /// ctor
  BbcRawv1();

  /// dtor
  virtual ~BbcRawv1();

  /// Clear Event
  void Reset();

  /** identify Function from PHObject
      @param os Output Stream 
   */
  void identify(std::ostream& os = std::cout) const;

  /// isValid returns non zero if object contains vailid data
  int isValid() const;

 
  /** set T0 for Bbc
      @param ival Number of Bbc Pmt's
   */
  void set_npmt(const short ival) {npmt=ival;return;}

  /// get Number of Bbc Pmt's
  short get_npmt() const {return npmt;}

  /** get Pmt id of Pmt no iPmt in TClonesArray
      @param iPmt no of Pmt in TClonesArray
   */
  short get_Pmt(const short iPmt)  const;

  /** get Adc of Pmt iPmt in TClonesArray
      @param iPmt no of Pmt in TClonesArray
   */
  short get_Adc(const short iPmt)  const;

  /** get Tdc0 of Pmt iPmt in TClonesArray
      @param iPmt no of Pmt in TClonesArray
   */
  short get_Tdc0(const short iPmt) const;

  /** get Tdc1 of Pmt iPmt in TClonesArray
      @param iPmt no of Pmt in TClonesArray
   */
  short get_Tdc1(const short iPmt) const;

  /** Add Bbc Raw hit object to TCLonesArray
      @param pmt Pmt id
      @param adc Adc value
      @param tdc0 Tdc0 value
      @param tdc1 Tdc1 value
      @param ipmt no of pmt
  */
   void AddBbcRawHit(const short pmt, const short adc, const short tdc0, 
                     const short tdc1, const short ipmt);

 protected:
  void Clear(Option_t *option = "");
  TClonesArray *GetBbcRawHits() const {return BbcRawHits;}

  short npmt;
  TClonesArray *BbcRawHits;


 private: // so the ClassDef does not show up with doc++
  ClassDef(BbcRawv1,1)
};

#endif


