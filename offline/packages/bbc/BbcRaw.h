//  virtual Bbc Raw class

#ifndef __BBCRAW_H
#define __BBCRAW_H

#include "PHObject.h"
#include <iostream>

///
class BbcRaw : public PHObject
{
 public:
  /// dtor
  virtual ~BbcRaw() {}

  /** identify Function from PHObject
      @param os Output Stream 
   */
  virtual void identify(std::ostream& os = std::cout) const;

  /// Clear Event
  virtual void Reset();

  /// isValid returns non zero if object contains vailid data
  virtual int isValid() const;

  /** set T0 for Bbc
      @param ival Number of Bbc Pmt's
   */
  virtual void set_npmt(const short ival);

  /// get Number of Bbc Pmt's
  virtual short get_npmt() const;

  /** get Pmt id of Pmt no iPmt in TClonesArray
      @param iPmt no of Pmt in TClonesArray
   */
  virtual short get_Pmt(const short iPmt) const;

  /** get Adc of Pmt iPmt in TClonesArray
      @param iPmt no of Pmt in TClonesArray
   */
  virtual short get_Adc(const short iPmt) const;

  /** get Tdc0 of Pmt iPmt in TClonesArray
      @param iPmt no of Pmt in TClonesArray
   */
  virtual short get_Tdc0(const short iPmt) const;

  /** get Tdc1 of Pmt iPmt in TClonesArray
      @param iPmt no of Pmt in TClonesArray
   */
  virtual short get_Tdc1(const short iPmt) const;

  /** Add Bbc Raw hit object to TCLonesArray
      @param pmt Pmt id
      @param adc Adc value
      @param tdc0 Tdc0 value
      @param tdc1 Tdc1 value
      @param ipmt no of pmt
  */
  virtual void AddBbcRawHit(const short pmt, const short adc, 
                            const short tdc0, const short tdc1, 
                            const short ipmt);

  /** Add Bbc Raw hit object to TCLonesArray
      @param pmt Pmt id
      @param adc Adc value
      @param tdc0 Tdc0 value
      @param ipmt no of pmt
  */
  virtual void AddBbcRawHit(const short pmt, const short adc, 
                            const short tdc0, const short ipmt);

 private:
  void virtual_warning(const char *funcname) const;

  ClassDef(BbcRaw,1)
};

#endif
