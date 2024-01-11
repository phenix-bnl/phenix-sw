#ifndef MUI_READOUT_ID_H
#define MUI_READOUT_ID_H
// $Id: TMuiReadoutID.hh,v 1.3 2006/12/20 17:04:58 hpereira Exp $ 
/*!
  \file    TMuiReadoutID.hh
  \brief   "hardware" representation of a MuID channel address.
  \author <a href="mailto:pope@phy.ornl.gov">Kyle Pope </a>    
  \version $Revision: 1.3 $
  \date    $Date: 2006/12/20 17:04:58 $
*/


#include <cstddef>
#include <iosfwd>
#include "MuiCommon.hh"

//! "hardware" representation of a MuID channel address.
/*!
 "hardware" representation of a MuID channel address.
 
 Indexing a MuID readout channel (in the "hardware" scheme) requires
 four numbers:
 
<ul>
<li>  the front-end module (FEM) number, fFEM (0-3),
<li>  the readout card (ROC) number within the FEM, fROC (0-19)
<li>  the word or cable number within the ROC, fWord (0-5)
<li>  the bit or channel number within the word, fChannel (0-15)
</ul>

@author <a href="mailto:pope@phy.ornl.gov">Kyle Pope </a>    
 
*/

class TMuiReadoutID
{
public:

  //! Number of ROCs per FEM.
  static const short kROCsPerFEM;
  
  //! Number of words per ROC.
  static const short kWordsPerROC;
  
  //! Number of words per FEM.
  static const short kWordsPerFEM;
  
  //! Number of channels per word.
  static const short kChannelsPerWord;
  
  //! Number of FEMs in the system.
  static const short kFEMsTotal;
  
  //! Number of ROCs in the system.
  static const short kROCsTotal;
  
  //! Number of words in the system.
  static const short kWordsTotal;
  
  //! Number of channels in the system.
  static const short kChannelsTotal;

  //! Constructor (FEM specified by module ID).
  TMuiReadoutID(
    const unsigned long& module_id=0,
    const short& roc=-1,
    const short& word=-1,
    const short& channel=-1,
    const short& word_index=-1);
  
  //! Destructor.
  ~TMuiReadoutID()
  {}

  //! front-end module id (0x0000, 0x0010, 0x1000, or 0x1010)
  unsigned long ModuleID() const {return fModuleID;};

  //! FEM identifier (0-3)
  short FEM() const;
  
  //! ROC identifier within FEM (0-19)
  short ROC() const {return fROC;};
  
  //! word identifier within ROC (0-5)
  short Word() const {return fWord;};
  
  //! channel identifier within word (0-15)
  short Channel() const {return fChannel;};
  
  //! Bit pattern for this channel (0x0 - 0xFFFF)
  unsigned short BitPattern() const {return fBits;};
  
  //! Position of the channel in the word list (0-119).
  short WordIndex() const {return fWordIndex;};
  
  //! Return true if all sub-identifiers are in range.
  bool IsValid() const;

  //! Set the sub-identifiers.
  void Set(
    const unsigned long& module_id, const short& roc, const short& word,
    const short& channel, const short& word_index=-1);

  private:
  
  //! module id (0x0000, 0x0010, 0x1000, or 0x1010)
  unsigned long fModuleID; 
  
  //! ROC identifier within FEM (0-19)
  short fROC;            
  
  //! word identifier within ROC (0-5)
  short fWord;           
  
  //! channel identifier within word (0-15)
  short fChannel;       
  
  //! Bit pattern for this channel (0x0 - 0xFFFF)
  unsigned short fBits; 
  
  //! Position of the channel in the word list (0-119).
  short fWordIndex;     

  //! Print TMuiReadoutID information to a stream.
  friend std::ostream& operator << (std::ostream& s, const TMuiReadoutID& n);

};

//! Returns the position of the given TMuiReadoutID object in the
size_t ChannelHash(const TMuiReadoutID& ident);

#endif  /* MUI_READOUT_ID_H */
