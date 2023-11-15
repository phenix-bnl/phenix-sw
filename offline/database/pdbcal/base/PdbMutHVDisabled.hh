// $Id: PdbMutHVDisabled.hh,v 1.8 2007/03/29 20:08:53 hpereira Exp $

/*!
   \file PdbMutHVDisabled.h
   \brief container for disabled HV channels in database
   \author J. Ying, X. He
   \version $Revision: 1.8 $
   \date $Date: 2007/03/29 20:08:53 $
*/

#ifndef __PDBMUTHVDISABLED_HH__
#define __PDBMUTHVDISABLED_HH__

#include "PdbCalChan.hh"
#include <vector>
#include <string>
#include <iostream>

//! container for disabled HV channels in database
class PdbMutHVDisabled : public PdbCalChan
{
  public:
  
  //! constructor
  PdbMutHVDisabled():
    runNumber( 0 ),
    numChnls( 0 )
  {}
  
  //! destructor
  virtual ~PdbMutHVDisabled()
  {}

  //! print
  virtual void print() const;

  //! retrieve list of disabled cards
  std::vector<std::string> getChanStringVec() const
  { return CharString; }
  
  //! number of disabled cards
  unsigned int getNumChnls() const
  { return CharString.size(); }

  //! number of disabled cards
  void setNumberOfDeadHVChnls( const int& nchnls)
  { 
    std::cout << "PdbMutHVDisabled::setNumberOfDeadHVChnls - this method is obsolete. " << std::endl;
    std::cout << "PdbMutHVDisabled::setNumberOfDeadHVChnls - The number of dead channels is retrieved from the vector size." << std::endl; 
  }

  //! list of disabled cards
  void clearDeadHVCharString( void )
  { CharString.clear(); }
  
  //! list of disabled cards
  void setDeadHVCharString( const std::vector<std::string> &inCharString)
  { CharString = inCharString; }

  //! disable card
  void addDeadHVCharString( const std::string& channel );
  
  //! remove duplicated cards from vector
  void removeDuplicates( void );
  
  //! used to ensure unicity in the vector of channels
  class SameChannelFTor
  {
    public:
    
    //! constructor
    SameChannelFTor( const std::string& channel ):
      _channel( channel )
    {}
    
    //! predicate
    bool operator() (const std::string& channel ) const
    { return _channel == channel; }
    
    private:
    
    //! predicted channel
    std::string _channel;
    
  };
  
  private:
  
  /*! \brief run number */
  /*! this number is not used anymore. The entry is based on a timestamp only */
  unsigned int runNumber;
  
  /*! \brief number of channels */
  /*! 
    this number is not used anymore. The number of dead channels
    is retrieved from the size of the vector
  */
  int numChnls;
  
  /*! \brief name of the channels */
  std::vector<std::string> CharString;

  ClassDef(PdbMutHVDisabled,1);
};

#endif /* __PDBMUTHVDISABLED_HH__ */
