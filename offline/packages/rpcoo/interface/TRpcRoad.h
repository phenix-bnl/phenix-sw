#ifndef __TRpcRoad_H__
#define __TRpcRoad_H__

/*!
	\file TRpcRoad.h
	\brief The RPC Track object 
	\author R. S. Hollis
  \version $Revision: 1.2 $
  \date    $Date: 2010/11/29 20:21:10 $
*/

#include <TDataType.h>
#include <PHKey.hh>

/*! @ingroup interface */
//!  The RPC Track object 

/*! 
  <b>The RPC Road Object</b><br>

  The RPC Road object presents an interface to mui road parameter
  objects.

  <p>
  To access all the hits associated with a road one uses the standard
  syntax for accessing associated objects in MUTOO.
*/

class TMuiRoadO;
class TRpcClus;

class TRpcRoad : public PHKey
{
  
 public:
  
  //! @name Constructors/Destructors
  //@{    
  
  //! Default constructor 
  TRpcRoad();
  
  //! Destructor 
  virtual ~TRpcRoad() {;}
  
  //! Construct with key and location 
  TRpcRoad(const Key& key) : PHKey(key) {;}
  TRpcRoad(const Key&, UShort_t arm, UShort_t index);

  virtual void set_arm(int arm)             { }
  virtual void set_index(int index)         { }
  virtual void set_muiroad(TMuiRoadO *road) { }
  virtual void set_golden(bool)             { }
  virtual void set_dca1(float dca)          { }
  virtual void set_dca3(float dca)          { }
  virtual void set_rpcclus1(TRpcClus *clus) { }
  virtual void set_rpcclus3(TRpcClus *clus) { }
  virtual void set_lowdca3(bool)            { }

  virtual int get_arm()            const { return 0; }
  virtual int get_index()          const { return 0; }
  virtual TMuiRoadO* get_muiroad() const { return NULL; }
  virtual bool get_golden()        const { return true; }
  virtual float get_dca1()         const { return 0; }
  virtual float get_dca3()         const { return 0; }
  virtual TRpcClus* get_rpcclus1() const { return NULL; }
  virtual TRpcClus* get_rpcclus3() const { return NULL; }
  virtual bool get_lowdca3()       const { return false; }


  //@}

  //! @name Track Parameters Interface
  //@{    
  virtual void print(std::ostream& os = std::cout) const;
  //@}
  
  ClassDef(TRpcRoad,1)
};
  
#endif 


