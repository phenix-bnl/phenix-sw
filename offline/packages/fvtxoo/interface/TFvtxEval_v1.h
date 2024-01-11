#ifndef __TFvtxEval_V1_H__
#define __TFvtxEval_V1_H__

#include<TDataType.h>
#include<PHKey.hh>
#include<PHException.h>
#include<TFvtxTrkEval.h>
#include<FVTXOO.h>


/*! @ingroup interface */
//! The Muon Tracker tracking evaluation object
/*! 
  The Muon Tracker tracking evaluation object
*/
class TFvtxEval : public PHKey
{
public:

  //! @name Constructors/Destructors
  //@{    

  /*! Default constructor */
  TFvtxEval(){;}

  /*! Constructor with key */
  TFvtxEval(const Key &key): PHKey(key){;}

  /*! Destructor*/
  virtual ~TFvtxEval(){;}
  
  //@}

  //! @name Per Hit Evaluation Residuals
  //@{    

  //@}

  //! @name Per Track Evaluation 
  //@{    

  /*! Read only access to the track eval object */
  virtual const TFvtxTrkEval* get_trk_eval() const {return 0;}
  /*! Set the track eval object */
  virtual void set_trk_eval(const TFvtxTrkEval& trk_eval) { }   

  //@}

  //! @name Locators
  //@{    
  /*! arm */
  virtual unsigned short get_arm() const { return 0;}
  /*! cage */
  virtual unsigned short get_cage() const {return 0;}
  /*! index */
  virtual unsigned short get_index() const { return 0;}
  /*! arm */
  virtual void set_arm( unsigned short arm) { }
  /*! cage */
  virtual void set_cage( unsigned short cage) { }
  /*! index */
  virtual void set_index( unsigned short index) { }
  //@}

  //! @name Dumpers
  //@{    
  /*! Print data members to ostream os, defaults to std::cout */
  virtual void print(std::ostream& os = std::cout) const {}
  //@}

  ClassDef(TFvtxEval,1)

};

class TFvtxEval_v1 : public TFvtxEval
{
public:

  TFvtxEval_v1();
  
  TFvtxEval_v1(const Key&,
	   unsigned short arm,
	   unsigned short cage,
	   unsigned short index);

  TFvtxEval_v1(const TFvtxEval* base_ptr);
  TFvtxEval_v1(const TFvtxEval& base_ref);

  virtual ~TFvtxEval_v1(){;}

  const TFvtxTrkEval* get_trk_eval() const { return &_trk_eval;}
  void set_trk_eval(const TFvtxTrkEval& trk_eval) { _trk_eval=trk_eval;}

  unsigned short get_arm() const { return _arm; }
  unsigned short get_cage() const { return _cage; }
  unsigned short get_index() const { return _index; }
  void set_arm( unsigned short arm) { _arm=arm;}
  void set_cage( unsigned short cage) { _cage=cage;}
  void set_index( unsigned short index) { _index=index;}
  void print(std::ostream& os, bool max) const;
  void print(std::ostream& os = std::cout) const { print(os, false); }

private:

  unsigned short _arm;
  unsigned short _cage;
  unsigned short _index;
  TFvtxTrkEval _trk_eval;
  
  ClassDef(TFvtxEval_v1,1)
};

#endif /* __TFvtxEval__v1_H__ */


















