#ifndef __TMUTEVAL_V1_H__
#define __TMUTEVAL_V1_H__

#include<TDataType.h>
#include<PHKey.hh>
#include<PHException.h>
#include<TMutTrkEval.hh>
#include<TMutEvalRes.hh>


/*! @ingroup interface */
//! The Muon Tracker tracking evaluation object
/*! 
  The Muon Tracker tracking evaluation object
*/
class TMutEval : public PHKey
{
public:

  /*! Name of the list of TMutResEval contained in this object */
  typedef std::vector<TMutEvalRes> eval_res_list;

  /*! Name of a const iterator to the reseval list */
  typedef std::vector<TMutEvalRes>::const_iterator const_evalees_iter;

  //! @name Constructors/Destructors
  //@{    

  /*! Default constructor */
  TMutEval(){;}

  /*! Constructor with key */
  TMutEval(const Key &key): PHKey(key){;}

  /*! Destructor*/
  virtual ~TMutEval(){;}
  
  //@}

  //! @name Per Hit Evaluation Residuals
  //@{    

  /*! Read only pointer to eval residuals */
  virtual const eval_res_list* get_eval_res_list() const {return 0;}
  /*! Add a eval residual to the list */
  virtual void push_eval_res_list(const TMutEvalRes& res_eval) { }

  //@}

  //! @name Per Track Evaluation 
  //@{    

  /*! Read only access to the track eval object */
  virtual const TMutTrkEval* get_trk_eval() const {return 0;}
  /*! Set the track eval object */
  virtual void set_trk_eval(const TMutTrkEval& trk_eval) { }   

  //@}

  //! @name Locators
  //@{    
  /*! arm */
  virtual UShort_t get_arm() const { return 0;}
  /*! octant */
  virtual UShort_t get_octant() const {return 0;}
  /*! index */
  virtual UShort_t get_index() const { return 0;}
  /*! arm */
  virtual void set_arm( UShort_t arm) { }
  /*! octant */
  virtual void set_octant( UShort_t octant) { }
  /*! index */
  virtual void set_index( UShort_t index) { }
  //@}

  //! @name Dumpers
  //@{    
  /*! Print data members to ostream os, defaults to std::cout */
  virtual void print(std::ostream& os = std::cout) const {}
  //@}

  ClassDef(TMutEval,1)

};

class TMutEval_v1 : public TMutEval
{
public:

  TMutEval_v1();
  
  TMutEval_v1(const Key&,
	   UShort_t arm,
	   UShort_t octant,
	   UShort_t index);

  TMutEval_v1(const TMutEval* base_ptr);
  TMutEval_v1(const TMutEval& base_ref);

  virtual ~TMutEval_v1(){;}

  const eval_res_list* get_eval_res_list() const { return &_eval_res_list;}
  void push_eval_res_list(const TMutEvalRes& res_eval) { _eval_res_list.push_back(res_eval);}
  const TMutTrkEval* get_trk_eval() const { return &_trk_eval;}
  void set_trk_eval(const TMutTrkEval& trk_eval) { _trk_eval = trk_eval; }

  UShort_t get_arm() const { return _arm; }
  UShort_t get_octant() const { return _octant; }
  UShort_t get_index() const { return _index; }
  void set_arm(UShort_t arm) { _arm = arm; }
  void set_octant(UShort_t octant) { _octant = octant; }
  void set_index(UShort_t index) { _index = index; }
  void print(std::ostream& os, bool max) const;
  void print(std::ostream& os = std::cout) const { print(os, false); }

private:

  UShort_t _arm;
  UShort_t _octant;
  UShort_t _index;
  eval_res_list _eval_res_list;
  TMutTrkEval _trk_eval;
  
  ClassDef(TMutEval_v1,1)
};

#endif /* __TMUTEVAL__v1_H__ */


















