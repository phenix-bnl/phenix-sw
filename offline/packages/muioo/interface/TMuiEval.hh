#ifndef __TMUIEVAL_H__
#define __TMUIEVAL_H__

#include<TDataType.h>
#include<PHKey.hh>
#include<PHException.h>
#include<TMuiRoadEval.hh>
#include<TMuiEvalRes.hh>


/*! @ingroup interface */
//! The Muon Id road finding evaluation object
/*! 
  The Muon Id road finding evaluation object
*/
class TMuiEval : public PHKey
{
public:

  /*! Name of the list of TMuiResEval contained in this object */
  typedef std::vector<TMuiEvalRes> eval_res_list;

  /*! Name of a const iterator to the reseval list */
  typedef std::vector<TMuiEvalRes>::const_iterator const_evalres_iter;

  //! @name Constructors/Destructors
  //@{    

  /*! Default constructor */
  TMuiEval(){;}

  /*! Constructor with key */
  TMuiEval(const Key &key): PHKey(key){;}

  /*! Destructor*/
  virtual ~TMuiEval(){;}
  
  //@}

  //! @name Per Hit Evaluation Residuals
  //@{    

  /*! Read only pointer to eval residuals */
  virtual const eval_res_list* get_eval_res_list() const {return 0;}
  /*! Add a eval residual to the list */
  virtual void push_eval_res_list(const TMuiEvalRes& res_eval) { }

  //@}

  //! @name Per Track Evaluation 
  //@{    

  /*! Read only access to the track eval object */
  virtual const TMuiRoadEval* get_road_eval() const {return 0;}
  /*! Set the track eval object */
  virtual void set_road_eval(const TMuiRoadEval& road_eval) { }   

  //@}

  //! @name Locators
  //@{    
  /*! arm */
  virtual UShort_t get_arm() const { return 0;}
  /*! index */
  virtual UShort_t get_index() const { return 0;}
  /*! arm */
  virtual void set_arm( UShort_t arm) { }
  /*! index */
  virtual void set_index( UShort_t index) { }
  //@}

  //! @name Dumpers
  //@{    
  /*! Print data members to ostream os, defaults to std::cout */
  virtual void print(std::ostream& os, bool max) const {}
  virtual void print(std::ostream& os = std::cout) const { print(os, false); }
  //@}

  ClassDef(TMuiEval,1)

};

class TMuiEval_v1 : public TMuiEval
{
public:

  TMuiEval_v1();
  
  TMuiEval_v1(const Key&,
	   UShort_t arm,
	   UShort_t index);

  TMuiEval_v1(const TMuiEval* base_ptr);
  TMuiEval_v1(const TMuiEval& base_ref);

  virtual ~TMuiEval_v1(){;}

  const eval_res_list* get_eval_res_list() const { return &_eval_res_list;}
  void push_eval_res_list(const TMuiEvalRes& res_eval) { _eval_res_list.push_back(res_eval);}
  const TMuiRoadEval* get_road_eval() const { return &_road_eval;}
  void set_road_eval(const TMuiRoadEval& road_eval) { _road_eval=road_eval;}   

  UShort_t get_arm() const { return _arm;}
  UShort_t get_index() const { return _index;}
  void set_arm( UShort_t arm) { _arm=arm;}
  void set_index( UShort_t index) { _index=index;}
  void print(std::ostream& os = std::cout, bool max=false) const;

private:

  UShort_t _arm;
  UShort_t _index;
  eval_res_list _eval_res_list;
  TMuiRoadEval _road_eval;
  
  ClassDef(TMuiEval_v1,1)
};

#endif /* __TMUIEVAL_H__ */
