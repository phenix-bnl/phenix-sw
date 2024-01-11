// Interface Object Class : TMuiPseudoLL1
// Description: Class for muon identifier road

#ifndef _TMUIPseudoLL1_H_
#define _TMUIPseudoLL1_H_

// CINT compatible headers//
//
#include<TDataType.h>
#include<PHKey.hh>
#include<PHException.h>
#include<MUIOO.h>

/*! @ingroup interface */
//!  The MUID Pseudo-LL1 Interface Object 

/*! 
  <b>The MUID pseudo-LL1 Interface Object </b><br>

  An interface class for offline LL1 emulator output.

  <p>
*/

class TMuiPseudoLL1 : public PHKey {

public:

  //! @name Constructors/Destructors
  //@{  
  
  /*! Default constructor */
  TMuiPseudoLL1()
  {}  
  
  /*! Default destructor */  
  virtual ~TMuiPseudoLL1()
  {}
  
  /*! Default constructor */  
  TMuiPseudoLL1(const Key& key) :
    PHKey(key)
    {}
  
  /*! Construct with key and location */
  TMuiPseudoLL1(const Key&, UShort_t arm, UShort_t index)
  {}
  
  //@}  
  
  //! @ Setters 
  //@{  
  
  /*! set emulator decision  */  
  virtual void set_emulator_decision(int i) {}

  virtual void set_symsetword_deep_horiz(unsigned int i, int iword) 
  {}
  
  virtual void set_symsetword_deep_vert(unsigned int i, int iword) 
  {}
  
  virtual void set_symsetword_shallow_horiz(unsigned int i, int iword) 
  {}
  
  virtual void set_symsetword_shallow_vert(unsigned int i, int iword) 
  {}

  //@}

  //! @ Getters
  //@{

  /*! get emulator decision  */  
  virtual int get_emulator_decision() const { return 0;}

  virtual unsigned int get_symsetword_deep_horiz(int iword) const 
  { return 0; }
  
  virtual unsigned int get_symsetword_deep_vert(int iword) const 
  { return 0; }
  
  virtual unsigned int get_symsetword_shallow_horiz(int iword) const 
  { return 0; }
  
  virtual unsigned int get_symsetword_shallow_vert(int iword) const 
  { return 0; }
  
  //@}

  //! @name Locators
  //@{
  /*! Arm [0,1] */
  virtual void set_arm( UShort_t arm) 
  {}

  /*! Index */
  virtual void set_index( UShort_t index)
  {}

  /*! Arm [0,1] */
  virtual UShort_t get_arm() const 
  {return 0;}

  /*! Index */
  virtual UShort_t get_index() const 
  {return 0;}

  //@}

  //! @name Dumpers
  //@{  

  virtual void print(std::ostream& os = std::cout) const 
  {}

  //@}

  ClassDef(TMuiPseudoLL1,1)

};


class TMuiPseudoLL1_v1 : public TMuiPseudoLL1 {

public:

  TMuiPseudoLL1_v1();  

  virtual ~TMuiPseudoLL1_v1(){}

  TMuiPseudoLL1_v1(const Key&,
    UShort_t arm,
    UShort_t index);  


  TMuiPseudoLL1_v1(const TMuiPseudoLL1*);  

  TMuiPseudoLL1_v1(const TMuiPseudoLL1&);  


  void set_arm( UShort_t arm) { _arm=arm;}
  void set_index( UShort_t index) { _index=index;}

  UShort_t get_arm() const {return _arm;}
  UShort_t get_index() const {return _index;}

  void set_emulator_decision(int i) 
  { _emulator_decision = i; }
  
  int get_emulator_decision() const 
  { return _emulator_decision;}

  void set_symsetword_deep_horiz(unsigned int i, int iword) 
  { _symsetword_deep_horiz[iword] = i;}
  
  void set_symsetword_deep_vert(unsigned int i, int iword)
  { _symsetword_deep_vert[iword] = i;}
  
  void set_symsetword_shallow_horiz(unsigned int i, int iword)
  { _symsetword_shallow_horiz[iword] = i;}
  
  void set_symsetword_shallow_vert(unsigned int i, int iword) 
  { _symsetword_shallow_vert[iword] = i;}

  unsigned int get_symsetword_deep_horiz(int iword) const 
  { return _symsetword_deep_horiz[iword];}
  
  unsigned int get_symsetword_deep_vert(int iword) const 
  { return _symsetword_deep_vert[iword];}
  
  unsigned int get_symsetword_shallow_horiz(int iword) const 
  { return _symsetword_shallow_horiz[iword];}
  
  unsigned int get_symsetword_shallow_vert(int iword) const
  { return _symsetword_shallow_vert[iword];}

  void print(std::ostream& os = std::cout) const 
  {
    MUIOO::PRINT(os, GetName());
    os << " arm: " << _arm << std::endl;
    os << " index: " << _index << std::endl;
    MUIOO::PRINT(os,"**");
  }

  void zero();
  
private:
  
  UShort_t _arm;
  UShort_t _index;
  
  int _emulator_decision;

  unsigned int _symsetword_deep_horiz[MUIOO::kLL1_NSymsetWords];
  unsigned int _symsetword_deep_vert[MUIOO::kLL1_NSymsetWords];
  unsigned int _symsetword_shallow_horiz[MUIOO::kLL1_NSymsetWords];
  unsigned int _symsetword_shallow_vert[MUIOO::kLL1_NSymsetWords];

  ClassDef(TMuiPseudoLL1_v1,1)
};

#endif /* _TMUIPSEUDOLL1_H_ */
	      








