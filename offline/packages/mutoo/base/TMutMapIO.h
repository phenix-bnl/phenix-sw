//
// Template Class : TMutMapIO
// Author: S.Kelly 
// Date: 2/11/01
// Description: implements conversion of TMutMap contents to
// ROOT compatible containers suitable for PHOOL I/O

#ifndef __TMUTMAPIO_H__
#define __TMUTMAPIO_H__

#include<cassert>
#include<TClonesArray.h>

template<typename Value, typename ValueImp=Value> 
class TMutMapIO 
{
public:

  typedef TClonesArray* array_ptr;
  
  //! constructor
  TMutMapIO() : 
    _initialized(false),  
    _i_size(1000), 
    _i_write(0)
  {;}
  
  //! constructor
  TMutMapIO(ULong_t size) : 
    _initialized(false),   
    _i_size(size), 
    _i_write(0) 
  {;} 
  
  //! destructor
  virtual ~TMutMapIO( void )
  { 
    if( _initialized )
    { delete _array_ptr; }
  }
  
  //! initialize TClonesArray
  void initialize() 
  {
    assert(!_initialized);
    _array_ptr = array_ptr(new TClonesArray(ValueImp().GetName(),_i_size));
    _initialized = true;
  }
  
  void clear() 
  {
    
    if(!_initialized) initialize();
    assert(_initialized);
    
    // clear array
    _array_ptr->Delete();
    
    // reset array size
    if( _i_size > 0 && _array_ptr->GetSize() > (int) _i_size )
    { _array_ptr->Expand( _i_size ); }
    
    // reset writing index
    _i_write = 0;
    
  }
  
  void insert(Value* obj_ptr) {

    // Check that the destination TClonesArray exists
    //
    if(!_initialized) initialize();
    assert(_initialized);

    // Construct Implementation from base
    //
    ValueImp* new_obj = new((*_array_ptr)[_i_write++]) ValueImp(*obj_ptr);

    // Set the global synchro flag before writing so 
    // associations are correctly re-established upon 
    // readback. 
    //    
    new_obj->set_synchro_flag();

    // This is so that the objects in the io array don't 
    // hold onto refptrs -- Refptrs are not persistent
    // objects and holding them can cause resource leaks
    // associated with circular references.
    new_obj->clear_associations();
    
  }

  //! returns pointer to TClonesArray
  void* get_array_address() { 
    if(!_initialized) initialize();
    assert(_initialized);
    return &_array_ptr; 
  }

  TClonesArray* get_array() { 
    if(!_initialized) initialize();
    assert(_initialized);
    return _array_ptr;
  }
  
  void set_array(TClonesArray* array_ptr){
    _initialized=true;
    _array_ptr = array_ptr;
  }

  void print(std::ostream& os=std::cout) const {
    if (!_initialized) return;
    for(int i = 0; i< _array_ptr->GetEntries(); ++i){
      Value* ptr  = static_cast<Value*>(_array_ptr->At(i));
      ptr->print(os);
    }
  }

  bool is_initialized() const { return _initialized; }
  
  private:
  
  //! true when TClonesArray was created
  bool _initialized;
  
  //! initial TClonesArray size
  ULong_t _i_size;

  //! pointer to TClonesArray
  array_ptr _array_ptr;
  
  //! current index in array
  ULong_t _i_write;
  
};

// provide a dummy specialization so we can compile
// PHMap<int,int> used in testPHMap.cxx
//
template<> class TMutMapIO<int>
{
  
  public:
  void initialize(){;}
  void clear(){;}
  void insert(int* obj_ptr){;}
  void* get_array_address(){return 0;}
  TClonesArray* get_array(){return 0;}
  void print(std::ostream& os=std::cout) const{;}
  bool is_initialized() const { return false; }
  
};

#endif

