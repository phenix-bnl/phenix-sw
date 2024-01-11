#ifndef __EMC_CONTAINER_T_H__
#define __EMC_CONTAINER_T_H__





#include <map>
#include <vector>
#include <cassert>

#include <Rtypes.h>
#include <TArray.h>
#include <TBuffer.h>
#include <TObject.h>
#include <TClass.h>
#include <TClonesArray.h>


#include <phool.h>
#include <PHObject.h>













template<class T> class emcContainerTNotObject: protected TArray {
  ////////////////////////////////////////////////////////////////////////////////// 
  //
  // an std::map-like table that does NOT inherit from PHObject (so you can derive
  // from it classes that inherit from PHObject).
  // 
  // this class is a basic container used through libemc.  content can be accessed
  // vector-like through an index (using get()), or std::map-like via a uniq key
  // (using find()).  the container is automaticaly resized to store new objects.
  // 
  // the implementation uses root's TClonesArray, so the contents can be accessed 
  // from root command line.
  //
  // the class is templated: only classes derived from the template parameter can 
  // be stored in this contianer.  due to the limitations of TClonesArray object 
  // stored in this array must be of a same type.  the actual type of content is 
  // determined at run time based on the first object stored.  the type of stored 
  // objects can be changed by deleting all elements, and inserting a new element 
  // of a different type (class).
  //
  // objects stored in this continer must have a method that can copy the contents
  // of the object (void T::copy(T const * from) ).  they also must also have a 
  // void set_container(emcContainerT<T> * container) methodd which is called when 
  // the object is added to a container, and a get_container() const method which 
  // returns the container this object is stored in, or NULL, if the object is not 
  // stored in any container.
  //
  // if you want to access the stored objects hash-like, then the class you want to 
  // store in this container (ie class T) must also have a type defining the type of 
  // the unique key (T::key_type), and a method that returns the key of an object 
  // (T::key_type T::get_key() const).  all objects stored in a container must have 
  // different keys.  you will recive an error if you try to store an object that 
  // has the same key as one of the objects already in the container.
  //
  // when the container is written to disc the index is not written out. the index is
  // recreated on the fly if neccesery.
  //
  // this class inherits from TArray to prevent the bypassing of it's streamer
  // (by making CanSplit() return false).
  //
  ////////////////////////////////////////////////////////////////////////////////// 
public:
  // key type of stored objects
  typedef typename T::key_type key_type;


public:
  // default constructor
  emcContainerTNotObject(): csize(0) { data = new TClonesArray(T::Class(), 2); }

  // copy constructor
  emcContainerTNotObject(emcContainerTNotObject<T> const & t): 
    data(NULL), csize(0) { data = new TClonesArray(T::Class(), 2); this->copy(&t); }

  // destructor
  virtual ~emcContainerTNotObject(){ clear(); delete data; data = NULL; }



  // copies content of other container into this container. previous content is erased.
  virtual void copy(emcContainerTNotObject<T> const * from);
  
  // returns type of object that can be stored in this container. 
  // if the container is empty returnes the class of the template parameter, otherwise
  // the actual type that can be stored.
  virtual TClass * gettype() const { return data->GetClass(); }

  // returns current capacity of this container.  note: the container is automaticaly 
  // resized when neccesary.
  virtual unsigned int capacity() const { return data->Capacity(); }
  
  // returns the actual number of objects stored in this container.
  virtual unsigned int size() const { return csize; }

  // resizes the container.  note: the container might not get actually resized (so
  // capacity() migth return smaller value than was requested with this call), but 
  // the container will expand automaticaly when needed.
  //
  // returns true if the operation was succesfull
  virtual bool resize(unsigned int size);

  // adds new object to the container.  actualy a copy of the data is added.
  // if the operation is succesfull, the object is adopted by the container (you
  // must not reference the object any more, and you must not call delete on it).
  //
  // the operation might fail if you try to store an object which is incompatible
  // with the container (ie you try to store Foo in a container for Bar) or with 
  // the content already stored (ie you try to store Foov2 in a container already
  // containing Foov1).
  //
  // if you try to store an object with a key that is already present in 
  // the container the operation will fail.
  //
  // returns the index of the stored object, -1 on error.
  virtual int add(T const * d);

  // returns pointer to stored object with a given key.  key is the key of the 
  // object you are looking for.  returns address of object, or NULL if not found.
  virtual T * find(key_type const & key) const;

  // returns poniter to stored object with given index.  index is index of object 
  // to be retrived, must be in range [0..size()-1]. returns NULL on error (index 
  // out of bounds)
  virtual T * get(unsigned int index) const;

  // finds index for a given object. looks over all stored objects and
  // checks if their address is equal with the given address (the inverse
  // lookup of get() ).  addr is the address of object to be located,
  // returns the index of the object or -1 if object is not found in container.
  virtual int teg(T * const addr) const;

  // clears contents of this contianer and storage type.
  virtual void clear(){ data->Delete("C"); csize = 0; index.clear(); }


  // recreates the index used by the find() method. the index is not saved to 
  // disk to save space, therfore it must be recreated when object is read back
  // into memory.
  void makeindex() const;


protected: public:
  // dummy function. need to implement this function because it is pure 
  // virtual in TArray.
  virtual void Set(int){}

  // dummy implementation of TArray.
  virtual Double_t GetAt(Int_t) const { return 0.; }

  // dummy implementation of TArray.
  virtual void SetAt(Double_t, Int_t){}
  

  TClonesArray * data; // TClonesArray storing the objects
  unsigned int csize; // cache of size(). to speed up code: according to jprof 97% of time was spent in size().
  mutable std::map<typename T::key_type, T *> index; //! index to speed up access by key  (key->address mapping)

  ClassDef(emcContainerTNotObject, 1);
};


templateClassImp(emcContainerTNotObject);








template<class T>
void emcContainerTNotObject<T>::copy(emcContainerTNotObject<T> const * from){
  clear();
  for(unsigned int i = 0; i < from->csize; i++)
    add( from->get(i)->clone() );
}



template<class T>
bool emcContainerTNotObject<T>::resize(unsigned int size){
  data->Expand(size); 
  if( T::__buildhash ) makeindex();
  return true;
}



template<class T>
int emcContainerTNotObject<T>::add(T const * d){
  TClass * oldclass = csize ? data->GetClass() : NULL;
  TClass * newclass = d->IsA();


  // do we already have an entry with this key?
  if( T::__buildhash  &&  index.find(d->get_key()) != index.end() ) return -1;


  // can we store this object at all?
  if( !d->InheritsFrom(T::Class()) ){
    std::cerr << /*this->ClassName()*/ "emcContainerT" << " can not store " << d->ClassName() << std::endl;
    return -1;
  }


  // can we store in the actual container
  if( csize != 0  &&  newclass != oldclass ){
    std::cerr << "this " << /*ClassName()*/ "emcContainerT" << " currently can hold only " << oldclass->GetName()
	      << ", " << newclass->GetName() << " does not fit in."
	      << std::endl;
    return -1;
  }

  
  // if we need a new TClonesArray, create it
  if( csize == 0 || oldclass != newclass ){
    if(data){ data->Delete("C"); delete data; data = NULL; }
    data = new TClonesArray(newclass, 2);
    data->BypassStreamer(kFALSE);
    csize = 0;
  }


  // resize container if needed
  if( capacity() == csize ) resize( capacity() * 2 );

  
  // add copy of this object (object is adopted by container (ok, at least taken care of))
  // we need to make a copy, because objects can only be added using the new-with-placement
  // operator
  // 
  // also, some RTTI pointer magic is required, because we don't know the exact
  // type of the object we have created (but ROOT knows, so it's all legal).
  //
  unsigned int i = csize++;
  T * newentry = dynamic_cast<T *>( (TObject*)newclass->New(data->AddrAt(i)) );
  assert(newentry != NULL);
  newentry->copy(d);
  assert( d->get_key() == newentry->get_key() );
  newentry->set_container(this);


  // update index
  if( T::__buildhash ) index[d->get_key()] = get(i);


  delete d;
  return i;
}



template<class T>
T * emcContainerTNotObject<T>::find(key_type const & key) const {
  if( !T::__buildhash ) return NULL;

  typename std::map<key_type, T *>::const_iterator i = index.find(key);
  if( i == index.end() ) return NULL;

  return i->second;
}



template<class T>
T * emcContainerTNotObject<T>::get(unsigned int index) const {
  if( csize <= index ) return NULL;
  return dynamic_cast<T *>( data->AddrAt(index) );
}



template<class T>
int emcContainerTNotObject<T>::teg(T * const addr) const {
  for(unsigned int i = 0; i < csize; i++)
    if( dynamic_cast<T *>( data->AddrAt(i) ) == addr ) return i;
  return -1;
}



template<class T> 
void emcContainerTNotObject<T>::makeindex() const {
  if( !T::__buildhash ) return;
  index.clear();
  for(unsigned int i = 0; i < csize; i++) index[ get(i)->get_key() ] = get(i);
}



/**
 * custom streamer for emcContainerTNotObject. this streamer fills the
 * index when the object is read in from disc.
 */
/*
template<class T>
void emcContainerTNotObject<T>::Streamer(TBuffer & buffer){
  
  if ( buffer.IsReading() ){

    emcContainerTNotObject<T>::Class()->ReadBuffer(buffer, this);

    if( T::__buildhash ){
      index.clear();
      for(unsigned int i = 0; i < csize; i++) index[ get(i)->get_key() ] = get(i);
    }
    
  } else {

    emcContainerTNotObject<T>::Class()->WriteBuffer(buffer, this);

  }

}
*/






#define emcContainerNotObjectDef(_classname)						\
   class _classname ## Container: public emcContainerTNotObject<_classname> {		\
   public:										\
    _classname ## Container(){}								\
											\
   public:										\
     /* typedef  emcContainerTNotObject<_classname>::key_type key_type;	*/		\
	/* typedef  emcContainerTNotObject<_classname>::value_type value_type; */	\
     /* typedef  emcContainerTNotObject<_classname>::iterator iterator; */		\
     /* typedef  emcContainerTNotObject<_classname>::const_iterator const_iterator; */	\
											\
   private:										\
     ClassDef(_classname ## Container, 1);						\
   };



#define emcContainerNotObjectImp(_classname)		\
   template class emcContainerTNotObject< _classname >;	\
   ClassImp( _classname ## Container );








template<class T> class emcContainerT: public PHObject, public emcContainerTNotObject<T> {
  ////////////////////////////////////////////////////////////////////////////////// 
  //
  // map like table that does inherits from PHObject.
  //
  // this class is an extension to emcContainerTNotObject: it contains implementation
  // of the functions: PHObject::clone(), PHObject::identify(), PHObject::Reset() and 
  // PHObject::isValid().
  //
  ////////////////////////////////////////////////////////////////////////////////// 

public:
  // default constructor
  emcContainerT(){}

  // destructor
  virtual ~emcContainerT(){ Reset(); }    // Reset must delete all elements

  

public:    // standard PHObject fuctions

  // implementation of PHObject::clone() for emcContainerT.
  emcContainerT * clone() const { return new emcContainerT<T>(*this); }

  // implementation of PHObject::identify for emcContainerT.
  void identify(std::ostream& os = std::cout) const {
    std::streambuf *outbuf = std::cout.rdbuf(os.rdbuf());
    Dump();
    std::cout.rdbuf(outbuf);
  }

  // implementation of PHObject::Reset() for emcContainerT.
  void Reset(){ emcContainerTNotObject<T>::clear(); }

  // implementation of PHObject::isValid() for emcContainerT. (dummy,
  // returns 1.)
  int isValid() const { return 1; }


  ClassDef(emcContainerT, 1);
};


templateClassImp(emcContainerT);



#define emcContainerDef(_classname)							\
   class _classname ## Container: public emcContainerT<_classname> {			\
   public:										\
    _classname ## Container(){}								\
											\
   public:										\
  /*   typedef  emcContainerT<_classname>::key_type key_type;	*/			\
  /*   typedef  emcContainerT<_classname>::value_type value_type;	*/		\
  /*   typedef  emcContainerT<_classname>::iterator iterator;	*/			\
  /*   typedef  emcContainerT<_classname>::const_iterator const_iterator;*/		\
											\
   static _classname ## Container * createdef(){ return new _classname ## Container; }	\
											\
   private:										\
     ClassDef(_classname ## Container, 1);						\
   };


#define emcContainerImp(_classname)			\
   template class emcContainerTNotObject< _classname >;	\
   template class emcContainerT< _classname >;		\
   ClassImp( _classname ## Container );





#endif /* ! __EMC_CONTAINER_T_H__ */

