#ifndef __EMCOMASCIIT_H__
#define __EMCOMASCIIT_H__

#ifndef __EMCOBJECTMANAGER_H__
#include "emcObjectManager.h"
#endif
#ifndef __EMCMANAGEABLE_H__
#include "emcManageable.h"
#endif

/** Base class for ASCII file DM plugins. 
    
    The CanRead and CanWrite are implemented here, while
    the actual reader and writer must be specified by
    the subclass(es).

    @ingroup dmplugins
 */

template<class T>
class emcOMAsciiT : public emcObjectManager
{
public:

  typedef bool (*ReaderFcn)(T& object, int code);
  typedef bool (*WriterFcn)(const T& object, int code);

  emcOMAsciiT(const char* name, const char* title, 
	      ReaderFcn reader,
	      WriterFcn writer=0);

  virtual ~emcOMAsciiT();

  virtual bool CanRead(const emcManageable& object) const;

  virtual bool CanWrite(const emcManageable& object) const;

  using emcObjectManager::Read;

  /// Will forward to the internal fReader.
  virtual bool Read(emcManageable& object,
		    const PHTimeStamp& time_stamp,
		    int id);

  virtual void Reset() {}

  /// Will forward to the internal fWriter.
  virtual bool Write(const emcManageable& object,
		     const PHTimeStamp& tStart,
		     int id = -1);

private:

  ReaderFcn fReader;
  WriterFcn fWriter;

  /** \internal
   */
  class changeName
  {
  public:
    changeName(const char* name)
    {
      name_ = emcManageable::GetStorageName(emcManageable::kFile_ASCII);
      name_ += ":";
      name_ += name;
    }
    
    const char* c_str() const
    {
      return name_.c_str();
    }
     
  private:
    std::string name_;
  };
};

//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________

#ifndef __EMCDATAMANAGER_H__
#include "emcDataManager.h"
#endif

//_____________________________________________________________________________
template<class T>
emcOMAsciiT<T>::emcOMAsciiT(const char* name, const char* title, 
			    emcOMAsciiT<T>::ReaderFcn reader,
			    emcOMAsciiT<T>::WriterFcn writer)
  : emcObjectManager(changeName(name).c_str(),title),
    fReader(reader), fWriter(writer)
{
}

//_____________________________________________________________________________
template<class T>
emcOMAsciiT<T>::~emcOMAsciiT()
{
}

//_____________________________________________________________________________
template<class T>
bool
emcOMAsciiT<T>::CanRead(const emcManageable& object) const
{
  if ( object.GetSource() != emcManageable::kFile_ASCII )
    {
      return false;
    }
  const T* test = dynamic_cast<const T*>(&object);
  if ( test ) 
    {
      return true;
    }
  else
    {
      return false;
    }
}

//_____________________________________________________________________________
template<class T>
bool
emcOMAsciiT<T>::CanWrite(const emcManageable& object) const
{
  if ( object.GetDestination() != emcManageable::kFile_ASCII )
    {
      return false;
    }
  const T* test = dynamic_cast<const T*>(&object);
  if ( test ) 
    {
      return true;
    }
  else
    {
      return false;
    }
}

//_____________________________________________________________________________
template<class T>
bool 
emcOMAsciiT<T>::Read(emcManageable& object,
		     const PHTimeStamp& /*time_stamp*/,
		     int id)
{
  T& tobject = static_cast<T&>(object);
  return fReader(tobject,id);
}

//_____________________________________________________________________________
template<class T>
bool 
emcOMAsciiT<T>::Write(const emcManageable& object,
		      const PHTimeStamp& /*time_stamp*/,
		      int id)
{
  if ( fWriter )
    {
      const T& tobject = static_cast<const T&>(object);
      return fWriter(tobject,id);
    }
  return false;
}

#endif
