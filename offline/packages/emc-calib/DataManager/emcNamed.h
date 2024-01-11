#ifndef __EMCNAMED_H__
#define __EMCNAMED_H__

#include <string>
#include <iosfwd>

/** A small utility class to give name, title and classname to objects.
 */

class emcNamed
{
public:

  /// Ctor with a name, a title and a classname.
  emcNamed(const char* name="", 
	   const char* title="", 
	   const char* classname="");

  emcNamed(const emcNamed&);

  emcNamed& operator=(const emcNamed&);

  virtual ~emcNamed();
  
  /// Get the classname
  virtual const char* GetClassName(void) const { return fClassName.c_str(); }
  /// Get the name
  virtual const char* GetName(void) const { return fName.c_str(); }
  /// Get the title
  virtual const char* GetTitle(void) const { return fTitle.c_str(); }

  virtual void NameIt(const std::string& name, 
		      const std::string& title, 
		      const std::string& classname);

  /// Set the classname
  virtual void SetClassName(const std::string& s) { fClassName = s; }
  /// Set the title
  virtual void SetTitle(const std::string& s) { fTitle = s; }
  /// Set the name
  virtual void SetName(const std::string& s) { fName = s; }

 protected:
  void Copy(emcNamed&) const;

 private:
  std::string fName;
  std::string fTitle;
  std::string fClassName;
};

std::ostream& operator << (std::ostream&, const emcNamed&);

#endif
