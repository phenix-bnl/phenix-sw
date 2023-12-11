#ifndef __PFSEVENTITERATOR_H__
#define __PFSEVENTITERATOR_H__


#define CTRL_OPEN 1
#define CTRL_DATA     3
#define CTRL_CLOSE    4

#define CTRL_REMOTESUCCESS 100
#define CTRL_REMOTEFAIL    101



#include <stdio.h>

#include <Eventiterator.h>
#include <Event.h>
//#ifndef WIN32
#include <gzbuffer.h>
//#else
//#include <buffer.h>
//#endif

/**
   The pfsEventiterator reads the event data from a pfs server.
   It creates and returns pointers to Event objects. At the end of the file 
   it returns 0 when there are no events left.
*/
#ifndef __CINT__
class WINDOWSEXPORT pfsEventiterator : public Eventiterator {
#else
class  pfsEventiterator : public Eventiterator {
#endif
public:

  virtual ~pfsEventiterator();

  /// This simple constructor just needs the file name of the data file.
  pfsEventiterator(const char *filename, const char *hostname=0, const int portnr = 0);
  pfsEventiterator(const char *url, int &status);  // phnxbox1.phenix.bnl.gov/dir/filename:port"

  /**
  This constructor gives you a status so you can learn that the creation
  of the pfsEventiterator object was successful. If the status is not 0,
  something went wrong and you should delete the object again.
  */
  //  pfsEventiterator(const char *filename, const char *hostname=0, const int portnr = 0, int &status);

  const char * getIdTag() const;

  virtual void identify(std::ostream& os = std::cout) const;


/**
   this member function returns a pointer to the Event object, or
   NULL if there are no events left.
*/   
  Event *getNextEvent();


private:
  int read_next_buffer();

  int readn (int fd, char *ptr, int nbytes);
  int writen (int fd, char *ptr, int nbytes);
  
  int Create(const char *file, const char *host, const int port);

  PHDWORD *bp;
  int allocatedsize;

  int current_index;
  int last_read_status;
  int buffer_size;
  buffer *bptr;

  char *host_name;
  char *file_name;
  int thePort;
  
  int sockfd;


};



#endif /* __PFSEVENTITERATOR_H__ */

