//
// pfsEventIterator   mlp 
//
// this iterator reads events from a pfs server


#include <pfsEventiterator.h>
#include <cstddef>
#include <cstring>
#include <cstdlib>

#include <unistd.h>
#include <sys/socket.h>
#include <netdb.h> 

#if defined(SunOS) || defined(OSF1)
#include <strings.h>
#endif


//#include <sys/types.h>
//#include <unistd.h>

// there are two similar constructors, one with just the
// filename, the other with an additional status value
// which is non-zero on return if anything goes wrong. 

//#ifndef LVL2_WINNT
#include <lzobuffer.h>
//#endif


pfsEventiterator::~pfsEventiterator()
{
     if (host_name != NULL) delete [] host_name;
     if (file_name != NULL) delete [] file_name;
     if (bp != NULL ) delete [] bp;
     if (bptr != NULL ) delete bptr;
     int controlword = htonl(CTRL_CLOSE);
     writen (sockfd,(char *)  &controlword, 4);

}  


pfsEventiterator::pfsEventiterator(const char *file, const char *host, const int port)
{
  Create(file, host, port);
}

pfsEventiterator::pfsEventiterator(const char *url, int &status)
{

  char *s0 = new char [ strlen(url) +1];

  strcpy( s0, url);
  char *s = s0;
 
  char *host, *name;

  host =  strsep ( &s,":");
  std::cout << "Host: " << host << std::endl;

  name =  strsep ( &s,":");
  std::cout << "name: " << name << std::endl;

  std::string port =  strsep ( &s,":");
  if (port == "") port = "0";

  std::cout << "port: " << port << std::endl;

  status = Create(name, host, atoi(port.c_str()));
}  
  
int pfsEventiterator::Create(const char *file, const char *host, const int port)
{
  int i;

  host_name = new char [ strlen(host) + 1 ];
  strcpy ( host_name, host );

  file_name  = new char [ strlen(file) + 1 ];
  strcpy (file_name, file);

  if ( port) thePort = port;
  else thePort = 7777;

  struct sockaddr_in server_addr;
  struct hostent *p_host;
  p_host = gethostbyname(host_name);
  //  std::cout << p_host->h_name << std::endl;

  bzero( (char*) &server_addr, sizeof(server_addr) );
  server_addr.sin_family = AF_INET;
  bcopy(p_host->h_addr, &(server_addr.sin_addr.s_addr), p_host->h_length);
  server_addr.sin_port = htons(thePort);

  if ( (sockfd = socket(AF_INET, SOCK_STREAM, 0) ) < 0 )
    {
      std::cout << "error in socket" << std::endl;
      //      exit(1);
    }

  int xs = 512*1024;
  setsockopt(sockfd, SOL_SOCKET, SO_SNDBUF, &xs, 4);

  if ( connect(sockfd, (struct sockaddr*) &server_addr
	       , sizeof(server_addr)) < 0 ) 
    {
      std::cout << "error in connect" << std::endl;
      exit(1);
    }

  char send_filename[256];
  strcpy( send_filename, file_name);

  int controlword = htonl(CTRL_OPEN);
  writen (sockfd,(char *)  &controlword, 4);
  writen (sockfd,send_filename, 256);

  readn (sockfd, (char *) &i, 4);
  i = ntohl(i);

  std::cout << "answer: " << i << std::endl;

  
  bptr = 0;
  bp = 0;
  allocatedsize = 0;
  
  last_read_status = 0;
  current_index = 0;
  if ( i ==  CTRL_REMOTESUCCESS ) return 0;
  return 1;


}  



void pfsEventiterator::identify (OSTREAM &os) const
{ 
  os << "pfsEventiterator reading " << file_name << " from server " << host_name << std::endl;

};

const char *  pfsEventiterator::getIdTag () const
{ 
  //  sprintf (idline, " -- pfsEventiterator reading from %s", thefilename);
  return "pfsEventiterator";
};



// and, finally, the only non-constructor member function to
// retrieve events from the iterator.

Event * pfsEventiterator::getNextEvent()
{
  Event *evt = 0;

  // if we had a read error before, we just return
  if (last_read_status) return NULL;

  // see if we have a buffer to read
  if (bptr == 0) 
    {
      if ( (last_read_status = read_next_buffer()) !=0 )
	{
	  return NULL;
	}
    }

  while (last_read_status == 0)
    {
      if (bptr) evt =  bptr->getEvent();
      if (evt) return evt;

      last_read_status = read_next_buffer();
    }

  return NULL;

}

// -----------------------------------------------------
// this is a private function to read the next buffer
// if needed. 

int pfsEventiterator::read_next_buffer()
{
  int ianswer;
  int i;

  buffer_size = 0;

  if (bptr) 
    {
      delete bptr;
      bptr = 0;
    }

  int controlword = htonl(CTRL_DATA);
  writen (sockfd,(char *)  &controlword, 4);

  readn (sockfd, (char *) &i, 4);
  ianswer =  ntohl(i);
  std::cout << __FILE__ << " " << __LINE__ << " answer: " << ianswer << std::endl;

  if ( ianswer < 0)
    {
      return -1;
    }

  buffer_size = ianswer;

  if (bp) 
    {
      if  (buffer_size > allocatedsize*4)
	{
	  delete [] bp;
	  i = (buffer_size +BUFFERBLOCKSIZE-1) /BUFFERBLOCKSIZE;
	  allocatedsize = i * BUFFERBLOCKSIZE/4;
	  bp = new PHDWORD[allocatedsize];
	}
    }
  else
    {
      i = (buffer_size +BUFFERBLOCKSIZE-1) /BUFFERBLOCKSIZE;
      allocatedsize = i * BUFFERBLOCKSIZE/4;
      bp = new PHDWORD[allocatedsize];
      //COUT << "allocating buffer, size is " << allocatedsize <<std::endl;
    }


  readn (sockfd, (char *) bp, buffer_size);

  // and initialize the current_index to be the first event

  if ( bp[1]== GZBUFFERMARKER || buffer::i4swap(bp[1])== GZBUFFERMARKER )
    {
      bptr = new gzbuffer(bp, allocatedsize );
    }

  else if ( bp[1]== LZO1XBUFFERMARKER || buffer::i4swap(bp[1])== LZO1XBUFFERMARKER )
    {
      bptr = new lzobuffer ( bp, allocatedsize );
    }

  else
    {
//#endif
      bptr = new buffer ( bp, allocatedsize );
//#ifndef WIN32
    }
//#endif
  return 0;
}


int pfsEventiterator::readn (int fd, char *ptr, int nbytes)
{

  int nread, nleft;
  //int nleft, nread;
  nleft = nbytes;
  while ( nleft>0 )
    {
      nread = read (fd, ptr, nleft);
      if ( nread <= 0 ) 
	{
	  return nread;
	}
      nleft -= nread;
      ptr += nread;
    }

  return (nbytes-nleft);
}
  


int pfsEventiterator::writen (int fd, char *ptr, int nbytes)
{
  int nleft, nwritten;
  nleft = nbytes;
  while ( nleft>0 )
    {
      nwritten = write (fd, ptr, nleft);
      if ( nwritten < 0 ) 
	return nwritten;

      nleft -= nwritten;
      ptr += nwritten;
    }
  return (nbytes-nleft);
}
