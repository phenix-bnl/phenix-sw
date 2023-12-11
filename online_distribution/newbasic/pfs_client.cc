/**

*/



#include <cstdlib>
#include <cerrno>
#include <string>
#include <iostream>
#include <iomanip>

#include <netdb.h>
#include <unistd.h>


#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/times.h>
#include <sys/types.h>



#ifdef HAVE_BSTRING_H
#include <bstring.h>
#else
#include <strings.h>
#endif

#ifdef HAVE_GETOPT_H
#include <getopt.h>
#endif



#define CTRL_OPEN     1
#define CTRL_DATA     3
#define CTRL_CLOSE    4

#define CTRL_REMOTESUCCESS 100
#define CTRL_REMOTEFAIL    101

#define TRUE 1
#define FALSE 0

int readn(int , char *, int);
int writen(int , char *, int);

void exitmsg()
{
  std::cout << "** usage: pfs_data filename" << std::endl;
  exit(0);
}


int main( int argc, char* argv[])
{
  int the_port;
  int number_of_buffers;
  int controlword, i;
  int status;
  char c;
  char *host_name = NULL;
  char *file_name = NULL;
  

  number_of_buffers = 1;
  the_port=5001;
  int waitinterval = 0;
  int identifyflag = 0;

  while ((c = getopt(argc, argv, "w:n:p:v")) != EOF)
    {
      switch (c) 
	{
	case 'n':
	  sscanf (optarg,"%d", &number_of_buffers);
	  break;
	case 'p':
	  sscanf (optarg,"%d", &the_port);
	  break;
	case 'w':
	  sscanf (optarg,"%d", &waitinterval);
	  break;

	}
    }

  if (argc >= optind+2)  
    {
      
      file_name = argv[optind];
      host_name = argv[optind+1];
    }
  else exitmsg();

  int sockfd;
  struct sockaddr_in server_addr;
  struct hostent *p_host;
  p_host = gethostbyname(host_name);
  std::cout << p_host->h_name << std::endl;

  bzero( (char*) &server_addr, sizeof(server_addr) );
  server_addr.sin_family = AF_INET;
  bcopy(p_host->h_addr, &(server_addr.sin_addr.s_addr), p_host->h_length);
  server_addr.sin_port = htons(the_port);


  if ( (sockfd = socket(AF_INET, SOCK_STREAM, 0) ) < 0 )
    {
      std::cout << "error in socket" << std::endl;
      exit(1);
    }

  int xs = 512*1024;
  
  int s = setsockopt(sockfd, SOL_SOCKET, SO_SNDBUF,
		     &xs, 4);
  if (s) std::cout << "setsockopt status = " << s << std::endl;

  if ( connect(sockfd, (struct sockaddr*) &server_addr
	       , sizeof(server_addr)) < 0 ) 
    {
      std::cout << "error in connect" << std::endl;
      exit(1);
    }

  char filename[256];
  strcpy( filename, "xxxxxxxx");

  controlword = htonl(CTRL_OPEN);
  writen (sockfd,(char *)  &controlword, 4);
  writen (sockfd,filename, 255);

  readn (sockfd, (char *) &i, 4);
  std::cout << "answer: " << ntohl(i) << std::endl;

  controlword = htonl(CTRL_CLOSE);
  writen (sockfd, (char *)&controlword, 4);

  close (sockfd);
  
  return 0;
}


int readn (int fd, char *ptr, int nbytes)
{
  int nleft, nread;
  nleft = nbytes;
  while ( nleft>0 )
    {
      nread = read (fd, ptr, nleft);
      if ( nread < 0 ) 
	return nread;
      else if (nread == 0) 
	break;
      nleft -= nread;
      ptr += nread;
    }
  return (nbytes-nleft);
}


int writen (int fd, char *ptr, int nbytes)
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


