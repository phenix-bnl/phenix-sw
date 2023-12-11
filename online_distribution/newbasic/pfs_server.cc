#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/time.h>

#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <unistd.h>

#include <sys/socket.h>
#include <netdb.h> 

#include <sys/types.h>

#include <sys/stat.h>

#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <sys/ioctl.h>

#include <stdio.h>
#include <iostream>
#include <iomanip>

#ifdef HAVE_GETOPT_H
#include <getopt.h>
#endif

#include <BufferConstants.h>
#include <buffer.h>


#define CTRL_OPEN 1
#define CTRL_DATA     3
#define CTRL_CLOSE    4

#define CTRL_REMOTESUCCESS 100
#define CTRL_REMOTEFAIL    101

#define TRUE 1
#define FALSE 0

FILE *fp=0;
int fd;

int the_port = 5001;
int verbose  = 0;

unsigned int *TheBuffer = 0;
int current_length = 0;

int sendNextBuffer(int dd_fd);



void exitmsg()
{
  std::cout << "** This is the Phenix Filesystem  Server." << std::endl;
  std::cout << "** usage: pfs_server [-v ] [-p  port-number]" << std::endl;
  exit(0);
}



void sig_chld(int signo)
{
  pid_t   pid;
  int             stat;

  while ( (pid = waitpid(-1, &stat, WNOHANG)) > 0)
    {
      //printf("child %d terminated\n", pid);
    }
  return;
}

int readn (int fd, char *ptr, int nbytes)
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




int main( int argc, char* argv[])
{

  int i;
  int pid;
  struct sockaddr_in out;

  int dd_fd;
  int controlword;

  
  int status;
  int sockfd;

  struct sockaddr client_addr;
  struct sockaddr_in server_addr;
  unsigned int client_addr_len = sizeof(client_addr);

  char c;

  while ((c = getopt(argc, argv, "p:v")) != EOF)
    {
      switch (c) 
	{
	case 'p':   // size of the file pieces
	  if ( !sscanf(optarg, "%d", &the_port) ) exitmsg();
	  break;

	case 'v':   // verbose
	  verbose = 1;
	  break;


	}
    }


  signal (SIGCHLD, sig_chld);


  // ------------------------
  // now set up the sockets

  if ( (sockfd = socket(PF_INET, SOCK_STREAM, 0)) < 0 ) 
    {
      std::cout << "cannot create socket" << std::endl;
      exit(1);
    }
  //  int xs = 64*1024+21845;
  int xs = 1024*1024;
  int old_rdbck,rdbck;
  socklen_t opt_rdbck = sizeof(int);

  getsockopt(sockfd,SOL_SOCKET, SO_RCVBUF,&old_rdbck,
	     &opt_rdbck);

  int s = setsockopt(sockfd, SOL_SOCKET, SO_RCVBUF,&xs, 4);
  if (s) std::cout  << "setsockopt status = " << s << std::endl;

  getsockopt(sockfd,SOL_SOCKET, SO_RCVBUF,&rdbck,&opt_rdbck);

  getsockopt(sockfd,SOL_SOCKET, SO_SNDBUF,&old_rdbck,  &opt_rdbck);
  
  xs = 1024*1024;
  s = setsockopt(sockfd, SOL_SOCKET, SO_SNDBUF, &xs, 4);
 
  getsockopt(sockfd,SOL_SOCKET, SO_SNDBUF,&rdbck, &opt_rdbck);

  bzero( (char*)&server_addr, sizeof(server_addr));
  server_addr.sin_family = PF_INET;
  server_addr.sin_addr.s_addr = htonl(INADDR_ANY);
  //  server_addr.sin_port = htons(nd->get_port());
  server_addr.sin_port = htons(the_port);

  int retries = 0;
  while ( (i = bind(sockfd, (struct sockaddr*) &server_addr, 
		    sizeof(server_addr))) < 0 )
    {

      std::cout << "tried port " <<   the_port << std::endl;
      sleep(3);
      if (retries++ > 3) exit(1);

    }

  listen(sockfd, 1);
      
  while (sockfd > 0)
    {
			

      client_addr_len = sizeof(out); 
      dd_fd = accept(sockfd,  (struct sockaddr *) &out, &client_addr_len);


      if ( dd_fd < 0 ) 
	{
	  std::cout << "error in accept socket" << std::endl;
	  exit(1);
	}

      

      // should be the default, but set it to blocking mode anyway
      i = ioctl (dd_fd,  FIONBIO, 0);

      char *host = new char[64]; 
      getnameinfo((struct sockaddr *) &out, sizeof(struct sockaddr_in), host, 64,
		  NULL, 0, NI_NOFQDN); 

      std::cout <<  "new connection accepted from " << host << std::endl;

      delete [] host;

      pid = 0;

      if ( ! verbose) 
	{
	  if ( ( pid = fork() ) < 0) 
	    {
	      std::cout <<  "fork error";
	      
	    };
	}

      if (pid > 0) 
	{
	  close(dd_fd);/* parent closes connected socket */
	} 
      else 
	{
	  close(sockfd);
	  sockfd = 0;
	}
    }

  
  int go_on = 1;
  
  int broken = 0;


  while ( go_on)
    {
      if ( (status = readn (dd_fd, (char *) &controlword, 4) ) <= 0)
	{
	  controlword = 0;
	  close(dd_fd);
	  exit(1);
	}
  
      controlword = ntohl(controlword);

      std::cout << "Control word: " << controlword << std::endl;
      
      char filename[256];

      switch (controlword)
        {
          // ------------------------
          // Begin run
        case  CTRL_OPEN:
          readn (dd_fd, filename, 256);

	  std::cout << filename << std::endl;

	  fp = fopen(filename, "r");
	  if (fp)
	    {
	      i = CTRL_REMOTESUCCESS;
	    }
	  else
	    {
	      i = CTRL_REMOTEFAIL;
	    }
	  i = htonl(i);

	  writen (dd_fd, (char *)&i, 4);
	  break;

	  // ------------------------
	  // end run
	case  CTRL_CLOSE:
	  std::cout << "closing" << std::endl;
	  close (dd_fd);
	  go_on = 0;
	  fclose(fp);
	  break;

	case  CTRL_DATA:
	  std::cout << "getting next buffer" << std::endl;
	  if (broken)
	    {
	      i = htonl(-1);
	      writen (dd_fd, (char *)&i, 4);
	    }
	  else
	  {
	    i = sendNextBuffer(dd_fd);
	    if (i) broken = 1;
	  }

	  break;
	}

    }
  return 0;

}  


int sendNextBuffer(int dd_fd)
{

  int length;
  int i;

  if (!fp) 
    {
      i = htonl(-1);
      writen (dd_fd, (char *)&i, 4);
      return -1;
    }

  unsigned int firstbuffer[2048];
  
  fread ( (char *)firstbuffer, 8192, 1, fp); 
  if ( feof(fp) || ferror(fp) ) 
    {
      COUT << "end of file" << std::endl;
      i = htonl(-1);
      writen (dd_fd, (char *)&i, 4);
      return -1;
    }
  
  if ( firstbuffer[1] == BUFFERMARKER ||  
       firstbuffer[1] == (int) GZBUFFERMARKER || 
       firstbuffer[1] == (int) LZO1XBUFFERMARKER )
    {
      
      length = firstbuffer[0];
    }
  
  else if ( buffer::i4swap(firstbuffer[1]) == BUFFERMARKER || 
	    buffer::i4swap(firstbuffer[1]) == (int) GZBUFFERMARKER ||
	    buffer::i4swap(firstbuffer[1]) == (int) LZO1XBUFFERMARKER )
    {
      
      length =  buffer::i4swap(firstbuffer[0]);
    }
  

  else
    {
      return -2;
    }
  
   std::cout << __FILE__ << " " << __LINE__ << " buffer length: " << length << std::endl;
 
  if ( TheBuffer == 0)
    {
      current_length = 4*1024*1024;
      TheBuffer = new unsigned int[current_length];
    }
  
  if ( length > current_length )
    {
      delete []  TheBuffer;
      current_length = length + 1024*1024;
      TheBuffer = new unsigned int[current_length];
    }
  
  
  memcpy ( TheBuffer, firstbuffer,8192);

  int ip = 8192;
  char *cp = (char *) TheBuffer;
  cp += 8192;

  while ( ip < length)
    {
      fread ( cp, BUFFERBLOCKSIZE, 1, fp);
      if ( feof(fp) || ferror(fp) ) 
	{
	  COUT << "error in buffer, salvaging" << std::endl;
	  break;
	}

      cp += BUFFERBLOCKSIZE;
      ip += BUFFERBLOCKSIZE;
      
    }

  i = htonl(length);
  writen (dd_fd, (char *)&i, 4);
  
  writen ( dd_fd, (char *) TheBuffer, length);
  return 0;

}


