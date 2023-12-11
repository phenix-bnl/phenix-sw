/*----------------------------------------------------------------------------*
 *  Copyright (c) 2000        Southeastern Universities Research Association, *
 *                            Thomas Jefferson National Accelerator Facility  *
 *                                                                            *
 *    This software was developed under a United States Government license    *
 *    described in the NOTICE file included as part of this distribution.     *
 *                                                                            *
 * TJNAF Data Acquisition Group, 12000 Jefferson Ave., Newport News, VA 23606 *
 *      heyes@cebaf.gov   Tel: (804) 269-7030    Fax: (804) 269-5800          *
 *----------------------------------------------------------------------------*
 * Description:
 *      Native method implementations for ET viewer. These methods get local
 *	subnets and get values of macros defined in include files.
 *
 * Author:
 *	Carl Timmer
 *	TJNAF Data Acquisition Group
 *
 * Revision History:
 *
 *----------------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "et_private.h"
#include "et_network.h"
#include "EtNative.h"

/* prototypes */
static struct ifi_info *get_ifi_info(int family, int doaliases);
static void   free_ifi_info(struct ifi_info *ifihead);


/******************************************************
 * The following 3 routines are taken from R. Stevens book,
 * UNIX Network Programming, Chapter 16. A few changes have
 * been made to simply things and fit them into the ET system
 * and the Java language.
 * These are used for finding our broadcast addresses.
 * Yes, we stole the code. See also et_network.c .
 ******************************************************/
 
JNIEXPORT jobjectArray JNICALL Java_EtNative_findSubnets
  (JNIEnv *env, jclass class_this)
{

/* max # of addresses/interfaces to check */
#define MAX_ADDRESSES 10

  struct ifi_info	*ifi, *ifihead;
  struct sockaddr	*sa;
  struct sockaddr_in	*baddr_in;
  int			i, count=0;
  char			*val, subnet_addrs[MAX_ADDRESSES][ET_IPADDRSTRLEN];
  
  /* java declarations */
  jclass       class_ex, class_st;
  jmethodID    mid;
  jthrowable   throw_obj;
  jstring      utf_string;
  jobjectArray subnets;
  
  /* create an object so we can throw an exception */
  class_ex  = (*env)->FindClass(env, "java/io/IOException");
  mid       = (*env)->GetMethodID(env, class_ex, "<init>", "()V");
  throw_obj = (jthrowable) (*env)->NewObject(env, class_ex, mid);
  
  /* look through MAX_ADDRESSES IPv4 interfaces at most */
  ifihead = ifi = get_ifi_info(AF_INET, MAX_ADDRESSES);
  if (ifi == NULL) {
    fprintf(stderr, "Java_EtViewer_findSubNets: cannot find network interface info\n");
    (*env)->Throw(env, throw_obj);
    return NULL;
  }

  for (;ifi != NULL; ifi = ifi->ifi_next) {
    /* if the interface is up and there's a broadcast address ... */
    if (((ifi->ifi_flags & IFF_UP) > 0) &&
        ((ifi->ifi_flags & IFF_BROADCAST) > 0)) {

      if ( (sa = ifi->ifi_brdaddr) != NULL) {
	baddr_in = (struct sockaddr_in *) sa;
	val = inet_ntoa(baddr_in->sin_addr);
	strncpy(subnet_addrs[count], val, ET_IPADDRSTRLEN-1);
	subnet_addrs[count][ET_IPADDRSTRLEN-1] = '\0';
	count++;
      }
      
    }
  }
  
  if (count == 0) {
    fprintf(stderr, "Java_EtViewer_findSubNets: cannot find subnet address\n");
    (*env)->Throw(env, throw_obj);
    return NULL;
  }
  
  /* create String array that we fill and send back */
  class_st = (*env)->FindClass(env, "java/lang/String");
  subnets  = (*env)->NewObjectArray(env, count, class_st, NULL);
  
  for (i=0; i < count; i++) {
    utf_string = (*env)->NewStringUTF(env, subnet_addrs[i]);
    (*env)->SetObjectArrayElement(env, subnets, i, utf_string);
    (*env)->DeleteLocalRef(env, utf_string);
  }
  
  /* free memory */
  free_ifi_info(ifihead);
  
  return subnets;
}

/******************************************************/
static struct ifi_info *get_ifi_info(int family, int doaliases)
{
  struct ifi_info	*ifi, *ifihead, **ifipnext;
  int			sockfd, len, lastlen, flags, myflags;
  char			*ptr, *buf, lastname[IFNAMSIZ], *cptr;
  struct ifconf		ifc;
  struct ifreq		*ifr, ifrcopy;
  struct sockaddr_in	*sinptr;

  if ((sockfd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
    fprintf(stderr, "get_ifi_info: socket error, %s.\n", strerror(errno));
    return NULL;
  }

  /* initial buffer size guess */
  len = 10 * sizeof(struct ifreq);
  lastlen = 0;
  
  for ( ; ; ) {
    buf = malloc(len);
    ifc.ifc_len = len;
    ifc.ifc_buf = buf;
    if (ioctl(sockfd, SIOCGIFCONF, &ifc) < 0) {
      if (errno != EINVAL || lastlen != 0) {
        fprintf(stderr, "get_ifi_info: ioctl error\n");
	close(sockfd);
	return NULL;
      }
    }
    else {
      if (ifc.ifc_len == lastlen) {
	/* success, len has not changed */
	break;
      }
      lastlen = ifc.ifc_len;
    }
    len += sizeof(struct ifreq);	/* increment */
    free(buf);
  }
  
  ifihead     = NULL;
  ifipnext    = &ifihead;
  lastname[0] = 0;

  for (ptr = buf; ptr < buf + ifc.ifc_len; ) {
    ifr = (struct ifreq *) ptr;

    switch (ifr->ifr_addr.sa_family) {
#ifdef IPV6
      case AF_INET6:	
	len = sizeof(struct sockaddr_in6);
	break;
#endif
      case AF_INET:	
      default:	
	len = sizeof(struct sockaddr);
	break;
    }
    
    /* for next one in buffer */
    ptr += sizeof(ifr->ifr_name) + len;

    /* ignore if not desired address family */
    if (ifr->ifr_addr.sa_family != family) {
      continue;
    }
    
    myflags = 0;
    if ( (cptr = strchr(ifr->ifr_name, ':')) != NULL) {
      *cptr = 0;	/* replace colon will null */
    }
    if (strncmp(lastname, ifr->ifr_name, IFNAMSIZ) == 0) {
      if (doaliases == 0) {
	continue;	/* already processed this interface */
      }
      myflags = IFI_ALIAS;
    }
    memcpy(lastname, ifr->ifr_name, IFNAMSIZ);

    ifrcopy = *ifr;
    ioctl(sockfd, SIOCGIFFLAGS, &ifrcopy);
    flags = ifrcopy.ifr_flags;
    /* ignore if interface not up */
    if ((flags & IFF_UP) == 0) {
	continue;
    }

    ifi = calloc(1, sizeof(struct ifi_info));
    *ifipnext = ifi;		/* prev points to this new one */
    ifipnext  = &ifi->ifi_next;	/* pointer to next one goes here */

    ifi->ifi_flags = flags;	/* IFF_xxx values */
    ifi->ifi_myflags = myflags;	/* IFI_xxx values */
    memcpy(ifi->ifi_name, ifr->ifr_name, IFI_NAME);
    ifi->ifi_name[IFI_NAME-1] = '\0';

    switch (ifr->ifr_addr.sa_family) {
    case AF_INET:
      sinptr = (struct sockaddr_in *) &ifr->ifr_addr;
      if (ifi->ifi_addr == NULL) {
	ifi->ifi_addr = calloc(1, sizeof(struct sockaddr_in));
	memcpy(ifi->ifi_addr, sinptr, sizeof(struct sockaddr_in));

	if (flags & IFF_BROADCAST) {
	  ioctl(sockfd, SIOCGIFBRDADDR, &ifrcopy);
	  sinptr = (struct sockaddr_in *) &ifrcopy.ifr_broadaddr;
	  ifi->ifi_brdaddr = calloc(1, sizeof(struct sockaddr_in));
	  memcpy(ifi->ifi_brdaddr, sinptr, sizeof(struct sockaddr_in));
	}

	if (flags & IFF_POINTOPOINT) {
	  ioctl(sockfd, SIOCGIFDSTADDR, &ifrcopy);
	  sinptr = (struct sockaddr_in *) &ifrcopy.ifr_dstaddr;
	  ifi->ifi_dstaddr = calloc(1, sizeof(struct sockaddr_in));
	  memcpy(ifi->ifi_dstaddr, sinptr, sizeof(struct sockaddr_in));
	}
      }
      break;

    default:
      break;
    }
  }
  free(buf);
  close(sockfd);
  return(ifihead);	/* pointer to first structure in linked list */
}

/******************************************************/
static void free_ifi_info(struct ifi_info *ifihead)
{
  struct ifi_info  *ifi, *ifinext;

  for (ifi = ifihead; ifi != NULL; ifi = ifinext) {
    if (ifi->ifi_addr != NULL) {
      free(ifi->ifi_addr);
    }
    if (ifi->ifi_brdaddr != NULL) {
      free(ifi->ifi_brdaddr);
    }
    if (ifi->ifi_dstaddr != NULL) {
      free(ifi->ifi_dstaddr);
    }
    ifinext = ifi->ifi_next;	/* can't fetch ifi_next after free() */
    free(ifi);			/* the ifi_info{} itself */
  }
}

/******************************************************/
JNIEXPORT void JNICALL Java_EtNative_findConstants
  (JNIEnv *env, jclass class_this)
{
	
  /* java declarations */
  jstring   utf_string;
  jfieldID  fid;
  jclass    class_con;
  
  /* find EtConstants class */
  class_con  = (*env)->FindClass(env, "EtConstants");
  
  /* set Strings */
  fid = (*env)->GetStaticFieldID(env, class_con,
				"ET_MULTICAST_ADDR", "Ljava/lang/String;");
  utf_string = (*env)->NewStringUTF(env, ET_MULTICAST_ADDR);
  (*env)->SetStaticObjectField(env, class_con, fid, utf_string);
  (*env)->DeleteLocalRef(env, utf_string);
  
  fid = (*env)->GetStaticFieldID(env, class_con,
				"ET_HOST_LOCAL", "Ljava/lang/String;");
  utf_string = (*env)->NewStringUTF(env, ET_HOST_LOCAL);
  (*env)->SetStaticObjectField(env, class_con, fid, utf_string);
  (*env)->DeleteLocalRef(env, utf_string);
  
  fid = (*env)->GetStaticFieldID(env, class_con,
				"ET_HOST_REMOTE", "Ljava/lang/String;");
  utf_string = (*env)->NewStringUTF(env, ET_HOST_REMOTE);
  (*env)->SetStaticObjectField(env, class_con, fid, utf_string);
  (*env)->DeleteLocalRef(env, utf_string);
  
  fid = (*env)->GetStaticFieldID(env, class_con,
				"ET_HOST_ANYWHERE", "Ljava/lang/String;");
  utf_string = (*env)->NewStringUTF(env, ET_HOST_ANYWHERE);
  (*env)->SetStaticObjectField(env, class_con, fid, utf_string);
  (*env)->DeleteLocalRef(env, utf_string);
  
  /* set integers */
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_MULTICAST", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_MULTICAST);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_BROADCAST", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_BROADCAST);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_DIRECT", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_DIRECT);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_BROADANDMULTICAST", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_BROADANDMULTICAST);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_BROADCAST_PORT", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_BROADCAST_PORT);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_MULTICAST_PORT", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_MULTICAST_PORT);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_SERVER_PORT", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_SERVER_PORT);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_MULTICAST_TTL", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_MULTICAST_TTL);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_STATION_SELECT_INTS", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_STATION_SELECT_INTS);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_FILENAME_LENGTH", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_FILENAME_LENGTH);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_FUNCNAME_LENGTH", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_FUNCNAME_LENGTH);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_STATNAME_LENGTH", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_STATNAME_LENGTH);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_VERSION", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_VERSION);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_MINORVERSION", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_MINORVERSION);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_IPADDRSTRLEN", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_IPADDRSTRLEN);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_MAXHOSTNAMELEN", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_MAXHOSTNAMELEN);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_ATTACHMENTS_MAX", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_ATTACHMENTS_MAX);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_NET_SYS_DATA", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_NET_SYS_DATA);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_NET_SYS_HIST", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_NET_SYS_HIST);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_MUTEX_UNLOCKED", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_MUTEX_UNLOCKED);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_MUTEX_LOCKED", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_MUTEX_LOCKED);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_MUTEX_SHARE", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_MUTEX_SHARE);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_MUTEX_NOSHARE", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_MUTEX_NOSHARE);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_ATT_CONTINUE", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_ATT_CONTINUE);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_ATT_QUIT", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_ATT_QUIT);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_ATT_UNBLOCKED", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_ATT_UNBLOCKED);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_ATT_BLOCKED", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_ATT_BLOCKED);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_ENDIAN_BIG", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_ENDIAN_BIG);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_ENDIAN_LITTLE", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_ENDIAN_LITTLE);
  
  /* set error codes */
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_OK", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_OK);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_ERROR", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_ERROR);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_ERROR_TOOMANY", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_ERROR_TOOMANY);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_ERROR_EXISTS", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_ERROR_EXISTS);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_ERROR_WAKEUP", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_ERROR_WAKEUP);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_ERROR_TIMEOUT", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_ERROR_TIMEOUT);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_ERROR_EMPTY", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_ERROR_EMPTY);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_ERROR_BUSY", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_ERROR_BUSY);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_ERROR_DEAD", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_ERROR_DEAD);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_ERROR_READ", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_ERROR_READ);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_ERROR_WRITE", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_ERROR_WRITE);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_ERROR_REMOTE", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_ERROR_REMOTE);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_ERROR_NOREMOTE", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_ERROR_NOREMOTE);
  
  /* station stuff */
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_STATION_UNUSED", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_STATION_UNUSED);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_STATION_CREATING", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_STATION_CREATING);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_STATION_IDLE", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_STATION_IDLE);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_STATION_ACTIVE", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_STATION_ACTIVE);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_STATION_USER_MULTI", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_STATION_USER_MULTI);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_STATION_USER_SINGLE", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_STATION_USER_SINGLE);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_STATION_NONBLOCKING", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_STATION_NONBLOCKING);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_STATION_BLOCKING", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_STATION_BLOCKING);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_STATION_SELECT_ALL", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_STATION_SELECT_ALL);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_STATION_SELECT_MATCH", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_STATION_SELECT_MATCH);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_STATION_SELECT_USER", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_STATION_SELECT_USER);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_STATION_RESTORE_OUT", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_STATION_RESTORE_OUT);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_STATION_RESTORE_IN", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_STATION_RESTORE_IN);
  
  fid = (*env)->GetStaticFieldID(env, class_con, "ET_STATION_RESTORE_GC", "I");
  (*env)->SetStaticIntField(env, class_con, fid, ET_STATION_RESTORE_GC);
  
  return;  
}







