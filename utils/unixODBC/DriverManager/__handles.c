/*********************************************************************
 *
 * This is based on code created by Peter Harvey,
 * (pharvey@codebydesign.com).
 *
 * Modified and extended by Nick Gorham
 * (nick@easysoft.com).
 *
 * Any bugs or problems should be considered the fault of Nick and not
 * Peter.
 *
 * copyright (c) 1999 Nick Gorham
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 **********************************************************************
 *
 * $Id: __handles.c,v 1.1.1.1 2004/02/18 20:37:42 dave Exp $
 *
 * $Log: __handles.c,v $
 * Revision 1.1.1.1  2004/02/18 20:37:42  dave
 * first import of unixODBC
 *
 * Revision 1.6  2003/06/04 12:49:45  lurcher
 *
 * Further PID logging tweeks
 *
 * Revision 1.5  2003/06/02 16:51:36  lurcher
 *
 * Add TracePid option
 *
 * Revision 1.4  2002/08/12 16:20:44  lurcher
 *
 * Make it try and find a working iconv set of encodings
 *
 * Revision 1.3  2002/08/12 13:17:52  lurcher
 *
 * Replicate the way the MS DM handles loading of driver libs, and allocating
 * handles in the driver. usage counting in the driver means that dlopen is
 * only called for the first use, and dlclose for the last. AllocHandle for
 * the driver environment is only called for the first time per driver
 * per application environment.
 *
 * Revision 1.2  2002/02/22 10:23:22  lurcher
 *
 * s/Trace File/TraceFile
 *
 * Revision 1.1.1.1  2001/10/17 16:40:07  lurcher
 *
 * First upload to SourceForge
 *
 * Revision 1.14  2001/06/25 12:55:15  nick
 *
 * Fix threading problem with multiple ENV's
 *
 * Revision 1.13  2001/06/04 15:24:49  nick
 *
 * Add port to MAC OSX and QT3 changes
 *
 * Revision 1.12  2001/05/15 13:33:44  jason
 *
 * Wrapped calls to stats with COLLECT_STATS
 *
 * Revision 1.11  2001/04/12 17:43:36  nick
 *
 * Change logging and added autotest to odbctest
 *
 * Revision 1.10  2001/03/02 14:24:23  nick
 *
 * Fix thread detection for Solaris
 *
 * Revision 1.9  2001/01/04 13:16:25  nick
 *
 * Add support for GNU portable threads and tidy up some UNICODE compile
 * warnings
 *
 * Revision 1.8  2000/12/18 11:51:59  martin
 *
 * stats specific mode to uodbc_open_stats.
 *
 * Revision 1.7  2000/12/18 11:03:58  martin
 *
 * Add support for the collection and retrieval of handle statistics.
 *
 * Revision 1.6  2000/12/17 11:17:22  nick
 *
 * Remove typo
 *
 * Revision 1.5  2000/12/17 11:00:32  nick
 *
 * Add thread safe bits to pooling
 *
 * Revision 1.4  2000/11/29 17:53:59  nick
 *
 * Fix race condition
 *
 * Revision 1.3  2000/10/25 09:39:42  nick
 *
 * Clear handles out, to avoid reuse
 *
 * Revision 1.2  2000/09/08 08:58:17  nick
 *
 * Add SQL_DRIVER_HDESC to SQLGetinfo
 *
 * Revision 1.1.1.1  2000/09/04 16:42:52  nick
 * Imported Sources
 *
 * Revision 1.16  2000/06/29 17:27:52  ngorham
 *
 * Add fast validate option
 *
 * Revision 1.15  2000/06/27 17:34:12  ngorham
 *
 * Fix a problem when the second part of the connect failed a seg fault
 * was generated in the error reporting
 *
 * Revision 1.14  2001/03/28 23:09:57  ngorham
 *
 * Fix logging
 *
 * Revision 1.13  2000/03/11 15:55:47  ngorham
 *
 * A few more changes and bug fixes (see NEWS)
 *
 * Revision 1.12  2000/02/25 00:02:00  ngorham
 *
 * Add a patch to support IBM DB2, and Solaris threads
 *
 * Revision 1.11  2000/02/22 22:14:45  ngorham
 *
 * Added support for solaris threads
 * Added check to overcome bug in PHP4
 * Fixed bug in descriptors and ODBC 3 drivers
 *
 * Revision 1.10  1999/12/11 13:01:57  ngorham
 *
 * Add some fixes to the Postgres driver for long types
 *
 * Revision 1.9  1999/12/01 09:20:07  ngorham
 *
 * Fix some threading problems
 *
 * Revision 1.8  1999/11/13 23:41:01  ngorham
 *
 * Alter the way DM logging works
 * Upgrade the Postgres driver to 6.4.6
 *
 * Revision 1.7  1999/11/10 03:51:34  ngorham
 *
 * Update the error reporting in the DM to enable ODBC 3 and 2 calls to
 * work at the same time
 *
 * Revision 1.6  1999/08/05 18:59:49  ngorham
 *
 * Typo error found by Greg Bentz
 *
 * Revision 1.5  1999/08/03 21:47:39  shandyb
 * Moving to automake: changed files in DriverManager
 *
 * Revision 1.4  1999/07/10 21:10:17  ngorham
 *
 * Adjust error sqlstate from driver manager, depending on requested
 * version (ODBC2/3)
 *
 * Revision 1.3  1999/07/04 21:05:08  ngorham
 *
 * Add LGPL Headers to code
 *
 * Revision 1.2  1999/06/30 23:56:56  ngorham
 *
 * Add initial thread safety code
 *
 * Revision 1.1.1.1  1999/05/29 13:41:09  sShandyb
 * first go at it
 *
 * Revision 1.1.1.1  1999/05/27 18:23:18  pharvey
 * Imported sources
 *
 * Revision 1.3  1999/05/09 23:27:11  nick
 * All the API done now
 *
 * Revision 1.2  1999/05/03 19:50:43  nick
 * Another check point
 *
 * Revision 1.1  1999/04/25 23:06:11  nick
 * Initial revision
 *
 *
 **********************************************************************/

#include <ctype.h>
#include "drivermanager.h"
#if defined ( COLLECT_STATS ) && defined( HAVE_SYS_SEM_H )
#include "__stats.h"
#include <uodbc_stats.h>
#endif

static char const rcsid[]= "$RCSfile: __handles.c,v $ $Revision: 1.1.1.1 $";

/*
 * these are used to enable us to check if a handle is
 * valid without the danger of a seg-vio.
 */

static DMHENV enviroment_root;
static DMHDBC connection_root;
static DMHSTMT statement_root;
static DMHDESC descriptor_root;


/*
 * use just one mutex for all the lists, this avoids any issues
 * with deadlocks, the performance issue should be minimal, if it
 * turns out to be a problem, we can readdress this
 *
 * We also have a mutex to protect the connection pooling code
 *
 * If compiled with thread support the DM allows four different
 * thread strategies
 *
 * Level 0 - Only the DM internal structures are protected
 * the driver is assumed to take care of it's self
 *
 * Level 1 - The driver is protected down to the statement level
 * each statement will be protected, and the same for the connect 
 * level for connect functions, note that descriptors are considered
 * equal to statements when it comes to thread protection.
 *
 * Level 2 - The driver is protected at the connection level. only
 * one thread can be in a particular driver at one time
 *
 * Level 3 - The driver is protected at the env level, only one thing
 * at a time.
 *
 * By default the driver open connections with a lock level of 3, 
 * this can be changed by adding the line
 *
 * Threading = N
 *
 * to the driver entry in odbcinst.ini, where N is the locking level 
 * (0-3)
 * 
 */

#ifdef HAVE_LIBPTH

#include <pth.h>

static pth_mutex_t mutex_lists = PTH_MUTEX_INIT;
static pth_mutex_t mutex_env = PTH_MUTEX_INIT;
static pth_mutex_t mutex_pool = PTH_MUTEX_INIT;
static int pth_init_called = 0;

static int mutex_entry( pth_mutex_t *mutex )
{
    if ( !pth_init_called )
    {
        pth_init();
        pth_init_called = 1;
    }
    return pth_mutex_acquire( mutex, 0, NULL );
}

static int mutex_exit( pth_mutex_t *mutex )
{
    return pth_mutex_release( mutex );
}

#elif HAVE_LIBPTHREAD

#include <pthread.h>

static pthread_mutex_t mutex_lists = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mutex_env = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mutex_pool = PTHREAD_MUTEX_INITIALIZER;

static int mutex_entry( pthread_mutex_t *mutex )
{
    return pthread_mutex_lock( mutex );
}

static int mutex_exit( pthread_mutex_t *mutex )
{
    return pthread_mutex_unlock( mutex );
}

#elif HAVE_LIBTHREAD

#include <thread.h>

static mutex_t mutex_lists;
static mutex_t mutex_env;
static mutex_t mutex_pool;

static int mutex_entry( mutex_t *mutex )
{
    return mutex_lock( mutex );
}

static int mutex_exit( mutex_t *mutex )
{
    return mutex_unlock( mutex );
}

#else

#define mutex_entry(x)
#define mutex_exit(x)

#endif

/*
 * protection for connection pooling
 */

void mutex_pool_entry( void )
{
    mutex_entry( &mutex_pool );
}

void mutex_pool_exit( void )
{
    mutex_exit( &mutex_pool );
}

/*
 * protection for lib loading and counting, reuse the lists mutex as this 
 * is the lowest level protection the DM uses
 */

void mutex_lib_entry( void )
{
    mutex_entry( &mutex_lists );
}

void mutex_lib_exit( void )
{
    mutex_exit( &mutex_lists );
}

/*
 * allocate and register a environment handle
 */

DMHENV __alloc_env( void )
{
    DMHENV environment = NULL;
    char s0[ 20 ];

    mutex_entry( &mutex_lists );

    environment = calloc( sizeof( *environment ), 1 );

    if ( environment )
    {
        char tracing_string[ 64 ];
        char tracing_file[ 64 ];

#if defined ( COLLECT_STATS ) && defined( HAVE_SYS_SEM_H )
        if (uodbc_open_stats(&environment->sh, UODBC_STATS_WRITE) != 0)
        {
            ;
        }
        uodbc_update_stats(environment->sh, UODBC_STATS_TYPE_HENV, (void *)1);
#endif
        
        /*
         * add to list of env handles
         */

        environment -> next_class_list = enviroment_root;
        enviroment_root = environment;
        environment -> type = HENV_MAGIC;

        SQLGetPrivateProfileString( "ODBC", "Trace", "No",
                    tracing_string, sizeof( tracing_string ), 
                    "odbcinst.ini" );

        if ( tracing_string[ 0 ] == '1' ||
                toupper( tracing_string[ 0 ] ) == 'Y' ||
                    ( toupper( tracing_string[ 0 ] ) == 'O' &&
                    toupper( tracing_string[ 1 ] ) == 'N' ))
        {
            SQLGetPrivateProfileString( "ODBC", "TraceFile", "/tmp/sql.log",
                    tracing_file, sizeof( tracing_file ), 
                    "odbcinst.ini" );

            /*
             * start logging
             */

            SQLGetPrivateProfileString( "ODBC", "TracePid", "No",
                    tracing_string, sizeof( tracing_string ), 
                    "odbcinst.ini" );

            if ( tracing_string[ 0 ] == '1' ||
                        toupper( tracing_string[ 0 ] ) == 'Y' ||
                        ( toupper( tracing_string[ 0 ] ) == 'O' &&
                        toupper( tracing_string[ 1 ] ) == 'N' ))
            {
                dm_log_open( "ODBC", tracing_file, 1 );
            }
            else
            {
                dm_log_open( "ODBC", tracing_file, 0 );
            }

            sprintf( environment -> msg,
                    "\n\t\tExit:[SQL_SUCCESS]\n\t\t\tEnvironment = %p",	environment );

            dm_log_write( __FILE__,
                    __LINE__,
                    LOG_INFO,
                    LOG_INFO, environment -> msg );
        }
    }

    setup_error_head( &environment -> error, environment,
            SQL_HANDLE_ENV );

    mutex_exit( &mutex_lists );

    return environment;
}

/*
 * check that a env is real
 */

int __validate_env( DMHENV env )
{
#ifdef FAST_HANDLE_VALIDATE

    if ( *(( int * ) env ) == HENV_MAGIC )
        return 1;
    else
        return 0;
#else

    DMHENV ptr;
    int ret = 0;

    mutex_entry( &mutex_lists );

    ptr = enviroment_root;

    while( ptr )
    {
        if ( ptr == env )
        {
            ret = 1;
            break;
        }

        ptr = ptr -> next_class_list;
    }

    mutex_exit( &mutex_lists );

    return ret;

#endif
}

/*
 * remove from list
 */

void __release_env( DMHENV environment )
{
    DMHENV last = NULL;
    DMHENV ptr;

    mutex_entry( &mutex_lists );

    ptr = enviroment_root;

    while( ptr )
    {
        if ( environment == ptr )
        {
            break;
        }
        last = ptr;
        ptr = ptr -> next_class_list;
    }

    if ( ptr )
    {
        if ( last )
        {
            last -> next_class_list = ptr -> next_class_list;
        }
        else
        {
            enviroment_root = ptr -> next_class_list;
        }
    }

    clear_error_head( &environment -> error );

	/*
	 * free log
	 */

    dm_log_close();

#if defined ( COLLECT_STATS ) && defined( HAVE_SYS_SEM_H )
    if (environment->sh)
        uodbc_close_stats(environment->sh);
#endif
    
    /*
     * clear just to make sure
     */

    memset( environment, 0, sizeof( *environment ));

    free( environment );

    mutex_exit( &mutex_lists );
}

/*
 * get the root, for use in SQLEndTran and SQLTransact
 */

DMHDBC __get_dbc_root( void )
{
    return connection_root;
}

/*
 * allocate and register a connection handle
 */

DMHDBC __alloc_dbc( void )
{
    DMHDBC connection = NULL;

    mutex_entry( &mutex_lists );

    connection = calloc( sizeof( *connection ), 1 );

    if ( connection )
    {
        /*
         * add to list of connection handles
         */

        connection -> next_class_list = connection_root;
        connection_root = connection;
        connection -> type = HDBC_MAGIC;
    }

    setup_error_head( &connection -> error, connection,
            SQL_HANDLE_DBC );

#ifdef HAVE_LIBPTH
    pth_mutex_init( &connection -> mutex );
    /*
     * for the moment protect on a environment level
     */
    connection -> protection_level = TS_LEVEL3;
#elif HAVE_LIBPTHREAD
    pthread_mutex_init( &connection -> mutex, NULL );
    /*
     * for the moment protect on a environment level
     */
    connection -> protection_level = TS_LEVEL3;
#elif HAVE_LIBTHREAD
    mutex_init( &connection -> mutex, USYNC_THREAD, NULL );
    connection -> protection_level = TS_LEVEL3;
#endif

    mutex_exit( &mutex_lists );

    return connection;
}

/*
 * adjust the threading level
 */

void dbc_change_thread_support( DMHDBC connection, int level )
{
#if defined ( HAVE_LIBPTHREAD ) || defined( HAVE_LIBTHREAD ) || defined( HAVE_LIBPTH )

    if ( connection -> protection_level == level )
        return;

    mutex_entry( &mutex_lists );

    /*
     * switch level
     * If the previous level was at less than connection level,
     * we need to create a lock at the environment level then release
     * the connection lock.
     * 
     * If we are moving from a greater than env lock, the current lock
     * on the connection will be ok
     */ 

    if ( level == TS_LEVEL3 )
    {
        mutex_entry( &mutex_env );
        mutex_exit( &connection -> mutex );
    }
    else if ( connection -> protection_level == TS_LEVEL3 )
    {
        /*
         * if we are moving from level 3 we have to create the new
         * connection lock, and remove the env lock
         */
        mutex_entry( &connection -> mutex );
        mutex_exit( &mutex_env );
    }
    connection -> protection_level = level;

    mutex_exit( &mutex_lists );
                        
#endif
}

/*
 * check that a connection is real
 */

int __validate_dbc( DMHDBC connection )
{
#ifdef FAST_HANDLE_VALIDATE
    if ( *(( int * ) connection ) == HDBC_MAGIC )
        return 1;
    else
        return 0;

#else

    DMHDBC ptr;
    int ret = 0;

    mutex_entry( &mutex_lists );

    ptr = connection_root;

    while( ptr )
    {
        if ( ptr == connection )
        {
            ret = 1;
            break;
        }

        ptr = ptr -> next_class_list;
    }

    mutex_exit( &mutex_lists );

    return ret;
#endif
}

/*
 * remove from list
 */

void __release_dbc( DMHDBC connection )
{
    DMHDBC last = NULL;
    DMHDBC ptr;

    mutex_entry( &mutex_lists );

    ptr = connection_root;

    while( ptr )
    {
        if ( connection == ptr )
        {
            break;
        }
        last = ptr;
        ptr = ptr -> next_class_list;
    }

    if ( ptr )
    {
        if ( last )
        {
            last -> next_class_list = ptr -> next_class_list;
        }
        else
        {
            connection_root = ptr -> next_class_list;
        }
    }

    clear_error_head( &connection -> error );

#ifdef HAVE_LIBPTH
#elif HAVE_LIBPTHREAD
    pthread_mutex_destroy( &connection -> mutex );
#elif HAVE_LIBTHREAD
    mutex_destroy( &connection -> mutex );
#endif

    /*
     * clear just to make sure
     */

    memset( connection, 0, sizeof( *connection ));

    free( connection );

    mutex_exit( &mutex_lists );
}

/*
 *  * get the statement root, for use in SQLEndTran and SQLTransact
 *   */

DMHSTMT __get_stmt_root( void )
{
    return statement_root;
}

/*
 * allocate and register a statement handle
 */

DMHSTMT __alloc_stmt( void )
{
    DMHSTMT statement = NULL;

    mutex_entry( &mutex_lists );

    statement = calloc( sizeof( *statement ), 1 );

    if ( statement )
    {
        /*
         * add to list of statement handles
         */

        statement -> next_class_list = statement_root;
        statement_root = statement;
        statement -> type = HSTMT_MAGIC;
    }

    setup_error_head( &statement -> error, statement,
            SQL_HANDLE_STMT );

#ifdef HAVE_LIBPTH
    pth_mutex_init( &statement -> mutex );
#elif HAVE_LIBPTHREAD
    pthread_mutex_init( &statement -> mutex, NULL );
#elif HAVE_LIBTHREAD
    mutex_init( &statement -> mutex, USYNC_THREAD, NULL );
#endif

    mutex_exit( &mutex_lists );

    return statement;
}

/*
 * clear all statements on a DBC
 */

int __clean_stmt_from_dbc( DMHDBC connection )
{
    DMHSTMT ptr, last;
    int ret = 0;

    mutex_entry( &mutex_lists );
    last = NULL;
    ptr = statement_root;

    while( ptr )
    {
        if ( ptr -> connection == connection )
        {
            if ( last )
            {
                last -> next_class_list = ptr -> next_class_list;
            }
            else
            {
                statement_root = ptr -> next_class_list;
            }
            clear_error_head( &ptr -> error );

#ifdef HAVE_LIBPTH
#elif HAVE_LIBPTHREAD
            pthread_mutex_destroy( &ptr -> mutex );
#elif HAVE_LIBTHREAD
            mutex_destroy( &ptr -> mutex );
#endif
            free( ptr );

            /*
             * go back to the start
             */

            last = NULL;
            ptr = statement_root;
        }
        else
        {
            last = ptr;
            ptr = ptr -> next_class_list;
        }
    }

    mutex_exit( &mutex_lists );

    return ret;
}

/*
 * check that a statement is real
 */

int __validate_stmt( DMHSTMT statement )
{
#ifdef FAST_HANDLE_VALIDATE

    if ( *(( int * ) statement ) == HSTMT_MAGIC )
        return 1;
    else
        return 0;

#else

    DMHSTMT ptr;
    int ret = 0;

    mutex_entry( &mutex_lists );

    ptr = statement_root;

    while( ptr )
    {
        if ( ptr == statement )
        {
            ret = 1;
            break;
        }

        ptr = ptr -> next_class_list;
    }

    mutex_exit( &mutex_lists );

    return ret;

#endif
}

/*
 * remove from list
 */

void __release_stmt( DMHSTMT statement )
{
    DMHSTMT last = NULL;
    DMHSTMT ptr;

    mutex_entry( &mutex_lists );

    ptr = statement_root;

    while( ptr )
    {
        if ( statement == ptr )
        {
            break;
        }
        last = ptr;
        ptr = ptr -> next_class_list;
    }

    if ( ptr )
    {
        if ( last )
        {
            last -> next_class_list = ptr -> next_class_list;
        }
        else
        {
            statement_root = ptr -> next_class_list;
        }
    }

    clear_error_head( &statement -> error );

#ifdef HAVE_LIBPTH
#elif HAVE_LIBPTHREAD
    pthread_mutex_destroy( &statement -> mutex );
#elif HAVE_LIBTHREAD
    mutex_destroy( &statement -> mutex );
#endif

    /*
     * clear just to make sure
     */

    memset( statement, 0, sizeof( *statement ));

    free( statement );

    mutex_exit( &mutex_lists );
}

/*
 * allocate and register a descriptor handle
 */

DMHDESC __alloc_desc( void )
{
    DMHDESC descriptor = NULL;

    mutex_entry( &mutex_lists );

    descriptor = calloc( sizeof( *descriptor ), 1 );

    if ( descriptor )
    {
        /*
         * add to list of descriptor handles
         */

        descriptor -> next_class_list = descriptor_root;
        descriptor_root = descriptor;
        descriptor -> type = HDESC_MAGIC;
    }

    setup_error_head( &descriptor -> error, descriptor,
            SQL_HANDLE_DESC );

#ifdef HAVE_LIBPTH
    pth_mutex_init( &descriptor -> mutex );
#elif HAVE_LIBPTHREAD
    pthread_mutex_init( &descriptor -> mutex, NULL );
#elif HAVE_LIBTHREAD
    mutex_init( &descriptor -> mutex, USYNC_THREAD, NULL );
#endif

    mutex_exit( &mutex_lists );

    return descriptor;
}

/*
 * check that a descriptor is real
 */

int __validate_desc( DMHDESC descriptor )
{
#ifdef FAST_HANDLE_VALIDATE

    if ( *(( int * ) descriptor ) == HDESC_MAGIC )
        return 1;
    else
        return 0;

#else

    DMHDESC ptr;
    int ret = 0;

    mutex_entry( &mutex_lists );

    ptr = descriptor_root;

    while( ptr )
    {
        if ( ptr == descriptor )
        {
            ret = 1;
            break;
        }

        ptr = ptr -> next_class_list;
    }

    mutex_exit( &mutex_lists );

    return ret;

#endif
}

/*
 * clear all descriptors on a DBC
 */

int __clean_desc_from_dbc( DMHDBC connection )
{
    DMHDESC ptr, last;
    int ret = 0;

    mutex_entry( &mutex_lists );
    last = NULL;
    ptr = descriptor_root;

    while( ptr )
    {
        if ( ptr -> connection == connection )
        {
            if ( last )
            {
                last -> next_class_list = ptr -> next_class_list;
            }
            else
            {
                descriptor_root = ptr -> next_class_list;
            }
            clear_error_head( &ptr -> error );

#ifdef HAVE_LIBPTH
#elif HAVE_LIBPTHREAD
            pthread_mutex_destroy( &ptr -> mutex );
#elif HAVE_LIBTHREAD
            mutex_destroy( &ptr -> mutex );
#endif
            free( ptr );

            /*
             * go back to the start
             */

            last = NULL;
            ptr = descriptor_root;
        }
        else
        {
            last = ptr;
            ptr = ptr -> next_class_list;
        }
    }

    mutex_exit( &mutex_lists );

    return ret;
}


/*
 * remove from list
 */

void __release_desc( DMHDESC descriptor )
{
    DMHDESC last = NULL;
    DMHDESC ptr;

    mutex_entry( &mutex_lists );

    ptr = descriptor_root;

    while( ptr )
    {
        if ( descriptor == ptr )
        {
            break;
        }
        last = ptr;
        ptr = ptr -> next_class_list;
    }

    if ( ptr )
    {
        if ( last )
        {
            last -> next_class_list = ptr -> next_class_list;
        }
        else
        {
            descriptor_root = ptr -> next_class_list;
        }
    }

    clear_error_head( &descriptor -> error );

#ifdef HAVE_LIBPTHREAD
#elif HAVE_LIBPTHREAD
    pthread_mutex_destroy( &descriptor -> mutex );
#elif HAVE_LIBTHREAD
    mutex_destroy( &descriptor -> mutex );
#endif

    /*
     * clear just to make sure
     */

    memset( descriptor, 0, sizeof( *descriptor ));

    free( descriptor );

    mutex_exit( &mutex_lists );
}

#if defined ( HAVE_LIBPTHREAD ) || defined ( HAVE_LIBTHREAD ) || defined( HAVE_LIBPTH )

void thread_protect( int type, void *handle )
{
    DMHENV environment;
    DMHDBC connection;
    DMHSTMT statement;
    DMHDESC descriptor;

    switch( type )
    {
      case SQL_HANDLE_ENV:
        mutex_entry( &mutex_env );
        break;

      case SQL_HANDLE_DBC:
        connection = handle;
        if ( connection -> protection_level == TS_LEVEL3 )
        {
            mutex_entry( &mutex_env );
        }
        else if ( connection -> protection_level == TS_LEVEL2 ||
                connection -> protection_level == TS_LEVEL1 )
        {
            mutex_entry( &connection -> mutex );
        }
        break;

      case SQL_HANDLE_STMT:
        statement = handle;
        if ( statement -> connection -> protection_level == TS_LEVEL3 )
        {
            mutex_entry( &mutex_env );
        }
        else if ( statement -> connection -> protection_level == TS_LEVEL2 )
        {
            mutex_entry( &statement -> connection -> mutex );
        }
        else if ( statement -> connection -> protection_level == TS_LEVEL1 )
        {
            mutex_entry( &statement -> mutex );
        }
        break;

      case SQL_HANDLE_DESC:
        descriptor = handle;
        if ( descriptor -> connection -> protection_level == TS_LEVEL3 )
        {
            mutex_entry( &mutex_env );
        }
        if ( descriptor -> connection -> protection_level == TS_LEVEL2 )
        {
            mutex_entry( &descriptor -> connection -> mutex );
        }
        if ( descriptor -> connection -> protection_level == TS_LEVEL1 )
        {
            mutex_entry( &descriptor -> mutex );
        }
        break;
    }
}

void thread_release( int type, void *handle )
{
    DMHENV environment;
    DMHDBC connection;
    DMHSTMT statement;
    DMHDESC descriptor;

    switch( type )
    {
      case SQL_HANDLE_ENV:
        mutex_exit( &mutex_env );
        break;

      case SQL_HANDLE_DBC:
        connection = handle;
        if ( connection -> protection_level == TS_LEVEL3 )
        {
            mutex_exit( &mutex_env );
        }
        else if ( connection -> protection_level == TS_LEVEL2 ||
                connection -> protection_level == TS_LEVEL1 )
        {
            mutex_exit( &connection -> mutex );
        }
        break;

      case SQL_HANDLE_STMT:
        statement = handle;
        if ( statement -> connection -> protection_level == TS_LEVEL3 )
        {
            mutex_exit( &mutex_env );
        }
        else if ( statement -> connection -> protection_level == TS_LEVEL2 )
        {
            mutex_exit( &statement -> connection -> mutex );
        }
        else if ( statement -> connection -> protection_level == TS_LEVEL1 )
        {
            mutex_exit( &statement -> mutex );
        }
        break;

      case SQL_HANDLE_DESC:
        descriptor = handle;
        if ( descriptor -> connection -> protection_level == TS_LEVEL3 )
        {
            mutex_exit( &mutex_env );
        }
        else if ( descriptor -> connection -> protection_level == TS_LEVEL2 )
        {
            mutex_exit( &descriptor -> connection -> mutex );
        }
        else if ( descriptor -> connection -> protection_level == TS_LEVEL1 )
        {
            mutex_exit( &descriptor -> mutex );
        }
        break;
    }
}

#endif
