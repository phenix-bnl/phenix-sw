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
 * $Id: SQLErrorW.c,v 1.1.1.1 2004/02/18 20:37:42 dave Exp $
 *
 * $Log: SQLErrorW.c,v $
 * Revision 1.1.1.1  2004/02/18 20:37:42  dave
 * first import of unixODBC
 *
 * Revision 1.6  2002/12/05 17:44:30  lurcher
 *
 * Display unknown return values in return logging
 *
 * Revision 1.5  2002/07/25 09:30:26  lurcher
 *
 * Additional unicode and iconv changes
 *
 * Revision 1.4  2002/07/24 08:49:52  lurcher
 *
 * Alter UNICODE support to use iconv for UNICODE-ANSI conversion
 *
 * Revision 1.3  2002/05/21 14:19:44  lurcher
 *
 * * Update libtool to escape from AIX build problem
 * * Add fix to avoid file handle limitations
 * * Add more UNICODE changes, it looks like it is native 16 representation
 *   the old way can be reproduced by defining UCS16BE
 * * Add iusql, its just the same as isql but uses the wide functions
 *
 * Revision 1.2  2001/12/13 13:00:32  lurcher
 *
 * Remove most if not all warnings on 64 bit platforms
 * Add support for new MS 3.52 64 bit changes
 * Add override to disable the stopping of tracing
 * Add MAX_ROWS support in postgres driver
 *
 * Revision 1.1.1.1  2001/10/17 16:40:05  lurcher
 *
 * First upload to SourceForge
 *
 * Revision 1.3  2001/07/03 09:30:41  nick
 *
 * Add ability to alter size of displayed message in the log
 *
 * Revision 1.2  2001/04/12 17:43:36  nick
 *
 * Change logging and added autotest to odbctest
 *
 * Revision 1.1  2000/12/31 20:30:54  nick
 *
 * Add UNICODE support
 *
 *
 **********************************************************************/

#include "drivermanager.h"

static char const rcsid[]= "$RCSfile: SQLErrorW.c,v $";

/*
 * unicode mapping function
 */

static SQLRETURN extract_sql_error_w( EHEAD *head,
        SQLWCHAR *sqlstate,
        SQLINTEGER *native_error,
        SQLWCHAR *message_text,
        SQLSMALLINT buffer_length,
        SQLSMALLINT *text_length )
{
    ERROR *err;
    SQLRETURN ret;

    if ( sqlstate )
    {
        SQLWCHAR *tmp;

        tmp = ansi_to_unicode_alloc((SQLCHAR*) "00000", SQL_NTS,  __get_connection( head ));
        wide_strcpy( sqlstate, tmp );
        free( tmp );
    }

    if ( head -> sql_error_head.error_count < 1 )
    {
        return SQL_NO_DATA;
    }

    err = head -> sql_error_head.error_list_head;
    head -> sql_error_head.error_list_head = err -> next;

    /*
     * is it the last
     */
    if ( head -> sql_error_head.error_list_tail == err )
        head -> sql_error_head.error_list_tail = NULL;

    /*
     * not empty yet
     */
    if ( head -> sql_error_head.error_list_head )
    {
        head -> sql_error_head.error_list_head -> prev = NULL;
    }

    head -> sql_error_head.error_count --;

    if ( sqlstate )
    {
        wide_strcpy( sqlstate, err -> sqlstate );
    }
    if ( buffer_length < wide_strlen( err -> msg ) + 1 )
    {
        ret = SQL_SUCCESS_WITH_INFO;
    }
    else
    {
        ret = SQL_SUCCESS;
    }

    if ( message_text )
    {
        if ( ret == SQL_SUCCESS )
        {
            wide_strcpy( message_text, err -> msg );
        }
        else
        {
            memcpy( message_text, err -> msg, buffer_length * 2 );
            message_text[ buffer_length - 1 ] = 0;
        }
    }

    if ( text_length )
    {
        *text_length = wide_strlen( err -> msg );
    }

	if ( native_error )
	{
		*native_error = err -> native_error;
	}

    /*
     * clean up
     */

    free( err -> msg );
    free( err );

    /*
     * map 3 to 2 if required
     */

    if ( SQL_SUCCEEDED( ret ) && sqlstate )
        __map_error_state_w( sqlstate, __get_version( head ));

    return ret;
}

SQLRETURN SQLErrorW( SQLHENV environment_handle,
           SQLHDBC connection_handle,
           SQLHSTMT statement_handle,
           SQLWCHAR *sqlstate,
           SQLINTEGER *native_error,
           SQLWCHAR *message_text,
           SQLSMALLINT buffer_length,
           SQLSMALLINT *text_length )
{
    SQLRETURN ret;
    SQLCHAR s0[ 32 ], s1[ 100 + LOG_MESSAGE_LEN ];
    SQLCHAR s2[ 100 + LOG_MESSAGE_LEN ];

    if ( statement_handle )
    {
        DMHSTMT statement = ( DMHSTMT ) statement_handle;

        if ( !__validate_stmt( statement ))
        {
            dm_log_write( __FILE__, 
                    __LINE__, 
                    LOG_INFO, 
                    LOG_INFO, 
                    "Error: SQL_INVALID_HANDLE" );

            return SQL_INVALID_HANDLE;
        }

        thread_protect( SQL_HANDLE_STMT, statement );

        if ( log_info.log_flag )
        {
            sprintf( statement -> msg, 
                "\n\t\tEntry:\
                \n\t\t\tStatement = %p\
                \n\t\t\tSQLState = %p\
                \n\t\t\tNative = %p\
                \n\t\t\tMessage Text = %p\
                \n\t\t\tBuffer Length = %d\
                \n\t\t\tText Len Ptr = %p",
                    statement,
                    sqlstate,
                    native_error,
                    message_text,
                    buffer_length,
                    text_length );

            dm_log_write( __FILE__, 
                    __LINE__, 
                    LOG_INFO, 
                    LOG_INFO, 
                    statement -> msg );
        }

        ret = extract_sql_error_w( &statement -> error,
                sqlstate,
                native_error,
                message_text,
                buffer_length,
                text_length );

        if ( log_info.log_flag )
        {
            if ( SQL_SUCCEEDED( ret ))
            {
                char *ts1, *ts2;

                sprintf( statement -> msg, 
                    "\n\t\tExit:[%s]\
                    \n\t\t\tSQLState = %s\
                    \n\t\t\tNative = %s\
                    \n\t\t\tMessage Text = %s",
                        __get_return_status( ret, s2 ),
                        ( ts1 = unicode_to_ansi_alloc( sqlstate, SQL_NTS, statement -> connection )),
                        __ptr_as_string( s0, native_error ),
                        __sdata_as_string( s1, SQL_CHAR, 
                            text_length, ( ts2 = unicode_to_ansi_alloc( message_text, SQL_NTS, statement -> connection ))));

                free( ts1 );
                free( ts2 );
            }
            else
            {
                sprintf( statement -> msg, 
                    "\n\t\tExit:[%s]",
                        __get_return_status( ret, s2 ));
            }

            dm_log_write( __FILE__, 
                    __LINE__, 
                    LOG_INFO, 
                    LOG_INFO, 
                    statement -> msg );
        }

        thread_release( SQL_HANDLE_STMT, statement );

        return ret;
    }
    else if ( connection_handle )
    {
        DMHDBC connection = ( DMHDBC ) connection_handle;

        if ( !__validate_dbc( connection ))
        {
            dm_log_write( __FILE__, 
                    __LINE__, 
                    LOG_INFO, 
                    LOG_INFO, 
                    "Error: SQL_INVALID_HANDLE" );

            return SQL_INVALID_HANDLE;
        }

        thread_protect( SQL_HANDLE_DBC, connection );

        if ( log_info.log_flag )
        {
            sprintf( connection -> msg, 
                "\n\t\tEntry:\
                \n\t\t\tConnection = %p\
                \n\t\t\tSQLState = %p\
                \n\t\t\tNative = %p\
                \n\t\t\tMessage Text = %p\
                \n\t\t\tBuffer Length = %d\
                \n\t\t\tText Len Ptr = %p",
                    connection,
                    sqlstate,
                    native_error,
                    message_text,
                    buffer_length,
                    text_length );

            dm_log_write( __FILE__, 
                    __LINE__, 
                    LOG_INFO, 
                    LOG_INFO, 
                    connection -> msg );
        }

        ret = extract_sql_error_w( &connection -> error,
                sqlstate,
                native_error,
                message_text,
                buffer_length,
                text_length );

        if ( log_info.log_flag )
        {
            if ( SQL_SUCCEEDED( ret ))
            {
                char *ts1, *ts2;

                sprintf( connection -> msg, 
                    "\n\t\tExit:[%s]\
                    \n\t\t\tSQLState = %s\
                    \n\t\t\tNative = %s\
                    \n\t\t\tMessage Text = %s",
                        __get_return_status( ret, s2 ),
                        ts1 = unicode_to_ansi_alloc( sqlstate, SQL_NTS, connection ),
                        __ptr_as_string( s0, native_error ),
                        __sdata_as_string( s1, SQL_CHAR, 
                            text_length, ( ts2 = unicode_to_ansi_alloc( message_text, SQL_NTS, connection ))));

                free( ts1 );
                free( ts2 );
            }
            else
            {
                sprintf( connection -> msg, 
                    "\n\t\tExit:[%s]",
                        __get_return_status( ret, s2 ));
            }

            dm_log_write( __FILE__, 
                    __LINE__, 
                    LOG_INFO, 
                    LOG_INFO, 
                    connection -> msg );
        }

        thread_release( SQL_HANDLE_DBC, connection );
    }
    else if ( environment_handle )
    {
        DMHENV environment = ( DMHENV ) environment_handle;

        if ( !__validate_env( environment ))
        {
            dm_log_write( __FILE__, 
                    __LINE__, 
                    LOG_INFO, 
                    LOG_INFO, 
                    "Error: SQL_INVALID_HANDLE" );

            return SQL_INVALID_HANDLE;
        }

        thread_protect( SQL_HANDLE_ENV, environment );

        if ( log_info.log_flag )
        {
            sprintf( environment -> msg, 
                "\n\t\tEntry:\
                \n\t\t\tEnvironment = %p\
                \n\t\t\tSQLState = %p\
                \n\t\t\tNative = %p\
                \n\t\t\tMessage Text = %p\
                \n\t\t\tBuffer Length = %d\
                \n\t\t\tText Len Ptr = %p",
                    environment,
                    sqlstate,
                    native_error,
                    message_text,
                    buffer_length,
                    text_length );

            dm_log_write( __FILE__, 
                    __LINE__, 
                    LOG_INFO, 
                    LOG_INFO, 
                    environment -> msg );
        }

        ret = extract_sql_error_w( &environment -> error,
                sqlstate,
                native_error,
                message_text,
                buffer_length,
                text_length );

        if ( log_info.log_flag )
        {
            if ( SQL_SUCCEEDED( ret ))
            {
                char *ts1, *ts2;

                sprintf( environment -> msg, 
                    "\n\t\tExit:[%s]\
                    \n\t\t\tSQLState = %s\
                    \n\t\t\tNative = %s\
                    \n\t\t\tMessage Text = %s",
                        __get_return_status( ret, s2 ),
                        ts1 = unicode_to_ansi_alloc( sqlstate, SQL_NTS, NULL ),
                        __ptr_as_string( s0, native_error ),
                        __sdata_as_string( s1, SQL_CHAR, 
                            text_length, ( ts2 = unicode_to_ansi_alloc( message_text, SQL_NTS, NULL ))));

                free( ts1 );
                free( ts2 );
            }
            else
            {
                sprintf( environment -> msg, 
                    "\n\t\tExit:[%s]",
                        __get_return_status( ret, s2 ));
            }

            dm_log_write( __FILE__, 
                    __LINE__, 
                    LOG_INFO, 
                    LOG_INFO, 
                    environment -> msg );
        }

        thread_release( SQL_HANDLE_ENV, environment );
    }
    else
    {
        dm_log_write( __FILE__, 
                    __LINE__, 
                    LOG_INFO, 
                    LOG_INFO, 
                    "Error: SQL_INVALID_HANDLE" );

        return SQL_INVALID_HANDLE;
    }

    return ret;
}

