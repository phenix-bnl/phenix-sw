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
 * $Id: SQLDriverConnectW.c,v 1.1.1.1 2004/02/18 20:37:42 dave Exp $
 *
 * $Log: SQLDriverConnectW.c,v $
 * Revision 1.1.1.1  2004/02/18 20:37:42  dave
 * first import of unixODBC
 *
 * Revision 1.13  2003/10/30 18:20:45  lurcher
 *
 * Fix broken thread protection
 * Remove SQLNumResultCols after execute, lease S4/S% to driver
 * Fix string overrun in SQLDriverConnect
 * Add initial support for Interix
 *
 * Revision 1.12  2003/08/08 11:14:21  lurcher
 *
 * Fix UNICODE problem in SQLDriverConnectW
 *
 * Revision 1.11  2002/12/20 11:36:46  lurcher
 *
 * Update DMEnvAttr code to allow setting in the odbcinst.ini entry
 *
 * Revision 1.10  2002/12/05 17:44:30  lurcher
 *
 * Display unknown return values in return logging
 *
 * Revision 1.9  2002/10/14 09:46:10  lurcher
 *
 * Remove extra return
 *
 * Revision 1.8  2002/07/25 09:30:26  lurcher
 *
 * Additional unicode and iconv changes
 *
 * Revision 1.7  2002/07/24 08:49:51  lurcher
 *
 * Alter UNICODE support to use iconv for UNICODE-ANSI conversion
 *
 * Revision 1.6  2002/07/04 17:27:56  lurcher
 *
 * Small bug fixes
 *
 * Revision 1.4  2002/05/24 12:42:50  lurcher
 *
 * Alter NEWS and ChangeLog to match their correct usage
 * Additional UNICODE tweeks
 *
 * Revision 1.3  2002/01/21 18:00:51  lurcher
 *
 * Assorted fixed and changes, mainly UNICODE/bug fixes
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
 * Revision 1.5  2001/05/15 10:57:44  nick
 *
 * Add initial support for VMS
 *
 * Revision 1.4  2001/04/16 15:41:24  nick
 *
 * Fix some problems calling non existing error funcs
 *
 * Revision 1.3  2001/04/12 17:43:36  nick
 *
 * Change logging and added autotest to odbctest
 *
 * Revision 1.2  2001/01/03 11:57:27  nick
 *
 * Fix some name collisions
 *
 * Revision 1.1  2000/12/31 20:30:54  nick
 *
 * Add UNICODE support
 *
 *
 **********************************************************************/

#include <string.h>
#include "drivermanager.h"

static char const rcsid[]= "$RCSfile: SQLDriverConnectW.c,v $";

int __parse_connection_string_w( struct con_struct *con_str,
    SQLWCHAR *str, int str_len )
{
struct con_pair *cp;
char *local_str, *ptr;
int len;
int got_dsn = 0;    /* if we have a DSN then ignore any DRIVER or FILEDSN */
int got_driver = 0;    /* if we have a DRIVER or FILEDSN then ignore any DSN */

    con_str -> count = 0;
    con_str -> list = NULL;

    if ( str_len == SQL_NTS )
    {
        len =  wide_strlen( str ) + sizeof( SQLWCHAR );
        local_str = malloc( len );
    }
    else
    {
        len = str_len + sizeof( SQLWCHAR );
        local_str = malloc( len );
    }

    unicode_to_ansi_copy( local_str, str, len, NULL );

    if ( !local_str || strlen( local_str ) == 0 ||
        ( strlen( local_str ) == 1 && *local_str == ';' ))
    {
        /* connection-string ::= empty-string [;] */
        free( local_str );

        __append_pair( con_str, "DSN", "DEFAULT" );
        return 0;
    }

    ptr = local_str;

    while(( cp = __get_pair( &ptr )) != NULL )
    {
        if ( strcasecmp( cp -> keyword, "DSN" ) == 0 )
        {
            if ( got_driver )
                continue;

            got_dsn = 1;
        }
        else if ( strcasecmp( cp -> keyword, "DRIVER" ) == 0 ||
            strcmp( cp -> keyword, "FILEDSN" ) == 0 )
        {
            if ( got_dsn )
                continue;

            got_driver = 1;
        }

        __append_pair( con_str, cp -> keyword, cp -> attribute );
        free( cp -> keyword );
        free( cp -> attribute );
        free( cp );
    }

    /* if no dsn or DRIVER, then set DSN=DEFAULT */
    if ( !got_driver && !got_dsn )
    {
        __append_pair( con_str, "DSN", "DEFAULT" );
    }

    free( local_str );

    return 0;
}

SQLRETURN SQLDriverConnectW(
    SQLHDBC            hdbc,
    SQLHWND            hwnd,
    SQLWCHAR            *conn_str_in,
    SQLSMALLINT        len_conn_str_in,
    SQLWCHAR            *conn_str_out,
    SQLSMALLINT        conn_str_out_max,
    SQLSMALLINT        *ptr_conn_str_out,
    SQLUSMALLINT       driver_completion )
{
    DMHDBC connection = (DMHDBC)hdbc;
    struct con_struct con_struct;
    char *driver = NULL, *dsn = NULL;
    char lib_name[ INI_MAX_PROPERTY_VALUE + 1 ];
    char driver_name[ INI_MAX_PROPERTY_VALUE + 1 ];
    SQLRETURN ret_from_connect;
    SQLCHAR s1[ 2048 ];
    int warnings;

    /*
     * check connection
     */

    strcpy( driver_name, "" );

    if ( !__validate_dbc( connection ))
    {
        dm_log_write( __FILE__, 
                __LINE__, 
                    LOG_INFO, 
                    LOG_INFO, 
                    "Error: SQL_INVALID_HANDLE" );

        return SQL_INVALID_HANDLE;
    }

    function_entry( connection );

    if ( log_info.log_flag )
    {
        sprintf( connection -> msg, "\n\t\tEntry:\
            \n\t\t\tConnection = %p\
            \n\t\t\tWindow Hdl = %p\
            \n\t\t\tStr In = %s\
            \n\t\t\tStr Out = %p\
            \n\t\t\tStr Out Max = %d\
            \n\t\t\tStr Out Ptr = %p\
            \n\t\t\tCompletion = %d",
                connection,
                hwnd,
                __wstring_with_length_hide_pwd( s1, conn_str_in, 
                    len_conn_str_in ), 
                conn_str_out,
                conn_str_out_max,
                ptr_conn_str_out,
                driver_completion );

        dm_log_write( __FILE__, 
                __LINE__, 
                LOG_INFO, 
                LOG_INFO, 
                connection -> msg );
    }

    thread_protect( SQL_HANDLE_DBC, connection );

    if ( len_conn_str_in < 0 && len_conn_str_in != SQL_NTS )
    {
        dm_log_write( __FILE__, 
                __LINE__, 
                LOG_INFO, 
                LOG_INFO, 
                "Error: HY090" );

        __post_internal_error( &connection -> error,
                ERROR_HY090, NULL,
                connection -> environment -> requested_version );

        return function_return( SQL_HANDLE_DBC, connection, SQL_ERROR );
    }

    if ( driver_completion == SQL_DRIVER_PROMPT &&
            hwnd == NULL )
    {
        dm_log_write( __FILE__, 
                __LINE__, 
                LOG_INFO, 
                LOG_INFO, 
                "Error: HY092" );

        __post_internal_error( &connection -> error,
                ERROR_HY092, NULL,
                connection -> environment -> requested_version );

        return function_return( SQL_HANDLE_DBC, connection, SQL_ERROR );
    }

    if ( driver_completion != SQL_DRIVER_PROMPT &&
            driver_completion != SQL_DRIVER_COMPLETE &&
            driver_completion != SQL_DRIVER_COMPLETE_REQUIRED &&
            driver_completion != SQL_DRIVER_NOPROMPT )
    {
        dm_log_write( __FILE__, 
                __LINE__, 
                LOG_INFO, 
                LOG_INFO, 
                "Error: HY110" );

        __post_internal_error( &connection -> error,
                ERROR_HY110, NULL,
                connection -> environment -> requested_version );

        return function_return( SQL_HANDLE_DBC, connection, SQL_ERROR );
    }

    /*
     * check the state of the connection
     */

    if ( connection -> state != STATE_C2 )
    {
        dm_log_write( __FILE__, 
                __LINE__, 
                LOG_INFO, 
                LOG_INFO, 
                "Error: 08002" );

        __post_internal_error( &connection -> error,
                ERROR_08002, NULL,
                connection -> environment -> requested_version );

        return function_return( SQL_HANDLE_DBC, connection, SQL_ERROR );
    }

    /*
     * parse the connection string
     */

    if ( !conn_str_in )
    {
        char *ansi_conn_str_in;

        /*
         * not quite by the book, but better than nothing
         */

        ansi_conn_str_in = "DSN=DEFAULT;";
        len_conn_str_in = strlen( ansi_conn_str_in );

        __parse_connection_string( &con_struct,
                ansi_conn_str_in, len_conn_str_in );
    }
    else
    {
        __parse_connection_string_w( &con_struct,
                conn_str_in, len_conn_str_in );
    }

    /*
     * look for some keywords
     *
     * TO_DO FILEDSN's
     *
     * have we got a DRIVER= attribute
     */

    driver = __get_attribute_value( &con_struct, "DRIVER" );
    if ( driver )
    {
        /*
         * look up the driver in the ini file
         */

        strcpy( driver_name, driver );

        SQLGetPrivateProfileString( driver, "Driver", "",
                lib_name, sizeof( lib_name ), "ODBCINST.INI" );

        if ( lib_name[ 0 ] == '\0' )
        {
            /*
             * at this point a box could pop up to allow the selection of a driver
             *
             * do this later
             */

            dm_log_write( __FILE__, 
                    __LINE__, 
                    LOG_INFO, 
                    LOG_INFO, 
                    "Error: IM002" );

            __post_internal_error( &connection -> error,
                    ERROR_IM002, NULL,
                    connection -> environment -> requested_version );
            __release_conn( &con_struct );

            return function_return( SQL_HANDLE_DBC, connection, SQL_ERROR );
        }

        strcpy( connection -> dsn, "" );
        __handle_attr_extensions( connection, NULL, driver_name );
    }
    else
    {
        dsn = __get_attribute_value( &con_struct, "DSN" );

        if ( !dsn )
        {
            dm_log_write( __FILE__, 
                    __LINE__, 
                    LOG_INFO, 
                    LOG_INFO, 
                    "Error: IM002" );

            __post_internal_error( &connection -> error,
                    ERROR_IM002, NULL,
                    connection -> environment -> requested_version );
            __release_conn( &con_struct );

            return function_return( SQL_HANDLE_DBC, connection, SQL_ERROR );
        }

        if ( strlen( dsn ) > SQL_MAX_DSN_LENGTH )
        {
            dm_log_write( __FILE__, 
                    __LINE__, 
                    LOG_INFO, 
                    LOG_INFO, 
                    "Error: IM012" );

            __post_internal_error( &connection -> error,
                    ERROR_IM012, NULL,
                    connection -> environment -> requested_version );

            return function_return( SQL_HANDLE_DBC, connection, SQL_ERROR );
        }

        /*
         * look up the dsn in the ini file
         */

        if ( !__find_lib_name( dsn, lib_name, driver_name ))
        {
            dm_log_write( __FILE__, 
                    __LINE__, 
                    LOG_INFO, 
                    LOG_INFO, 
                    "Error: IM002" );

            __post_internal_error( &connection -> error,
                    ERROR_IM002, NULL,
                    connection -> environment -> requested_version );
            __release_conn( &con_struct );

            return function_return( SQL_HANDLE_DBC, connection, SQL_ERROR );
        }

        strcpy( connection -> dsn, dsn );
        __handle_attr_extensions( connection, dsn, driver_name );
    }

    if ( dsn )
    {
        /*
         * do we have any Environment, Connection, or Statement attributes set in the ini ?
         */

        __handle_attr_extensions( connection, dsn, driver_name );
    }

    __release_conn( &con_struct );

    /*
     * we have now got the name of a lib to load
     */
    if ( !__connect_part_one( connection, lib_name, driver_name, &warnings ))
    {
        return function_return( SQL_HANDLE_DBC, connection, SQL_ERROR );
    }

    if ( !CHECK_SQLDRIVERCONNECTW( connection ) &&
        !CHECK_SQLDRIVERCONNECT( connection ))
    {
        dm_log_write( __FILE__, 
                __LINE__, 
                LOG_INFO, 
                LOG_INFO, 
                "Error: IM001" );

        __disconnect_part_one( connection );
        __post_internal_error( &connection -> error,
                ERROR_IM001, NULL,
                connection -> environment -> requested_version );

        return function_return( SQL_HANDLE_DBC, connection, SQL_ERROR );
    }

    if ( CHECK_SQLDRIVERCONNECTW( connection ))
    {
        if ( CHECK_SQLSETCONNECTATTR( connection ))
        {
            int lret;
                
            lret = SQLSETCONNECTATTR( connection,
                    connection -> driver_dbc,
                    SQL_ATTR_ANSI_APP,
                    SQL_AA_FALSE,
                    0 );
        }

        ret_from_connect = SQLDRIVERCONNECTW( connection,
                connection -> driver_dbc,
                hwnd,
                conn_str_in,
                len_conn_str_in,
                conn_str_out,
                conn_str_out_max,
                ptr_conn_str_out,
                driver_completion );

        if ( ret_from_connect != SQL_SUCCESS )
        {
            SQLWCHAR sqlstate[ 6 ];
            SQLINTEGER native_error;
            SQLSMALLINT ind;
            SQLWCHAR message_text[ SQL_MAX_MESSAGE_LENGTH + 1 ];
            SQLRETURN ret;

            /*
             * get the errors from the driver before
             * loseing the connection 
             */

            if ( CHECK_SQLERRORW( connection ))
            {
                do
                {
                    ret = SQLERRORW( connection,
                            SQL_NULL_HENV,
                            connection -> driver_dbc,
                            SQL_NULL_HSTMT,
                            sqlstate,
                            &native_error,
                            message_text,
                            sizeof( message_text ),
                            &ind );


                    if ( SQL_SUCCEEDED( ret ))
                    {
                        __post_internal_error_ex_w( &connection -> error,
                                sqlstate,
                                native_error,
                                message_text,
                                SUBCLASS_ODBC, SUBCLASS_ODBC );
                    }
                }
                while( SQL_SUCCEEDED( ret ));
            }
            else if ( CHECK_SQLGETDIAGRECW( connection ))
            {
                int rec = 1;

                do
                {
                    ret = SQLGETDIAGRECW( connection,
                            SQL_HANDLE_DBC,
                            connection -> driver_dbc,
                            rec ++,
                            sqlstate,
                            &native_error,
                            message_text,
                            sizeof( message_text ),
                            &ind );


                    if ( SQL_SUCCEEDED( ret ))
                    {
                        __post_internal_error_ex_w( &connection -> error,
                                sqlstate,
                                native_error,
                                message_text,
                                SUBCLASS_ODBC, SUBCLASS_ODBC );
                    }
                }
                while( SQL_SUCCEEDED( ret ));
            }


            /* 
             * if it was a error then return now
             */

            if ( !SQL_SUCCEEDED( ret_from_connect ))
            {
                __disconnect_part_one( connection );

                sprintf( connection -> msg,
                        "\n\t\tExit:[%s]",
                            __get_return_status( ret_from_connect, s1 ));

                dm_log_write( __FILE__,
                        __LINE__,
                        LOG_INFO,
                        LOG_INFO,
                        connection -> msg );

                return function_return( SQL_HANDLE_DBC, connection, ret_from_connect );
            }
        }
        connection -> unicode_driver = 1;
    }
    else
    {
        char *in_str, *out_str;
        int len;

        if ( conn_str_in )
        {
            if ( len_conn_str_in == SQL_NTS )
            {
                len = wide_strlen( conn_str_in ) + sizeof( SQLWCHAR );
            }
            else
            {
                len = len_conn_str_in + sizeof( SQLWCHAR );
            }
            in_str = malloc( len );
            unicode_to_ansi_copy( in_str, conn_str_in, len, connection );
        }
        else
        {
            in_str = NULL;
        }

        if ( conn_str_out && conn_str_out_max > 0 )
        {
            out_str = malloc( conn_str_out_max + sizeof( SQLWCHAR ) );
        }
        else
        {
            out_str = NULL;
        }
       
        ret_from_connect = SQLDRIVERCONNECT( connection,
                connection -> driver_dbc,
                hwnd,
                (SQLCHAR*)in_str,
                len_conn_str_in,
                (SQLCHAR*)out_str,
                conn_str_out_max,
                ptr_conn_str_out,
                driver_completion );

        if ( in_str )
        {
            free( in_str );
        }

        if ( out_str )
        {
            if ( SQL_SUCCEEDED( ret_from_connect ))
            {
                ansi_to_unicode_copy( conn_str_out, out_str, SQL_NTS, connection );
            }

            free( out_str );
        }

        if ( ret_from_connect != SQL_SUCCESS )
        {
            SQLCHAR sqlstate[ 6 ];
            SQLINTEGER native_error;
            SQLSMALLINT ind;
            SQLCHAR message_text[ SQL_MAX_MESSAGE_LENGTH + 1 ];
            SQLRETURN ret;

            /*
             * get the errors from the driver before
             * loseing the connection 
             */

            if ( CHECK_SQLERROR( connection ))
            {
                do
                {
                    ret = SQLERROR( connection,
                            SQL_NULL_HENV,
                            connection -> driver_dbc,
                            SQL_NULL_HSTMT,
                            sqlstate,
                            &native_error,
                            message_text,
                            sizeof( message_text ),
                            &ind );

                    if ( SQL_SUCCEEDED( ret ))
                    {
                        __post_internal_error_ex( &connection -> error,
                                sqlstate,
                                native_error,
                                message_text,
                                SUBCLASS_ODBC, SUBCLASS_ODBC );
                    }
                }
                while( SQL_SUCCEEDED( ret ));
            }
            else if ( CHECK_SQLGETDIAGREC( connection ))
            {
                int rec = 1;

                do
                {
                    ret = SQLGETDIAGREC( connection,
                            SQL_HANDLE_DBC,
                            connection -> driver_dbc,
                            rec ++,
                            sqlstate,
                            &native_error,
                            message_text,
                            sizeof( message_text ),
                            &ind );

                    if ( SQL_SUCCEEDED( ret ))
                    {
                        __post_internal_error_ex( &connection -> error,
                                sqlstate,
                                native_error,
                                message_text,
                                SUBCLASS_ODBC, SUBCLASS_ODBC );
                    }
                }
                while( SQL_SUCCEEDED( ret ));
            }

            /* 
             * if it was a error then return now
             */

            if ( !SQL_SUCCEEDED( ret_from_connect ))
            {
                __disconnect_part_one( connection );

                sprintf( connection -> msg,
                        "\n\t\tExit:[%s]",
                            __get_return_status( ret_from_connect, s1 ));

                dm_log_write( __FILE__,
                        __LINE__,
                        LOG_INFO,
                        LOG_INFO,
                        connection -> msg );

                return function_return( SQL_HANDLE_DBC, connection, ret_from_connect );
            }
        }
        connection -> unicode_driver = 0;
    }

    /*
     * we should be connected now
     */

    connection -> state = STATE_C4;

    /*
     * did we get the type we wanted
     */

    if ( connection -> driver_version !=
            connection -> environment -> requested_version )
    {
        connection -> driver_version =
            connection -> environment -> requested_version;

        __post_internal_error( &connection -> error,
                ERROR_01000, "Driver does not support the requested version",
                connection -> environment -> requested_version );
        ret_from_connect = SQL_SUCCESS_WITH_INFO;
    }

    if ( !__connect_part_two( connection ))
    {
        __disconnect_part_two( connection );
        __disconnect_part_one( connection );

        return function_return( SQL_HANDLE_DBC, connection, SQL_ERROR );
    }

    if ( log_info.log_flag )
    {
        if ( conn_str_out && wide_strlen( conn_str_out ) > 64 )
        {
            sprintf( connection -> msg, 
                    "\n\t\tExit:[%s]\
                    \n\t\t\tConnection Out [%.64s...]",
                        __get_return_status( ret_from_connect, s1 ),
                        __wstring_with_length_hide_pwd( s1, conn_str_out, SQL_NTS ));
        }
        else
        {
            char null[ 20 ];

            strcpy( null, "NULL" );

            sprintf( connection -> msg, 
                    "\n\t\tExit:[%s]\
                    \n\t\t\tConnection Out [%s]",
                        __get_return_status( ret_from_connect, s1 ),
                        __wstring_with_length_hide_pwd( s1, conn_str_out, SQL_NTS ));
        }

        dm_log_write( __FILE__, 
                __LINE__, 
                LOG_INFO, 
                LOG_INFO, 
                connection -> msg );
    }

    if ( warnings && ret_from_connect == SQL_SUCCESS )
    {
        ret_from_connect = SQL_SUCCESS_WITH_INFO;
    }

    return function_return( SQL_HANDLE_DBC, connection, ret_from_connect );
}
