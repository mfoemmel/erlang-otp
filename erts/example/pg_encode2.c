#include <erl_driver.h>

#include <libpq-fe.h>

#include <ei.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "pg_encode2.h"

void encode_ok(ei_x_buff* x)
{
    const char* k_ok = "ok";
    ei_x_encode_atom(x, k_ok);
}

void encode_error(ei_x_buff* x, PGconn* conn)
{
    const char* k_error = "error";
    ei_x_encode_tuple_header(x, 2);
    ei_x_encode_atom(x, k_error);
    ei_x_encode_string(x, PQerrorMessage(conn));
}

void encode_result(ei_x_buff* x, PGresult* res, PGconn* conn)
{
    int row, n_rows, col, n_cols, fsize;

    switch (PQresultStatus(res)) {
    case PGRES_TUPLES_OK: 
	n_rows = PQntuples(res); 
	n_cols = PQnfields(res); 
	ei_x_encode_tuple_header(x, 2); 
	encode_ok(x);
 	ei_x_encode_list_header(x, 1);
	for (col = 0; col < n_cols; ++col) {
	    ei_x_encode_list_header(x, 1);
	    ei_x_encode_string(x, PQfname(res, col));
	}
	ei_x_encode_empty_list(x); 
	for (row = 0; row < n_rows; ++row) {
	    ei_x_encode_list_header(x, 1);
	    for (col = 0; col < n_cols; ++col) {
		ei_x_encode_list_header(x, 1);
		fsize = PQgetlength(res, row, col);
		ei_x_encode_binary(x, PQgetvalue(res, row, col), fsize);
	    }
	    ei_x_encode_empty_list(x);
	}
	ei_x_encode_empty_list(x); 
	break; 
    case PGRES_COMMAND_OK:
	ei_x_encode_tuple_header(x, 2);
        encode_ok(x);
	ei_x_encode_string(x, PQcmdTuples(res));
        break;
    default:
	encode_error(x, conn);
	break;
    }
}

