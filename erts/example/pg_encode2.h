void encode_ok(ei_x_buff* x);
void encode_error(ei_x_buff* x, PGconn* conn);
void encode_result(ei_x_buff* x, PGresult* res, PGconn* conn);
