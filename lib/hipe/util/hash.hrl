%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%		    REPRESENTATION OF HASH TABLES
%

-record(hashtable,{occupancy=0,
		   size,
		   table,
		   min_occ,
		   max_occ,
		   min_occ_ratio=0.0,
		   max_occ_ratio=1.0}
       ).
