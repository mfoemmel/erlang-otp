# snmp-v2tov1.awk
# nawk script - pass 1 of translation from SNMP v2 SMI to v1 SMI
# mbj@erlang.ericsson.se 971114
#

# Translate v2 IMPORTs to their v1 equivalents
/IMPORT/          { import = 1; isave = 0; print $0; next}
(import == 1) && ($1 == ";") { import = 0 }
(import == 1) && ($1 != "FROM") { for (i = 1; i <= NF; i++) {
                                      sub(",", "", $i);
                                      imp[i+isave] = $i;
                                     }
                                  isave = isave + (i-1) ; next }
(import == 1) && ($1 == "FROM") { print_imp($2, imp, isave); isave = 0; next }

# str is 1 if we're inside a string, and 0 otherwise.
/\"/ { str = 1 }
$NF ~ /\"$/ { str = 0 }

# Just reprint all comments
/^--/ { print $0; next }

# Place comments around MODULE-IDENTITY
/MODULE-IDENTITY/ && (str == 0) { moduleid = 1; print "--", $0; next }
(moduleid == 1) && ($1 == "::=") { moduleid = 0; print "--", $0; next }
moduleid == 1     { print "--", $0; next }

# Translate TEXTUAL-CONVENTION into an ordinary type assignement.
# Place comments around body.
/TEXTUAL-CONVENTION/ && (str==0) { textual = 1; 
                       print $1, $2;
                       print "--TEXTUAL-CONVENTION"; next }
(textual == 1) && ($1 == "SYNTAX") { textual = 0;
                                     print "--SYNTAX\n";
                                     for (i = 2; i <= NF; i++) print $i;
                                     next }
textual == 1     { sub("--", "-- --", $0); print "--", $0; next }

# Translate OBJECT-IDENTITY into an OBJECT IDENTIFIER.
# Place comments around body.
/OBJECT-IDENTITY/ && (str==0) { objid = 1; 
                       print $1, "OBJECT IDENTIFIER";
                       print "--OBJECT-IDENTITY"; next }
(objid == 1) && ($1 == "::=") { objid = 0; print $0; next }
objid == 1     { sub("--", "-- --", $0); print "--", $0; next }

# Place comments around MODULE-COMPLIANCE
/MODULE-COMPLIANCE/ && (str == 0) { modcomp = 1; print "--", $0; next }
(modcomp == 1) && ($1 == "::=") { modcomp = 0; print "--", $0; next }
modcomp == 1     { sub("--", "-- --", $0); print "--", $0; next }

# Place comments around OBJECT-GROUP
/OBJECT-GROUP/ && (str == 0){ objgr = 1; print "--", $0; next }
(objgr == 1) && ($1 == "::=") { objgr = 0; print "--", $0; next }
objgr == 1     { sub("--", "-- --", $0); print "--", $0; next }

/OBJECT-GROUP/ { print "tjolaopp" }

# Place comments around NOTIFICATION-GROUP
/NOTIFICATION-GROUP/ && (str == 0){ notgr = 1; print "--", $0; next }
(notgr == 1) && ($1 == "::=") { notgr = 0; print "--", $0; next }
notgr == 1     { sub("--", "-- --", $0); print "--", $0; next }

# Translate NOTIFICATION-TYPE into a TRAP-TYPE.
/NOTIFICATION-TYPE/ && (str == 0) { trap = 1; print $1, " TRAP-TYPE";
                      printf "    ENTERPRISE "; tri = 1; next }
(trap == 1) && ($1 == "OBJECTS") { tra[tri++] = $0; next }
(trap == 1) && ($1 == "STATUS") { next }
(trap == 1) && ($1 == "::=") { print $3; pr_trap(tra, tri);
                               printf "    ::= "; print $4;
                               tri = 1; trap=0;next }
trap == 1 { tra[tri++] = $0; next }

/UNITS/ { sub("--", "-- --", $0); print "--", $0; next }

{ print $0 }


# Print v1 IMPORT statements
function print_imp(mib, imp, isave) {
  for (i = 1; i <= isave; i++) {
    if (imp[i] == "Counter32") {
      print "   ", imp[i];
      print "        FROM RFC1155-SMI";
    }
    else
    if (imp[i] == "Gauge32") {
      print "   ", imp[i];
      print "        FROM RFC1155-SMI";
      }
    else
      if (imp[i] == "TimeTicks") {
      print "   ", imp[i];
      print "        FROM RFC1155-SMI";
      }
    else
      if (imp[i] == "Opaque") {
      print "   ", imp[i];
      print "        FROM RFC1155-SMI";
      }
    else
      if (imp[i] == "IpAddress") {
      print "   ", imp[i];
      print "        FROM RFC1155-SMI";
      }
    else
      if (imp[i] == "NetworkAddress") {
      print "   ", imp[i];
      print "        FROM RFC1155-SMI";
      }
    else
      if (imp[i] == "enterprises") {
      print "   ", imp[i];
      print "        FROM RFC1155-SMI";
      }
    else
      if (imp[i] == "private") {
      print "   ", imp[i];
      print "        FROM RFC1155-SMI";
      }
    else
      if (imp[i] == "experimental") {
      print "   ", imp[i];
      print "        FROM RFC1155-SMI";
      }
    else
      if (imp[i] == "mgmt") {
      print "   ", imp[i];
      print "        FROM RFC1155-SMI";
      }
    else
      if (imp[i] == "internet") {
      print "   ", imp[i];
      print "        FROM RFC1155-SMI";
      }
    else
      if (imp[i] == "directory") {
      print "   ", imp[i];
      print "        FROM RFC1155-SMI";
      }
    else
      if (imp[i] == "DisplayString") {
      print "   ", imp[i];
      print "        FROM RFC1213-MIB";
      }
    else
      if (imp[i] == "mib-2") {
      print "   ", imp[i];
      print "        FROM RFC1213-MIB";
      }
    else
      if (imp[i] == "OBJECT-TYPE") {
      print "   ", imp[i];
      print "        FROM RFC-1212";
      }
    else
    if (imp[i] == "Integer32")
      ;
    else
    if (imp[i] == "MODULE-IDENTITY")
      ;
    else
    if (imp[i] == "TEXTUAL-CONVENTION")
      ;
    else
    if (imp[i] == "OBJECT-IDENTITY")
      ;
    else
    if (imp[i] == "OBJECT-GROUP")
      ;
    else
    if (imp[i] == "MODULE-COMPLIANCE")
      ;
    else
    if (imp[i] == "NOTIFICATION-GROUP")
      ;
    else
      if (imp[i] == "NOTIFICATION-TYPE") {
	print "    TRAP-TYPE";
	print "        FROM RFC-1215";
      }
    else
      if (imp[i] == "DateAndTime") {
	print "   ", imp[i];
	print "        FROM STANDARD-MIB";
      }
    else
      if (imp[i] == "TruthValue") {
	print "   ", imp[i];
	print "        FROM STANDARD-MIB";
      }
    else
      if (imp[i] == "RowStatus") {
	print "   ", imp[i];
	print "        FROM STANDARD-MIB";
      }
    else {
      print "   ", imp[i];
      print "        FROM", mib;
    }
  }
}

# Print a trap
function pr_trap(tra, tri) {
  for (i = 1; i < tri; i++)
    print tra[i];
}
