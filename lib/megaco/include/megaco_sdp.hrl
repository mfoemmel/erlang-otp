%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
%%----------------------------------------------------------------------
%% Purpose: 
%%----------------------------------------------------------------------
%%
%%
%% Explanaition of the fields in the SDP body
%%   (See RFC 2327 and RFC 3266 for the complete decription)
%%
%% Session descriptions
%%
%%  v        protocol version
%%  o        owner/creator and session identifier 
%%  s        session name                          (not used)
%%  i        session information                   (not used)
%%  u        URI of description                    (not used)
%%  e        email address                         (not used)
%%  p        phone number                          (not used) 
%%  c        connection information
%%  b        bandwidth information
%%  z        time zone adjustment
%%  k        encryption key
%%  a        zero or more session attribute lines
%%  Zero or more media descriptions
%%
%% Time descriptions
%%
%%  t        time the session is active
%%  r        zero or more repeat times
%%  
%% Media descriptions
%%
%%  m        media name and transport address
%%  i        media title
%%  c        connection information - optional if included at session-level
%%  b        bandwidth information
%%  k        encryption key
%%  a        zero or more media attribute lines
%%
%% An SDP-body is a list of the folowing tuples
%%
%%   {FieldID, FieldValue} where
%%
%%   FieldID = atom()
%%   FiledValue is a term()
%%
%% FieldID    FieldValue
%% 
%% o          #megaco_sdp_o{}
%% c          #megaco_sdp_c{}
%% m          #megaco_sdp_m{}
%% a          #megaco_sdp_a_rtpmap{}  iff 'att-field'=rtpmap
%% a          #megaco_sdp_a_ptime{}   iff 'att-field'=ptime
%% <other>    string()
%%
%% An example SDP description is:
%%
%%      v=0
%%      o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4
%%      s=SDP Seminar
%%      i=A Seminar on the session description protocol
%%      u=http://www.cs.ucl.ac.uk/staff/M.Handley/sdp.03.ps
%%      e=mjh@isi.edu (Mark Handley)
%%      c=IN IP4 224.2.17.12/127
%%      t=2873397496 2873404696
%%      a=recvonly
%%      m=audio 49170 RTP/AVP 0
%%      m=video 51372 RTP/AVP 31
%%      m=application 32416 udp wb
%%      a=orient:portrait
%% 
%% 
%% An example SDP description with IPv6 addresses
%%
%%    v=0
%%    o=nasa1 971731711378798081 0 IN IP6 2201:056D::112E:144A:1E24
%%    s=(Almost) live video feed from Mars-II satellite
%%    p=+1 713 555 1234
%%    c=IN IP6 FF1E:03AD::7F2E:172A:1E24
%%    t=3338481189 3370017201
%%    m=audio 6000 RTP/AVP 2
%%    a=rtpmap:2 G726-32/8000
%%    m=video 6024 RTP/AVP 107
%%    a=rtpmap:107 H263-1998/90000
%% 
%%----------------------------------------------------------------------

-ifndef(megaco_sdp_).
-define(megaco_sdp_, true).


%% ===================================================================
%% 
%% Protocol Version
%% 
%% v=0
%% 
%% This field gives the version of the Session Description Protocol.
%% There is no minor version number.
%% 

-record(megaco_sdp_v, {
	  version					% integer()
	 }
       ).




%% ===================================================================
%% 
%% Origin
%%
%% o=<username> <session id> <version> <network type> <address type>
%% <address>
%%
%% The "o=" field gives the originator of the session (their username
%% and the address of the user's host) plus a session id and session
%% version number.
%%
%% <username> is the user's login on the originating host, or it is
%% "-" if the originating host does not support the concept of user
%% ids.  <username> must not contain spaces.  <session id> is a
%% numeric string such that the tuple of <username>, <session id>,
%% <network type>, <address type> and <address> form a globally unique
%% identifier for the session.
%%
%% The method of <session id> allocation is up to the creating tool,
%% but it has been suggested that a Network Time Protocol (NTP)
%% timestamp be used to ensure uniqueness [1].
%%
%% <version> is a version number for this announcement.  It is needed
%% for proxy announcements to detect which of several announcements
%% for the same session is the most recent.  Again its usage is up to
%% the creating tool, so long as <version> is increased when a
%% modification is made to the session data.  Again, it is recommended
%% (but not mandatory) that an NTP timestamp is used.
%%
%% <network type> is a text string giving the type of network.
%% Initially "IN" is defined to have the meaning "Internet".  
%% 
%% <address type> is a text string giving the type of the address 
%% that follows. Initially "IP4" and "IP6" are defined.  
%% 
%% <address> is the globally unique address of the machine from which
%% the session was created.  For an address type of IP4, this is
%% either the fully-qualified domain name of the machine, or the
%% dotted-decimal representation of the IP version 4 address of the
%% machine.  For an address type of IP6, this is either the
%% fully-qualified domain name of the machine, or the compressed
%% textual representation of the IP version 6 address of the machine.
%% For both IP4 and IP6, the fully-qualified domain name is the form
%% that SHOULD be given unless this is unavailable, in which case the
%% globally unique address may be substituted.  A local IP address
%% MUST NOT be used in any context where the SDP description might
%% leave the scope in which the address is meaningful.
%%
%% In general, the "o=" field serves as a globally unique identifier
%% for this version of this session description, and the subfields
%% excepting the version taken together identify the session
%% irrespective of any modifications.
%% 

-record(megaco_sdp_o, {
          user_name,          % string()
          session_id,         % integer()
          version,            % integer()
          network_type = in,  % in | string()
          address_type = ip4, % ip4 | ip6 | string()
          address             % string()
         }
       ).




%% ===================================================================
%% 
%% Session Name
%%
%% s=<session name>
%%
%% The "s=" field is the session name.  There must be one and only one
%% "s=" field per session description, and it must contain ISO 10646
%% characters.
%% 

-record(megaco_sdp_s, {
	  name                                  % string()
	 }
       ).




%% ===================================================================
%% 
%% Session and Media Information
%%
%% i=<session description>
%%
%% The "i=" field is information about the session.  There may be at
%% most one session-level "i=" field per session description, and at
%% most one "i=" field per media. Although it may be omitted, this is
%% discouraged for session announcements, and user interfaces for
%% composing sessions should require text to be entered.  If it is
%% present it must contain ISO 10646 characters.
%%
%% A single "i=" field can also be used for each media definition.  In
%% media definitions, "i=" fields are primarily intended for labeling
%% media streams. As such, they are most likely to be useful when a
%% single session has more than one distinct media stream of the same
%% media type.  An example would be two different whiteboards, one for
%% slides and one for feedback and questions.
%% 

-record(megaco_sdp_i, {
	  session_descriptor                    % string()
	 }
       ).




%% ===================================================================
%% 
%% URI
%%
%% u=<URI>
%%
%% o A URI is a Universal Resource Identifier as used by WWW clients
%%
%% o The URI should be a pointer to additional information about the
%%   conference
%%
%% o This field is optional, but if it is present it should be specified
%%   before the first media field
%%
%% o No more than one URI field is allowed per session description
%% 

-record(megaco_sdp_u, {
	  uri                                   % string()
	 }
       ).




%% ===================================================================
%% 
%% Email Address and Phone Number
%%
%% e=<email address>
%% p=<phone number>
%%
%% o These specify contact information for the person responsible for
%%   the conference.  This is not necessarily the same person that
%%   created the conference announcement.
%%
%% o Either an email field or a phone field must be specified.
%%   Additional email and phone fields are allowed.
%%
%% o If these are present, they should be specified before the first
%%   media field.
%%
%% o More than one email or phone field can be given for a session
%%   description.
%%
%% o Phone numbers should be given in the conventional international
%%   format - preceded by a "+ and the international country code.
%%   There must be a space or a hyphen ("-") between the country code
%%   and the rest of the phone number.  Spaces and hyphens may be used
%%   to split up a phone field to aid readability if desired. For
%%   example:
%%
%%                 p=+44-171-380-7777 or p=+1 617 253 6011
%%
%% o Both email addresses and phone numbers can have an optional free
%%   text string associated with them, normally giving the name of the
%%   person who may be contacted.  This should be enclosed in
%%   parenthesis if it is present.  For example:
%%
%%                      e=mjh@isi.edu (Mark Handley)
%%
%%   The alternative RFC822 name quoting convention is also allowed for
%%   both email addresses and phone numbers.  For example,
%%
%%                      e=Mark Handley <mjh@isi.edu>
%%
%%   The free text string should be in the ISO-10646 character set with
%%   UTF-8 encoding, or alternatively in ISO-8859-1 or other encodings
%%   if the appropriate charset session-level attribute is set.
%% 

-record(megaco_sdp_e, {
	  email                                 % string()
	 }
       ).

-record(megaco_sdp_p, {
	  phone_number                          % string()
	 }
       ).




%% ===================================================================
%% 
%% Connection Data
%% 
%% c=<network type> <address type> <connection address>
%%
%% The "c=" field contains connection data.
%%
%% A session announcement must contain one "c=" field in each media
%% description (see below) or a "c=" field at the session-level.  It may
%% contain a session-level "c=" field and one additional "c=" field per
%% media description, in which case the per-media values override the
%% session-level settings for the relevant media.
%%
%% The first sub-field is the network type, which is a text string
%% giving the type of network.  Initially "IN" is defined to have the
%% meaning "Internet".
%%
%% The second sub-field is the address type.  This allows SDP to be used
%% for sessions that are not IP based.  Currently only IP4 is defined.
%%
%% The third sub-field is the connection address.  Optional extra
%% subfields may be added after the connection address depending on the
%% value of the <address type> field.
%%
%% For IP4 addresses, the connection address is defined as follows:
%%
%% o Typically the connection address will be a class-D IP multicast
%%   group address.  If the session is not multicast, then the
%%   connection address contains the fully-qualified domain name or the
%%   unicast IP address of the expected data source or data relay or
%%   data sink as determined by additional attribute fields. It is not
%%   expected that fully-qualified domain names or unicast addresses
%%   will be given in a session description that is communicated by a
%%   multicast announcement, though this is not prohibited.  If a
%%   unicast data stream is to pass through a network address
%%   translator, the use of a fully-qualified domain name rather than an
%%   unicast IP address is RECOMMENDED.  In other cases, the use of an
%%   IP address to specify a particular interface on a multi-homed host
%%   might be required.  Thus this specification leaves the decision as
%%   to which to use up to the individual application, but all
%%   applications MUST be able to cope with receiving both formats.
%% 
%% o Conferences using an IP multicast connection address must also have
%%   a time to live (TTL) value present in addition to the multicast
%%   address.  The TTL and the address together define the scope with
%%   which multicast packets sent in this conference will be sent. TTL
%%   values must be in the range 0-255.
%%
%%   The TTL for the session is appended to the address using a slash as
%%   a separator.  An example is:
%%
%%                         c=IN IP4 224.2.1.1/127
%%
%%   Hierarchical or layered encoding schemes are data streams where the
%%   encoding from a single media source is split into a number of
%%   layers.  The receiver can choose the desired quality (and hence
%%   bandwidth) by only subscribing to a subset of these layers.  Such
%%   layered encodings are normally transmitted in multiple multicast
%%   groups to allow multicast pruning.  This technique keeps unwanted
%%   traffic from sites only requiring certain levels of the hierarchy.
%%   For applications requiring multiple multicast groups, we allow the
%%   following notation to be used for the connection address:
%%
%%          <base multicast address>/<ttl>/<number of addresses>
%%
%%   If the number of addresses is not given it is assumed to be one.
%%   Multicast addresses so assigned are contiguously allocated above
%%   the base address, so that, for example:
%%
%%                        c=IN IP4 224.2.1.1/127/3
%%
%%   would state that addresses 224.2.1.1, 224.2.1.2 and 224.2.1.3 are
%%   to be used at a ttl of 127.  This is semantically identical to
%%   including multiple "c=" lines in a media description:
%%
%%                         c=IN IP4 224.2.1.1/127
%%                         c=IN IP4 224.2.1.2/127
%%                         c=IN IP4 224.2.1.3/127
%%
%%   Multiple addresses or "c=" lines can only be specified on a per-
%%   media basis, and not for a session-level "c=" field.
%%
%%   It is illegal for the slash notation described above to be used for
%%   IP unicast addresses.
%% 

-record(megaco_sdp_c, {
          network_type = in,                    % in | string()
          address_type = ip4,                   % ip4 | ip6 | string()
          connection_addr                       % string() | #conn_addr{}
         }).

%% Only if address type = ip4
-record(megaco_sdp_c_conn_addr, {
	  base,                                 % string()
	  ttl,                                  % integer()
	  num_of                                % undefined | integer()
	 }).



%% ===================================================================
%% 
%% Bandwidth
%%
%% b=<modifier>:<bandwidth-value>
%%
%% o This specifies the proposed bandwidth to be used by the session or
%%   media, and is optional.
%%
%% o <bandwidth-value> is in kilobits per second
%%
%% o <modifier> is a single alphanumeric word giving the meaning of the
%%   bandwidth figure.
%%
%% o Two modifiers are initially defined:
%%
%% CT Conference Total: An implicit maximum bandwidth is associated with
%%   each TTL on the Mbone or within a particular multicast
%%   administrative scope region (the Mbone bandwidth vs. TTL limits are
%%   given in the MBone FAQ). If the bandwidth of a session or media in
%%   a session is different from the bandwidth implicit from the scope,
%%   a `b=CT:...' line should be supplied for the session giving the
%%   proposed upper limit to the bandwidth used. The primary purpose of
%%   this is to give an approximate idea as to whether two or more
%%   conferences can co-exist simultaneously.
%%
%% AS Application-Specific Maximum: The bandwidth is interpreted to be
%%   application-specific, i.e., will be the application's concept of
%%   maximum bandwidth.  Normally this will coincide with what is set on
%%   the application's "maximum bandwidth" control if applicable.
%%
%%   Note that CT gives a total bandwidth figure for all the media at
%%   all sites.  AS gives a bandwidth figure for a single media at a
%%   single site, although there may be many sites sending
%%   simultaneously.
%%
%% o Extension Mechanism: Tool writers can define experimental bandwidth
%%   modifiers by prefixing their modifier with "X-". For example:
%%
%%                               b=X-YZ:128
%%
%%   SDP parsers should ignore bandwidth fields with unknown modifiers.
%%   Modifiers should be alpha-numeric and, although no length limit is
%%   given, they are recommended to be short.
%% 

-record(megaco_sdp_b, {
          modifier,                             % string()
          bandwidth                             % integer()
         }).




%% ===================================================================
%% 
%% Times, Repeat Times and Time Zones
%%
%% t=<start time>  <stop time>
%% r=<repeat interval> <active duration> <list of offsets from start-time>
%% z=<adjustment time> <offset> <adjustment time> <offset> ....
%%
%% o "t=" fields specify the start and stop times for a conference
%%   session.  Multiple "t=" fields may be used if a session is active
%%   at multiple irregularly spaced times; each additional "t=" field
%%   specifies an additional period of time for which the session will
%%   be active.  If the session is active at regular times, an "r="
%%   field (see below) should be used in addition to and following a
%%   "t=" field - in which case the "t=" field specifies the start and
%%   stop times of the repeat sequence.
%%
%% o The first and second sub-fields give the start and stop times for
%%   the conference respectively.  These values are the decimal
%%   representation of Network Time Protocol (NTP) time values in
%%   seconds [1].  To convert these values to UNIX time, subtract
%%   decimal 2208988800.
%%
%% o If the stop-time is set to zero, then the session is not bounded,
%%   though it will not become active until after the start-time.  If
%%   the start-time is also zero, the session is regarded as
%%   permanent.
%%
%%   User interfaces should strongly discourage the creation of
%%   unbounded and permanent sessions as they give no information
%%   about when the session is actually going to terminate, and so
%%   make scheduling difficult.
%%
%%   The general assumption may be made, when displaying unbounded
%%   sessions that have not timed out to the user, that an unbounded
%%   session will only be active until half an hour from the current
%%   time or the session start time, whichever is the later.  If
%%   behaviour other than this is required, an end-time should be
%%   given and modified as appropriate when new information becomes
%%   available about when the session should really end.
%%
%%   Permanent sessions may be shown to the user as never being active
%%   unless there are associated repeat times which state precisely
%%   when the session will be active.  In general, permanent sessions
%%   should not be created for any session expected to have a duration
%%   of less than 2 months, and should be discouraged for sessions
%%   expected to have a duration of less than 6 months.
%%
%% o "r=" fields specify repeat times for a session.  For example, if
%%   a session is active at 10am on Monday and 11am on Tuesday for one
%%   hour each week for three months, then the <start time> in the
%%   corresponding "t=" field would be the NTP representation of 10am
%%   on the first Monday, the <repeat interval> would be 1 week, the
%%   <active duration> would be 1 hour, and the offsets would be zero
%%   and 25 hours. The corresponding "t=" field stop time would be the
%%   NTP representation of the end of the last session three months
%%   later. By default all fields are in seconds, so the "r=" and "t="
%%   fields might be:
%%
%%                         t=3034423619 3042462419
%%                          r=604800 3600 0 90000
%%
%%   To make announcements more compact, times may also be given in
%%   units of days, hours or minutes. The syntax for these is a number
%%   immediately followed by a single case-sensitive character.
%%   Fractional units are not allowed - a smaller unit should be used
%%   instead.  The following unit specification characters are
%%   allowed:
%%
%%                       d - days (86400 seconds)
%%                      h - minutes (3600 seconds)
%%                       m - minutes (60 seconds)
%%       s - seconds (allowed for completeness but not recommended)
%%
%%   Thus, the above announcement could also have been written:
%%
%%                             r=7d 1h 0 25h
%%
%%   Monthly and yearly repeats cannot currently be directly specified
%%   with a single SDP repeat time - instead separate "t" fields
%%   should be used to explicitly list the session times.
%% 
%%   To schedule a repeated session which spans a change from
%%   daylight- saving time to standard time or vice-versa, it is
%%   necessary to specify offsets from the base repeat times. This is
%%   required because different time zones change time at different
%%   times of day, different countries change to or from daylight time
%%   on different dates, and some countries do not have daylight
%%   saving time at all.
%%
%%   Thus in order to schedule a session that is at the same time
%%   winter and summer, it must be possible to specify unambiguously
%%   by whose time zone a session is scheduled.  To simplify this task
%%   for receivers, we allow the sender to specify the NTP time that a
%%   time zone adjustment happens and the offset from the time when
%%   the session was first scheduled.  The "z" field allows the sender
%%   to specify a list of these adjustment times and offsets from the
%%   base time.
%%
%%   An example might be:
%%
%%                      z=2882844526 -1h 2898848070 0
%%
%%   This specifies that at time 2882844526 the time base by which the
%%   session's repeat times are calculated is shifted back by 1 hour,
%%   and that at time 2898848070 the session's original time base is
%%   restored. Adjustments are always relative to the specified start
%%   time - they are not cumulative.
%%
%% o If a session is likely to last several years, it is expected that
%%   the session announcement will be modified periodically rather
%%   than transmit several years worth of adjustments in one
%%   announcement.
%% 

-record(megaco_sdp_t, {
          start,                                % integer()
          stop                                  % integer()
         }).

-record(megaco_sdp_r, {
          repeat_interval,                      % string()
          active_duration,                      % string()
	  list_of_offsets                       % [ string() ]
	 }
       ).
      
-record(megaco_sdp_z, {
	  list_of_adjustments                   % [ string() ]
	 }
       ).




%% ===================================================================
%% 
%% Encryption Keys
%% 
%% k=<method>
%% k=<method>:<encryption key>
%%
%% o The session description protocol may be used to convey encryption
%%   keys.  A key field is permitted before the first media entry (in
%%   which case it applies to all media in the session), or for each
%%   media entry as required.
%%
%% o The format of keys and their usage is outside the scope of this
%%   document, but see [3].
%%
%% o The method indicates the mechanism to be used to obtain a usable
%%   key by external means, or from the encoded encryption key given.
%%
%%   The following methods are defined:
%%
%%    k=clear:<encryption key>
%%      The encryption key (as described in [3] for  RTP  media  streams
%%      under  the  AV  profile)  is  included untransformed in this key
%%      field.
%%
%%    k=base64:<encoded encryption key>
%%      The encryption key (as described in [3] for RTP media streams
%%      under the AV profile) is included in this key field but has been
%%      base64 encoded because it includes characters that are
%%      prohibited in SDP.
%%
%%    k=uri:<URI to obtain key>
%%      A Universal Resource Identifier as used by WWW clients is
%%      included in this key field.  The URI refers to the data
%%      containing the key, and may require additional authentication
%%      before the key can be returned.  When a request is made to the
%%      given URI, the MIME content-type of the reply specifies the
%%      encoding for the key in the reply.  The key should not be
%%      obtained until the user wishes to join the session to reduce
%%      synchronisation of requests to the WWW server(s).
%%
%%    k=prompt
%%      No key is included in this SDP description, but the session or
%%      media stream referred to by this key field is encrypted.  The
%%      user should be prompted for the key when attempting to join the
%%      session, and this user-supplied key should then be used to
%%      decrypt the media streams.
%% 

-record(megaco_sdp_k, {
	  method,            % clear | base64 | uri | prompt | string()
	  encryption_key     % undefined | string()
	 }
       ).




%% ===================================================================
%% 
%% Attributes
%% 
%% a=<attribute>
%% a=<attribute>:<value>
%%
%% Attributes are the primary means for extending SDP.  Attributes may
%% be defined to be used as "session-level" attributes, "media-level"
%% attributes, or both.
%%
%% A media description may have any number of attributes ("a=" fields)
%% which are media specific.  These are referred to as "media-level"
%% attributes and add information about the media stream.  Attribute
%% fields can also be added before the first media field; these
%% "session-level" attributes convey additional information that
%% applies to the conference as a whole rather than to individual
%% media; an example might be the conference's floor control policy.
%%
%% Attribute fields may be of two forms:
%%
%% o property attributes.  A property attribute is simply of the form
%%   "a=<flag>".  These are binary attributes, and the presence of the
%%   attribute conveys that the attribute is a property of the
%%   session.  An example might be "a=recvonly".
%%
%% o value attributes.  A value attribute is of the form
%%   "a=<attribute>:<value>".  An example might be that a whiteboard
%%   could have the value attribute "a=orient:landscape"
%%
%% Attribute interpretation depends on the media tool being invoked.
%% Thus receivers of session descriptions should be configurable in
%% their interpretation of announcements in general and of attributes
%% in particular.
%%
%% Attribute names must be in the US-ASCII subset of ISO-10646/UTF-8.
%%
%% Attribute values are byte strings, and MAY use any byte value
%% except 0x00 (Nul), 0x0A (LF), and 0x0D (CR). By default, attribute
%% values are to be interpreted as in ISO-10646 character set with
%% UTF-8 encoding.  Unlike other text fields, attribute values are NOT
%% normally affected by the `charset' attribute as this would make
%% comparisons against known values problematic.  However, when an
%% attribute is defined, it can be defined to be charset-dependent, in
%% which case it's value should be interpreted in the session charset
%% rather than in ISO-10646.
%%
%% Attributes that will be commonly used can be registered with IANA
%% (see Appendix B).  Unregistered attributes should begin with "X-"
%% to prevent inadvertent collision with registered attributes.  In
%% either case, if an attribute is received that is not understood, it
%% should simply be ignored by the receiver.
%% 
%% Suggested Attributes
%% 
%% The following attributes are suggested.  Since application writers
%% may add new attributes as they are required, this list is not
%% exhaustive.
%%
%% a=cat:<category>
%%   This attribute gives the dot-separated hierarchical category of
%%   the session.  This is to enable a receiver to filter unwanted
%%   sessions by category.  It would probably have been a compulsory
%%   separate field, except for its experimental nature at this time.
%%   It is a session-level attribute, and is not dependent on charset.
%%
%% a=keywds:<keywords>
%%   Like the cat attribute, this is to assist identifying wanted
%%   sessions at the receiver.  This allows a receiver to select
%%   interesting session based on keywords describing the purpose of
%%   the session.  It is a session-level attribute. It is a charset
%%   dependent attribute, meaning that its value should be interpreted
%%   in the charset specified for the session description if one is
%%   specified, or by default in ISO 10646/UTF-8.
%%
%% a=tool:<name and version of tool>
%%   This gives the name and version number of the tool used to create
%%   the session description.  It is a session-level attribute, and is
%%   not dependent on charset.
%%
%% a=ptime:<packet time>
%%   This gives the length of time in milliseconds represented by the
%%   media in a packet. This is probably only meaningful for audio
%%   data.  It should not be necessary to know ptime to decode RTP or
%%   vat audio, and it is intended as a recommendation for the
%%   encoding/packetisation of audio.  It is a media attribute, and is
%%   not dependent on charset.
%% 
%% a=recvonly
%%   This specifies that the tools should be started in receive-only
%%   mode where applicable. It can be either a session or media
%%   attribute, and is not dependent on charset.
%%
%% a=sendrecv
%%   This specifies that the tools should be started in send and
%%   receive mode.  This is necessary for interactive conferences with
%%   tools such as wb which defaults to receive only mode. It can be
%%   either a session or media attribute, and is not dependent on
%%   charset.
%%
%% a=sendonly
%%   This specifies that the tools should be started in send-only
%%   mode.  An example may be where a different unicast address is to
%%   be used for a traffic destination than for a traffic source. In
%%   such a case, two media descriptions may be use, one sendonly and
%%   one recvonly. It can be either a session or media attribute, but
%%   would normally only be used as a media attribute, and is not
%%   dependent on charset.
%%
%% a=orient:<whiteboard orientation>
%%   Normally this is only used in a whiteboard media specification.
%%   It specifies the orientation of a the whiteboard on the screen.
%%   It is a media attribute. Permitted values are `portrait',
%%   `landscape' and `seascape' (upside down landscape). It is not
%%   dependent on charset
%%
%% a=type:<conference type>
%%   This specifies the type of the conference.  Suggested values are
%%   `broadcast', `meeting', `moderated', `test' and `H332'.
%%   `recvonly' should be the default for `type:broadcast' sessions,
%%   `type:meeting' should imply `sendrecv' and `type:moderated'
%%   should indicate the use of a floor control tool and that the
%%   media tools are started so as to "mute" new sites joining the
%%   conference.
%%
%%   Specifying the attribute type:H332 indicates that this loosely
%%   coupled session is part of a H.332 session as defined in the ITU
%%   H.332 specification [10].  Media tools should be started
%%   `recvonly'.
%%
%%   Specifying the attribute type:test is suggested as a hint that,
%%   unless explicitly requested otherwise, receivers can safely avoid
%%   displaying this session description to users.
%%   
%%   The type attribute is a session-level attribute, and is not
%%   dependent on charset.
%% 
%% a=charset:<character set>
%%   This specifies the character set to be used to display the
%%   session name and information data.  By default, the ISO-10646
%%   character set in UTF-8 encoding is used. If a more compact
%%   representation is required, other character sets may be used such
%%   as ISO-8859-1 for Northern European languages.  In particular,
%%   the ISO 8859-1 is specified with the following SDP attribute:
%%   
%%                         a=charset:ISO-8859-1
%%   
%%   This is a session-level attribute; if this attribute is present,
%%   it must be before the first media field.  The charset specified
%%   MUST be one of those registered with IANA, such as ISO-8859-1.
%%   The character set identifier is a US-ASCII string and MUST be
%%   compared against the IANA identifiers using a case-insensitive
%%   comparison.  If the identifier is not recognised or not
%%   supported, all strings that are affected by it SHOULD be regarded
%%   as byte strings.
%%   
%%   Note that a character set specified MUST still prohibit the use
%%   of bytes 0x00 (Nul), 0x0A (LF) and 0x0d (CR). Character sets
%%   requiring the use of these characters MUST define a quoting
%%   mechanism that prevents these bytes appearing within text fields.
%%
%% a=sdplang:<language tag>
%%   This can be a session level attribute or a media level attribute.
%%   As a session level attribute, it specifies the language for the
%%   session description.  As a media level attribute, it specifies
%%   the language for any media-level SDP information field associated
%%   with that media.  Multiple sdplang attributes can be provided
%%   either at session or media level if multiple languages in the
%%   session description or media use multiple languages, in which
%%   case the order of the attributes indicates the order of
%%   importance of the various languages in the session or media from
%%   most important to least important.
%%   
%%   In general, sending session descriptions consisting of multiple
%%   languages should be discouraged.  Instead, multiple descriptions
%%   should be sent describing the session, one in each language.
%%   However this is not possible with all transport mechanisms, and
%%   so multiple sdplang attributes are allowed although not
%%   recommended.
%%   
%%   The sdplang attribute value must be a single RFC 1766 language
%%   tag in US-ASCII.  It is not dependent on the charset attribute.
%%   An sdplang attribute SHOULD be specified when a session is of
%%   sufficient scope to cross geographic boundaries where the
%%   language of recipients cannot be assumed, or where the session is
%%   in a different language from the locally assumed norm.
%% 
%% a=lang:<language tag>
%%   This can be a session level attribute or a media level attribute.
%%   As a session level attribute, it specifies the default language
%%   for the session being described.  As a media level attribute, it
%%   specifies the language for that media, overriding any session-
%%   level language specified.  Multiple lang attributes can be
%%   provided either at session or media level if multiple languages
%%   if the session description or media use multiple languages, in
%%   which case the order of the attributes indicates the order of
%%   importance of the various languages in the session or media from
%%   most important to least important.
%%
%%   The lang attribute value must be a single RFC 1766 language tag
%%   in US-ASCII. It is not dependent on the charset attribute.  A
%%   lang attribute SHOULD be specified when a session is of
%%   sufficient scope to cross geographic boundaries where the
%%   language of recipients cannot be assumed, or where the session is
%%   in a different language from the locally assumed norm.
%%
%% a=framerate:<frame rate>
%%   This gives the maximum video frame rate in frames/sec.  It is
%%   intended as a recommendation for the encoding of video data.
%%   Decimal representations of fractional values using the notation
%%   "<integer>.<fraction>" are allowed.  It is a media attribute, is
%%   only defined for video media, and is not dependent on charset.
%%
%% a=quality:<quality>
%%   This gives a suggestion for the quality of the encoding as an
%%   integer value.
%%   
%%   The intention of the quality attribute for video is to specify a
%%   non-default trade-off between frame-rate and still-image quality.
%%   For video, the value in the range 0 to 10, with the following
%%   suggested meaning:
%%   
%%   10 - the best still-image quality the compression scheme can
%%   give.
%%   
%%   5 - the default behaviour given no quality suggestion.
%%   
%%   0 - the worst still-image quality the codec designer thinks is
%%       still usable.
%%   
%%   It is a media attribute, and is not dependent on charset.
%% 
%% a=fmtp:<format> <format specific parameters>
%%   This attribute allows parameters that are specific to a
%%   particular format to be conveyed in a way that SDP doesn't have
%%   to understand them.  The format must be one of the formats
%%   specified for the media.  Format-specific parameters may be any
%%   set of parameters required to be conveyed by SDP and given
%%   unchanged to the media tool that will use this format.
%%
%%   It is a media attribute, and is not dependent on charset.
%% 

-record(megaco_sdp_a, {
	  attribute,                            % string()
	  value                                 % undefined | string()
	 }
       ).

-record(megaco_sdp_a_rtpmap, {
          payload_type,                         % integer()
          encoding_name,                        % string()
          clock_rate,                           % integer()
          encoding_parms = []                   % [ string() ]
         }
       ).
                
-record(megaco_sdp_a_ptime, {
          packet_time                           % integer()
         }
       ).

-record(megaco_sdp_a_quality, {
          quality                               % integer()
         }
       ).

-record(megaco_sdp_a_fmtp, {
          format,                               % string()
          param                                 % string()
         }
       ).




%% ===================================================================
%% 
%% Media Announcements
%% 
%% m=<media> <port> <transport> <fmt list>
%%
%% A session description may contain a number of media descriptions.
%% Each media description starts with an "m=" field, and is terminated
%% by either the next "m=" field or by the end of the session
%% description.  A media field also has several sub-fields:
%%
%% o The first sub-field is the media type.  Currently defined media are
%%   "audio", "video", "application", "data" and "control", though this
%%   list may be extended as new communication modalities emerge (e.g.,
%%   telepresense).  The difference between "application" and "data" is
%%   that the former is a media flow such as whiteboard information, and
%%   the latter is bulk-data transfer such as multicasting of program
%%   executables which will not typically be displayed to the user.
%%   "control" is used to specify an additional conference control
%%   channel for the session.
%%
%% o The second sub-field is the transport port to which the media
%%   stream will be sent.  The meaning of the transport port depends on
%%   the network being used as specified in the relevant "c" field and
%%   on the transport protocol defined in the third sub-field.  Other
%%   ports used by the media application (such as the RTCP port, see
%%   [2]) should be derived algorithmically from the base media port.
%%
%%   Note: For transports based on UDP, the value should be in the range
%%   1024 to 65535 inclusive.  For RTP compliance it should be an even
%%   number.
%%
%%   For applications where hierarchically encoded streams are being
%%   sent to a unicast address, it may be necessary to specify multiple
%%   transport ports.  This is done using a similar notation to that
%%   used for IP multicast addresses in the "c=" field:
%%
%%        m=<media> <port>/<number of ports> <transport> <fmt list>
%%
%%   In such a case, the ports used depend on the transport protocol.
%%   For RTP, only the even ports are used for data and the
%%   corresponding one-higher odd port is used for RTCP.  For example:
%%
%%                       m=video 49170/2 RTP/AVP 31
%%
%%   would specify that ports 49170 and 49171 form one RTP/RTCP pair and
%%   49172 and 49173 form the second RTP/RTCP pair.  RTP/AVP is the
%%   transport protocol and 31 is the format (see below).
%%
%%   It is illegal for both multiple addresses to be specified in the
%%   "c=" field and for multiple ports to be specified in the "m=" field
%%   in the same session description.
%% 
%% o The third sub-field is the transport protocol.  The transport
%%   protocol values are dependent on the address-type field in the "c="
%%   fields.  Thus a "c=" field of IP4 defines that the transport
%%   protocol runs over IP4.  For IP4, it is normally expected that most
%%   media traffic will be carried as RTP over UDP.  The following
%%   transport protocols are preliminarily defined, but may be extended
%%   through registration of new protocols with IANA:
%%
%%   - RTP/AVP - the IETF's Realtime Transport Protocol using the
%%     Audio/Video profile carried over UDP.
%%
%%   - udp - User Datagram Protocol
%%
%%   If an application uses a single combined proprietary media format
%%   and transport protocol over UDP, then simply specifying the
%%   transport protocol as udp and using the format field to distinguish
%%   the combined protocol is recommended.  If a transport protocol is
%%   used over UDP to carry several distinct media types that need to be
%%   distinguished by a session directory, then specifying the transport
%%   protocol and media format separately is necessary. RTP is an
%%   example of a transport-protocol that carries multiple payload
%%   formats that must be distinguished by the session directory for it
%%   to know how to start appropriate tools, relays, mixers or
%%   recorders.
%%
%%   The main reason to specify the transport-protocol in addition to
%%   the media format is that the same standard media formats may be
%%   carried over different transport protocols even when the network
%%   protocol is the same - a historical example is vat PCM audio and
%%   RTP PCM audio.  In addition, relays and monitoring tools that are
%%   transport-protocol-specific but format-independent are possible.
%%
%%   For RTP media streams operating under the RTP Audio/Video Profile
%%   [3], the protocol field is "RTP/AVP".  Should other RTP profiles be
%%   defined in the future, their profiles will be specified in the same
%%   way.  For example, the protocol field "RTP/XYZ" would specify RTP
%%   operating under a profile whose short name is "XYZ".
%% 
%% o The fourth and subsequent sub-fields are media formats.  For audio
%%   and video, these will normally be a media payload type as defined
%%   in the RTP Audio/Video Profile.
%%
%%   When a list of payload formats is given, this implies that all of
%%   these formats may be used in the session, but the first of these
%%   formats is the default format for the session.
%%
%%   For media whose transport protocol is not RTP or UDP the format
%%   field is protocol specific.  Such formats should be defined in an
%%   additional specification document.
%%
%%   For media whose transport protocol is RTP, SDP can be used to
%%   provide a dynamic binding of media encoding to RTP payload type.
%%   The encoding names in the RTP AV Profile do not specify unique
%%   audio encodings (in terms of clock rate and number of audio
%%   channels), and so they are not used directly in SDP format fields.
%%   Instead, the payload type number should be used to specify the
%%   format for static payload types and the payload type number along
%%   with additional encoding information should be used for dynamically
%%   allocated payload types.
%%
%%   An example of a static payload type is u-law PCM coded single
%%   channel audio sampled at 8KHz.  This is completely defined in the
%%   RTP Audio/Video profile as payload type 0, so the media field for
%%   such a stream sent to UDP port 49232 is:
%%
%%                         m=video 49232 RTP/AVP 0
%%
%%   An example of a dynamic payload type is 16 bit linear encoded
%%   stereo audio sampled at 16KHz.  If we wish to use dynamic RTP/AVP
%%   payload type 98 for such a stream, additional information is
%%   required to decode it:
%%
%%                        m=video 49232 RTP/AVP 98
%%                        a=rtpmap:98 L16/16000/2
%%
%%   The general form of an rtpmap attribute is:
%%
%%   a=rtpmap:<payload type> <encoding name>/<clock rate>[/<encoding
%%   parameters>]
%%
%%   For audio streams, <encoding parameters> may specify the number of
%%   audio channels.  This parameter may be omitted if the number of
%%   channels is one provided no additional parameters are needed.  For
%%   video streams, no encoding parameters are currently specified.
%%
%%   Additional parameters may be defined in the future, but
%%   codecspecific parameters should not be added.  Parameters added to
%%   an rtpmap attribute should only be those required for a session
%%   directory to make the choice of appropriate media too to
%%   participate in a session.  Codec-specific parameters should be
%%   added in other attributes.
%%
%%   Up to one rtpmap attribute can be defined for each media format
%%   specified. Thus we might have:
%%
%%                     m=audio 49230 RTP/AVP 96 97 98
%%                     a=rtpmap:96 L8/8000
%%                     a=rtpmap:97 L16/8000
%%                     a=rtpmap:98 L16/11025/2
%% 
%%   RTP profiles that specify the use of dynamic payload types must
%%   define the set of valid encoding names and/or a means to register
%%   encoding names if that profile is to be used with SDP.
%%
%%   Experimental encoding formats can also be specified using rtpmap.
%%   RTP formats that are not registered as standard format names must
%%   be preceded by "X-".  Thus a new experimental redundant audio
%%   stream called GSMLPC using dynamic payload type 99 could be
%%   specified as:
%%
%%                        m=video 49232 RTP/AVP 99
%%                        a=rtpmap:99 X-GSMLPC/8000
%%
%%   Such an experimental encoding requires that any site wishing to
%%   receive the media stream has relevant configured state in its
%%   session directory to know which tools are appropriate.
%%
%%   Note that RTP audio formats typically do not include information
%%   about the number of samples per packet.  If a non-default (as
%%   defined in the RTP Audio/Video Profile) packetisation is required,
%%   the "ptime" attribute is used as given below.
%%
%% o Formats for non-RTP media should be registered as MIME content
%%   types as described in Appendix B.  For example, the LBL whiteboard
%%   application might be registered as MIME content-type application/wb
%%   with encoding considerations specifying that it operates over UDP,
%%   with no appropriate file format.  In SDP this would then be
%%   expressed using a combination of the "media" field and the "fmt"
%%   field, as follows:
%%
%%                       m=application 32416 udp wb
%% 

%% ma_media() -> audio | video | application | data | control
-record(megaco_sdp_m, {
          media,         % ma_media() | string()
          port,          % integer()
          num_ports,     % undefined | integer()
          transport,     % string()
          fmt_list = []  % [ string() ]
         }).

          
-endif.
