%% Generated by the Erlang ASN.1 compiler version:1.6.8
%% Purpose: Erlang record definitions for each named and unnamed
%% SEQUENCE and SET, and macro definitions for each value
%% definition,in module megaco_ber_bin_drv_media_gateway_control_v2



-record('TimeNotation',{
date, time}).

-record('H221NonStandard',{
t35CountryCode1, t35CountryCode2, t35Extension, manufacturerCode}). % with extension mark

-record('NonStandardData',{
nonStandardIdentifier, data}).

-record('StatisticsParameter',{
statName, statValue = asn1_NOVALUE}).

-record('PackagesItem',{
packageName, packageVersion}). % with extension mark

-record('ServiceChangeProfile',{
profileName}).

-record('ServiceChangeResParm',{
serviceChangeMgcId = asn1_NOVALUE, serviceChangeAddress = asn1_NOVALUE, serviceChangeVersion = asn1_NOVALUE, serviceChangeProfile = asn1_NOVALUE, timestamp = asn1_NOVALUE}). % with extension mark

-record('ServiceChangeParm',{
serviceChangeMethod, serviceChangeAddress = asn1_NOVALUE, serviceChangeVersion = asn1_NOVALUE, serviceChangeProfile = asn1_NOVALUE, serviceChangeReason, serviceChangeDelay = asn1_NOVALUE, serviceChangeMgcId = asn1_NOVALUE, timeStamp = asn1_NOVALUE, nonStandardData = asn1_NOVALUE,
%% with extensions
serviceChangeInfo = asn1_NOVALUE}).

-record('DigitMapValue',{
startTimer = asn1_NOVALUE, shortTimer = asn1_NOVALUE, longTimer = asn1_NOVALUE, digitMapBody,
%% with extensions
durationTimer = asn1_NOVALUE}).

-record('DigitMapDescriptor',{
digitMapName = asn1_NOVALUE, digitMapValue = asn1_NOVALUE}).

-record('ModemDescriptor',{
mtl, mpl, nonStandardData = asn1_NOVALUE}).

-record('SigParameter',{
sigParameterName, value, extraInfo = asn1_NOVALUE}). % with extension mark

-record('Signal',{
signalName, streamID = asn1_NOVALUE, sigType = asn1_NOVALUE, duration = asn1_NOVALUE, notifyCompletion = asn1_NOVALUE, keepActive = asn1_NOVALUE, sigParList}). % with extension mark

-record('SeqSigList',{
id, signalList}).

-record('EventSpec',{
eventName, streamID = asn1_NOVALUE, eventParList}). % with extension mark

-record('SecondRequestedActions',{
keepActive = asn1_NOVALUE, eventDM = asn1_NOVALUE, signalsDescriptor = asn1_NOVALUE}). % with extension mark

-record('SecondRequestedEvent',{
pkgdName, streamID = asn1_NOVALUE, eventAction = asn1_NOVALUE, evParList}). % with extension mark

-record('SecondEventsDescriptor',{
requestID = asn1_NOVALUE, eventList}). % with extension mark

-record('RequestedActions',{
keepActive = asn1_NOVALUE, eventDM = asn1_NOVALUE, secondEvent = asn1_NOVALUE, signalsDescriptor = asn1_NOVALUE}). % with extension mark

-record('RequestedEvent',{
pkgdName, streamID = asn1_NOVALUE, eventAction = asn1_NOVALUE, evParList}). % with extension mark

-record('EventsDescriptor',{
requestID = asn1_NOVALUE, eventList}). % with extension mark

-record('MuxDescriptor',{
muxType, termList, nonStandardData = asn1_NOVALUE}). % with extension mark

-record('TerminationStateDescriptor',{
propertyParms, eventBufferControl = asn1_NOVALUE, serviceState = asn1_NOVALUE}). % with extension mark

-record('LocalRemoteDescriptor',{
propGrps}). % with extension mark

-record('PropertyParm',{
name, value, extraInfo = asn1_NOVALUE}). % with extension mark

-record('LocalControlDescriptor',{
streamMode = asn1_NOVALUE, reserveValue = asn1_NOVALUE, reserveGroup = asn1_NOVALUE, propertyParms}). % with extension mark

-record('StreamParms',{
localControlDescriptor = asn1_NOVALUE, localDescriptor = asn1_NOVALUE, remoteDescriptor = asn1_NOVALUE}). % with extension mark

-record('StreamDescriptor',{
streamID, streamParms}).

-record('MediaDescriptor',{
termStateDescr = asn1_NOVALUE, streams = asn1_NOVALUE}). % with extension mark

-record('TerminationID',{
wildcard, id}). % with extension mark

-record('ServiceChangeReply',{
terminationID, serviceChangeResult}). % with extension mark

-record('ServiceChangeRequest',{
terminationID, serviceChangeParms}). % with extension mark

-record('EventParameter',{
eventParameterName, value, extraInfo = asn1_NOVALUE}). % with extension mark

-record('ObservedEvent',{
eventName, streamID = asn1_NOVALUE, eventParList, timeNotation = asn1_NOVALUE}). % with extension mark

-record('ObservedEventsDescriptor',{
requestId, observedEventLst}).

-record('NotifyReply',{
terminationID, errorDescriptor = asn1_NOVALUE}). % with extension mark

-record('NotifyRequest',{
terminationID, observedEventsDescriptor, errorDescriptor = asn1_NOVALUE}). % with extension mark

-record('IndAudPackagesDescriptor',{
packageName, packageVersion}). % with extension mark

-record('IndAudStatisticsDescriptor',{
statName}).

-record('IndAudDigitMapDescriptor',{
digitMapName = asn1_NOVALUE}).

-record('IndAudSignal',{
signalName, streamID = asn1_NOVALUE}). % with extension mark

-record('IndAudSeqSigList',{
id, signalList = asn1_NOVALUE}).

-record('IndAudEventBufferDescriptor',{
eventName, streamID = asn1_NOVALUE}). % with extension mark

-record('IndAudEventsDescriptor',{
requestID = asn1_NOVALUE, pkgdName, streamID = asn1_NOVALUE}). % with extension mark

-record('IndAudTerminationStateDescriptor',{
propertyParms, eventBufferControl = asn1_NOVALUE, serviceState = asn1_NOVALUE}). % with extension mark

-record('IndAudLocalRemoteDescriptor',{
propGroupID = asn1_NOVALUE, propGrps}). % with extension mark

-record('IndAudPropertyParm',{
name}). % with extension mark

-record('IndAudLocalControlDescriptor',{
streamMode = asn1_NOVALUE, reserveValue = asn1_NOVALUE, reserveGroup = asn1_NOVALUE, propertyParms = asn1_NOVALUE}). % with extension mark

-record('IndAudStreamParms',{
localControlDescriptor = asn1_NOVALUE, localDescriptor = asn1_NOVALUE, remoteDescriptor = asn1_NOVALUE}). % with extension mark

-record('IndAudStreamDescriptor',{
streamID, streamParms}).

-record('IndAudMediaDescriptor',{
termStateDescr = asn1_NOVALUE, streams = asn1_NOVALUE}). % with extension mark

-record('AuditDescriptor',{
auditToken = asn1_NOVALUE,
%% with extensions
auditPropertyToken = asn1_NOVALUE}).

-record('AuditResult',{
terminationID, terminationAuditResult}).

-record('AuditRequest',{
terminationID, auditDescriptor}). % with extension mark

-record('SubtractRequest',{
terminationID, auditDescriptor = asn1_NOVALUE}). % with extension mark

-record('AmmsReply',{
terminationID, terminationAudit = asn1_NOVALUE}). % with extension mark

-record('AmmRequest',{
terminationID, descriptors}). % with extension mark

-record('TopologyRequest',{
terminationFrom, terminationTo, topologyDirection,
%% with extensions
streamID = asn1_NOVALUE}).

-record('CommandRequest',{
command, optional = asn1_NOVALUE, wildcardReturn = asn1_NOVALUE}). % with extension mark

-record('ContextAttrAuditRequest',{
topology = asn1_NOVALUE, emergency = asn1_NOVALUE, priority = asn1_NOVALUE}). % with extension mark

-record('ContextRequest',{
priority = asn1_NOVALUE, emergency = asn1_NOVALUE, topologyReq = asn1_NOVALUE}). % with extension mark

-record('ActionReply',{
contextId, errorDescriptor = asn1_NOVALUE, contextReply = asn1_NOVALUE, commandReply}).

-record('ActionRequest',{
contextId, contextRequest = asn1_NOVALUE, contextAttrAuditReq = asn1_NOVALUE, commandRequests}).

-record('ErrorDescriptor',{
errorCode, errorText = asn1_NOVALUE}).

-record('TransactionAck',{
firstAck, lastAck = asn1_NOVALUE}).

-record('TransactionReply',{
transactionId, immAckRequired = asn1_NOVALUE, transactionResult}). % with extension mark

-record('TransactionPending',{
transactionId}). % with extension mark

-record('TransactionRequest',{
transactionId, actions}). % with extension mark

-record('IP6Address',{
address, portNumber = asn1_NOVALUE}).

-record('IP4Address',{
address, portNumber = asn1_NOVALUE}).

-record('DomainName',{
name, portNumber = asn1_NOVALUE}).

-record('Message',{
version, mId, messageBody}). % with extension mark

-record('AuthenticationHeader',{
secParmIndex, seqNum, ad}).

-record('MegacoMessage',{
authHeader = asn1_NOVALUE, mess}).

