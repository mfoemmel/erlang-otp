package jive.erlang;

import java.net.*;
import java.io.*;

/**
 * Unpacker is a class that parses messages sent from the Erlang side.
 * The class transforms the messages into EVar objects depending of the
 * type of variable.
 *
 * <p>Accepted variable types are:
 * <tt>EString, EInteger, EFloat, EAtom, EList, ETuple, EProcess, EBinary
 * </tt>
 *
 * <p>The Unpacker class will either parse an expression and return the EVar or
 * it will parse a message consisting of a receiver and a message and then
 * deliver the message to a given receiver.
 *
 * @version     1.2, 8 Mar 1997
 * @author      Kaj Nygren (kaj@medialab.ericsson.se)
 * @author      Joakim Greben&ouml; (jocke@erix.ericsson.se)
 */
public class Unpacker {
  InputStream input;
  EReceiver receiver;
  public Socket s;
  private static final int EERROR = 60;
  private static final int ERROR_SECURITY = 1;
  private static final int ERROR_IO = 2;

  /**
   * Creates an Unpacker that parses messages from the
   * Erlang side and deliver them to a receiver on the Java side. 
   */
  public Unpacker(InputStream input,EReceiver receiver) {
    this.input = input;
    this.receiver = receiver;
  }
  
  /** 
   * This method parses the input stream for an Erlang string and
   * returns an EString object containing the string.
   * <p>If the tag parameter is true it will expect an ESTRING tag
   * first in the stream, otherwise it will assume that the stream contains
   * a valid string.
   *
   * @exception JiveException if a connection or parsing error occurred.
   */
  public synchronized EString getString(boolean tag) throws JiveException {
    if (tag) 
      if (getByte() != EVar.ESTRING) 
	throw new JiveException("Error when parsing message: String expected");
    int length = getInt();
    if (length > 0) {
      byte buffer[]=new byte[length];
      getBytes(buffer);
      return new EString(byteToString(buffer));
    } else
      return new EString();
  }
  
  /** 
   * This method parses the input stream for an Erlang atom and
   * returns an EAtom object containing the atom.
   * <p>If the tag parameter is true it will expect an EATOM tag
   * first in the stream, otherwise it will assume that the stream contains
   * a valid atom.
   *
   * @exception JiveException if a connection or parsing error occurred.
   */
  public synchronized EAtom getAtom(boolean tag) throws JiveException {
    if (tag) 
      if (getByte() != EVar.EATOM) 
	throw new JiveException("Error when parsing message: Atom expected");
    int length = getInt();
    if (length > 0) {
      byte buffer[]=new byte[length];
      getBytes(buffer);
      return new EAtom(byteToString(buffer));
    } else
      return new EAtom();
  }
  
  /** 
   * This method parses the input stream for an Erlang float and
   * returns an EFloat object containing the float.
   * <p>If the tag parameter is true it will expect an EFLOAT tag
   * first in the stream, otherwise it will assume that the stream contains
   * a valid float.
   *
   * @exception JiveException if a connection or parsing error occurred.
   */
  public synchronized EFloat getFloat(boolean tag) throws JiveException {
    if (tag) 
      if (getByte() != EVar.EFLOAT) 
	throw new JiveException("Error when parsing message: Float expected");
    int length = getInt();
    byte buffer[]=new byte[length];
    getBytes(buffer);
    try {
      double d = Double.valueOf(byteToString(buffer)).doubleValue();
      return new EFloat(d);
    } catch (NumberFormatException e) {
      throw new JiveException("Error when parsing message: Float conversion error");
    }      
  }

  /** 
   * This method parses the input stream for an Erlang integer and
   * returns an EInteger object containing the integer.
   * <p>If the tag parameter is true it will expect an EINTEGER tag
   * first in the stream, otherwise it will assume that the stream contains
   * a valid integer.
   *
   * @exception JiveException if a connection or parsing error occurred.
   */
  public synchronized EInteger getInteger(boolean tag) throws JiveException {
    if (tag) 
      if (getByte() != EVar.EINTEGER) 
	throw new JiveException("Error when parsing message: Integer expected");
    int tmp = getInt();
    return new EInteger(tmp);
  }

  /** 
   * This method parses the input stream for an Erlang pid-id and
   * returns a EProcess object containing the pid-id.
   * <p>The pid-id is NOT the same as an Erlang pid. Instead it is an id-value
   * used internally by the jive Erlang server.
   * <p>If the tag parameter is true it will expect an EPROCESS tag
   * first in the stream, otherwise it will assume that the stream contains
   * a valid pid-id.
   *
   * @exception JiveException if a connection or parsing error occurred.
   */
  public synchronized EProcess getProcess(boolean tag) throws JiveException {
    if (tag) 
      if (getByte() != EVar.EPROCESS) 
	throw new JiveException("Error when parsing message: Process expected");
    int tmp = getInt();
    return new EProcess(tmp);
  }

  /** 
   * This method parses the input stream for an Erlang tuple and
   * returns a ETuple object containing the tuple.
   * <p>If the tag parameter is true it will expect an ETUPLE tag
   * first in the stream, otherwise it will assume that the stream contains
   * a valid tuple.
   *
   * @exception JiveException if a connection or parsing error occurred.
   */
  public synchronized ETuple getTuple(boolean tag) throws JiveException {
    if (tag) 
      if (getByte() != EVar.ETUPLE) 
	throw new JiveException("Error when parsing message: Tuple expected");
    int length = getInt();
    EVar args[] = new EVar[length];
    for (int i=0;i<length;i++) 
      args[i] = (EVar)parseExpression();
    return new ETuple(args);
  }

  /** 
   * This method parses the input stream for an Erlang list and
   * returns a EList object containing the list.
   * <p>If the tag parameter is true it will expect an ELIST tag
   * first in the stream, otherwise it will assume that the stream contains
   * a valid list.
   *
   * @exception JiveException if a connection or parsing error occurred.
   */
  public synchronized EList getList(boolean tag) throws JiveException {
    if (tag) 
      if (getByte() != EVar.ELIST) 
	throw new JiveException("Error when parsing message: List expected");
    int length = getInt();
    EVar args[] = new EVar[length];
    for (int i=0;i<length;i++) 
      args[i] = (EVar)parseExpression();
    return new EList(args);
  }

  /** 
   * This method parses the input stream for an Erlang binary and
   * returns a EBinary object containing the binary.
   * <p>If the tag parameter is true it will expect an ELIST tag
   * first in the stream, otherwise it will assume that the stream contains
   * a valid list.
   *
   * @exception JiveException if a connection or parsing error occurred.
   */
  public synchronized EBinary getBinary(boolean tag) throws JiveException {
    if (tag) 
      if (getByte() != EVar.EBINARY) 
	throw new JiveException("Error when parsing message: Binary expected");
    int length = getInt();
    if (length > 0) {
      byte buffer[]=new byte[length];
      getBytes(buffer);
      return new EBinary(buffer);
    } else
      return new EBinary();
  }

  /** 
   * This method parses the input stream for an Erlang reference and
   * returns a EReference object containing the reference.
   * <p>If the tag parameter is true it will expect an ELIST tag
   * first in the stream, otherwise it will assume that the stream contains
   * a valid list.
   *
   * @exception JiveException if a connection or parsing error occurred.
   */
  public synchronized EReference getReference(boolean tag) throws JiveException {
    if (tag) 
      if (getByte() != EVar.EREFERENCE) 
	throw new JiveException("Error when parsing message: Reference expected");
    int length = getInt();
    if (length > 0) {
      byte buffer[]=new byte[length];
      getBytes(buffer);
      return new EReference(buffer);
    } else
      return new EReference();
  }

  /** 
   * This method parses the input stream for an Erlang port and
   * returns a EPort object containing the port.
   * <p>If the tag parameter is true it will expect an ELIST tag
   * first in the stream, otherwise it will assume that the stream contains
   * a valid list.
   *
   * @exception JiveException if a connection or parsing error occurred.
   */
  public synchronized EPort getPort(boolean tag) throws JiveException {
    if (tag) 
      if (getByte() != EVar.EPORT) 
	throw new JiveException("Error when parsing message: Port expected");
    int length = getInt();
    if (length > 0) {
      byte buffer[]=new byte[length];
      getBytes(buffer);
      return new EPort(buffer);
    } else
      return new EPort();
  }

  /** 
   * This method parses the input stream for an Erlang pid and
   * returns a EPid object containing the pid.
   * <p>If the tag parameter is true it will expect an ELIST tag
   * first in the stream, otherwise it will assume that the stream contains
   * a valid list.
   *
   * @exception JiveException if a connection or parsing error occurred.
   */
  public synchronized EPid getPid(boolean tag) throws JiveException {
    if (tag) 
      if (getByte() != EVar.EPID) 
	throw new JiveException("Error when parsing message: Pid expected");
    int length = getInt();
    if (length > 0) {
      byte buffer[]=new byte[length];
      getBytes(buffer);
      return new EPid(buffer);
    } else
      return new EPid();
  }

  /** 
   * This method parses the input stream for an error message and
   * throws a JiveException with a given message
   * <p>If the tag parameter is true it will expect an EERROR tag
   * first in the stream, otherwise it will assume that the stream contains
   * a valid error message.
   *
   * @exception JiveSecurityException is thrown if a security violation
   * occurred.
   * @exception JiveIOException is thrown if a communication error occurred.
   * @exception JiveException is thrown with an error text given by the server.
   */
  public synchronized void getError(boolean tag) throws JiveSecurityException,JiveIOException,JiveException {
    if (tag) 
      if (getByte() != EERROR) 
	throw new JiveException("Error when parsing message");
    int errNo = getByte(); // Not used at the moment
    EString errMsg = getString(true);
    // Throw a correct Exception
    switch(errNo) {
    case ERROR_SECURITY:
      throw new JiveSecurityException(errMsg.value());
    case ERROR_IO:
      throw new JiveIOException(errMsg.value());
    default:
      throw new JiveException(errMsg.value());
    }
  }
  
  /** 
   * This method parses an expression from the input stream and returns
   * the expression as an EVar object.
   *
   * @exception JiveSecurityException is thrown if a security violation
   * occurred.
   * @exception JiveIOException is thrown if a communication error occurred.
   * @exception JiveException is thrown with an error text given by the server.
   */
  public synchronized EVar parseExpression() throws JiveSecurityException,JiveIOException,JiveException {
    byte type = (byte)getByte();
    switch (type) {
    case EVar.ESTRING:
      return (EVar)getString(false);
    case EVar.EATOM:
      return (EVar)getAtom(false);
    case EVar.EFLOAT:
      return (EVar)getFloat(false);
    case EVar.EINTEGER:
      return (EVar)getInteger(false);
    case EVar.EPROCESS:
      return (EVar)getProcess(false);
    case EVar.ETUPLE:
      return (EVar)getTuple(false);
    case EVar.ELIST:
      return (EVar)getList(false);
    case EVar.EBINARY:
      return (EVar)getBinary(false);
    case EVar.EREFERENCE:
      return (EVar)getReference(false);
    case EVar.EPORT:
      return (EVar)getPort(false);
    case EVar.EPID:
      return (EVar)getPid(false);
    case EERROR:
      getError(false);
    }
    throw new JiveException("Error when parsing message: Unknown tag found "+
			    type );
  }
  
  /**
   * Parses a message sent from the Erlang server to a client.
   * First the receiver of the message is read from the input stream, then
   * the Erlang expression is read and converted to an EVar object.
   * Finally the EVar object is delivered to the recipient.
   *
   * @exception JiveSecurityException is thrown if a security violation
   * occurred.
   * @exception JiveIOException is thrown if a communication error occurred.
   * @exception JiveException is thrown with an error text given by the server.
   */
  public synchronized void parseMessage() throws JiveSecurityException,JiveIOException,JiveException {
    getInt(); // Skip message length
    EInteger receiverID = getInteger(true);
    EVar message = parseExpression();
    if (message != null) {
      EReceive target = receiver.toTarget(receiverID);
      if (target != null) 
	// A thread is no longer started to receive messages.
	target.receive(message);
        //new ReceiveThread(target, message);
    }
  }
  
  /**
   * Parses a reply sent from the Erlang server to a client.
   * The reply is either an Erlang variable, which is returned as an EVar,
   * or an error message which causes a JiveException to be thrown.
   *
   * @exception JiveSecurityException is thrown if a security violation
   * occurred.
   * @exception JiveIOException is thrown if a communication error occurred.
   * @exception JiveException is thrown with an error text given by the server.
   */
  public synchronized EVar parseReply() throws JiveSecurityException,JiveIOException,JiveException {
    getInt(); // Skip message length
    return parseExpression();
  }
  
  /** 
   * Performs a non-blocking read of a bytearray. This is really awful
   * but Netscape on Windows95 doesn't handle non-blocking IO.
   *
   * <p>When this bug is fixed in Netscape this method will be patched.
   *
   * @exception JiveIOException is thrown if a communication error occurred.
   */

  /* The Socket buffer in Netscape/Win95 only is 4kB! We therefore read
     1kB/read to avoid this nasty bug. This routine should be replaced by
     the commented one when the bug is gone! */

private synchronized void getBytes(byte buffer[]) throws JiveIOException
{
    int block = 1024;
    int bytesLeft =  buffer.length;
    int read;
    
    try {
	while (bytesLeft > 0) {
	    read = 0;
	    if (bytesLeft < block) 
		block = bytesLeft;
	    while (input.available() < block) {
		try {
		    Thread.sleep(100);
		} catch(InterruptedException e) {}
	    }
	    while (read < block)
	    {
		read += input.read(buffer, buffer.length - bytesLeft + read,
				   block - read);
	    }
	    bytesLeft -= block;
	}
    } catch (IOException ioE) {
	throw new JiveIOException(ioE.getMessage());
    }
}

  /** 
   * Performs a non-blocking read of a byte. This is really awful
   * but Netscape on Windows95 doesn't handle non-blocking IO.
   *
   * <p>When this bug is fixed in Netscape this method will be patched.
   *
   * @exception JiveIOException is thrown if a communication error occurred.
   */
  private synchronized int getByte() throws JiveIOException {
    try {
      // Avoid blocking 
      while (input.available() == 0) {
	try {
	  Thread.sleep(100);
	} catch (InterruptedException e) {}
      }
      
      // Now we have data in stream
      return input.read();
    } catch (IOException ioE) {
      throw new JiveIOException(ioE.getMessage());
    }
  }

  /**
   * Reads a Java int from the input stream,
   * i.e. Four bytes, highest order first in the stream.
   *
   * @exception JiveIOException is thrown if a communication error occurred.
   */
  private synchronized int getInt() throws JiveIOException {
    byte buffer[]=new byte[4];
    getBytes(buffer);
    int tmp = unsigned(buffer[0]) << 24 | unsigned(buffer[1]) << 16 |
              unsigned(buffer[2]) << 8  | unsigned(buffer[3]);
    return tmp;
  }

  /** 
   * Converts a signed byte to a unsigned int.
   * Used by getInt to construct a valid int from 4 bytes.
   */
  private int unsigned(byte b) {
    int i=(int)b;
    return (256+i)%256;
  }
  
  /**
   * Converts a byte array to a Java String.
   */
  static private String byteToString(byte text[]) {
    return new String(text,0,0,text.length);
  }
}
