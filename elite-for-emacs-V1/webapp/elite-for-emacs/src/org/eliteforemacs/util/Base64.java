package org.eliteforemacs.util;
/*
 * Does Base64 encoding/decoding as described in section (6.8.) of RFC 2045.
 */
public class Base64 {

/**
 * the Base64 translation table; does the same as 'encode()', but
 * this way decoding should be faster.
 */
public final static char[] translationTable = new char[]{
'A','B','C','D','E','F','G','H','I','J','K','L','M',
'N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
'a','b','c','d','e','f','g','h','i','j','k','l','m',
'n','o','p','q','r','s','t','u','v','w','x','y','z',
'0','1','2','3','4','5','6','7','8','9','+','/'
};

/* line break characters for use in encoding output */
private static char[] lbcs = {'\r','\n'};

/**
 * Encode string to Base64 formatted char array.
 * @param text This argument must contain the text or binary data that shall be
 *             encoded. This version of the 'encode()' method accepts a String and
 *             converts all line breaks to CRLF (or whatever else you have
 *             defined by using 'setLbcs()') prior to encoding.
 *             "CRLF" is equal to the Java string "\r\n".
 */
public static char[] encode(String text) {
if(text == null) { return null; }
return encode(convertCRLF((String)text).toCharArray());
}

/**
 * Encode char array to Base64 formatted char array.
 * @param text This argument must contain the text or binary data that shall be
 *             encoded. This version of the 'encode()' method accepts a char array
 *             as input and will - in contrast to the other 'encode()' method -
 *             not try to do any conversion at all.
 */
public static char[] encode(char[] text) {
if(text == null) { return null; }
char[] a = text;

/* we first need to calculate the size of our base64 encoded array */
int size = a.length / 3 * 4;
if(a.length % 3 > 0) {
size += 4;
}
/* + line break characters */
if(size % 68 > 0) {
  size += (size / 68 + 1) * lbcs.length;
} else {
size += (size / 68) * lbcs.length;
}

/* encoding... */
char[] b = new char[size];
int p = 0;
int wp = 0;
int lc = 0;
int[] y = new int[4];
/* encode 24 bit entities */
while(p / 3 < a.length / 3) {
y[0] = (a[p] & 252) >>> 2;
y[1] = (a[p] & 3) << 4 | (a[p+1] & 240) >>> 4;
y[2] = (a[p+1] & 15) << 2 | (a[p+2] & 192) >>> 6;
y[3] = a[p+2] & 63;
for(int i = 0; i < 4; i++) {
b[wp++] = translationTable[y[i]];
}
lc++;
/* 68 characters per line */
if(lc == 17) {
for(int i = 0; i < lbcs.length; i++) {
b[wp++] = lbcs[i];
}
lc = 0;
}
p += 3;
}
/* encode remaining partial entities (8 or 16 bits) */
if(a.length - p == 1) {
y[0] = (a[p] & 252) >>> 2;
y[1] = (a[p] & 3) << 4;
for(int i = 0; i < 2; i++) {
b[wp++] = translationTable[y[i]];
}
b[wp++] = '=';
b[wp++] = '=';
lc++;
} else
if(a.length - p == 2) {
y[0] = (a[p] & 252) >>> 2;
y[1] = (a[p] & 3) << 4 | (a[p+1] & 240) >>> 4;
y[2] = (a[p+1] & 15) << 2;
for(int i = 0; i < 3; i++) {
b[wp++] = translationTable[y[i]];
}
b[wp++] = '=';
lc++;
}
/* don't forget final line break */
if(lc > 0) {
for(int i = 0; i < lbcs.length; i++) {
b[wp++] = lbcs[i];
}
}
return b;
}

/**
 * Decodes Base64 formatted byte array or string into a char array.
 * @param text This argument must contain the text or data as a string
 *             object which shall be decoded.
 */
public static char[] decode(String text) {
return decode(text.toCharArray());
}

public static byte[] decodeToByteArray(String text) {
return charArrayToByteArray(decode(text.toCharArray()));
}

public static byte[] decodeToByteArray(char[] text) {
return charArrayToByteArray(decode(text));
}

/**
 * Decodes Base64 formatted byte array or string into a char array.
 * @param text This argument must contain the text or data as a char
 *             array object which shall be decoded.
 */
public static char[] decode(char[] text) {
if(text == null) { return null; }
char[] a = text;

/* remove all junk */
int p = 0;  // read position
int wp = 0; // write position
while(p < a.length && a[p] != '=') {
if(a[p] >= 'a' && a[p] <= 'z' || a[p] >= 'A' && a[p] <= 'Z' ||
   a[p] >= '0' && a[p] <= '9' || a[p] == '+' || a[p] == '/') {
a[wp++] = a[p];
}
p++;
}
int len = wp;

/* now calculate the size of our decoded byte array */
int size = len / 4 * 3;
if(len % 4 == 2) {
size++;
} else
if(len % 4 == 3) {
size += 2;
}

/* decoding... */
char[] b = new char[size];
int rp = 0;
wp = 0;
while(rp/4 < len/4) {
b[wp++] = (char) (decode(a[rp]) << 2 | (decode(a[rp+1]) & 48) >> 4);
b[wp++] = (char) ((decode(a[rp+1]) & 15) << 4 | (decode(a[rp+2]) & 60) >> 2);
b[wp++] = (char) ((decode(a[rp+2]) & 3) << 6 | decode(a[rp+3]));
rp += 4;
}
/* is there at least one more character to decode? */
if(len - rp >= 2) {
b[wp++] = (char) (decode(a[rp]) << 2 | (decode(a[rp+1]) & 48) >> 4);
}
/* is there a second chracter to decode? */
 if(len - rp == 3) {
b[wp++] = (char) ((decode(a[rp+1]) & 15) << 4 | (decode(a[rp+2]) & 60) >> 2);
}
return b;
}

/**
 * Decodes a Base64 character into its 6-bit value (0..63).
 */
public static char decode(char a) {
if(a >= 'A' && a <= 'Z') {
return (char) (a-'A');
}
if(a >= 'a' && a <= 'z') {
return (char) (26+a-'a');
}
if(a >= '0' && a <= '9') {
return (char) (52+a-'0');
}
if(a == '+') {
return (char) 62;
}
return (char) 63;
}

/**
 * Encodes a 6-bit value (0..63) into a Base64 character.
 */
public static char encode(int a) {
if(a >= 0 && a <= 25) {
return (char) (a+'A');
}
if(a >= 26 && a <= 51) {
return (char) (a-26+'a');
}
if(a >= 52 && a <= 61) {
return (char) (a-52+'0');
}
if(a == 62) {
return (char) '+';
}
return (char) '/';
}

/**
 * set the line break chracters for use in encoding output. Default is
 * '\r\n'.
 */
public static void setLbcs(char[] l) {
lbcs = l;
}

/**
 * convert '\n'-line breaks to '\r\n'-line breaks.
 */
private static String convertCRLF(String text) {
if(text.length() == 0) {
return text;
}
StringBuffer t = new StringBuffer(text);
int p = 0;
if(t.charAt(p) == '\n') {
t.insert(p,'\r');
p++;
}
p++;
while( p < t.length() ) {
if(t.charAt(p) == '\n' && t.charAt(p-1) != '\r') {
t.insert(p,'\r');
p++;
}
p++;
}
return t.toString();
}

private static byte[] charArrayToByteArray(char[] a) {
if(a == null || a.length == 0) { return null; }
byte[] b = new byte[a.length];
for(int i = 0; i < a.length; i++) {
b[i] = (byte) a[i];
}
return b;
}
}
