#ifndef SFP_H
#define SFP_H

/* TO BIN */
#define BYTE_TO_BIN(byte) \
(byte & 0x0080 ? '1' : '0'), (byte & 0x0040 ? '1' : '0'), \
(byte & 0x0020 ? '1' : '0'), (byte & 0x0010 ? '1' : '0'), \
(byte & 0x0008 ? '1' : '0'), (byte & 0x0004 ? '1' : '0'), \
(byte & 0x0002 ? '1' : '0'), (byte & 0x0001 ? '1' : '0')

#define U16_TO_BIN(var) BYTE_TO_BIN(var>>8), BYTE_TO_BIN(var)
#define U24_TO_BIN(var) BYTE_TO_BIN(var>>16),U16_TO_BIN(var)
#define U32_TO_BIN(var) U16_TO_BIN(var>>16), U16_TO_BIN(var)

/* PRINT P */
#define BYTE_TO_BIN_P "%c%c%c%c%c%c%c%c"
#define U16_TO_BIN_P  BYTE_TO_BIN_P"_"BYTE_TO_BIN_P
#define U24_TO_BIN_P  BYTE_TO_BIN_P"_"U16_TO_BIN_P
#define U32_TO_BIN_P  U16_TO_BIN_P"_"U16_TO_BIN_P

typedef unsigned int sfp;

sfp int2sfp(int input);
int sfp2int(sfp input);
sfp float2sfp(float input);
float sfp2float(sfp input);

sfp sfp_add(sfp in1, sfp in2);
sfp sfp_mul(sfp in1, sfp in2);

#endif
