#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "sfp.h"

int main(){
	char type[2];
	int input_i[2];
	union _union{
		unsigned int ui;
		float f;
	} input_f[2];
	sfp input_sfp[2];
	sfp add_sfp;
	sfp mul_sfp;

	scanf("%c ", &type[0]);
	if (type[0] == 'i') {
		scanf("%d ", &input_i[0]);
	} else if (type[0] == 'f') {
		scanf("%f ", &input_f[0].f);
	} else{
		printf("Invalid type - 1\n");
		return 0;
	}

	scanf("%c ", &type[1]);
	if (type[1] == 'i')
		scanf("%d", &input_i[1]);
	else if (type[1] == 'f')
		scanf("%f", &input_f[1].f);
	else{
		printf("Invalid type - 2\n");
		return 0;
	}

	printf("[ Test 1: Type Conversion ]\n");

	if(type[0] == 'i') 
		input_sfp[0] = int2sfp(input_i[0]);
	else 
		input_sfp[0] = float2sfp(input_f[0].f);

	if(type[1] == 'i') 
		input_sfp[1] = int2sfp(input_i[1]);
	else 
		input_sfp[1] = float2sfp(input_f[1].f);

	if(type[0] == 'i') 
		printf("    INT   "U32_TO_BIN_P" is converted into SFP   "U24_TO_BIN_P"\n",
			 U32_TO_BIN(input_i[0]), U24_TO_BIN(input_sfp[0]));
	else 
		printf("    FLOAT "U32_TO_BIN_P" is converted into SFP   "U24_TO_BIN_P"\n",
			 U32_TO_BIN(input_f[0].ui), U24_TO_BIN(input_sfp[0]));

	if(type[1] == 'i') 
		printf("    INT   "U32_TO_BIN_P" is converted into SFP   "U24_TO_BIN_P"\n",
		 	U32_TO_BIN(input_i[1]), U24_TO_BIN(input_sfp[1]));
	else 
		printf("    FLOAT "U32_TO_BIN_P" is converted into SFP   "U24_TO_BIN_P"\n",
			 U32_TO_BIN(input_f[1].ui), U24_TO_BIN(input_sfp[1]));

	if(type[0] == 'i') 
		input_i[0] = sfp2int(input_sfp[0]);
	else 
		input_f[0].f = sfp2float(input_sfp[0]);

	if(type[1] == 'i') 
		input_i[1] = sfp2int(input_sfp[1]);
	else 
		input_f[1].f = sfp2float(input_sfp[1]);

	if(type[0] == 'i') 
		printf("    SFP   "U24_TO_BIN_P" is converted into INT   "U32_TO_BIN_P"\n", 
			U24_TO_BIN(input_sfp[0]), U32_TO_BIN(input_i[0]));
	else 
		printf("    SFP   "U24_TO_BIN_P" is converted into FLOAT "U32_TO_BIN_P"\n", 
			U24_TO_BIN(input_sfp[0]), U32_TO_BIN(input_f[0].ui));

	if(type[1] == 'i') 
		printf("    SFP   "U24_TO_BIN_P" is converted into INT   "U32_TO_BIN_P"\n", 
			U24_TO_BIN(input_sfp[1]), U32_TO_BIN(input_i[1]));
	else 
		printf("    SFP   "U24_TO_BIN_P" is converted into FLOAT "U32_TO_BIN_P"\n",
			 U24_TO_BIN(input_sfp[1]), U32_TO_BIN(input_f[1].ui));

	printf("\n[ Test 2: Addition ]\n");

	add_sfp = sfp_add(input_sfp[0], input_sfp[1]);
	printf("    "U24_TO_BIN_P" + "U24_TO_BIN_P" = "U24_TO_BIN_P"\n",
		 U24_TO_BIN(input_sfp[0]), U24_TO_BIN(input_sfp[1]), U24_TO_BIN(add_sfp));

	printf("\n[ Test 3: Multiplication ]\n");

	mul_sfp = sfp_mul(input_sfp[0], input_sfp[1]);
	printf("    "U24_TO_BIN_P" * "U24_TO_BIN_P" = "U24_TO_BIN_P"\n",
		U24_TO_BIN(input_sfp[0]), U24_TO_BIN(input_sfp[1]), U24_TO_BIN(mul_sfp));

	return 0;
}
