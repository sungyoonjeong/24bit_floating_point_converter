#include "sfp.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#define SFP_BIAS		63 // (k=7, Bias = 2^(7-1)-1)
#define SFP_BIT			24 // SFP 전체 비트수
#define SFP_FRAC		16 // SFP fraction 비트 수
#define SFP_EXP			7  // SFP exponent 비트 수

sfp int2sfp(int input){
	// Type Declaration //
	sfp exp = 0;    // exp 변수 설정
	sfp frac = 0;   // frac 변수 설정
	unsigned int mask = 0x1; // masking 할 mask 0000 0000 0000 0000 0000 0000 0000 0001 설정
	sfp ret = 0;  // return 값 설정
	sfp i = 0;    // for문에 쓰일 i 설정

	// Negative Input //
	if(input < 0){     // input 값이 0보다 작을 경우
		ret = ret | 0x800000; // return 값과 1000 0000 0000 0000 0000 0000 0000 0000 OR 연산 => sign 비트에 1을 넣기 위함
		input = -input;    // input에 -input 저장
	}
	// Zero Input //
	else if(input == 0) return ret;  // input 값이 0일경우 return값(0)그대로 출력

	// Find Max Radix //  => 1이 존재하는 최대 비트 위치 파악
	for(exp = 30; exp > 0; exp--)     // exp를 30 으로 설정(int는 sign 비트를 제외하고 총 31비트)
		if((mask << exp) & input) break;  
	// mask(0x1를 exp(처음:30) 만큼 left shift 함으로써 31번재 비트 부터 확인작업실시, input값과 and 연산한 값이 1일경우 break;
	// exp 값을 구할 수있음

	// Mask Frac Bit // => frac 비트 파악
	for(i = 0; i < SFP_FRAC ; i++){  // 0번재 비트부터 SFP_FRAC(16비트) 까지 확인 작업 실시 
		if((exp - i - 1 >=0) && (input & (mask << (exp -i -1))))
			//exp(지수값)-1-i이 0보다 큰지 확인, mask(1)을 (exp-i-1)만큼 leftshift => 두개를 and연산해서 1일경우 frac비트에 1을 복사 
			frac = frac | 0x01;
		if(i != SFP_FRAC -1) frac = frac << 1;
		// 1일 아닐경우 frac비트를 1개씩 leftshift	
	}
	ret = ret | frac;  //완성된 frac비트를 return 값에 마스킹

	// Mask Exp Bit //
	exp = exp + SFP_BIAS;  //exp = E + BIAS
	exp = exp << SFP_FRAC; //exp 를 frac비트 만큼 leftshift(exp는 17번째 비트부터이기 때문에)
	ret = ret | exp;	   //return 값에 exp값 저장 => return값 = frac + exp

	return ret;
}

int sfp2int(sfp input){
	// denormalized
	// 1)sign = 1, exp = 11...1, frac = 0/!=0 => TMin
	// 2)sign = 0, exp = 11...1, frac = 0 => Tmax
	// 3)sign = 0, exp = 11...1, frac != 0 => NaN(TMin)
	// 4)exp = 0 = > ret = 0

	// sfp 표현 가능하지만, int범위에 맞지 않는 경우
	// 1) int 최소값 : 2^-31 => 1 101 1110 0000 0000 0000 0000 일경우 TMin
	// 2) input < 2^-31 => TMin
	// 3) input > 2^31 -1 => exp가 32+bias될때부터 => exp 가 95 (1011111)보다 크면 TMax

	// V = (-1)^s * M * 2^E
	// M * 2^E는 BINARY 상으로 1 XXXX(XXXX는 fraction bit)
	// (-1)^s는 MSB를 구하여 ret 값에 바로 적용 시킴

	sfp exp = 0;
	sfp frac = 0;
	sfp mask = 0x8000; 
	int ret = 0;
	sfp i;

	// Zero Input //
	if(input ==0) return 0;

	exp = (input & 0x7f0000)>>16;
	frac = input & 0xffff;

	// Denormalized Case //
	if(exp == 127){ //exp 비트가 1111111일경우
		if((frac ==0) && ((input & 0x800000) == 0)) ret = 0x7fffffff; // Infinity일경우
		else ret = 0x80000000; // -Infinity, NaN 일경우
	}
	else if(exp == 0){ // exp가 0일경우 항상 1보다 작은수 이므로 0으로 반환 
		ret = 0;
	}
	// Denormalized Case End //

	// Int로 표현할수 없는 범위 //
	if(exp > 95){ // sfp input이 int 최대값(2^31 -1)보다 클경우
		ret = 0x7fffffff;
	}
	if (input == 0xde0000){ //sfp input이 int 최소값(-2^31)과 작거나 같을경우(exp값은 94, frac값이 다른경우)
		if((input & 0xffff) == 0){
			ret = 0x80000000;
		}
		else{
			return 0x80000000;
		}
	}

	// Normalized Case //
	else{ // M * 2^E 구함
		for(ret = 1, i=0 ; i < exp - SFP_BIAS; i++){
			ret = ret << 1;
			if(mask & frac) ret = ret | 0x1;
			mask = mask >> 1;
		}
	}
	// Negative Input //
	if ((input & 0x800000) == 1) ret = -ret;

	return ret;
}

sfp float2sfp(float input){
	sfp sign = 0;
	sfp exp = 0;
	sfp frac = 0;
	sfp ret = 0;
	int in;
	int e_mask = 0x7f800000;
	int f_mask = 0x007fffff;

	memcpy((void*)(&in), (void*)(&input), sizeof(float));
	// Negative Input //
	if(in & 0x80000000){
		ret = ret | 0x800000;
	}
	// Zero Input //
	else if(in == 0) return ret;

	// Mask Exp Bit //
	exp = (in & e_mask)>>23;
	frac = in & f_mask;

	if(exp == 255){
		if(frac == 0){
			if((in & 0x80000000) == 0) ret = 0x7fffff;
			else ret = 0xffffff;
		}
		else ret = (0x7f800000 + frac);
	}
	else if(exp == 0) ret = 0;
	else{
		exp = exp - 127;
		exp = exp + 63;
		exp = exp << 16;
		ret = ret | exp;

		// Mask Frac Bit //
		frac = frac >> 7;
		ret = ret | frac;      //answer3 에서 답이 다름.

		return ret;
	}
}
float sfp2float(sfp input){
	sfp exp = 0;
	sfp frac = 0;
	float ret = 0;
	int x;
	int y;
	int e_mask = 0x7f0000;
	int f_mask = 0xffff;

	memcpy((void*)(&x), (void*)(&input), sizeof(float));
	memcpy((void*)(&y), (void*)(&ret), sizeof(float));

	// Negative Input //
	if(x & 0x800000){
		y= y | 0x80000000;
	}

	// Zero Input //
	if(x == 0) return ret;

	// Mask Exp Bit //
	exp = (x & e_mask)>>SFP_FRAC;
	exp = exp - SFP_BIAS;
	exp = exp + 127; // float bias = 127
	exp = exp << 23; // float frac = 23
	y = y | exp;

	// Mask Frac Bit //
	frac = (x & f_mask)<<7;
	y = y | frac;

	memcpy((void*)(&ret), (void*)(&y), sizeof(float));
	return ret;
}

int roundtoeven(int input, int num){ // round to even 선언 , frac = input, num = shift right할 수
	int a;
	memcpy((void*)(&a), (void*)(&input), sizeof(float));
	int fraction = input;
	int fraction_temp = fraction;
	int i;
	int G,R,S;
	int check = 0x01;

	for(i=0 ; i<num-1;i++)
	{
		check = (check<<1) + 0x01;
	}
	S = check & fraction; 
	// shift left 하여 버려지는 frac bit를 S bit로 저장하기 위해 num-1만큼 계속해서 11...1만들며 check에 저장 
	// check와 fraction을 & 연산하여 frac 비트가 1이 있을경우 S는 1이다.
	for(i=0; i<num ; i++){
		fraction = fraction >> 1; 
		G = fraction & 0x1; 
		R = fraction_temp & 0x1;
		fraction_temp = fraction_temp >> 1; //G=shift right을 한 후 마지막 bit를 저장, R=shift right 하기전 마지막 bit저장
	}
	if (G==1){ //G가 1일경우
		if(R==0) return input >> num; // xxx0 / 0xxx 형식 따라서, 버림
		else if(R==1)return (input >> num) + 1; //R이 1이면 올림.
	}
	else if(G==0){ 
		if(R==0) return input >> num; // 버림 연산 (G, R 이 0)
		else if(R== 1){
			if(S==0)return input >> num; // 버림 (R=1, S=0)
			else if(S!=0) return (input >>num)+1; // 올림(R=1, S=1)
		}
	}
	else return input; //그외에는 shift가 일어나지 않으므로 input그대로 출력
}

sfp sfp_add(sfp in1, sfp in2){
	// sfp input1, sfp input2 덧셈
	// floating point 덧셈 방법    
	// 1) 지수를 같게 조정하고, 덧셈을 실행한다.(즉, 지수가 작은 쪽을 지수가 큰쪽의 수로 조정한다)
	// 2) 덧셈을 수행
	// 3) 계산 결과 가수를 정규화 한다.
	// 4) 이에 맞게 지수값 조정


	// Special Case //
	// 1. (+무한) + (+무한) = (+무한)
	// 2. (+무한) + (-무한) = NaN(TMin)
	// 3. (+무한) + normal value = +무한
	// 4. (-무한) + (-무한) = (-무한)
	// 5. (-무한) + normal value = -무한
	// 6. 둘중에 하나만 NaN이라면 NaN(Tmin)으로 출력
	int sign_in1, sign_in2,sign;
	int exp_in1, exp_in2, exp_diff, exp;
	int E_in1, E_in2, E_diff, E;
	int frac_in1, frac_in2, frac;

	sign_in1 = (in1 & 0x800000) >> 23;
	sign_in2 = (in2 & 0x800000) >> 23;

	exp_in1 = (in1 & 0x7f0000) >> 16;
	exp_in2 = (in2 & 0x7f0000) >> 16;

	E_in1 = exp_in1 - 63;
	E_in2 = exp_in2 - 63; //지수값 구하기

	frac_in1 = in1 & 0xffff;
	frac_in2 = in2 & 0xffff;

	// special case //
	if(sign_in1 == 0 && E_in1 == 64 && frac_in1 == 0){ // in1 = +infinity
		if(sign_in2 == 0 && E_in2 == 64 && frac_in1 ==0) return in1; // in2 = +infinity => in1 
		else if(sign_in2 == 1 && E_in2 == 64 && frac_in2 == 0) return 0x7f0001; //in2 = -infinity => NaN
		else if(E_in2 == 64 && frac_in2 !=0) return in2; //in2 = NaN => in2
		else return in1;
	}
	else if(sign_in1 == 1 && E_in1 == 64 && frac_in1 == 0){ //in1 = -infinity
		if(sign_in2 == 0 && E_in2 == 64 && frac_in2 == 0) return 0x7f0001; //in2 = +infinity => NaN
		else if(sign_in2 == 1 && E_in2 == 64 && frac_in2 == 0) return in1; //in2 = -infinity => in1
		else if(E_in2 == 64 && frac_in2 != 0) return in2; // in2 = NaN => in2
		else return in1;
	}
	else if(E_in1 == 64 && frac_in1 != 0) return in1; //in1 = NaN => in1
	// special case end //

	if(E_in1 > -63 && exp_in1 != 64) frac_in1 = frac_in1 + 0x10000;
	if(E_in2 > -63 && E_in2 != 64) frac_in2 = frac_in2 + 0x10000;

	if(E_in1 > E_in2){ //in1의 지수값이 in2의 지수값보다 클떄
		E_diff = E_in1 - E_in2; //지수값의 차이 => 작은 지수값을 가진 수의 frac을 오른쪽으로 shift할때 사용
		frac_in2 = roundtoeven(frac_in2, E_diff); // frac_in2 roundtoeven

		if(sign_in1 == sign_in2){ //부호가 같을경우
			sign = sign_in1; //input중 하나의 부호 적용
			E = E_in1; //큰 지수값의 지수를 따른다.
			frac = frac_in1 + frac_in2; //frac을 더함
			if(frac >= 0x20000){ // frac 값이 10.xxxxx(일반적으로 1.xxxx) 보다 크거나 같아질경우
				frac = frac >> 1; // frac을 오른쪽으로 쉬프트
				E++; // 지수값을 + 1해준다.
			}
		}
		else if(sign_in1 != sign_in2){ //부호가 다를경우 frac의 차를 구한다.
			sign = sign_in1; 
			E = E_in1;
			frac = frac_in1 - frac_in2; 

			while(frac < 0x10000){ //frac이 0.000..1xx일경우 1.xxxx가 될떄까지 쉬프트 연산 E는 쉬프트한 만큼 뺴준다.
				frac= frac << 1; 
				E--;
			}
		}
	}

	else if(E_in1 < E_in2){ //in2의 지수가 더 클경우 위와 똑같이 해준다.
		E_diff = E_in2 - E_in1;
		frac_in1 = roundtoeven(frac_in1, E_diff);

		if(sign_in1 == sign_in2){
			sign = sign_in1;
			E = E_in2;
			frac = frac_in1 + frac_in2;

			if(frac >= 0x20000){
				frac = frac >> 1;
				E++;
			}
		}
		else if(sign_in1 != sign_in2){
			sign = sign_in2;
			E = E_in2;
			frac = frac_in2 - frac_in1;

			while(frac < 0x10000){
				frac = frac << 1;
				E--;
			}
		}
	}
	else{ //지수가 같을경우
		if(sign_in1 == sign_in2){//부호가 같을경우 frac끼리의 합을 구한다.
			sign = sign_in1;
			E = E_in1;
			frac = frac_in1 + frac_in2;

			if(frac >= 0x20000){  //위에서와 같이 frac 비트 조작
				frac = frac >> 1;
				E++;
			}
			while(frac < 0x10000){
				frac = frac << 1;
				E--;
			}
		}
		else if(sign_in1 != sign_in2){ //부호가 다를경우
			int diff = frac_in2 - frac_in1; //frac의 차이가 
			if(diff < 0){					//음수이면
				sign = sign_in1;            // in1의 크기가 더 크므로 in1의 부호를 따라간다.
				frac = -diff;
				while(frac < 0x10000){      
					frac = frac << 1;
					E--;
				}
			}
			else if(diff >0){
				sign = sign_in2;
				frac = diff;
				while(frac < 0x10000){
					frac = frac << 1;
					E--;
				}
			}
			else return 0;
		}
	}
	if(E > -63){   // 계산 결과가 정규화일 경우
		return sign | ((E+63)<<16) | (frac - 0x10000); 
		// 1.xxx 형태의 frac에서 1을 뺴주고, exp를 만들어서 |연산을 이용하여 return
	}
	else{ //비정규화 형태일경우(E가 -63보다 같거나 작을경우)
		int diff = -E - 63 + 1; 
		frac = roundtoeven(frac, diff);
		return sign | ((E + 63)<<16)|frac;
	}
}
sfp sfp_mul(sfp in1, sfp in2){
	int sign_in1, sign_in2,sign;
	int exp_in1, exp_in2, exp;
	int E_in1, E_in2, E_diff, E;
	int frac_in1, frac_in2, frac;
	int a,b;

	memcpy((void*)(&a), (void*)(&in1), sizeof(int));
	memcpy((void*)(&b), (void*)(&in2), sizeof(int));

	sign_in1 = (a & 800000) >> 23;
	sign_in2 = (b & 800000) >> 23;

	exp_in1 = (a & 0x7f0000);
	exp_in2 = (b & 0x7f0000);

	E_in1 = (exp_in2 >> 16) - 63;
	E_in2 = (exp_in2 >> 16) - 63;

	frac_in1 = a & 0xffff;
	frac_in2 = a & 0xffff;

	// Special case //
	if(((in1 & 0x800000) == 0) && (E_in1 == 64) && (frac_in1 == 0)) {
		if(((in2 & 0x800000) == 0) && (E_in2 == 64) && (frac_in2 == 0)) return in1;
		else if(((in2 & 0x800000) != 0) && (E_in2 == 64) && (frac_in2 == 0)) return in2;
		else if((E_in2 == 64) && (frac_in2 != 0)) return in2;
		else if(in2 == 0x000000) return 0x7f0001;
		else if(in2 == 0x800000) return 0x7f0001;
		else if((in2 & 0x800000) == 0) return in1;
		else if((in2 & 0x800000) != 0) return 0xff0000;
	}
	else if(((in1 & 0x800000) != 0) && (E_in1 == 64) && (frac_in1 == 0)) {
		if(((in2 & 0x800000) == 0) && (E_in2 == 64) && (frac_in1 == 0)) return in1;
		else if(((in2 & 0x800000) != 0) && (E_in2 == 64) && (frac_in2 == 0)) return 0x7f0000;
		else if((E_in2 == 64) && (frac_in2 != 0)) return in2;
		else if(in2 == 0x000000) return 0x7f0001;
		else if(in2 == 0x800000) return 0x7f0001;
		else if((in2 & 0x800000) == 0) return in1;
		else if((in2 & 0x800000) != 0) return 0x7f0000;
	}
	else if((E_in1 == 0) && (frac_in1 != 0)) {
		return in1;
	}
	else if((in1 == 0x000000) || (in1 == 0x800000)) {
		if((E_in2 == 64) && (frac_in2 == 0)) return 0x7f0001;
		else if((E_in2 == 64) && (frac_in2 != 0)) return in2;
		else return 0;
	}
	//
	if(E_in1 > -63 && E_in1 != 64) frac_in1 = frac_in1 + 0x10000;
	if(E_in2 > -63 && E_in2 != 64) frac_in2 = frac_in2 + 0x10000;

	sign = sign_in1 ^ sign_in2; // sign 은 xor을 이용하여 구함.
	E = E_in1 + E_in2;			// 지수값은 더해준다.
	frac = frac_in1 * frac_in2; // frac값은 곱해준다.

	int frac_temp = frac;
	int cnt = 0;
	if(frac >= 0x20000){//frac이 111.xxxx의 형태일경우 오른쪽으로 shift하면서 frac_temp 에 저장하고 cnt에 shift횟수 저장
		while(frac_temp >= 0x20000){
			frac_temp = frac_temp >> 1;
			E++;
			cnt++;
		}
		frac = roundtoeven(frac,cnt); //shift횟수 cnt 만큼 roundtoeven
	}
	else{
		while(frac < 0x10000){ //0.000xxx의 형태일경우 shift left 시행
			frac = frac <<1;
			E--;
		}
	}
	E = E-16;
	if(E > -63){
		return (sign<<23) | ((E + 63) << 16) | (frac - 0x10000);
	}
	else{
		int diff = -E - 63 + 1;
		frac = roundtoeven(frac,diff);
		return (sign<<23) | ((E+63) << 16) | frac;
	}
}



