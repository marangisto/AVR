#include <AVR/Bits.h>
#include <AVR/Delay.h>
#include <AVR/Seg7.h>

typedef output_t<PD, 3> D0;
typedef output_t<PC, 2> D1;
typedef output_t<PB, 1> D2;
typedef output_t<PD, 7> D3;

typedef output_t<PD, 4> A;
typedef output_t<PB, 2> B;
typedef output_t<PD, 5> C;
typedef output_t<PD, 1> D;
typedef output_t<PD, 0> E;
typedef output_t<PC, 3> F;
typedef output_t<PD, 6> G;
typedef output_t<PD, 2> H;

typedef bits_t<D0, D1, D2, D3> digit_t;
typedef bits_t<A, B, C, D, E, F, G, H> segment_t;
typedef seg7_t<digit_t, segment_t> seg7;

void setup()
{
	seg7::setup();
	seg7::auto_refresh();
}

void loop()
{
	static uint16_t i = 0;
	static uint8_t j = 0, k = 0;
	static char chars[4] = { '4', '5', '6', '7' };
	static const uint8_t nchar = sizeof(chars) / sizeof(*chars);
	static char *s = " 0123456789ABCDEFGHIJKLMNOPQRSTUVXYZ    ";

	seg7::write(i++);

	delay_ms(5);
}

int main()
{
	setup();
	for (;;)
		loop();
}

