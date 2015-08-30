#include "../AVR/Bits.h"
#include "../AVR/Delay.h"
#include "Seg7.h"

typedef pin_t<PD, 3> D0;
typedef pin_t<PC, 2> D1;
typedef pin_t<PB, 1> D2;
typedef pin_t<PD, 7> D3;

typedef pin_t<PD, 4> A;
typedef pin_t<PB, 2> B;
typedef pin_t<PD, 5> C;
typedef pin_t<PD, 1> D;
typedef pin_t<PD, 0> E;
typedef pin_t<PC, 3> F;
typedef pin_t<PD, 6> G;
typedef pin_t<PD, 2> H;

typedef bits_t<D0, D1, D2, D3> digit_t;
typedef bits_t<A, B, C, D, E, F, G, H> segment_t;
typedef seg7_t<digit_t, segment_t> seg7;

void setup()
{
	seg7::setup();
}

void loop()
{
	static uint16_t i = 0;
	static uint8_t j = 0, k = 0;
	static char chars[4] = { '4', '5', '6', '7' };
	static const uint8_t nchar = sizeof(chars) / sizeof(*chars);
	static char *s = " 0123456789ABCDEFGHIJKLMNOPQRSTUVXYZ    ";

	if ((++j & 0xff) == 0)
	{
		seg7::write(s + i);
		if (++i > 36)
			i = 0;
	}

	seg7::refresh();

	delay_ms(1);
}

int main()
{
	setup();
	for (;;)
		loop();
}
