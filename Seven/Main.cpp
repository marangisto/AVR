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

void setup()
{
	digital_out<D0>();
	digital_out<D1>();
	digital_out<D2>();
	digital_out<D3>();

	write_bits<digit_t>(~0);

	digital_out<A>();
	digital_out<B>();
	digital_out<C>();
	digital_out<D>();
	digital_out<E>();
	digital_out<F>();
	digital_out<G>();
	digital_out<H>();

	write_bits<segment_t>(~0);
}

void loop()
{
	static uint16_t i = 0;
	static uint8_t j = 0, k = 0;
	static char chars[4] = { '4', '5', '6', '7' };
	static const uint8_t nchar = sizeof(chars) / sizeof(*chars);

	if ((++j & 0x7f) == 0)
	{
		uint16_t a = i;

		for (uint8_t c = 0; c < nchar; ++c)
		{
			chars[nchar - c - 1] = '0' + a % 10;
			a /= 10;
		}

		chars[0] = (i & 0x7f) >= ' ' ? ;

		++i;
	}

	write_bits<digit_t>(~0);
	write_bits<segment_t>(~font_t::seg7(chars[k]));
	write_bits<digit_t>(~(1 << k));

	if (++k >= nchar)
		k = 0;

	delay_ms(1);
}

int main()
{
	setup();
	for (;;)
		loop();
}

